// Helper function to safely check if Shiny is fully initialized
function isShinyReady() {
    return typeof Shiny !== 'undefined' && 
           typeof Shiny.setInputValue === 'function';
  }
  
  // Helper function to safely handle progress updates
  function safeSetProgress(value, message) {
    try {
      // Try different methods to update progress
      if (typeof Shiny !== 'undefined') {
        // Method 1: Try via session
        if (typeof Shiny.shinyapp !== 'undefined' && 
            typeof Shiny.shinyapp.session !== 'undefined' && 
            typeof Shiny.shinyapp.session.sendCustomMessage === 'function') {
          Shiny.shinyapp.session.sendCustomMessage('shiny-set-progress', {
            value: value,
            message: message || null,
            detail: null
          });
          return true;
        }
        
        // Method 2: Try direct notification API
        if (typeof Shiny.notification !== 'undefined' && 
            typeof Shiny.notification.show === 'function') {
          if (value >= 1) {
            // Close all notifications
            if (typeof Shiny.notification.remove === 'function') {
              Shiny.notification.remove('validate_notif');
            }
          } else {
            // Update notification
            Shiny.notification.show({
              id: 'validate_notif',
              type: 'message',
              message: message || 'Processing...'
            });
          }
          return true;
        }
      }
      
      return false;
    } catch (e) {
      console.warn('Failed to update progress:', e);
      return false;
    }
  }
  
  // Flag to track if validation has failed
  var validationFailed = false;
  
  // Function to reset the validation checklist
  function resetValidationChecklist() {
    console.log('Resetting validation checklist UI');
    
    // Reset validation failed flag
    validationFailed = false;
    
    // Reset all step icons to waiting state
    document.querySelectorAll('[data-step-key] .step-icon').forEach(function(icon) {
      icon.textContent = "⋯";
      icon.style.color = "#ffc107";
    });
    
    // Reset all step messages to imperative form
    document.querySelectorAll('[data-step-key] .step-message').forEach(function(message) {
      if (message.hasAttribute('data-imperative')) {
        message.textContent = message.getAttribute('data-imperative');
      }
    });
    
    // Clear all issue containers
    document.querySelectorAll('.step-issue-container').forEach(function(container) {
      container.innerHTML = '';
    });
    
    // Hide summary section
    var summarySection = document.getElementById('validate_dataset-validation_summary');
    if (summarySection) {
      summarySection.style.display = 'none';
    }
  }
  
  // Function to initialize our validation handler
  function initValidator() {
    console.log('Initializing validation handler...');
    
    // Check if validator is loaded
    var validatorLoaded = typeof window.psychDSValidator !== 'undefined' && 
                         typeof window.psychDSValidator.validateWeb === 'function';
    
    console.log('Validator loaded status:', validatorLoaded);
    
    // Only proceed if both Shiny and the validator are ready
    if (isShinyReady()) {
      // Notify Shiny when the validator is loaded
      Shiny.setInputValue('validator_loaded', validatorLoaded, {priority: 'event'});
      
      // Handler for resetting validation UI
      Shiny.addCustomMessageHandler('reset_validation_ui', function(message) {
        resetValidationChecklist();
      });
      
      // Set up a single, clean handler for validation requests
      Shiny.addCustomMessageHandler('run_validation', function(fileData) {
        console.log('Validation requested with data keys:', Object.keys(fileData));
        
        // Reset the checklist UI before starting a new validation
        resetValidationChecklist();
        
        // Reset the validation failed flag
        validationFailed = false;
        
        if (validatorLoaded) {
          console.log('Running validation with provided file data');
          
          // Convert any string text values to functions
          // This is necessary because R functions don't serialize properly to JavaScript
          console.log('Processing file tree to ensure text fields are functions');
          processFileTree(fileData);
          
          // Create event emitter for tracking validation progress
          var eventEmitter = new EventEmitter3();
          
          // Create a shared step status object to track validation progress
          var stepStatus = {};
  
          // Define the hierarchical structure of steps - must match the TypeScript implementation
          var validationSteps = [
            {
              key: "start",
              message: {
                imperative: "Start validation",
                pastTense: "Validation started"
              },
              subSteps: []
            },
            {
              key: "check-folder",
              message: {
                imperative: "Find project folder",
                pastTense: "Project folder found"
              },
              subSteps: [
                {
                  key: "build-tree",
                  message: {
                    imperative: "Crawl project folder and construct file tree",
                    pastTense: "Project folder crawled and file tree constructed"
                  }
                }
              ]
            },
            {
              key: "find-metadata",
              message: {
                imperative: "Find metadata file",
                pastTense: 'Metadata file "dataset_description.json" found in the root folder'
              },
              subSteps: []
            },
            {
              key: "find-data-dir",
              message: {
                imperative: 'Find "data" subfolder',
                pastTense: '"data" subfolder found in the root folder'
              },
              subSteps: []
            },
            {
              key: "parse-metadata",
              message: {
                imperative: 'Parse "dataset_description.json" metadata file',
                pastTense: 'Successfully parsed "dataset_description.json" metadata file'
              },
              subSteps: [
                {
                  key: "metadata-utf8",
                  message: {
                    imperative: "Check metadata file for utf-8 encoding",
                    pastTense: "Metadata file is utf-8 encoded"
                  }
                },
                {
                  key: "metadata-json",
                  message: {
                    imperative: "Parse metadata file as JSON",
                    pastTense: "Metadata file parsed successfully"
                  }
                },
                {
                  key: "metadata-jsonld",
                  message: {
                    imperative: "Validate metadata file as JSON-LD",
                    pastTense: "Metadata file is valid JSON-LD"
                  }
                },
                {
                  key: "metadata-fields",
                  message: {
                    imperative: 'Check metadata file for required "name", "description", and "variableMeasured" fields',
                    pastTense: 'Metadata file contains required "name", "description", and "variableMeasured" fields.'
                  }
                },
                {
                  key: "metadata-type",
                  message: {
                    imperative: 'Check metadata file for field "@type" with value "Dataset"',
                    pastTense: 'Metadata file has "@type" field with value "Dataset"'
                  }
                }
              ]
            },
            {
              key: "check-for-csv",
              message: {
                imperative: 'Check for CSV data files in "data" subfolder',
                pastTense: 'CSV data files found in "data" subfolder'
              },
              subSteps: []
            },
            {
              key: "validate-csvs",
              message: {
                imperative: 'Check that all CSV data files are valid',
                pastTense: 'All CSV data files are valid'
              },
              subSteps: [
                {
                  key: "csv-keywords",
                  message: {
                    imperative: 'Check filename for keyword formatting ',
                    pastTense: 'Filename uses valid keyword formatting'
                  }
                },
                {
                  key: "csv-parse",
                  message: {
                    imperative: 'Parse data file as CSV',
                    pastTense: 'Data file successfully parsed as CSV'
                  }
                },
                {
                  key: "csv-header",
                  message: {
                    imperative: 'Check for header line',
                    pastTense: 'Header line found'
                  }
                },
                {
                  key: "csv-nomismatch",
                  message: {
                    imperative: 'Check all lines for equal number of cells',
                    pastTense: 'All lines have equal number of cells'
                  }
                },
                {
                  key: "csv-rowid",
                  message: {
                    imperative: 'Check for any row_id columns with non-unique values',
                    pastTense: 'All row_id columns have unique values'
                  }
                }
              ]
            },
            {
              key: "check-variableMeasured",
              message: {
                imperative: 'Confirm that all column headers in CSV data files are found in "variableMeasured" metadata field',
                pastTense: 'All column headers in CSV data files were found in "variableMeasured" metadata field'
              },
              subSteps: []
            }
          ];
  
          // Initialize all steps as incomplete
          function initializeStepStatus() {
            validationSteps.forEach(function(superStep) {
              stepStatus[superStep.key] = { complete: false, success: false };
              
              if (superStep.subSteps && superStep.subSteps.length > 0) {
                superStep.subSteps.forEach(function(subStep) {
                  stepStatus[subStep.key] = { complete: false, success: false };
                });
              }
            });
          }
          
          initializeStepStatus();
  
          // Function to update UI elements directly
          function updateStepUI(stepKey, success, issue) {
            // If validation has already failed, don't update any more steps
            if (validationFailed && !stepStatus[stepKey].complete) {
              console.log(`Skipping UI update for ${stepKey} because validation already failed`);
              return;
            }
            
            // Find the step element by its data attribute
            var stepElement = document.querySelector(`[data-step-key="${stepKey}"]`);
            if (!stepElement) {
              console.log(`DOM element not found for step: ${stepKey}`);
              return;
            }
            
            console.log(`Updating UI for step: ${stepKey}, success: ${success}`);
            
            // Update the icon
            var iconElement = stepElement.querySelector('.step-icon');
            if (iconElement) {
              iconElement.textContent = success ? "✓" : "✗";
              iconElement.style.color = success ? "#4caf50" : "#f44336";
            }
            
            // Update the message text
            var messageElement = stepElement.querySelector('.step-message');
            if (messageElement && messageElement.hasAttribute('data-past-tense')) {
              messageElement.textContent = messageElement.getAttribute('data-past-tense');
            }
            
            // Handle issues
            if (issue) {
              var issueContainer = stepElement.querySelector('.step-issue-container');
              if (issueContainer) {
                issueContainer.innerHTML = '';
                var issueElement = document.createElement('div');
                issueElement.className = 'step-issue';
                issueElement.style.marginTop = '10px';
                issueElement.style.color = '#f44336';
                issueElement.style.paddingLeft = '20px';
                
                var strongEl = document.createElement('strong');
                strongEl.textContent = 'Issue: ';
                issueElement.appendChild(strongEl);
                issueElement.appendChild(document.createTextNode(issue.reason || 'Unknown error'));
                
                // Add evidence if available
                if (issue.files && issue.files.size > 0) {
                  var evidenceElement = document.createElement('div');
                  evidenceElement.className = 'step-evidence';
                  evidenceElement.style.marginTop = '5px';
                  evidenceElement.style.color = '#666';
                  evidenceElement.style.paddingLeft = '20px';
                  evidenceElement.style.fontSize = '0.9em';
                  
                  // Create a formatted list of evidence items
                  var evidenceList = document.createElement('ul');
                  evidenceList.style.marginTop = '5px';
                  evidenceList.style.paddingLeft = '20px';
                  
                  // Iterate through the files Map to get evidence
                  issue.files.forEach(function(fileInfo, filePath) {
                    if (fileInfo && fileInfo.evidence) {
                      var evidenceItem = document.createElement('li');
                      evidenceItem.textContent = `${fileInfo.name}: ${fileInfo.evidence}`;
                      evidenceList.appendChild(evidenceItem);
                    }
                  });
                  
                  if (evidenceList.childNodes.length > 0) {
                    evidenceElement.appendChild(document.createElement('strong')).textContent = 'Evidence: ';
                    evidenceElement.appendChild(evidenceList);
                    issueElement.appendChild(evidenceElement);
                  }
                }
                
                issueContainer.appendChild(issueElement);
              }
            }
          }
          
          // Function to update a superstep based on its substeps
          function updateSuperStepStatus(superStep) {
            if (!superStep.subSteps || superStep.subSteps.length === 0) return;
            
            // If validation has already failed, don't update any more steps
            if (validationFailed && !stepStatus[superStep.key].complete) {
              console.log(`Skipping superstep update for ${superStep.key} because validation already failed`);
              return;
            }
            
            var allSubStepsComplete = superStep.subSteps.every(function(subStep) {
              return stepStatus[subStep.key] && stepStatus[subStep.key].complete;
            });
            
            var allSubStepsSuccess = superStep.subSteps.every(function(subStep) {
              return stepStatus[subStep.key] && stepStatus[subStep.key].success;
            });
            
            stepStatus[superStep.key] = {
              complete: allSubStepsComplete,
              success: allSubStepsSuccess
            };
            
            // Update the UI for the superstep
            updateStepUI(superStep.key, allSubStepsSuccess, null);
          }
  
          // Set up listeners for all steps and update the UI accordingly
          validationSteps.forEach(function(superStep) {
            if (!superStep.subSteps || superStep.subSteps.length === 0) {
              // For supersteps without substeps
              eventEmitter.on(superStep.key, function(data) {
                console.log(`Step event received for ${superStep.key}:`, data);
                
                // If validation has already failed, don't update any more steps
                if (validationFailed) {
                  console.log(`Skipping update for ${superStep.key} because validation already failed`);
                  return;
                }
                
                // Update status for this step
                stepStatus[superStep.key] = {
                  complete: true,
                  success: data.success,
                  issue: data.issue
                };
                
                // Update the UI directly
                updateStepUI(superStep.key, data.success, data.issue);
                
                // If this step failed, set the validation failed flag
                if (!data.success) {
                  validationFailed = true;
                  
                  // Send the halt notification to Shiny
                  if (isShinyReady()) {
                    Shiny.setInputValue('validation_halted', true, {priority: 'event'});
                  }
                }
                
                // Send updates to Shiny
                if (isShinyReady()) {
                  Shiny.setInputValue('validation_step_status', {
                    stepStatus: Object.entries(stepStatus)
                  }, {priority: 'event'});
                }
              });
            } else {
              // For supersteps with substeps
              superStep.subSteps.forEach(function(subStep) {
                eventEmitter.on(subStep.key, function(data) {
                  console.log(`Substep event received for ${subStep.key}:`, data);
                  
                  // If validation has already failed, don't update any more steps
                  if (validationFailed) {
                    console.log(`Skipping update for ${subStep.key} because validation already failed`);
                    return;
                  }
                  
                  // Update status for this substep
                  stepStatus[subStep.key] = {
                    complete: true,
                    success: data.success,
                    issue: data.issue
                  };
                  
                  // Update the UI directly
                  updateStepUI(subStep.key, data.success, data.issue);
                  
                  // Update the parent superstep's status
                  updateSuperStepStatus(superStep);
                  
                  // If this step failed, set the validation failed flag
                  if (!data.success) {
                    validationFailed = true;
                    
                    // Send the halt notification to Shiny
                    if (isShinyReady()) {
                      Shiny.setInputValue('validation_halted', true, {priority: 'event'});
                    }
                  }
                  
                  // Send updates to Shiny
                  if (isShinyReady()) {
                    Shiny.setInputValue('validation_step_status', {
                      stepStatus: Object.entries(stepStatus)
                    }, {priority: 'event'});
                  }
                });
              });
            }
          });
  
          // Also listen for final events
          eventEmitter.on('complete', function() {
            console.log('Validation complete event received');
            
            if (isShinyReady()) {
              Shiny.setInputValue('validation_complete', true, {priority: 'event'});
            }
          });
  
          eventEmitter.on('validation-halted', function() {
            console.log('Validation halted event received');
            
            if (isShinyReady()) {
              Shiny.setInputValue('validation_halted', true, {priority: 'event'});
            }
          });
          
          // Now run the validation with the event emitter
          console.log('Starting validation with event listeners configured');
          window.psychDSValidator.validateWeb(fileData, {emitter: eventEmitter})
            .then(function(result) {
              console.log('Validation completed, result:', result);
              
              // Store result for summary section
              window.lastValidationResult = result;
              
              // Safety check - if result is null or undefined, create a default result
              if (!result) {
                console.warn('Warning: Validator returned null/undefined result');
                result = {
                  valid: false,
                  summary: {
                    totalFiles: countFilesInTree(fileData),
                    dataTypes: [],
                    error: 'Validator returned null result'
                  },
                  issues: {
                    errors: [{
                      key: 'VALIDATION_ERROR',
                      reason: 'The validator returned a null result. This might indicate an issue with the file structure.',
                      files: []
                    }]
                  }
                };
              }
              
              if (isShinyReady()) {
                Shiny.setInputValue('validation_results', result, {priority: 'event'});
              }
              
              // Update progress
              safeSetProgress(1, 'Complete');
              
              // Update the summary section
              updateSummary(result.valid);
            })
            .catch(function(error) {
              console.error('Validation error:', error);
              
              // Create a proper error result
              var errorResult = {
                valid: false,
                summary: {
                  totalFiles: countFilesInTree(fileData),
                  dataTypes: [],
                  error: error.message || error.toString()
                },
                issues: {
                  errors: [{
                    key: 'VALIDATION_ERROR',
                    reason: error.message || error.toString(),
                    files: []
                  }]
                }
              };
              
              if (isShinyReady()) {
                // Send both the error and a fallback result
                Shiny.setInputValue('validation_error', error.message || error.toString(), {priority: 'event'});
                Shiny.setInputValue('validation_results', errorResult, {priority: 'event'});
              }
              
              // Update progress
              safeSetProgress(1, 'Error');
            });
        } else {
          console.error('Validator not loaded');
          if (isShinyReady()) {
            Shiny.setInputValue('validation_error', 'Validator not loaded', {priority: 'event'});
          }
          
          // Update progress
          safeSetProgress(1, 'Error: Validator not loaded');
        }
      });
      
      console.log('Validation handler initialized successfully');
    } else {
      console.log('Shiny not fully loaded yet, waiting...');
      setTimeout(initValidator, 100);
    }
  }
  
  // Helper function to process a file tree and convert string text values to functions
  function processFileTree(tree) {
    if (!tree) return;
    
    Object.keys(tree).forEach(function(key) {
      var entry = tree[key];
      
      // Process file entries
      if (entry && entry.type === 'file' && entry.file) {
        // If text is a string, convert it to a function
        if (typeof entry.file.text === 'string') {
          var textContent = entry.file.text;
          entry.file.text = function() {
            return textContent;
          };
          console.log('Converted text to function for:', entry.file.path || key);
        }
      }
      
      // Process directory entries recursively
      if (entry && entry.type === 'directory' && entry.contents) {
        processFileTree(entry.contents);
      }
    });
  }
  
  // Helper function to count files in a tree
  function countFilesInTree(tree) {
    if (!tree) return 0;
    
    var count = 0;
    
    Object.keys(tree).forEach(function(key) {
      var entry = tree[key];
      
      // Count file entries
      if (entry && entry.type === 'file') {
        count++;
      }
      
      // Process directory entries recursively
      if (entry && entry.type === 'directory' && entry.contents) {
        count += countFilesInTree(entry.contents);
      }
    });
    
    return count;
  }
  
  // Function to update the summary section
  function updateSummary(isValid) {
    var summarySection = document.getElementById('validate_dataset-validation_summary');
    var summaryContent = document.getElementById('validate_dataset-summary_content');
    
    if (summarySection && summaryContent) {
      // Show the summary section
      summarySection.style.display = 'block';
      
      // Update styles and content
      summaryContent.style.backgroundColor = isValid ? '#e8f5e9' : '#ffebee';
      
      // Create summary content
      var html = '<h3 style="color: ' + (isValid ? '#4caf50' : '#f44336') + '";>';
      html += isValid ? '✓ Dataset is valid' : '✗ Dataset has validation errors';
      html += '</h3>';
      
      // Add details
      html += '<div style="margin-top: 15px;"><h4>Details:</h4><ul>';
      
      // Count files (if available from results)
      if (window.lastValidationResult && window.lastValidationResult.summary) {
        html += '<li>Total files scanned: ' + window.lastValidationResult.summary.totalFiles + '</li>';
        
        if (window.lastValidationResult.summary.dataTypes && window.lastValidationResult.summary.dataTypes.length > 0) {
          html += '<li>Data types found: ' + window.lastValidationResult.summary.dataTypes.join(', ') + '</li>';
        }
      }
      
      // Add validation status
      html += '<li style="color: ' + (isValid ? '#4caf50' : '#f44336') + '; font-weight: bold;">';
      html += isValid ? 'This dataset appears to be Psych-DS compatible' : 'This dataset does not appear to be Psych-DS compatible';
      html += '</li></ul></div>';
      
      // Add errors section if not valid
      if (!isValid && window.lastValidationResult && 
          window.lastValidationResult.issues && 
          window.lastValidationResult.issues.errors && 
          window.lastValidationResult.issues.errors.length > 0) {
        
        html += '<div style="margin-top: 15px;"><h4>Errors:</h4><ul>';
        
        window.lastValidationResult.issues.errors.forEach(function(error) {
          html += '<li><p style="font-weight: bold;">' + error.key + '</p>';
          html += '<p>' + error.reason + '</p></li>';
        });
        
        html += '</ul></div>';
      }
      
      // Update the content
      summaryContent.innerHTML = html;
    }
  }
  
  // Use the proper Shiny initialization event to start our code
  $(document).on('shiny:connected', function() {
    console.log('Shiny connected event fired');
    // Short delay to ensure everything is fully loaded
    setTimeout(initValidator, 100);
  });