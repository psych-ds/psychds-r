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

          // Process the file tree to convert text to functions
          processFileTree(fileData);

          // Validate the tree structure before sending to validator
          if (!validateFileTree || !validateFileTree(fileData)) {
            console.warn('File tree validation failed, but continuing anyway');
          }

          // Validation setup complete - ready to start
          
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
            
            // Handle issues - ONLY display if this is the first failure
            if (issue && !success) {
              var currentStepIndex = stepOrder[stepKey];
              
              // CRITICAL: Only show error if this is THE first failure
              if (currentStepIndex === firstFailedStepIndex) {
                console.log(`Showing error for ${stepKey} as it is the first failure at index ${firstFailedStepIndex}`);
                
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
              } else {
                console.log(`NOT showing error for ${stepKey} at index ${currentStepIndex} - first failure is at ${firstFailedStepIndex}`);
                // Clear any existing error display for this step
                var issueContainer = stepElement.querySelector('.step-issue-container');
                if (issueContainer) {
                  issueContainer.innerHTML = '';
                }
              }
            } else if (!issue) {
              // No issue, clear any existing error display
              var issueContainer = stepElement.querySelector('.step-issue-container');
              if (issueContainer) {
                issueContainer.innerHTML = '';
              }
            }
          }
          
          // Function to update a superstep based on its substeps
          function updateSuperStepStatus(superStep) {
            if (!superStep.subSteps || superStep.subSteps.length === 0) return;
            
            // Don't update superstep if any of its substeps come after the first failure
            if (firstFailedStepIndex !== -1) {
              var hasSubstepsAfterFailure = superStep.subSteps.some(function(subStep) {
                return stepOrder[subStep.key] > firstFailedStepIndex;
              });
              
              if (hasSubstepsAfterFailure) {
                console.log(`Skipping superstep update for ${superStep.key} because it has substeps after the failure point`);
                return;
              }
            }
            
            // Check how many substeps have reported
            var reportedSubsteps = superStep.subSteps.filter(function(subStep) {
              return stepStatus[subStep.key] && stepStatus[subStep.key].complete;
            });
            
            // Only update the superstep if ALL substeps have reported OR if there's a failure
            var hasFailure = reportedSubsteps.some(function(subStep) {
              return stepStatus[subStep.key] && !stepStatus[subStep.key].success;
            });
            
            if (reportedSubsteps.length < superStep.subSteps.length && !hasFailure) {
              console.log(`Skipping superstep update for ${superStep.key} - only ${reportedSubsteps.length}/${superStep.subSteps.length} substeps reported and no failure yet`);
              return;
            }
            
            var allSubStepsComplete = superStep.subSteps.every(function(subStep) {
              return stepStatus[subStep.key] && stepStatus[subStep.key].complete;
            });
            
            var allSubStepsSuccess = superStep.subSteps.every(function(subStep) {
              return stepStatus[subStep.key] && stepStatus[subStep.key].success;
            });
            
            // Only set the superstep as complete if all substeps are actually complete
            stepStatus[superStep.key] = {
              complete: allSubStepsComplete,
              success: allSubStepsSuccess
            };
            
            // Only update UI if the superstep is actually complete or failed
            if (allSubStepsComplete || hasFailure) {
              updateStepUI(superStep.key, allSubStepsSuccess, null);
            }
          }
  
          // Track which step failed first and its position
          var firstFailedStepIndex = -1;
          var stepOrder = {};
          var stepIndex = 0;
          
          // Build a map of step keys to their order index
          validationSteps.forEach(function(superStep) {
            stepOrder[superStep.key] = stepIndex++;
            if (superStep.subSteps && superStep.subSteps.length > 0) {
              superStep.subSteps.forEach(function(subStep) {
                stepOrder[subStep.key] = stepIndex++;
              });
            }
          });
          
          console.log('DEBUG: Step order mapping:', stepOrder);
          
          // Helper function to get step message
          function getStepMessage(stepKey, isPastTense) {
            // Find the step configuration
            for (var i = 0; i < validationSteps.length; i++) {
              var step = validationSteps[i];
              if (step.key === stepKey) {
                return isPastTense ? step.message.pastTense : step.message.imperative;
              }
              // Check substeps
              if (step.subSteps) {
                for (var j = 0; j < step.subSteps.length; j++) {
                  if (step.subSteps[j].key === stepKey) {
                    return isPastTense ? step.subSteps[j].message.pastTense : step.subSteps[j].message.imperative;
                  }
                }
              }
            }
            return '';
          }
          
          // Set up listeners for all steps and update the UI accordingly
          validationSteps.forEach(function(superStep) {
            console.log('DEBUG: Setting up listener for step:', superStep.key);
            if (!superStep.subSteps || superStep.subSteps.length === 0) {
              // For supersteps without substeps
              eventEmitter.on(superStep.key, function(data) {
                console.log(`DEBUG: Step event received for ${superStep.key}:`, data);
                console.log(`DEBUG: Current firstFailedStepIndex:`, firstFailedStepIndex);
                
                var currentStepIndex = stepOrder[superStep.key];
                
                // Check if this step comes after the first failed step
                if (firstFailedStepIndex !== -1 && currentStepIndex > firstFailedStepIndex) {
                  console.log(`DEBUG: Skipping update for ${superStep.key} (index ${currentStepIndex}) because it comes after failed step at index ${firstFailedStepIndex}`);
                  return;
                }
                
                // If this is a failure and it's earlier than any previous failure
                if (!data.success) {
                  if (firstFailedStepIndex === -1 || currentStepIndex < firstFailedStepIndex) {
                    // This is now the first failure
                    firstFailedStepIndex = currentStepIndex;
                    validationFailed = true;
                    console.log(`DEBUG: New first failure at step ${superStep.key} (index ${currentStepIndex})`);
                    
                    // CRITICAL: Remove ALL existing error displays immediately
                    console.log('DEBUG: Removing all existing error displays due to new earlier failure');
                    document.querySelectorAll('.step-issue').forEach(function(errorDiv) {
                      console.log('DEBUG: Removing error div:', errorDiv);
                      errorDiv.remove();
                    });
                    
                    // Clear all issue containers to be safe
                    document.querySelectorAll('.step-issue-container').forEach(function(container) {
                      container.innerHTML = '';
                    });
                    
                    // Clear any steps that come after this new first failure
                    Object.keys(stepStatus).forEach(function(stepKey) {
                      if (stepOrder[stepKey] > currentStepIndex && stepStatus[stepKey].complete) {
                        console.log(`DEBUG: Clearing step ${stepKey} that comes after new first failure`);
                        // Reset the UI for this step - use the correct selector
                        var stepElement = document.querySelector('[data-step-key="' + stepKey + '"]');
                        if (stepElement) {
                          var icon = stepElement.querySelector('.step-icon');
                          var message = stepElement.querySelector('.step-message');
                          if (icon) {
                            console.log('DEBUG: Resetting icon for ' + stepKey + ' to pending');
                            icon.textContent = '⋯';
                            icon.style.color = '#666666';
                          }
                          if (message && message.hasAttribute('data-imperative')) {
                            console.log('DEBUG: Resetting message for ' + stepKey + ' to imperative form');
                            message.textContent = message.getAttribute('data-imperative');
                          }
                          }
                          // Remove any issue display
                          var issueDiv = stepElement.querySelector('.step-issue');
                          if (issueDiv) {
                            issueDiv.remove();
                          }
                        }
                        // Clear from stepStatus
                        delete stepStatus[stepKey];
                      });
                    
                    // Also clear any parent supersteps that contain cleared substeps
                    validationSteps.forEach(function(step) {
                      if (step.subSteps && step.subSteps.length > 0) {
                        // Check if any of this superstep's substeps were cleared
                        var hasCleared = step.subSteps.some(function(sub) {
                          return stepOrder[sub.key] > currentStepIndex;
                        });
                        if (hasCleared) {
                          console.log(`DEBUG: Clearing parent superstep ${step.key} because its substeps were cleared`);
                          var stepElement = document.getElementById('validate_dataset-step-' + step.key);
                          if (stepElement) {
                            var icon = stepElement.querySelector('.step-icon');
                            var message = stepElement.querySelector('.step-message');
                            if (icon) {
                              icon.classList.remove('step-success', 'step-error');
                              icon.classList.add('step-pending');
                              icon.innerHTML = '⋯';
                            }
                            if (message) {
                              message.textContent = getStepMessage(step.key, false);
                            }
                            // Remove ALL issue displays (including nested ones)
                            var allIssueDivs = stepElement.querySelectorAll('.step-issue');
                            allIssueDivs.forEach(function(issueDiv) {
                              issueDiv.remove();
                            });
                            
                            // Also clear any substep containers that might have issues
                            var substepContainer = stepElement.querySelector('.substep-container');
                            if (substepContainer) {
                              // Reset all substeps in the container
                              var substepElements = substepContainer.querySelectorAll('[id^="validate_dataset-step-"]');
                              substepElements.forEach(function(subElement) {
                                var subIcon = subElement.querySelector('.step-icon');
                                var subMessage = subElement.querySelector('.step-message');
                                if (subIcon) {
                                  subIcon.classList.remove('step-success', 'step-error');
                                  subIcon.classList.add('step-pending');
                                  subIcon.innerHTML = '⋯';
                                }
                                if (subMessage) {
                                  // Get the substep key from the element ID
                                  var subKey = subElement.id.replace('validate_dataset-step-', '');
                                  subMessage.textContent = getStepMessage(subKey, false);
                                }
                                // Remove any issue displays from substeps
                                var subIssueDivs = subElement.querySelectorAll('.step-issue');
                                subIssueDivs.forEach(function(issueDiv) {
                                  issueDiv.remove();
                                });
                              });
                            }
                          }
                          // Clear from stepStatus
                          delete stepStatus[step.key];
                        }
                      }
                    });
                    
                    // Send the halt notification to Shiny
                    if (isShinyReady()) {
                      Shiny.setInputValue('validation_halted', true, {priority: 'event'});
                    }
                  } else {
                    // This failure comes after the first failure, don't display it
                    console.log(`DEBUG: Ignoring failure at ${superStep.key} (index ${currentStepIndex}) because first failure is at index ${firstFailedStepIndex}`);
                    return;
                  }
                }
                
                // Update status for this step
                stepStatus[superStep.key] = {
                  complete: true,
                  success: data.success,
                  issue: data.issue
                };
                
                // Update the UI directly
                updateStepUI(superStep.key, data.success, data.issue);
                
                // Send updates to Shiny
                if (isShinyReady()) {
                  var formattedSteps = Object.entries(stepStatus).map(function(entry) {
                    return [entry[0], entry[1]]; // [stepKey, stepData]
                  });
              
                  Shiny.setInputValue('validation_step_status', {
                    stepStatus: formattedSteps
                  }, {priority: 'event'});
                }
              });
            } else {
              // For supersteps with substeps
              superStep.subSteps.forEach(function(subStep) {
                eventEmitter.on(subStep.key, function(data) {
                  console.log(`Substep event received for ${subStep.key}:`, data);
                  
                  var currentStepIndex = stepOrder[subStep.key];
                  
                  // Check if this step comes after the first failed step
                  if (firstFailedStepIndex !== -1 && currentStepIndex > firstFailedStepIndex) {
                    console.log(`DEBUG: Skipping update for ${subStep.key} (index ${currentStepIndex}) because it comes after failed step at index ${firstFailedStepIndex}`);
                    return;
                  }
                  
                  // If this is a failure and it's earlier than any previous failure
                  if (!data.success) {
                    if (firstFailedStepIndex === -1 || currentStepIndex < firstFailedStepIndex) {
                      // This is now the first failure
                      firstFailedStepIndex = currentStepIndex;
                      validationFailed = true;
                      console.log(`DEBUG: New first failure at substep ${subStep.key} (index ${currentStepIndex})`);
                      
                      // CRITICAL: Remove ALL existing error displays immediately
                      console.log('DEBUG: Removing all existing error displays due to new earlier failure');
                      document.querySelectorAll('.step-issue').forEach(function(errorDiv) {
                        console.log('DEBUG: Removing error div:', errorDiv);
                        errorDiv.remove();
                      });
                      
                      // Clear all issue containers to be safe
                      document.querySelectorAll('.step-issue-container').forEach(function(container) {
                        container.innerHTML = '';
                      });
                      
                      // Clear any steps that come after this new first failure
                      Object.keys(stepStatus).forEach(function(stepKey) {
                        if (stepOrder[stepKey] > currentStepIndex && stepStatus[stepKey].complete) {
                          console.log(`DEBUG: Clearing step ${stepKey} that comes after new first failure`);
                          // Reset the UI for this step - use the correct selector
                          var stepElement = document.querySelector('[data-step-key="' + stepKey + '"]');
                          if (stepElement) {
                            var icon = stepElement.querySelector('.step-icon');
                            var message = stepElement.querySelector('.step-message');
                            if (icon) {
                              console.log('DEBUG: Resetting icon for ' + stepKey + ' to pending');
                              icon.textContent = '⋯';
                              icon.style.color = '#666666';
                            }
                            if (message && message.hasAttribute('data-imperative')) {
                              console.log('DEBUG: Resetting message for ' + stepKey + ' to imperative form');
                              message.textContent = message.getAttribute('data-imperative');
                            }
                            }
                            if (message) {
                              message.textContent = getStepMessage(stepKey, false);
                            }
                            // Remove any issue display
                            var issueDiv = stepElement.querySelector('.step-issue');
                            if (issueDiv) {
                              issueDiv.remove();
                            }
                          }
                          // Clear from stepStatus
                          delete stepStatus[stepKey];
                        });
                      
                      // Also clear any parent supersteps that contain cleared substeps
                      validationSteps.forEach(function(step) {
                        if (step.subSteps && step.subSteps.length > 0) {
                          // Check if any of this superstep's substeps were cleared
                          var hasCleared = step.subSteps.some(function(sub) {
                            return stepOrder[sub.key] > currentStepIndex;
                          });
                          if (hasCleared) {
                            console.log(`DEBUG: Clearing parent superstep ${step.key} because its substeps were cleared`);
                            var stepElement = document.getElementById('validate_dataset-step-' + step.key);
                            if (stepElement) {
                              var icon = stepElement.querySelector('.step-icon');
                              var message = stepElement.querySelector('.step-message');
                              if (icon) {
                                icon.classList.remove('step-success', 'step-error');
                                icon.classList.add('step-pending');
                                icon.innerHTML = '⋯';
                              }
                              if (message) {
                                message.textContent = getStepMessage(step.key, false);
                              }
                              // Remove ALL issue displays (including nested ones)
                              var allIssueDivs = stepElement.querySelectorAll('.step-issue');
                              allIssueDivs.forEach(function(issueDiv) {
                                issueDiv.remove();
                              });
                              
                              // Also clear any substep containers that might have issues
                              var substepContainer = stepElement.querySelector('.substep-container');
                              if (substepContainer) {
                                // Reset all substeps in the container
                                var substepElements = substepContainer.querySelectorAll('[id^="validate_dataset-step-"]');
                                substepElements.forEach(function(subElement) {
                                  var subIcon = subElement.querySelector('.step-icon');
                                  var subMessage = subElement.querySelector('.step-message');
                                  if (subIcon) {
                                    subIcon.classList.remove('step-success', 'step-error');
                                    subIcon.classList.add('step-pending');
                                    subIcon.innerHTML = '⋯';
                                  }
                                  if (subMessage) {
                                    // Get the substep key from the element ID
                                    var subKey = subElement.id.replace('validate_dataset-step-', '');
                                    subMessage.textContent = getStepMessage(subKey, false);
                                  }
                                  // Remove any issue displays from substeps
                                  var subIssueDivs = subElement.querySelectorAll('.step-issue');
                                  subIssueDivs.forEach(function(issueDiv) {
                                    issueDiv.remove();
                                  });
                                });
                              }
                            }
                            // Clear from stepStatus
                            delete stepStatus[step.key];
                          }
                        }
                      });
                      
                      // Send the halt notification to Shiny
                      if (isShinyReady()) {
                        Shiny.setInputValue('validation_halted', true, {priority: 'event'});
                      }
                    } else {
                      // This failure comes after the first failure, don't display it
                      console.log(`DEBUG: Ignoring failure at ${subStep.key} (index ${currentStepIndex}) because first failure is at index ${firstFailedStepIndex}`);
                      return;
                    }
                  }
                  
                  // Update status for this substep
                  stepStatus[subStep.key] = {
                    complete: true,
                    success: data.success,
                    issue: data.issue
                  };
                  
                  // Update the UI directly
                  updateStepUI(subStep.key, data.success, data.issue);
                  
                  // Update the parent superstep's status only if this step is still valid
                  // Don't update if there's a failure at an earlier index
                  if (firstFailedStepIndex === -1 || currentStepIndex <= firstFailedStepIndex) {
                    updateSuperStepStatus(superStep);
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
  if (!tree) {
    console.warn('processFileTree called with null/undefined tree');
    return;
  }
  
  // Make sure we have an object to process
  if (typeof tree !== 'object') {
    console.warn('processFileTree called with non-object:', typeof tree);
    return;
  }
  
  try {
    Object.keys(tree).forEach(function(key) {
      var entry = tree[key];
      
      if (!entry) {
        console.warn('Null entry for key:', key);
        return;
      }
      
      // Process file entries
      if (entry.type === 'file' && entry.file) {
        // Make sure file object exists
        if (!entry.file) {
          entry.file = {};
        }
        
        // Handle the text field
        if (entry.file.hasOwnProperty('text')) {
          var textContent = entry.file.text;
          
          // Check for error codes
          if (textContent === 'ERROR_BINARY_FILE') {
            console.warn('Binary file detected (likely Excel):', entry.file.path || key);
            // Create a function that returns empty string
            // This prevents the validator from crashing
            entry.file.text = function() {
              return '';
            };
            // Mark the file as problematic
            entry.file.isBinary = true;
            entry.file.validationError = 'File appears to be in binary format (possibly Excel), not valid CSV';
            
          } else if (textContent === 'ERROR_ENCODING') {
            console.warn('Encoding error in file:', entry.file.path || key);
            entry.file.text = function() {
              return '';
            };
            entry.file.hasEncodingError = true;
            entry.file.validationError = 'File contains characters that cannot be decoded as UTF-8';
            
          } else if (textContent === 'ERROR_READ_FAILED') {
            console.warn('Read error for file:', entry.file.path || key);
            entry.file.text = function() {
              return '';
            };
            entry.file.hasReadError = true;
            entry.file.validationError = 'Could not read file contents';
            
          } else if (typeof textContent === 'string') {
            // Normal text content - wrap in function
            var content = textContent;
            entry.file.text = function() {
              return content;
            };
            console.log('Converted text to function for:', entry.file.path || key);
            
          } else if (typeof textContent === 'function') {
            // Already a function, leave it as is
            console.log('Text already a function for:', entry.file.path || key);
            
          } else {
            // Unexpected type - create empty function
            console.warn('Unexpected text type for', entry.file.path || key, ':', typeof textContent);
            entry.file.text = function() {
              return '';
            };
          }
        } else {
          // No text field - add empty function
          console.log('No text field for file, adding empty function:', entry.file.path || key);
          entry.file.text = function() {
            return '';
          };
        }
        
        // Ensure path and name exist
        if (!entry.file.path) {
          entry.file.path = '/' + key;
        }
        if (!entry.file.name) {
          entry.file.name = key;
        }
      }
      
      // Process directory entries recursively
      if (entry.type === 'directory') {
        // Ensure contents exists
        if (!entry.contents) {
          entry.contents = {};
          console.log('Added empty contents to directory:', key);
        } else if (typeof entry.contents === 'object') {
          // Recursively process contents
          processFileTree(entry.contents);
        } else {
          console.warn('Invalid contents type for directory:', key, typeof entry.contents);
          entry.contents = {};
        }
      }
    });
  } catch (e) {
    console.error('Error in processFileTree:', e);
    console.error('Stack trace:', e.stack);
  }
}

// Also add this validation function to check the tree before sending to validator
function validateFileTree(tree) {
  console.log('Validating file tree structure...');
  
  if (!tree || typeof tree !== 'object') {
    console.error('Invalid tree: not an object');
    return false;
  }
  
  var hasErrors = false;
  
  function checkNode(node, path) {
    if (!node || typeof node !== 'object') {
      console.error('Invalid node at path:', path);
      hasErrors = true;
      return;
    }
    
    Object.keys(node).forEach(function(key) {
      var entry = node[key];
      var fullPath = path ? path + '/' + key : key;
      
      if (!entry) {
        console.error('Null entry at:', fullPath);
        hasErrors = true;
        return;
      }
      
      if (!entry.type) {
        console.error('Missing type at:', fullPath);
        hasErrors = true;
      }
      
      if (entry.type === 'file') {
        if (!entry.file) {
          console.error('Missing file object at:', fullPath);
          hasErrors = true;
        } else {
          if (typeof entry.file.text !== 'function') {
            console.error('Text is not a function at:', fullPath);
            hasErrors = true;
          }
        }
      } else if (entry.type === 'directory') {
        if (!entry.contents) {
          console.warn('Missing contents for directory at:', fullPath);
          // This is okay, just means empty directory
        } else if (typeof entry.contents === 'object') {
          checkNode(entry.contents, fullPath);
        } else {
          console.error('Invalid contents type at:', fullPath);
          hasErrors = true;
        }
      } else {
        console.warn('Unknown type at:', fullPath, ':', entry.type);
      }
    });
  }
  
  checkNode(tree, '');
  
  if (hasErrors) {
    console.error('File tree validation failed');
  } else {
    console.log('File tree validation passed');
  }
  
  return !hasErrors;
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