// Data Dictionary Validation Handler
// This script handles validation requests from the data dictionary page

$(document).on('shiny:connected', function() {
  console.log('Data dictionary validation handler initialized');
  
  // Check if validator is loaded
  var validatorLoaded = typeof window.psychDSValidator !== 'undefined' && 
                        typeof window.psychDSValidator.validateWeb === 'function';
  
  if (!validatorLoaded) {
    console.warn('Psych-DS validator not loaded');
    return;
  }
  
  // Handler for data dictionary validation requests
  Shiny.addCustomMessageHandler('run_dict_validation', function(fileData) {
    console.log('Data dictionary validation requested');
    
    // Process file tree (same as main validator)
    if (typeof processFileTree === 'function') {
      processFileTree(fileData);
    }
    
    // Create event emitter for validation
    var eventEmitter = new EventEmitter3();
    
    // Listen for validation step updates
    eventEmitter.on('step', function(step) {
      Shiny.setInputValue('dict_validation_step', {
        key: step.key,
        complete: step.complete,
        success: step.success,
        timestamp: Date.now()
      }, {priority: 'event'});
    });
    
    // Listen for validation completion
    eventEmitter.on('validation:complete', function(result) {
      console.log('Dictionary validation complete:', result.isValid);
      
      Shiny.setInputValue('dict_validation_complete', {
        isValid: result.isValid,
        issues: result.issues,
        summary: result.summary,
        timestamp: Date.now()
      }, {priority: 'event'});
    });
    
    // Run validation
    try {
      var result = window.psychDSValidator.validateWeb(fileData, eventEmitter);
      
      // Handle synchronous result
      if (result && typeof result.then === 'function') {
        // If it's a promise
        result.then(function(validationResult) {
          console.log('Validation promise resolved:', validationResult.isValid);
        }).catch(function(error) {
          console.error('Validation error:', error);
          Shiny.setInputValue('dict_validation_complete', {
            isValid: false,
            issues: {
              errors: [{
                key: 'validation-error',
                reason: 'Validation failed: ' + error.message
              }]
            },
            timestamp: Date.now()
          }, {priority: 'event'});
        });
      }
    } catch (error) {
      console.error('Validation exception:', error);
      Shiny.setInputValue('dict_validation_complete', {
        isValid: false,
        issues: {
          errors: [{
            key: 'validation-error',
            reason: 'Validation exception: ' + error.message
          }]
        },
        timestamp: Date.now()
      }, {priority: 'event'});
    }
  });
});
