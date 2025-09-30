// Validation checklist JavaScript with debugging

console.log("[ValidationChecklist] JavaScript loaded");

// Counter for message tracking
let updateCount = 0;

// Log function that both logs to console and sends to Shiny
function debugLog(message) {
  console.log("[ValidationChecklist] " + message);
  
  // If Shiny exists, send to a debug value
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('validation_debug_js', {
      timestamp: new Date().toISOString(),
      message: message,
      updateCount: updateCount
    }, {priority: 'event'});
  }
}

// Handler for updating element HTML
Shiny.addCustomMessageHandler('updateElement', function(message) {
  updateCount++;
  debugLog("updateElement called (#" + updateCount + "): " + message.selector);
  
  try {
    // Find the element by the selector
    const element = document.querySelector(message.selector);
    
    if (element) {
      // Update the HTML content
      element.innerHTML = message.html;
      debugLog("Element updated successfully: " + message.selector);
    } else {
      debugLog("ERROR: Element not found: " + message.selector);
      
      // Try to create a list of all present IDs for debugging
      const allElements = document.querySelectorAll('[id]');
      let idList = "";
      for (let i = 0; i < Math.min(allElements.length, 20); i++) {
        idList += allElements[i].id + ", ";
      }
      if (allElements.length > 20) idList += "... and " + (allElements.length - 20) + " more";
      
      debugLog("Available elements with IDs: " + idList);
    }
  } catch (error) {
    debugLog("ERROR updating element: " + error.message);
  }
});

// Debug function to manually track validation events
window.trackValidationEvent = function(eventName, data) {
  debugLog("Manual event tracked: " + eventName);
  console.log("Event data:", data);
  
  // If Shiny exists, send the event
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('validation_event', {
      event: eventName,
      data: JSON.stringify(data)
    }, {priority: 'event'});
  }
};

// Intercept and log the stepStatusChange event
const originalEmit = EventEmitter.prototype.emit;
if (originalEmit) {
  debugLog("Successfully patched EventEmitter");
  
  EventEmitter.prototype.emit = function(type, ...args) {
    if (type === 'stepStatusChange') {
      debugLog("stepStatusChange event intercepted");
      console.log("stepStatusChange data:", args[0]);
      
      // When this specific event is emitted, track it
      window.trackValidationEvent("stepStatusChange", args[0]);
    }
    return originalEmit.apply(this, [type, ...args]);
  };
} else {
  debugLog("WARNING: Could not patch EventEmitter");
}

// When DOM is loaded, log page initialization
document.addEventListener('DOMContentLoaded', function() {
  debugLog("DOM loaded, validation checklist ready");
  
  // Find existing validation checklist elements
  const containers = document.querySelectorAll('.validation-checklist-container');
  debugLog("Found " + containers.length + " validation checklist containers");
  
  // Try to get the progress tracker from window if available
  if (window.psychDSValidator && window.psychDSValidator.ValidationProgressTracker) {
    debugLog("Found ValidationProgressTracker in global scope");
  } else {
    debugLog("WARNING: ValidationProgressTracker not found in global scope");
  }
});