// Improved telemetry.js with duplicate prevention
(function() {
  // Debug helper function
  function debug(message) {
    console.log("[Telemetry] " + message);
  }

  // Initialize tracking when the app loads
  $(document).ready(function() {
    debug("Initializing telemetry...");
    initializeVisitTracking();
    setupButtonTracking();
  });

  // Set up visit tracking with localStorage
  function initializeVisitTracking() {
    try {
      // Get stored value with explicit fallback
      var rawCount = window.localStorage.getItem("vividVolcanoVisits");
      debug("Raw localStorage value: " + rawCount);
      
      // Handle null, undefined, NaN cases
      var visitCount = 1; // Default to 1
      
      if (rawCount !== null && rawCount !== undefined && rawCount !== "") {
        var parsedCount = parseInt(rawCount, 10);
        if (!isNaN(parsedCount)) {
          visitCount = parsedCount + 1;
        }
      }
      
      // Make sure it's at least 1
      if (visitCount < 1) visitCount = 1;
      
      // Save updated count
      debug("Setting localStorage to: " + visitCount);
      window.localStorage.setItem("vividVolcanoVisits", visitCount.toString());
      
      // Send to Shiny with explicit event name
      debug("Sending visit_count to Shiny: " + visitCount);
      Shiny.setInputValue("telemetry_visit_count", visitCount);
      
      // Debug info
      debug("Visit tracking initialized successfully");
    } catch (e) {
      console.error("Error in visit tracking: ", e);
    }
  }

  // FIXED: Button tracking with debouncing to prevent duplicates
  function setupButtonTracking() {
    debug("Setting up button tracking with debounce");
    
    // Track last clicked time to prevent duplicates
    var lastClickTime = {};
    var DEBOUNCE_TIME = 300; // milliseconds
    
    function trackButtonClick(buttonType, btnId) {
      var now = new Date().getTime();
      var lastClick = lastClickTime[buttonType] || 0;
      
      // Prevent duplicate clicks within debounce window
      if (now - lastClick > DEBOUNCE_TIME) {
        lastClickTime[buttonType] = now;
        
        debug("Tracking " + buttonType + " click (ID: " + btnId + ")");
        Shiny.setInputValue("telemetry_button_click", {
          button: buttonType,
          timestamp: new Date().toISOString()
        });
        
        return true;
      } else {
        debug("Ignoring duplicate " + buttonType + " click");
        return false;
      }
    }
    
    // Single event handler for all buttons
    $(document).on("click", "#upload, #run_gsea, #draw_volcano", function(e) {
      var btnId = $(this).attr("id");
      debug("Button clicked: " + btnId);
      
      var buttonType = null;
      if (btnId === "upload") buttonType = "upload";
      else if (btnId === "run_gsea") buttonType = "gsea";
      else if (btnId === "draw_volcano") buttonType = "volcano";
      
      if (buttonType) {
        trackButtonClick(buttonType, btnId);
      }
    });
    
    debug("Button tracking initialized with debounce protection");
  }
})();