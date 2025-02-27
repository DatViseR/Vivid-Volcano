// Improved telemetry.js with fixed visit counter
// For Vivid Volcano app - Version 2.0
(function() {
  // Debug helper function
  function debug(message) {
    console.log("[Telemetry] " + message);
  }

  // Constants
  const STORAGE_KEY = "vividVolcanoVisits";
  const DEBOUNCE_TIME = 300; // milliseconds
  
  // Initialize tracking when the app loads
  $(document).ready(function() {
    debug("Initializing telemetry v2.0...");
    
    // Wait briefly to ensure DOM is fully loaded
    setTimeout(function() {
      initializeVisitTracking();
      setupButtonTracking();
    }, 100);
  });

  // ----- FIXED VISIT TRACKING -----
  function initializeVisitTracking() {
    debug("Starting visit tracking...");
    
    try {
      // 1. Verify localStorage is available
      if (!storageAvailable('localStorage')) {
        debug("localStorage not available - using fallback");
        trackVisitWithoutStorage();
        return;
      }
      
      // 2. Get the current value
      let visitCount;
      try {
        // Read and log the raw value for debugging
        const rawValue = window.localStorage.getItem(STORAGE_KEY);
        debug(`Raw localStorage value: "${rawValue}"`);
        
        if (rawValue === null || rawValue === undefined || rawValue === "") {
          // First visit or cleared storage
          debug("No previous visit count found, this is first visit");
          visitCount = 1;
        } else {
          // Parse existing value and increment
          const storedCount = parseInt(rawValue, 10);
          if (isNaN(storedCount)) {
            debug(`Invalid stored value: "${rawValue}", resetting to 1`);
            visitCount = 1;
          } else {
            visitCount = storedCount + 1;
            debug(`Incrementing visit count from ${storedCount} to ${visitCount}`);
          }
        }
      } catch (parseError) {
        debug("Error parsing visit count: " + parseError);
        visitCount = 1;
      }
      
      // 3. Store the updated value back to localStorage
      try {
        window.localStorage.setItem(STORAGE_KEY, visitCount.toString());
        debug(`Saved visit count ${visitCount} to localStorage`);
        
        // Verify storage worked by reading it back
        const verifyValue = window.localStorage.getItem(STORAGE_KEY);
        debug(`Verification - localStorage now contains: "${verifyValue}"`);
        
        if (verifyValue !== visitCount.toString()) {
          debug("WARNING: Storage verification failed!");
        }
      } catch (storageError) {
        debug("Error storing visit count: " + storageError);
      }
      
      // 4. Send to Shiny
      debug(`Sending visit count to Shiny: ${visitCount}`);
      Shiny.setInputValue("telemetry_visit_count", visitCount);
      
      // 5. Schedule verification
      setTimeout(verifyVisitStorage, 1000);
      
    } catch (e) {
      debug("Error in visit tracking: " + e.message);
      trackVisitWithoutStorage();
    }
  }
  
  // Verify localStorage is working
  function storageAvailable(type) {
    try {
      const storage = window[type];
      const testKey = '__storage_test__';
      storage.setItem(testKey, testKey);
      storage.removeItem(testKey);
      return true;
    } catch (e) {
      return false;
    }
  }
  
  // Fallback for when localStorage isn't available
  function trackVisitWithoutStorage() {
    debug("Using session cookie fallback for visit tracking");
    Shiny.setInputValue("telemetry_visit_count", 1);
    Shiny.setInputValue("telemetry_storage_issue", true);
  }
  
  // Verification function to check storage is persisting
  function verifyVisitStorage() {
    try {
      const currentValue = window.localStorage.getItem(STORAGE_KEY);
      debug(`Storage verification check: "${currentValue}"`);
      
      // Set a flag that can be used for debugging
      window.telemetryVisitVerified = true;
      window.telemetryVisitValue = currentValue;
    } catch (e) {
      debug("Storage verification failed: " + e.message);
    }
  }

  // ----- BUTTON TRACKING (KEEP AS-IS) -----
  function setupButtonTracking() {
    debug("Setting up button tracking with debounce");
    
    // Track last clicked time to prevent duplicates
    var lastClickTime = {};
    
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
  
  // Expose diagnostic functions globally
  window.telemetryDiagnostics = {
    checkVisitCount: function() {
      try {
        const value = window.localStorage.getItem(STORAGE_KEY);
        console.log("Current visit count in localStorage:", value);
        return value;
      } catch (e) {
        console.error("Error checking visit count:", e);
        return null;
      }
    },
    resetVisitCount: function() {
      try {
        window.localStorage.removeItem(STORAGE_KEY);
        console.log("Visit count reset");
        return true;
      } catch (e) {
        console.error("Error resetting visit count:", e);
        return false;
      }
    },
    testStorage: function() {
      let results = {
        available: storageAvailable('localStorage'),
        canWrite: false,
        canRead: false,
        canPersist: false
      };
      
      if (results.available) {
        try {
          window.localStorage.setItem('__test_key__', 'test_value');
          results.canWrite = true;
          
          const readValue = window.localStorage.getItem('__test_key__');
          results.canRead = readValue === 'test_value';
          
          window.localStorage.removeItem('__test_key__');
          results.canPersist = window.localStorage.getItem('__test_key__') === null;
        } catch (e) {
          console.error("Storage test error:", e);
        }
      }
      
      console.log("Storage test results:", results);
      return results;
    }
  };
})();