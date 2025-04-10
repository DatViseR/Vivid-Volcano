#' VividTelemetry
#' 
#' A telemetry module for the Vivid Volcano app that tracks user activity
#' using Supabase REST API with SQLite fallback.
#' 
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbIsValid
#' @importFrom RSQLite SQLite
#' @importFrom digest digest
#' @importFrom httr GET POST PATCH add_headers content status_code
#' @importFrom jsonlite toJSON fromJSON
#' 
VividTelemetry <- R6::R6Class(
  "VividTelemetry",
  
  private = list(
    # Flag to track if Supabase API is available
    supabase_available = FALSE,
    
    # Supabase API settings
    supabase_url = NULL ,
    supabase_key = NULL,
    
    # Helper function to make API calls to Supabase
    call_supabase_api = function(endpoint, method = "GET", body = NULL) {
      if (!private$supabase_available) {
        return(NULL)
      }
      
      # Construct full URL
      url <- paste0(private$supabase_url, "/rest/v1/", endpoint)
      
      tryCatch({
        # Add headers for authentication and content type
        headers <- httr::add_headers(
          `apikey` = private$supabase_key,
          `Authorization` = paste("Bearer", private$supabase_key),
          `Content-Type` = "application/json",
          `Prefer` = if (method == "POST") "return=minimal" else "return=representation"
        )
        
        # Make the appropriate request based on method
        response <- switch(method,
                           "GET" = httr::GET(url, headers),
                           "POST" = httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE)),
                           "PATCH" = httr::PATCH(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE)),
                           # Default case
                           httr::GET(url, headers)
        )
        
        # Check if the request was successful
        if (httr::status_code(response) < 300) {
          if (method == "GET") {
            # Parse and return JSON response
            return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
          } else {
            # Return success for non-GET requests
            return(TRUE)
          }
        } else {
          message("❌ Supabase API error: ", httr::status_code(response), 
                  " - ", httr::content(response, "text", encoding = "UTF-8"))
          return(NULL)
        }
      }, error = function(e) {
        message("❌ Error calling Supabase API: ", e$message)
        return(NULL)
      })
    }
  ),
  
  public = list(
    # Properties
    session_id = NULL,
    browser_info = "Unknown",
    
    # Event counters
    upload_count = 0,
    gsea_count = 0,
    volcano_count = 0,
    
    # Visit tracking (populated from browser local storage)
    visit_count = 1,
    
    # Initialize telemetry
    initialize = function(user_agent = NULL) {
      # Generate unique session ID
      self$session_id <- paste0(
        "VividVolcano-",
        format(Sys.time(), "%Y%m%d%H%M%S"), "-",
        substr(digest::digest(paste0(Sys.time(), sample(1e9, 1))), 1, 8)
      )
      
      # Extract browser info if available
      if (!is.null(user_agent)) {
        self$browser_info <- self$parse_user_agent(user_agent)
      }
      
      # Get Supabase credentials from environment
      private$supabase_url <- Sys.getenv("SUPABASE_URL")
      private$supabase_key <- Sys.getenv("SUPABASE_KEY")
      
      # Check if Supabase is configured
      if (private$supabase_url != "" && private$supabase_key != "") {
        message("✓ Supabase API credentials found")
        private$supabase_available <- TRUE
        
        # Test Supabase connection
        test_result <- private$call_supabase_api("app_usage_stats?limit=1")
        if (!is.null(test_result)) {
          message("✅ Successfully connected to Supabase REST API")
        } else {
          message("⚠️ Couldn't connect to Supabase API, will use SQLite only")
          private$supabase_available <- FALSE
        }
      } else {
        message("⚠️ Supabase API credentials not found, will use SQLite only")
        private$supabase_available <- FALSE
      }
      
      # Setup SQLite database (always as fallback)
      self$setup_sqlite_database()
      
      message("✅ Telemetry initialized with session ID: ", self$session_id)
      message("✓ Browser: ", self$browser_info)
      message("✓ Supabase API available: ", private$supabase_available)
    },
    
    # Extract browser info from user agent
    parse_user_agent = function(user_agent) {
      if (is.null(user_agent) || user_agent == "") {
        return("Unknown")
      }
      
      # Simple pattern matching for common browsers
      browser <- "Unknown"
      if (grepl("Firefox/", user_agent)) {
        browser <- "Firefox"
      } else if (grepl("Chrome/", user_agent) && !grepl("Chromium/", user_agent) && !grepl("Edg/", user_agent)) {
        browser <- "Chrome"
      } else if (grepl("Safari/", user_agent) && !grepl("Chrome/", user_agent) && !grepl("Edg/", user_agent)) {
        browser <- "Safari"
      } else if (grepl("Edg/", user_agent)) {
        browser <- "Edge"
      } else if (grepl("MSIE|Trident/", user_agent)) {
        browser <- "Internet Explorer"
      } else if (grepl("Opera|OPR/", user_agent)) {
        browser <- "Opera"
      }
      
      return(browser)
    },
    
    # Setup SQLite database
    setup_sqlite_database = function() {
      if (!dir.exists("data")) {
        dir.create("data", showWarnings = FALSE)
      }
      
      tryCatch({
        con <- self$get_sqlite_connection()
        
        # Create app_usage_stats table
        DBI::dbExecute(con, "
          CREATE TABLE IF NOT EXISTS app_usage_stats (
            session_id TEXT PRIMARY KEY,
            session_start TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            session_end TIMESTAMP,
            visit_count INTEGER,
            browser TEXT,
            upload_count INTEGER DEFAULT 0,
            gsea_count INTEGER DEFAULT 0,
            volcano_count INTEGER DEFAULT 0
          )
        ")
        
        DBI::dbDisconnect(con)
        message("✅ Successfully set up SQLite telemetry tables")
      }, error = function(e) {
        message("❌ SQLite setup error: ", e$message)
      })
    },
    
    # Update visitor info from browser local storage
    update_visitor_info = function(visit_count) {
      self$visit_count <- as.integer(visit_count)
      
      # Update the database with visitor information
      success <- FALSE
      
      # Try Supabase API first if available
      if (private$supabase_available) {
        session_data <- list(
          session_id = self$session_id,
          visit_count = self$visit_count,
          browser = self$browser_info,
          session_start = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )
        
        result <- private$call_supabase_api("app_usage_stats", "POST", session_data)
        if (!is.null(result)) {
          message("✅ Successfully recorded visitor info in Supabase")
          success <- TRUE
        } else {
          message("⚠️ Failed to record visitor info in Supabase, falling back to SQLite")
        }
      }
      
      # Fall back to SQLite if needed
      if (!success) {
        success <- self$update_sqlite_visitor_info()
      }
      
      return(success)
    },
    
    # SQLite specific update function
    update_sqlite_visitor_info = function() {
      success <- FALSE
      
      tryCatch({
        con <- self$get_sqlite_connection()
        if (!is.null(con)) {
          # Check if session already exists
          existing <- DBI::dbGetQuery(
            con, 
            "SELECT COUNT(*) as count FROM app_usage_stats WHERE session_id = ?",
            params = list(self$session_id)
          )
          
          if (existing$count == 0) {
            DBI::dbExecute(
              con,
              "INSERT INTO app_usage_stats (
                session_id, visit_count, browser
              ) VALUES (?, ?, ?)",
              params = list(
                self$session_id,
                self$visit_count,
                self$browser_info
              )
            )
            message("✅ Visitor information recorded in SQLite")
            success <- TRUE
          } else {
            message("✓ Session already exists in SQLite database")
            success <- TRUE
          }
          
          DBI::dbDisconnect(con)
        }
      }, error = function(e) {
        message("❌ SQLite visitor info error: ", e$message)
      })
      
      return(success)
    },
    
    # Track button clicks
    increment_counter = function(counter_type) {
      if (counter_type == "upload") {
        self$upload_count <- self$upload_count + 1
        message("✓ Upload counter incremented: ", self$upload_count)
      } else if (counter_type == "gsea") {
        self$gsea_count <- self$gsea_count + 1
        message("✓ GSEA counter incremented: ", self$gsea_count)
      } else if (counter_type == "volcano") {
        self$volcano_count <- self$volcano_count + 1
        message("✓ Volcano counter incremented: ", self$volcano_count)
      } else {
        message("⚠️ Unknown counter type: ", counter_type)
      }
    },
    
    # End session and update stats
    end_session = function() {
      # Update session end time and counters
      message("Ending session with counters - Upload: ", self$upload_count,
              ", GSEA: ", self$gsea_count,
              ", Volcano: ", self$volcano_count)
      
      success <- FALSE
      
      # Try Supabase API first if available
      if (private$supabase_available) {
        session_data <- list(
          session_end = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
          upload_count = self$upload_count,
          gsea_count = self$gsea_count, 
          volcano_count = self$volcano_count
        )
        
        # Use PATCH to update existing record
        result <- private$call_supabase_api(
          paste0("app_usage_stats?session_id=eq.", self$session_id), 
          "PATCH", 
          session_data
        )
        
        if (!is.null(result)) {
          message("✅ Successfully updated session stats in Supabase")
          success <- TRUE
        } else {
          message("⚠️ Failed to update session stats in Supabase, trying to insert")
          
          # If update failed, try to insert a complete record
          session_data <- list(
            session_id = self$session_id,
            visit_count = self$visit_count,
            browser = self$browser_info,
            upload_count = self$upload_count,
            gsea_count = self$gsea_count,
            volcano_count = self$volcano_count,
            session_start = format(Sys.time() - as.difftime(2, units = "mins"), "%Y-%m-%dT%H:%M:%SZ"),
            session_end = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
          )
          
          result <- private$call_supabase_api("app_usage_stats", "POST", session_data)
          if (!is.null(result)) {
            message("✅ Successfully inserted session stats in Supabase")
            success <- TRUE
          } else {
            message("⚠️ Failed to insert session stats in Supabase, falling back to SQLite")
          }
        }
      }
      
      # Fall back to SQLite
      if (!success) {
        success <- self$end_session_sqlite()
      }
      
      return(success)
    },
    
    # SQLite-specific end session
    end_session_sqlite = function() {
      success <- FALSE
      
      tryCatch({
        con <- self$get_sqlite_connection()
        if (!is.null(con)) {
          # Check if the session exists first
          existing <- DBI::dbGetQuery(
            con,
            "SELECT COUNT(*) as count FROM app_usage_stats WHERE session_id = ?",
            params = list(self$session_id)
          )
          
          if (existing$count > 0) {
            # Update existing record
            result <- DBI::dbExecute(
              con,
              "UPDATE app_usage_stats SET 
                session_end = datetime('now'), 
                upload_count = ?,
                gsea_count = ?,
                volcano_count = ?
              WHERE session_id = ?",
              params = list(
                self$upload_count,
                self$gsea_count,
                self$volcano_count,
                self$session_id
              )
            )
            
            if (result > 0) {
              message("✅ Session stats updated in SQLite")
              success <- TRUE
            }
          } else {
            # Insert if no record exists
            result <- DBI::dbExecute(
              con,
              "INSERT INTO app_usage_stats (
                session_id, visit_count, browser, upload_count, gsea_count, volcano_count,
                session_start, session_end
              ) VALUES (?, ?, ?, ?, ?, ?, 
                datetime('now', '-2 minutes'), datetime('now'))",
              params = list(
                self$session_id,
                self$visit_count,
                self$browser_info,
                self$upload_count,
                self$gsea_count,
                self$volcano_count
              )
            )
            
            if (result > 0) {
              message("✅ Session stats inserted in SQLite during end_session")
              success <- TRUE
            }
          }
          
          DBI::dbDisconnect(con)
        }
      }, error = function(e) {
        message("❌ SQLite end_session error: ", e$message)
      })
      
      return(success)
    },
    
    # Get SQLite connection
    get_sqlite_connection = function() {
      tryCatch({
        con <- DBI::dbConnect(
          RSQLite::SQLite(),
          "data/telemetry.sqlite"
        )
        
        if (DBI::dbIsValid(con)) {
          return(con)
        } else {
          message("❌ Invalid SQLite connection")
          return(NULL)
        }
      }, error = function(e) {
        message("❌ SQLite connection error: ", e$message)
        return(NULL)
      })
    },
    
    # Utility function for debugging
    print_status = function() {
      cat("==== Telemetry Status ====\n")
      cat("Session ID: ", self$session_id, "\n")
      cat("Browser: ", self$browser_info, "\n")
      cat("Visit count: ", self$visit_count, "\n")
      cat("Is first time user: ", self$visit_count == 1, "\n")
      cat("Upload count: ", self$upload_count, "\n")
      cat("GSEA count: ", self$gsea_count, "\n")
      cat("Volcano count: ", self$volcano_count, "\n")
      cat("Supabase API available: ", private$supabase_available, "\n")
      cat("=========================\n")
    }
  )
)

# Function to create a new telemetry instance
create_telemetry <- function(user_agent = NULL) {
  tryCatch({
    telemetry <- VividTelemetry$new(user_agent)
    return(telemetry)
  }, error = function(e) {
    message("❌ Failed to create telemetry instance: ", e$message)
    return(NULL)
  })
}

# Create a JavaScript file if it doesn't exist
ensure_js_file <- function() {
  js_dir <- "www"
  js_file <- file.path(js_dir, "telemetry.js")
  
  if (!dir.exists(js_dir)) {
    dir.create(js_dir, showWarnings = FALSE)
  }
  
  if (!file.exists(js_file)) {
    js_content <- '
// Client-side telemetry for visitor tracking
// Uses browser local storage to maintain privacy
(function() {
  // Initialize tracking when the app loads
  $(document).ready(function() {
    console.log("Telemetry JS initialized");
    initializeVisitTracking();
    setupButtonTracking();
  });

  // Set up visit tracking using local storage
  function initializeVisitTracking() {
    // Get current values or set defaults if first visit
    let visitCount = localStorage.getItem("vividVolcanoVisits") || 0;
    
    // Increment visit count
    visitCount = parseInt(visitCount) + 1;
    
    // Save updated count
    localStorage.setItem("vividVolcanoVisits", visitCount);
    
    // Send to Shiny
    Shiny.setInputValue("telemetry_visit_count", visitCount);
    console.log("Visit count recorded: " + visitCount);
  }

  // Set up button click tracking
  function setupButtonTracking() {
    // Exact button ID targeting for your app
    $(document).on("click", "#upload", function() {
      console.log("Upload button clicked");
      Shiny.setInputValue("telemetry_button_click", {
        button: "upload",
        timestamp: new Date().toISOString()
      });
    });
    
    $(document).on("click", "#run_gsea", function() {
      console.log("GSEA button clicked");
      Shiny.setInputValue("telemetry_button_click", {
        button: "gsea",
        timestamp: new Date().toISOString()
      });
    });
    
    $(document).on("click", "#draw_volcano", function() {
      console.log("Volcano button clicked");
      Shiny.setInputValue("telemetry_button_click", {
        button: "volcano",
        timestamp: new Date().toISOString()
      });
    });
    
    console.log("Button tracking initialized");
  }
})();
    '
    
    writeLines(js_content, js_file)
    message("✅ Created telemetry.js file in www directory")
  } else {
    message("✓ telemetry.js file already exists")
  }
}

# Automatically create JS file when module is sourced
ensure_js_file()