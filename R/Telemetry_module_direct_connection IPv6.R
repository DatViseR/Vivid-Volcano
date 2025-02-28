## Optimized telemetry module for Vivid-Volcano
# Uses browser local storage for visitor tracking with robust error handling
library(DBI)
library(RPostgres)
library(RSQLite)
library(jsonlite)
library(R6)
library(digest)

# VividTelemetry class
VividTelemetry <- R6::R6Class(
  "VividTelemetry",
  
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
      
      # Setup database tables
      self$setup_database()
      
      message("✅ Telemetry initialized with session ID: ", self$session_id)
      message("✓ Browser: ", self$browser_info)
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
    
    # Setup database tables
    setup_database = function() {
      # First try PostgreSQL
      supabase_connected <- FALSE
      
      tryCatch({
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          # Create app_usage_stats table with optimized columns
          DBI::dbExecute(con, "
            CREATE TABLE IF NOT EXISTS app_usage_stats (
              session_id TEXT PRIMARY KEY,
              session_start TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
              session_end TIMESTAMPTZ,
              visit_count INTEGER,
              browser TEXT,
              upload_count INTEGER DEFAULT 0,
              gsea_count INTEGER DEFAULT 0,
              volcano_count INTEGER DEFAULT 0
            )
          ")
          
          DBI::dbDisconnect(con)
          supabase_connected <- TRUE
          message("✅ Successfully set up PostgreSQL telemetry tables")
        }
      }, error = function(e) {
        message("❌ PostgreSQL setup error: ", e$message)
      })
      
      # Fall back to SQLite if needed
      if (!supabase_connected) {
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
      }
    },
    
    # Update visitor info from browser local storage
    update_visitor_info = function(visit_count) {
      self$visit_count <- as.integer(visit_count)
      
      # Update the database with visitor information
      success <- FALSE
      
      tryCatch({
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          # Check if session already exists before inserting
          existing <- DBI::dbGetQuery(
            con,
            "SELECT COUNT(*) as count FROM app_usage_stats WHERE session_id = $1",
            params = list(self$session_id)
          )
          
          if (existing$count == 0) {
            # Insert new record if it doesn't exist
            result <- DBI::dbExecute(
              con,
              "INSERT INTO app_usage_stats (
                session_id, visit_count, browser
              ) VALUES ($1, $2, $3)",
              params = list(
                self$session_id,
                self$visit_count,
                self$browser_info
              )
            )
            
            if (result > 0) {
              message("✅ Successfully inserted visitor info: ", self$session_id)
              success <- TRUE
            } else {
              message("⚠️ No rows were inserted for session: ", self$session_id)
            }
          } else {
            message("✓ Session already exists in database")
            success <- TRUE
          }
          
          DBI::dbDisconnect(con)
        } else {
          message("❌ Failed to get PostgreSQL connection")
        }
      }, error = function(e) {
        message("❌ PostgreSQL error in update_visitor_info: ", e$message)
      })
      
      # Fall back to SQLite
      if (!success) {
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
      }
      
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
    
    # End session and update stats - FIXED VERSION
    end_session = function() {
      # Update session end time and counters
      message("Ending session with counters - Upload: ", self$upload_count,
              ", GSEA: ", self$gsea_count,
              ", Volcano: ", self$volcano_count)
      
      success <- FALSE
      
      tryCatch({
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          # Check if the session exists first
          existing <- DBI::dbGetQuery(
            con,
            "SELECT COUNT(*) as count FROM app_usage_stats WHERE session_id = $1",
            params = list(self$session_id)
          )
          
          if (existing$count > 0) {
            # Update existing record - ONLY update end time and counters
            result <- DBI::dbExecute(
              con,
              "UPDATE app_usage_stats SET 
                session_end = CURRENT_TIMESTAMP, 
                upload_count = $1,
                gsea_count = $2,
                volcano_count = $3
               WHERE session_id = $4",
              params = list(
                self$upload_count,
                self$gsea_count,
                self$volcano_count,
                self$session_id
              )
            )
            
            if (result > 0) {
              message("✅ Session stats updated in PostgreSQL")
              success <- TRUE
            } else {
              message("⚠️ No rows were updated in PostgreSQL")
            }
          } else {
            # If no record exists, insert one with start and end times
            result <- DBI::dbExecute(
              con,
              "INSERT INTO app_usage_stats (
                session_id, visit_count, browser, upload_count, gsea_count, volcano_count, 
                session_start, session_end
              ) VALUES ($1, $2, $3, $4, $5, $6, 
                CURRENT_TIMESTAMP - INTERVAL '2 MINUTES', CURRENT_TIMESTAMP)",
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
              message("✅ Session stats inserted in PostgreSQL during end_session")
              success <- TRUE
            } else {
              message("⚠️ Failed to insert session stats in PostgreSQL")
            }
          }
          
          DBI::dbDisconnect(con)
        } else {
          message("❌ Failed to get PostgreSQL connection for session end")
        }
      }, error = function(e) {
        message("❌ PostgreSQL end_session error: ", e$message)
      })
      
      # Fall back to SQLite
      if (!success) {
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
              # Update existing record - only update end time and counters
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
              # Insert if no record exists - with distinct start and end times
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
      }
      
      return(success)
    },
    
    # Get PostgreSQL connection with improved validation
    get_postgres_connection = function() {
      tryCatch({
        # Check environment variables
        dbname <- Sys.getenv("TELEMETRY_DB_NAME")
        host <- Sys.getenv("TELEMETRY_DB_HOST")
        user <- Sys.getenv("TELEMETRY_DB_USER")
        password <- Sys.getenv("TELEMETRY_DB_PASSWORD")
        
        if (dbname == "" || host == "" || user == "" || password == "") {
          message("❌ Missing database credentials in environment variables")
          return(NULL)
        }
        
        con <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname = dbname,
          host = host,
          port = as.integer(Sys.getenv("TELEMETRY_DB_PORT", "5432")),
          user = user,
          password = password
        )
        
        if (DBI::dbIsValid(con)) {
          # Test the connection with a simple query
          test <- DBI::dbGetQuery(con, "SELECT 1 as test")
          if (nrow(test) > 0 && test$test == 1) {
            return(con)
          } else {
            message("❌ PostgreSQL connection test failed")
            DBI::dbDisconnect(con)
            return(NULL)
          }
        } else {
          message("❌ Invalid PostgreSQL connection")
          return(NULL)
        }
      }, error = function(e) {
        message("❌ PostgreSQL connection error: ", e$message)
        return(NULL)
      })
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

  // Set up button click tracking - PRECISE TARGETING FOR YOUR BUTTONS
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