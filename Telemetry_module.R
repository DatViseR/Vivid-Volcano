# Custom telemetry implementation using RPostgres
library(DBI)
library(RPostgres)
library(jsonlite)
library(R6)

# Simple telemetry class that uses RPostgres directly
VividTelemetry <- R6::R6Class(
  "VividTelemetry",
  
  public = list(
    app_name = "Vivid-Volcano",
    session_id = NULL,
    
    initialize = function(app_name = "Vivid-Volcano") {
      self$app_name <- app_name
      self$session_id <- paste0(
        app_name, "-", 
        format(Sys.time(), "%Y%m%d%H%M%S"), "-",
        paste0(sample(0:9, 6, replace = TRUE), collapse = "")
      )
      
      # Create tables on initialization
      self$setup_database()
      
      message("✅ Telemetry initialized with session ID: ", self$session_id)
    },
    
    setup_database = function() {
      # First try PostgreSQL
      supabase_connected <- FALSE
      
      tryCatch({
        # Test PostgreSQL connection
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          # Create tables if needed
          DBI::dbExecute(con, "
            CREATE TABLE IF NOT EXISTS telemetry_events (
              id SERIAL PRIMARY KEY,
              session_id TEXT,
              event_name TEXT,
              timestamp TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
              properties JSONB
            )
          ")
          
          DBI::dbExecute(con, "
            CREATE TABLE IF NOT EXISTS telemetry_sessions (
              session_id TEXT PRIMARY KEY,
              app_name TEXT,
              start_time TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
              end_time TIMESTAMPTZ,
              user_properties JSONB
            )
          ")
          
          # Insert session record
          DBI::dbExecute(
            con,
            "INSERT INTO telemetry_sessions (session_id, app_name, user_properties) VALUES ($1, $2, $3)",
            params = list(
              self$session_id,
              self$app_name,
              toJSON(list(
                system = Sys.info()["sysname"],
                r_version = R.version$version.string
              ))
            )
          )
          
          DBI::dbDisconnect(con)
          supabase_connected <- TRUE
          message("✅ Successfully set up PostgreSQL telemetry tables")
        }
      }, error = function(e) {
        message("❌ PostgreSQL setup error: ", e$message)
      })
      
      if (!supabase_connected) {
        # Fall back to SQLite
        if (!dir.exists("data")) {
          dir.create("data", showWarnings = FALSE)
        }
        
        tryCatch({
          con <- self$get_sqlite_connection()
          DBI::dbExecute(con, "
            CREATE TABLE IF NOT EXISTS telemetry_events (
              id INTEGER PRIMARY KEY,
              session_id TEXT,
              event_name TEXT,
              timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
              properties TEXT
            )
          ")
          
          DBI::dbExecute(con, "
            CREATE TABLE IF NOT EXISTS telemetry_sessions (
              session_id TEXT PRIMARY KEY,
              app_name TEXT,
              start_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
              end_time TIMESTAMP,
              user_properties TEXT
            )
          ")
          
          # Insert session record
          DBI::dbExecute(
            con,
            "INSERT INTO telemetry_sessions (session_id, app_name, user_properties) VALUES (?, ?, ?)",
            params = list(
              self$session_id,
              self$app_name,
              toJSON(list(
                system = Sys.info()["sysname"],
                r_version = R.version$version.string
              ))
            )
          )
          
          DBI::dbDisconnect(con)
          message("✅ Successfully set up SQLite telemetry tables")
        }, error = function(e) {
          message("❌ SQLite setup error: ", e$message)
          # Continue without telemetry in this case
        })
      }
    },
    
    log_event = function(event_name, properties = list()) {
      # Add standard properties
      properties$app_name <- self$app_name
      properties$session_id <- self$session_id
      properties_json <- toJSON(properties, auto_unbox = TRUE)
      
      # Try PostgreSQL first
      success <- FALSE
      
      tryCatch({
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          DBI::dbExecute(
            con,
            "INSERT INTO telemetry_events (session_id, event_name, properties) VALUES ($1, $2, $3)",
            params = list(
              self$session_id,
              event_name,
              properties_json
            )
          )
          DBI::dbDisconnect(con)
          success <- TRUE
          return(TRUE)
        }
      }, error = function(e) {
        message("❌ PostgreSQL log_event error: ", e$message)
      })
      
      # Fall back to SQLite
      if (!success) {
        tryCatch({
          con <- self$get_sqlite_connection()
          if (!is.null(con)) {
            DBI::dbExecute(
              con,
              "INSERT INTO telemetry_events (session_id, event_name, properties) VALUES (?, ?, ?)",
              params = list(
                self$session_id,
                event_name,
                properties_json
              )
            )
            DBI::dbDisconnect(con)
            return(TRUE)
          }
        }, error = function(e) {
          message("❌ SQLite log_event error: ", e$message)
        })
      }
      
      return(FALSE)
    },
    
    end_session = function() {
      # Try PostgreSQL first
      success <- FALSE
      
      tryCatch({
        con <- self$get_postgres_connection()
        if (!is.null(con)) {
          DBI::dbExecute(
            con,
            "UPDATE telemetry_sessions SET end_time = CURRENT_TIMESTAMP WHERE session_id = $1",
            params = list(self$session_id)
          )
          DBI::dbDisconnect(con)
          success <- TRUE
        }
      }, error = function(e) {
        message("❌ PostgreSQL end_session error: ", e$message)
      })
      
      # Fall back to SQLite
      if (!success) {
        tryCatch({
          con <- self$get_sqlite_connection()
          if (!is.null(con)) {
            DBI::dbExecute(
              con,
              "UPDATE telemetry_sessions SET end_time = CURRENT_TIMESTAMP WHERE session_id = ?",
              params = list(self$session_id)
            )
            DBI::dbDisconnect(con)
          }
        }, error = function(e) {
          message("❌ SQLite end_session error: ", e$message)
        })
      }
    },
    
    get_postgres_connection = function() {
      tryCatch({
        con <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname = Sys.getenv("TELEMETRY_DB_NAME"),
          host = Sys.getenv("TELEMETRY_DB_HOST"),
          port = as.integer(Sys.getenv("TELEMETRY_DB_PORT", "5432")),
          user = Sys.getenv("TELEMETRY_DB_USER"),
          password = Sys.getenv("TELEMETRY_DB_PASSWORD")
        )
        
        if (DBI::dbIsValid(con)) {
          return(con)
        } else {
          return(NULL)
        }
      }, error = function(e) {
        message("❌ PostgreSQL connection error: ", e$message)
        return(NULL)
      })
    },
    
    get_sqlite_connection = function() {
      tryCatch({
        con <- DBI::dbConnect(
          RSQLite::SQLite(),
          "data/telemetry.sqlite"
        )
        
        if (DBI::dbIsValid(con)) {
          return(con)
        } else {
          return(NULL)
        }
      }, error = function(e) {
        message("❌ SQLite connection error: ", e$message)
        return(NULL)
      })
    }
  )
)

# Function to create a new telemetry instance
create_telemetry <- function(app_name = "Vivid-Volcano") {
  tryCatch({
    telemetry <- VividTelemetry$new(app_name)
    return(telemetry)
  }, error = function(e) {
    message("❌ Failed to create telemetry instance: ", e$message)
    return(NULL)
  })
}