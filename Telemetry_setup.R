# Add to the top of your app.R file, after other library() calls
library(shiny.telemetry)



# Load environment variables or use defaults if missing
get_env <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = NA)
  if (is.na(value)) {
    return(default)
  }
  return(value)
}

# Initialize telemetry with Supabase PostgreSQL
initialize_telemetry <- function() {
  # First attempt to connect to Supabase
  tryCatch({
    telemetry <- Telemetry$new(
      app_name = "Vivid-Volcano",
      data_storage = DataStoragePostgreSQL$new(
        host = get_env("TELEMETRY_DB_HOST"),
        port = as.integer(get_env("TELEMETRY_DB_PORT", "5432")),
        user = get_env("TELEMETRY_DB_USER"),
        password = get_env("TELEMETRY_DB_PASSWORD"),
        dbname = get_env("TELEMETRY_DB_NAME")
      )
    )
    
    message("✅ Successfully connected to Supabase PostgreSQL database")
    return(telemetry)
    
  }, error = function(e) {
    # If connection fails, fall back to local SQLite for development
    warning("❌ Failed to connect to PostgreSQL: ", e$message, 
            "\nFalling back to local SQLite storage")
    
    # Create data directory if it doesn't exist
    if (!dir.exists("data")) {
      dir.create("data", showWarnings = FALSE)
    }
    
    telemetry <- Telemetry$new(
      app_name = "Vivid-Volcano",
      data_storage = DataStorageSQLite$new(
        db_path = "data/telemetry.sqlite"
      )
    )
    
    message("✅ Using local SQLite database at data/telemetry.sqlite")
    return(telemetry)
  })
}