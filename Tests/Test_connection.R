# Test script for Supabase PostgreSQL connection
library(DBI)
library(RPostgres)

# Test function to check connection
test_connection <- function() {
  # Print connection parameters (hiding password)
  cat("Testing connection to Supabase PostgreSQL database...\n")
  cat("Connection parameters:\n")
  cat("- Host:", Sys.getenv("TELEMETRY_DB_HOST"), "\n")
  cat("- Port:", Sys.getenv("TELEMETRY_DB_PORT", "5432"), "\n")
  cat("- Database:", Sys.getenv("TELEMETRY_DB_NAME"), "\n")
  cat("- User:", Sys.getenv("TELEMETRY_DB_USER"), "\n")
  cat("- Password: [hidden]\n\n")
  
  # Attempt to connect
  tryCatch({
    # Create connection
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("TELEMETRY_DB_NAME"),
      host = Sys.getenv("TELEMETRY_DB_HOST"),
      port = as.integer(Sys.getenv("TELEMETRY_DB_PORT", "5432")),
      user = Sys.getenv("TELEMETRY_DB_USER"),
      password = Sys.getenv("TELEMETRY_DB_PASSWORD")
    )
    
    # Test if connection is valid
    if (dbIsValid(con)) {
      cat("✅ Successfully connected to database!\n")
      
      # List tables to verify we have access
      tables <- dbListTables(con)
      cat("Tables in database:\n")
      print(tables)
      
      # Close connection
      dbDisconnect(con)
      cat("✅ Successfully disconnected\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("❌ Connection failed: ", e$message, "\n")
    return(FALSE)
  })
}

# Run the test
test_connection()