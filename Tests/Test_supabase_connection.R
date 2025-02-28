## Test connection to Supabase PostgreSQL database

# Keep the environment variables set
pg_bin <- "C:/Program Files/PostgreSQL/17/bin"
Sys.setenv(PATH = paste(pg_bin, Sys.getenv("PATH"), sep = ";"))

# Load required libraries
library(DBI)
library(RPostgres)

# Test actual connection to Supabase
cat("Testing connection to Supabase PostgreSQL database...\n")

tryCatch({
  # Get the connection parameters from environment variables
  host <- Sys.getenv("TELEMETRY_DB_HOST")
  port <- as.numeric(Sys.getenv("TELEMETRY_DB_PORT"))
  dbname <- Sys.getenv("TELEMETRY_DB_NAME")
  user <- Sys.getenv("TELEMETRY_DB_USER")
  password <- Sys.getenv("TELEMETRY_DB_PASSWORD")
  
  # Display connection info (without showing password)
  cat("Connection parameters:\n")
  cat("- Host:", host, "\n")
  cat("- Port:", port, "\n")
  cat("- Database:", dbname, "\n")
  cat("- User:", user, "\n")
  cat("- Password: [hidden]\n\n")
  
  # Create a connection
  con <- dbConnect(RPostgres::Postgres(),
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
  
  cat("✅ Successfully connected to database!\n")
  
  # Test by listing tables
  tables <- dbListTables(con)
  cat("Tables in database:\n")
  print(tables)
  
  # Disconnect
  dbDisconnect(con)
  cat("✅ Successfully disconnected\n")
  
}, error = function(e) {
  cat("❌ Connection failed:", e$message, "\n")
  
  # Provide helpful advice based on error
  if (grepl("SCRAM", e$message)) {
    cat("\nStill seeing SCRAM authentication error.\n")
    cat("This could happen if the environment variables were not passed to your script.\n")
  } else if (grepl("password authentication", e$message)) {
    cat("\nPassword authentication failed. Please check your credentials.\n")
  } else if (grepl("connect", e$message)) {
    cat("\nCould not connect to the server. Please check if:\n")
    cat("1. The host is correct\n")
    cat("2. The port is correct\n")
    cat("3. Network allows the connection\n")
  }
})