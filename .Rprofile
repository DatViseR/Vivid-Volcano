source("renv/activate.R")

# Vivid-Volcano project .Rprofile
# Automatically set PostgreSQL environment on project startup

.First <- function() {
  message("Setting up PostgreSQL environment for Vivid-Volcano...")
  
  # Set environment variables for PostgreSQL
  pg_bin <- "C:/Program Files/PostgreSQL/17/bin"
  pg_lib <- "C:/Program Files/PostgreSQL/17/lib"
  
  Sys.setenv(PATH = paste(pg_bin, Sys.getenv("PATH"), sep = ";"))
  Sys.setenv(PKG_CONFIG_PATH = file.path(pg_lib, "pkgconfig"))
  Sys.setenv(LIB_PQ = pg_bin)
  
  # Only show confirmation if PostgreSQL is found
  if (file.exists(file.path(pg_bin, "libpq.dll"))) {
    message("✓ PostgreSQL environment configured successfully")
  }
}

