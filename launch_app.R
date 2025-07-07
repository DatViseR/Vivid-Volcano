#!/usr/bin/env Rscript

# Vivid Volcano App Launcher
# This script provides robust browser launching with platform-specific handling
# and fallback mechanisms for when automatic browser opening fails.

cat("🌋 Vivid Volcano Launcher\n")
cat("========================\n\n")

# Check if we're in the right directory
if (!file.exists("app.R") || !file.exists("renv.lock")) {
    cat("❌ Error: Not in Vivid Volcano directory\n")
    cat("Please navigate to the Vivid-Volcano directory and try again.\n")
    quit(status = 1)
}

# Activate renv if available
if (file.exists("renv/activate.R")) {
    cat("📦 Activating renv environment...\n")
    source("renv/activate.R")
}

# Check essential packages
essential_packages <- c("shiny", "dplyr", "ggplot2", "DT")
missing_packages <- c()

for (pkg in essential_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        missing_packages <- c(missing_packages, pkg)
    }
}

if (length(missing_packages) > 0) {
    cat("❌ Missing essential packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Please run: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    quit(status = 1)
}

cat("✅ All essential packages verified\n")

# Detect operating system
get_os <- function() {
    if (.Platform$OS.type == "windows") {
        return("windows")
    } else if (Sys.info()["sysname"] == "Darwin") {
        return("macos")
    } else {
        return("linux")
    }
}

# Platform-specific browser launcher
launch_browser <- function(url, os) {
    success <- FALSE
    
    if (os == "macos") {
        # macOS-specific browser launching with multiple fallbacks
        cat("🍎 Detected macOS - using enhanced browser launching...\n")
        
        # Method 1: Try system default browser via open command
        tryCatch({
            system(paste("open", url), wait = FALSE)
            success <- TRUE
            cat("✅ Browser launched via 'open' command\n")
        }, error = function(e) {
            cat("⚠️ 'open' command failed, trying alternatives...\n")
        })
        
        # Method 2: Try specific browsers if default failed
        if (!success) {
            browsers <- c(
                "/Applications/Safari.app/Contents/MacOS/Safari",
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                "/Applications/Firefox.app/Contents/MacOS/firefox"
            )
            
            for (browser in browsers) {
                if (file.exists(browser)) {
                    tryCatch({
                        system(paste("'", browser, "'", url), wait = FALSE)
                        success <- TRUE
                        cat("✅ Browser launched:", basename(browser), "\n")
                        break
                    }, error = function(e) {
                        # Continue to next browser
                    })
                }
            }
        }
        
    } else if (os == "linux") {
        # Linux browser launching
        tryCatch({
            system(paste("xdg-open", url), wait = FALSE)
            success <- TRUE
            cat("✅ Browser launched via xdg-open\n")
        }, error = function(e) {
            cat("⚠️ xdg-open failed\n")
        })
        
    } else if (os == "windows") {
        # Windows browser launching
        tryCatch({
            system(paste("start", url), wait = FALSE)
            success <- TRUE
            cat("✅ Browser launched via start command\n")
        }, error = function(e) {
            cat("⚠️ start command failed\n")
        })
    }
    
    return(success)
}

# Custom browser launching function
custom_launch_browser <- function(appUrl, browser.path = getOption("browser")) {
    os <- get_os()
    
    cat("🚀 Starting Vivid Volcano...\n")
    cat("📍 App URL:", appUrl, "\n")
    cat("🖥️  Operating System:", os, "\n\n")
    
    # Try platform-specific launching
    if (launch_browser(appUrl, os)) {
        cat("🎉 App should now be opening in your browser!\n")
        cat("🛑 To stop the app, press Ctrl+C in this terminal\n\n")
        return(TRUE)
    }
    
    # Fallback: provide manual instructions
    cat("⚠️ Automatic browser launching failed\n")
    cat("📋 Manual Instructions:\n")
    cat("   1. Open your web browser\n")
    cat("   2. Navigate to:", appUrl, "\n")
    cat("   3. Or copy and paste this URL into your browser's address bar\n")
    cat("🛑 To stop the app, press Ctrl+C in this terminal\n\n")
    
    # Copy URL to clipboard if possible (macOS/Linux)
    if (os == "macos") {
        tryCatch({
            system(paste("echo '", appUrl, "' | pbcopy"))
            cat("📋 URL copied to clipboard!\n\n")
        }, error = function(e) {})
    } else if (os == "linux") {
        tryCatch({
            system(paste("echo '", appUrl, "' | xclip -selection clipboard"))
            cat("📋 URL copied to clipboard!\n\n")
        }, error = function(e) {})
    }
    
    return(FALSE)
}

# Load shiny library
library(shiny)

cat("🌋 Launching Vivid Volcano...\n\n")

# Run the app with custom browser launching
tryCatch({
    runApp(
        appDir = "app.R",
        host = "127.0.0.1",
        port = NULL,  # Let shiny choose an available port
        launch.browser = custom_launch_browser
    )
}, error = function(e) {
    cat("❌ Error launching app:", e$message, "\n")
    cat("💡 Try running manually with: shiny::runApp('app.R')\n")
    quit(status = 1)
}, interrupt = function(e) {
    cat("\n👋 Vivid Volcano session ended\n")
    quit(status = 0)
})