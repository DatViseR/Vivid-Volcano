#!/bin/bash

# Vivid Volcano App Launcher Shell Script
# Provides an easy way to launch the Vivid Volcano app with enhanced browser support

set -e

# Color and formatting functions
print_header() {
    echo
    echo "================================================================"
    echo "  $1"
    echo "================================================================"
}

print_info() {
    echo "ℹ️  $1"
}

print_success() {
    echo "✅ $1"
}

print_error() {
    echo "❌ $1"
}

print_warning() {
    echo "⚠️  $1"
}

# Main launcher function
main() {
    print_header "VIVID VOLCANO LAUNCHER"
    
    # Check if we're in the right directory
    if [[ ! -f "app.R" ]] || [[ ! -f "renv.lock" ]]; then
        print_error "Not in Vivid Volcano directory"
        print_info "Please navigate to the Vivid-Volcano directory and try again"
        print_info "Expected files: app.R, renv.lock"
        exit 1
    fi
    
    # Check if R is available
    if ! command -v R >/dev/null 2>&1; then
        print_error "R is not installed or not in PATH"
        print_info "Please install R (version 4.4+ recommended) from https://cran.r-project.org/"
        exit 1
    fi
    
    print_success "Found Vivid Volcano installation"
    print_info "🌋 Starting enhanced launcher..."
    
    # Check if launch_app.R exists
    if [[ -f "launch_app.R" ]]; then
        print_info "📱 Using enhanced launcher with macOS browser support"
        print_info "🛑 To stop the app, press Ctrl+C"
        echo
        
        # Run the enhanced launcher
        R --slave --no-restore --no-save -f launch_app.R
        
    else
        print_warning "Enhanced launcher not found, falling back to basic mode"
        print_info "📱 The app will attempt to open in your default web browser"
        print_info "🛑 To stop the app, press Ctrl+C"
        echo
        
        # Fallback to basic runApp
        R --slave --no-restore --no-save -e "
            # Activate renv if available
            if (file.exists('renv/activate.R')) {
                source('renv/activate.R')
            }
            
            # Load shiny and run app
            library(shiny)
            
            # Enhanced browser launching for macOS
            if (Sys.info()['sysname'] == 'Darwin') {
                custom_browser <- function(url) {
                    cat('🍎 macOS detected - trying enhanced browser launching...\n')
                    cat('📍 App URL:', url, '\n')
                    
                    # Try multiple methods
                    success <- FALSE
                    
                    # Method 1: system open command
                    tryCatch({
                        system(paste('open', url), wait = FALSE)
                        success <- TRUE
                        cat('✅ Browser launched successfully\n')
                    }, error = function(e) {
                        cat('⚠️ open command failed, providing manual instructions...\n')
                    })
                    
                    if (!success) {
                        cat('📋 Manual Instructions:\n')
                        cat('   1. Open your web browser\n')
                        cat('   2. Navigate to:', url, '\n')
                        cat('🛑 To stop the app, press Ctrl+C\n')
                        
                        # Try to copy to clipboard
                        tryCatch({
                            system(paste('echo', shQuote(url), '| pbcopy'))
                            cat('📋 URL copied to clipboard!\n')
                        }, error = function(e) {})
                    }
                    
                    return(TRUE)
                }
                
                runApp('app.R', launch.browser = custom_browser, host = '127.0.0.1')
            } else {
                runApp('app.R', launch.browser = TRUE, host = '127.0.0.1')
            }
        "
    fi
    
    local exit_code=$?
    
    echo
    if [[ $exit_code -eq 0 ]]; then
        print_success "👋 Vivid Volcano session ended successfully"
    else
        print_warning "App ended with issues (exit code: $exit_code)"
    fi
}

# Handle interruption gracefully
trap 'echo; print_info "👋 Launcher interrupted"; exit 0' INT TERM

# Run main function
main "$@"