#!/bin/bash

# ====================================================================
# Vivid Volcano - Core Installation Script (PostgreSQL-free)
# ====================================================================
# Author: DatViseR
# Date: 2025-06-21
# Description: Clones Vivid Volcano repository, sets up core renv environment and runs the application
# Repository: https://github.com/DatViseR/Vivid-Volcano
#  ====================================================================

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Logging functions
print_header() {
    echo -e "\n${PURPLE}=================================${NC}"
    echo -e "${PURPLE}$1${NC}"
    echo -e "${PURPLE}=================================${NC}\n"
}

print_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

print_info() {
    echo -e "${CYAN}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Utility functions
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

check_r_version() {
    local r_version=$(R --version | head -n1 | grep -o 'R version [0-9]\+\.[0-9]\+\.[0-9]\+' | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+')
    local major=$(echo $r_version | cut -d. -f1)
    local minor=$(echo $r_version | cut -d. -f2)
    
    if [[ $major -gt 4 ]] || [[ $major -eq 4 && $minor -ge 4 ]]; then
        return 0
    else
        return 1
    fi
}

detect_os() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if command_exists lsb_release; then
            echo $(lsb_release -si | tr '[:upper:]' '[:lower:]')
        elif [[ -f /etc/os-release ]]; then
            echo $(grep '^ID=' /etc/os-release | cut -d= -f2 | tr -d '"' | tr '[:upper:]' '[:lower:]')
        else
            echo "linux"
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "msys" ]]; then
        echo "windows"
    else
        echo "unknown"
    fi
}

# Essential system dependencies (minimal set)
install_essential_dependencies() {
    print_header "INSTALLING ESSENTIAL SYSTEM DEPENDENCIES"
    
    local os=$(detect_os)
    print_info "Detected OS: $os"
    
    case $os in
        ubuntu|debian)
            print_step "Available Ubuntu/Debian dependencies"
            if command_exists apt-get; then
                print_info "Updating package list..."
                sudo apt-get update -qq
                
                # Define packages with descriptions
                declare -A packages_desc=(
                    ["libssl-dev"]="SSL library - Required for secure connections (openssl, httr packages)"
                    ["libcurl4-openssl-dev"]="cURL library - Required for HTTP requests (curl, httr packages)"
                    ["libxml2-dev"]="XML library - Optional for XML processing (xml2 package)"
                    ["libfontconfig1-dev"]="Font configuration - Optional for advanced text rendering"
                    ["libcairo2-dev"]="Cairo graphics - Optional for high-quality graphics output"
                    ["libharfbuzz-dev"]="Text shaping - Optional for complex text layout"
                    ["libfribidi-dev"]="Bidirectional text - Optional for right-to-left text support"
                    ["libfreetype6-dev"]="Font rendering - Optional for custom font support"
                    ["libpng-dev"]="PNG support - Optional for PNG image processing"
                    ["libjpeg-dev"]="JPEG support - Optional for JPEG image processing"
                )
                
                local packages_to_install=()
                
                echo
                print_info "Please choose which dependencies to install:"
                echo
                
                for package in "${!packages_desc[@]}"; do
                    echo -e "${CYAN}Package:${NC} ${package}"
                    echo -e "${YELLOW}Description:${NC} ${packages_desc[$package]}"
                    read -p "Install $package? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        packages_to_install+=("$package")
                        echo -e "${GREEN}✓ Will install $package${NC}"
                    else
                        echo -e "${YELLOW}⚬ Skipping $package${NC}"
                    fi
                    echo
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    print_info "Installing selected packages: ${packages_to_install[*]}"
                    sudo apt-get install -y "${packages_to_install[@]}" || {
                        print_warning "Some packages failed to install, but continuing with core installation..."
                    }
                    print_success "Selected dependencies installation completed"
                else
                    print_info "No packages selected for installation"
                fi
            else
                print_warning "apt-get not found, skipping system dependencies"
            fi
            ;;
            
        fedora|centos|rhel)
            print_step "Available Red Hat/Fedora dependencies"
            if command_exists dnf; then
                # Define packages with descriptions
                declare -A packages_desc=(
                    ["openssl-devel"]="SSL library - Required for secure connections (openssl, httr packages)"
                    ["libcurl-devel"]="cURL library - Required for HTTP requests (curl, httr packages)"
                    ["libxml2-devel"]="XML library - Optional for XML processing (xml2 package)"
                    ["fontconfig-devel"]="Font configuration - Optional for advanced text rendering"
                    ["cairo-devel"]="Cairo graphics - Optional for high-quality graphics output"
                    ["harfbuzz-devel"]="Text shaping - Optional for complex text layout"
                    ["fribidi-devel"]="Bidirectional text - Optional for right-to-left text support"
                    ["freetype-devel"]="Font rendering - Optional for custom font support"
                    ["libpng-devel"]="PNG support - Optional for PNG image processing"
                    ["libjpeg-turbo-devel"]="JPEG support - Optional for JPEG image processing"
                )
                
                local packages_to_install=()
                
                echo
                print_info "Please choose which dependencies to install:"
                echo
                
                for package in "${!packages_desc[@]}"; do
                    echo -e "${CYAN}Package:${NC} ${package}"
                    echo -e "${YELLOW}Description:${NC} ${packages_desc[$package]}"
                    read -p "Install $package? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        packages_to_install+=("$package")
                        echo -e "${GREEN}✓ Will install $package${NC}"
                    else
                        echo -e "${YELLOW}⚬ Skipping $package${NC}"
                    fi
                    echo
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    print_info "Installing selected packages: ${packages_to_install[*]}"
                    sudo dnf install -y "${packages_to_install[@]}" || {
                        print_warning "Some packages failed to install, but continuing..."
                    }
                    print_success "Selected dependencies installation completed"
                else
                    print_info "No packages selected for installation"
                fi
            elif command_exists yum; then
                print_info "Using yum package manager - installing essential packages only"
                declare -A packages_desc=(
                    ["openssl-devel"]="SSL library - Required for secure connections"
                    ["libcurl-devel"]="cURL library - Required for HTTP requests"
                    ["libxml2-devel"]="XML library - Optional for XML processing"
                )
                
                local packages_to_install=()
                
                echo
                for package in "${!packages_desc[@]}"; do
                    echo -e "${CYAN}Package:${NC} ${package}"
                    echo -e "${YELLOW}Description:${NC} ${packages_desc[$package]}"
                    read -p "Install $package? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        packages_to_install+=("$package")
                        echo -e "${GREEN}✓ Will install $package${NC}"
                    else
                        echo -e "${YELLOW}⚬ Skipping $package${NC}"
                    fi
                    echo
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    sudo yum install -y "${packages_to_install[@]}" || {
                        print_warning "Some packages failed to install, but continuing..."
                    }
                fi
            else
                print_warning "Neither dnf nor yum found, skipping system dependencies"
            fi
            ;;
            
        macos)
            print_step "Available macOS dependencies (via Homebrew)"
            if command_exists brew; then
                declare -A packages_desc=(
                    ["openssl"]="SSL library - Required for secure connections (openssl, httr packages)"
                    ["curl"]="cURL library - Required for HTTP requests (curl, httr packages)"
                    ["libxml2"]="XML library - Optional for XML processing (xml2 package)"
                    ["cairo"]="Cairo graphics - Optional for high-quality graphics output"
                    ["harfbuzz"]="Text shaping - Optional for complex text layout"
                    ["fribidi"]="Bidirectional text - Optional for right-to-left text support"
                    ["freetype"]="Font rendering - Optional for custom font support"
                    ["libpng"]="PNG support - Optional for PNG image processing"
                    ["jpeg"]="JPEG support - Optional for JPEG image processing"
                )
                
                local packages_to_install=()
                
                echo
                print_info "Please choose which dependencies to install:"
                echo
                
                for package in "${!packages_desc[@]}"; do
                    echo -e "${CYAN}Package:${NC} ${package}"
                    echo -e "${YELLOW}Description:${NC} ${packages_desc[$package]}"
                    read -p "Install $package? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        packages_to_install+=("$package")
                        echo -e "${GREEN}✓ Will install $package${NC}"
                    else
                        echo -e "${YELLOW}⚬ Skipping $package${NC}"
                    fi
                    echo
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    print_info "Installing selected packages: ${packages_to_install[*]}"
                    brew install "${packages_to_install[@]}" || {
                        print_warning "Some packages failed to install, but continuing..."
                    }
                    print_success "Selected dependencies installation completed"
                else
                    print_info "No packages selected for installation"
                fi
            else
                print_warning "Homebrew not found. Install Homebrew first: https://brew.sh"
            fi
            ;;
            
        *)
            print_warning "Unknown OS: $os - skipping system dependencies"
            print_info "You may need to install development libraries manually if needed"
            ;;
    esac
}

# Prerequisites check
check_prerequisites() {
    print_header "CHECKING PREREQUISITES"
    
    local missing_deps=()
    
    # Check Git
    if ! command_exists git; then
        missing_deps+=("git")
        print_error "Git is not installed"
    else
        print_success "Git found: $(git --version)"
    fi
    
    # Check R
    if ! command_exists R; then
        missing_deps+=("R")
        print_error "R is not installed"
    else
        local r_version=$(R --version | head -n1)
        print_success "R found: $r_version"
        
        if ! check_r_version; then
            print_warning "R version should be 4.4+ for optimal compatibility"
        fi
    fi
    
    # Check available disk space (minimum 2GB)
    local available_space=$(df . | tail -1 | awk '{print $4}')
    local space_gb=$((available_space / 1024 / 1024))
    
    if [[ $space_gb -lt 2 ]]; then
        print_warning "Available disk space: ${space_gb}GB (recommend 2GB+)"
    else
        print_success "Available disk space: ${space_gb}GB"
    fi
    
    # Check internet connectivity
    if curl -s 1 github.com >/dev/null 2>&1; then
        print_success "Internet connectivity confirmed"
    else
        missing_deps+=("internet")
        print_error "No internet connectivity to GitHub"
    fi
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        print_error "Missing dependencies: ${missing_deps[*]}"
        print_info "Please install missing dependencies and run the script again"
        exit 1
    fi
    
    print_success "All prerequisites satisfied"
}

# Clone or update repository
setup_repository() {
    print_header "SETTING UP REPOSITORY"
    
    local repo_url="https://github.com/DatViseR/Vivid-Volcano.git"
    local repo_dir="Vivid-Volcano"
    
    if [[ -d "$repo_dir" ]]; then
        print_info "Found existing Vivid-Volcano directory"
        cd "$repo_dir"
        
        if [[ -d ".git" ]]; then
            print_step "Updating existing repository"
            git pull origin master || {
                print_warning "Could not update repository (continuing with existing version)"
            }
        else
            print_warning "Directory exists but is not a git repository"
            print_info "Using existing directory as-is"
        fi
    else
        print_step "Cloning Vivid Volcano repository"
        git clone "$repo_url" "$repo_dir" || {
            print_error "Failed to clone repository"
            exit 1
        }
        print_success "Repository cloned successfully"
        cd "$repo_dir"
    fi
    
    # Verify essential files
    print_step "Verifying repository structure"
    local essential_files=("app.R" "renv.lock" "renv/activate.R")
    local missing_files=()
    
    for file in "${essential_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            missing_files+=("$file")
        fi
    done
    
    if [[ ${#missing_files[@]} -gt 0 ]]; then
        print_error "Missing essential files: ${missing_files[*]}"
        exit 1
    fi
    
    print_success "Repository structure verified"
    print_info "Current directory: $(pwd)"
}

# Core renv environment setup without problematic packages
setup_core_renv_environment() {
    print_header "SETTING UP CORE RENV ENVIRONMENT"
    
    print_step "Installing renv globally (if needed)"
    
    R --slave --no-restore --no-save -e "
        if (!requireNamespace('renv', quietly = TRUE)) {
            cat('Installing renv globally...\n')
            install.packages('renv', repos = 'https://cloud.r-project.org/')
        } else {
            cat('renv already available globally\n')
        }
    " || {
        print_error "Failed to install renv"
        exit 1
    }
    
    print_success "renv is available"
    
    print_step "Setting up core environment with selective package installation"
    print_info "This will skip problematic packages and focus on core functionality..."
    
    # Create core restoration script that excludes problematic packages
    cat > renv_core_restore.R << 'EOF'
# Core renv restoration script - excludes problematic packages

cat("=== VIVID VOLCANO CORE INSTALLATION ===\n")
cat("Starting at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set options
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 600)
options(download.file.method = "auto")

# Function to activate renv
activate_renv <- function() {
    cat("1. Activating renv environment...\n")
    source("renv/activate.R")
    cat("   ✓ renv environment activated\n")
    cat("   ✓ Primary library path:", .libPaths()[1], "\n")
    return(TRUE)
}

# Function to install core packages only
install_core_packages <- function() {
    cat("\n2. Installing core packages (excluding problematic ones)...\n")
    
    library(renv)
    
    # Define core packages for Vivid Volcano functionality
    core_packages <- c(
        # Shiny core
        "shiny", "shinyjs", "htmltools", "htmlwidgets", "httpuv",
        
        # Data manipulation
        "dplyr", "tidyr", "readr", "data.table", "magrittr",
        "tibble", "purrr", "stringr", "lubridate",
        
        # Visualization core
        "ggplot2", "ggtext", "ggrepel", "scales", "gridExtra",
        "RColorBrewer", "colourpicker", "viridisLite",
        
        # Data tables and GT
        "DT", "gt", "reactable",
        
        # Shiny semantic
        "shiny.semantic", "semantic.dashboard", "semantic.assets",
        
        # Essential utilities
        "jsonlite", "digest", "rlang", "cli", "glue",
        "lifecycle", "vctrs", "pillar", "fansi", "utf8",
        
        # File I/O and web
        "httr", "curl", "mime", "base64enc", "markdown",
        
        # Alerts and notifications
        "shinyalert",
        
        # Arrow (should work without system deps)
        "arrow"
    )
    
    # Optional packages (try but don't fail if they don't work)
    optional_packages <- c(
        "plotly",      # Interactive plots
        "Cairo",       # Graphics device
        "webshot2",    # Screenshots
        "gt"           # Tables
    )
    
    # Packages to SKIP (known to cause issues)
    skip_packages <- c(
        "RPostgres",   # PostgreSQL - not needed for core functionality
        "RSQLite",     # SQLite - not needed for core functionality
        "V8",          # JavaScript - optional
        "xml2"         # XML - optional for core functionality
    )
    
    cat("   Installing", length(core_packages), "core packages...\n")
    
    # Install core packages
    failed_core <- c()
    for (pkg in core_packages) {
        cat("   Installing", pkg, "... ")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat("✓\n")
            TRUE
        }, error = function(e) {
            cat("✗ (", e$message, ")\n")
            failed_core <<- c(failed_core, pkg)
            FALSE
        })
    }
    
    cat("\n   Attempting optional packages...\n")
    
    # Try optional packages
    failed_optional <- c()
    for (pkg in optional_packages) {
        cat("   Installing", pkg, "... ")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat("✓\n")
            TRUE
        }, error = function(e) {
            cat("⚠ (optional - ", e$message, ")\n")
            failed_optional <<- c(failed_optional, pkg)
            FALSE
        })
    }
    
    cat("\n   Core installation summary:\n")
    cat("   ✓ Successful core packages:", length(core_packages) - length(failed_core), "/", length(core_packages), "\n")
    cat("   ⚠ Failed core packages:", length(failed_core), "\n")
    cat("   ⚠ Failed optional packages:", length(failed_optional), "\n")
    cat("   🚫 Skipped problematic packages:", length(skip_packages), "\n")
    
    if (length(failed_core) > 0) {
        cat("   Failed core packages:", paste(failed_core, collapse = ", "), "\n")
    }
    
    # Return success if most core packages installed
    success_rate <- (length(core_packages) - length(failed_core)) / length(core_packages)
    return(success_rate >= 0.8)  # 80% success rate required
}

# Function to verify installation
verify_installation <- function() {
    cat("\n3. Verifying core installation...\n")
    
    # Essential packages for basic app functionality
    essential <- c("shiny", "dplyr", "ggplot2", "DT", "shiny.semantic")
    
    missing_essential <- c()
    for (pkg in essential) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            missing_essential <- c(missing_essential, pkg)
            cat("   ✗", pkg, "(missing)\n")
        } else {
            cat("   ✓", pkg, "\n")
        }
    }
    
    if (length(missing_essential) == 0) {
        cat("   ✓ All essential packages verified\n")
        return(TRUE)
    } else {
        cat("   ✗ Missing essential packages:", paste(missing_essential, collapse = ", "), "\n")
        return(FALSE)
    }
}

# Function to test basic app loading
test_basic_app <- function() {
    cat("\n4. Testing basic app components...\n")
    
    tryCatch({
        # Test essential libraries
        library(shiny, quietly = TRUE)
        library(dplyr, quietly = TRUE)
        library(ggplot2, quietly = TRUE)
        
        cat("   ✓ Core libraries load successfully\n")
        
        # Test if app.R can be parsed (not executed)
        if (file.exists("app.R")) {
            parsed <- parse("app.R")
            cat("   ✓ app.R syntax is valid\n")
        }
        
        return(TRUE)
        
    }, error = function(e) {
        cat("   ✗ Error testing app:", e$message, "\n")
        return(FALSE)
    })
}

# Function to create snapshot
create_core_snapshot <- function() {
    cat("\n5. Creating environment snapshot...\n")
    
    tryCatch({
        # Update the lockfile to reflect our core installation
        renv::snapshot(prompt = FALSE)
        cat("   ✓ Core environment snapshot created\n")
    }, error = function(e) {
        cat("   ⚠ Could not create snapshot:", e$message, "\n")
        cat("   (This is not critical for app functionality)\n")
    })
}

# Generate summary
generate_summary <- function(install_success, verify_success, test_success) {
    cat("\n=== CORE INSTALLATION SUMMARY ===\n")
    cat("Completion time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Working directory:", getwd(), "\n")
    cat("R version:", R.version.string, "\n")
    cat("renv library:", .libPaths()[1], "\n")
    
    # Installation status
    if (install_success && verify_success && test_success) {
        cat("Installation status: ✓ CORE FUNCTIONALITY READY\n")
        status <- "READY"
    } else if (install_success && verify_success) {
        cat("Installation status: ⚠ CORE PACKAGES INSTALLED (app may work)\n")
        status <- "PARTIAL"
    } else {
        cat("Installation status: ✗ CORE INSTALLATION INCOMPLETE\n")
        status <- "INCOMPLETE"
    }
    
    # Check for data files
    data_files <- c("GO.parquet2", "www/demo_data.csv")
    cat("\nData files status:\n")
    for (file in data_files) {
        if (file.exists(file)) {
            size_mb <- round(file.info(file)$size / 1024 / 1024, 2)
            cat("   ✓", file, "(", size_mb, "MB)\n")
        } else {
            cat("   ✗", file, "(missing)\n")
        }
    }
    
    cat("\n=== APPLICATION READY ===\n")
    if (status == "READY") {
        cat("🎉 Core installation successful!\n")
        cat("The application is ready to run in your browser.\n")
    } else if (status == "PARTIAL") {
        cat("⚠ Partial installation. The app may still work.\n")
        cat("Some features may be limited.\n")
    } else {
        cat("❌ Installation incomplete. Check error messages above.\n")
        cat("You may try running the app anyway.\n")
    }
    
    cat("\nNote: This core installation excludes PostgreSQL and other\n")
    cat("problematic packages. The app focuses on volcano plot generation\n")
    cat("and GO analysis without database connectivity.\n")
    cat("================================\n")
    
    return(status)
}

# Main execution
main <- function() {
    status <- "INCOMPLETE"
    
    tryCatch({
        # Step 1: Activate renv
        activate_result <- activate_renv()
        if (!activate_result) stop("Failed to activate renv")
        
        # Step 2: Install core packages
        install_result <- install_core_packages()
        
        # Step 3: Verify installation
        verify_result <- verify_installation()
        
        # Step 4: Test basic app functionality
        test_result <- test_basic_app()
        
        # Step 5: Create snapshot
        create_core_snapshot()
        
        # Step 6: Generate summary
        status <- generate_summary(install_result, verify_result, test_result)
        
        if (status %in% c("READY", "PARTIAL")) {
            cat("\n🎉 VIVID VOLCANO CORE INSTALLATION COMPLETED! 🎉\n")
            return(TRUE)
        } else {
            cat("\n⚠ INSTALLATION COMPLETED WITH ISSUES ⚠\n")
            return(FALSE)
        }
        
    }, error = function(e) {
        cat("\n❌ CORE INSTALLATION FAILED ❌\n")
        cat("Error:", e$message, "\n")
        cat("\nTry:\n")
        cat("1. Running the script again\n")
        cat("2. Installing R packages manually\n")
        cat("3. Checking system dependencies\n")
        
        return(FALSE)
    })
}

# Execute main function
result <- main()
if (result) {
    cat("✅ Ready to use Vivid Volcano!\n")
} else {
    cat("⚠ Installation had issues but may still work\n")
}
EOF

    # Execute the core restoration
    R --no-restore --no-save < renv_core_restore.R
    
    local exit_code=$?
    
    # Clean up
    rm -f renv_core_restore.R
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "Core renv environment setup completed"
    else
        print_warning "Core setup completed with some issues"
    fi
}

# Generate simple report
generate_simple_report() {
    print_header "GENERATING INSTALLATION REPORT"
    
    local report_file="vivid_volcano_core_report_$(date +%Y%m%d_%H%M%S).txt"
    
    cat > "$report_file" << EOF
VIVID VOLCANO CORE INSTALLATION REPORT
======================================
Date: $(date)
User: $USER
System: $(uname -a)
Working Directory: $(pwd)
Script Version: 3.0 (Core - PostgreSQL-free)

STRATEGY:
- Focus on core Shiny app functionality
- Exclude problematic packages (RPostgres, RSQLite, V8, etc.)
- Ensure volcano plot generation and GO analysis work
- Skip database connectivity features

PREREQUISITES:
- Git: $(git --version 2>/dev/null || echo "Not found")
- R Version: $(R --version | head -n1 2>/dev/null || echo "Not found")
- Internet: $(curl -s 1 github.com >/dev/null 2>&1 && echo "Connected" || echo "Not connected")

REPOSITORY:
- Directory: $(pwd)
- Essential files: $(test -f app.R && echo "app.R ✓" || echo "app.R ✗") $(test -f renv.lock && echo "renv.lock ✓" || echo "renv.lock ✗")

PACKAGE STRATEGY:
- INCLUDED: shiny, dplyr, ggplot2, DT, ggtext, ggrepel, arrow, gt, etc.
- EXCLUDED: RPostgres, RSQLite, V8, xml2 (problematic)
- OPTIONAL: Cairo, webshot2, plotly (nice to have)

DATA FILES:
- GO.parquet2: $(test -f GO.parquet2 && echo "Present ($(du -h GO.parquet2 | cut -f1))" || echo "Missing")
- Demo Data: $(test -f www/demo_data.csv && echo "Present" || echo "Missing")

TO RUN VIVID VOLCANO:
Enhanced launcher (recommended): ./launch_app.sh
R launcher with browser support: R -f launch_app.R
Basic command: R -e "shiny::runApp()"
From RStudio: Open and run app.R

LIMITATIONS IN THE CORE MASTER VERSION designed to work locally:
- No PostgreSQL database connectivity (Install dependencies manually and clone deployed with telemetry branch of the repository)
- Limited database features 

For support: https://github.com/DatViseR/Vivid-Volcano or datviser@gmail.com
EOF

    print_success "Core installation report saved: $report_file"
}

# Launch application function
launch_application() {
    print_header "LAUNCHING VIVID VOLCANO"
    
    # Check if we're in the right directory
    if [[ ! -f "app.R" ]] || [[ ! -f "renv.lock" ]]; then
        print_error "Not in Vivid Volcano directory"
        print_info "Cannot launch application - essential files missing"
        return 1
    fi
    
    # Detect operating system for enhanced browser launching
    local os_type="unknown"
    if [[ "$OSTYPE" == "darwin"* ]]; then
        os_type="macos"
        print_info "🍎 macOS detected - using enhanced browser launching"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        os_type="linux"
        print_info "🐧 Linux detected"
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
        os_type="windows"
        print_info "🪟 Windows detected"
    fi
    
    print_info "🌋 Starting Vivid Volcano (Core Version)..."
    print_info "📱 The app will open in your default web browser"
    print_info "🛑 To stop the app, press Ctrl+C in this terminal"
    
    # Use enhanced launcher if available, otherwise fallback to basic launcher
    if [[ -f "launch_app.R" ]]; then
        print_info "🚀 Using enhanced launcher with improved browser support..."
        
        R --slave --no-restore --no-save -f launch_app.R
    else
        print_info "🚀 Using basic launcher..."
        
        # Launch the application with enhanced browser launching for macOS
        R --slave --no-restore --no-save -e "
            # Activate renv
            source('renv/activate.R')
            
            # Check essential packages
            essential <- c('shiny', 'dplyr', 'ggplot2', 'DT')
            missing <- c()
            
            for (pkg in essential) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing <- c(missing, pkg)
                }
            }
            
            if (length(missing) > 0) {
                cat('❌ Missing essential packages:', paste(missing, collapse = ', '), '\n')
                cat('Installation may be incomplete\n')
                quit(status = 1)
            }
            
            cat('✅ Core environment verified\n')
            cat('🚀 Launching Vivid Volcano...\n\n')
            
            # Load shiny library
            library(shiny)
            
            # Enhanced browser launching for macOS
            if (Sys.info()['sysname'] == 'Darwin') {
                cat('🍎 macOS detected - using enhanced browser launching...\n')
                
                # Custom browser launcher for macOS
                custom_browser <- function(url) {
                    cat('📍 App URL:', url, '\n')
                    success <- FALSE
                    
                    # Method 1: Try system open command
                    tryCatch({
                        system(paste('open', url), wait = FALSE)
                        success <- TRUE
                        cat('✅ Browser launched via open command\n')
                    }, error = function(e) {
                        cat('⚠️ open command failed, trying alternatives...\n')
                    })
                    
                    # Method 2: Try specific browsers if default failed
                    if (!success) {
                        browsers <- c(
                            '/Applications/Safari.app/Contents/MacOS/Safari',
                            '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
                            '/Applications/Firefox.app/Contents/MacOS/firefox'
                        )
                        
                        for (browser in browsers) {
                            if (file.exists(browser)) {
                                tryCatch({
                                    system(paste(shQuote(browser), url), wait = FALSE)
                                    success <- TRUE
                                    cat('✅ Browser launched:', basename(browser), '\n')
                                    break
                                }, error = function(e) {
                                    # Continue to next browser
                                })
                            }
                        }
                    }
                    
                    # Fallback: provide manual instructions
                    if (!success) {
                        cat('⚠️ Automatic browser launching failed\n')
                        cat('📋 Manual Instructions:\n')
                        cat('   1. Open your web browser\n')
                        cat('   2. Navigate to:', url, '\n')
                        cat('🛑 To stop the app, press Ctrl+C\n')
                        
                        # Try to copy URL to clipboard
                        tryCatch({
                            system(paste('echo', shQuote(url), '| pbcopy'))
                            cat('📋 URL copied to clipboard!\n')
                        }, error = function(e) {})
                    }
                    
                    cat('🛑 To stop the app, press Ctrl+C in this terminal\n\n')
                    return(TRUE)
                }
                
                runApp('app.R', launch.browser = custom_browser, host = '127.0.0.1')
            } else {
                # Standard launching for other platforms
                runApp('app.R', launch.browser = TRUE, host = '127.0.0.1')
            }
        "
    fi
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_info "👋 Vivid Volcano session ended successfully"
    else
        print_warning "Application ended with issues"
        print_info "💡 Alternative ways to run the app:"
        print_info "   • Use enhanced launcher: ./launch_app.sh"
        print_info "   • Use R launcher: R -f launch_app.R"
        print_info "   • Basic manual run: R -e \"shiny::runApp()\""
    fi
}

# Main installation function
main() {
    local start_time=$(date +%s)
    
    print_header "VIVID VOLCANO CORE INSTALLER"
    print_info "Repository: https://github.com/DatViseR/Vivid-Volcano"
    print_info "Started at: $(date)"
    
    # Ask user about system dependencies with individual choice
    echo
    print_info "System dependencies will be presented individually for your selection"
    read -p "Proceed with system dependency installation? (Y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Nn]$ ]]; then
        print_info "Skipping all system dependencies"
    else
        install_essential_dependencies || {
            print_warning "System dependencies installation had issues, but continuing..."
        }
    fi
    
    # Execute installation steps
    check_prerequisites
    setup_repository
    setup_core_renv_environment
    generate_simple_report
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local minutes=$((duration / 60))
    local seconds=$((duration % 60))
    
    print_header "CORE INSTALLATION COMPLETED!"
    print_success "Total time: ${minutes}m ${seconds}s"
    print_info "Location: $(pwd)"
    
    echo
    print_header "READY TO USE VIVID VOLCANO!"
    
    print_step "Features available:"
    echo "   ✅ Upload CSV/TSV omics data"
    echo "   ✅ Generate publication-ready volcano plots"
    echo "   ✅ Perform GO enrichment analysis"
    echo "   ✅ Interactive and static plot downloads"
    echo "   ✅ Custom gene labeling and coloring"
    echo
    print_step "Excluded from core version:"
    echo "   ❌ PostgreSQL database features"
    echo "   ❌ Some advanced graphics options"
    echo "   ❌ Web screenshot features"
    echo
    
    print_success "🎉 Core functionality ready - Vivid Volcano is installed! 🌋"
    
    # Ask user if they want to run the application now
    echo
    print_info "The application is ready to run in your browser."
    read -p "Would you like to run Vivid Volcano now? (Y/n): " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
        echo
        launch_application
    else
        echo
        print_info "You can run Vivid Volcano later by navigating to the Vivid-Volcano directory and running:"
        print_info "🚀 Enhanced launcher (recommended): ./launch_app.sh"
        print_info "🚀 R launcher with browser support: R -f launch_app.R"  
        print_info "🚀 Basic command: R -e \"shiny::runApp()\""
        print_info "🚀 Or run the app.R file directly in RStudio"
        echo
        print_success "Installation complete! 🌋"
    fi
    
    return 0
}

# Error handling
trap 'print_error "Installation interrupted"; exit 1' INT TERM

# Run main function
main "$@"
exit 0
