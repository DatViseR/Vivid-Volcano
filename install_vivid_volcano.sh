#!/bin/bash

# ====================================================================
# Vivid Volcano - Universal Installation Script 
# ====================================================================
# Author: DatViseR
# Date: 2025-07-07
# Description: Universal installation for local and cloud environments
# Repository: https://github.com/DatViseR/Vivid-Volcano
# Works on: macOS, Linux, Codespaces, Gitpod, and other containers
# ====================================================================

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

detect_environment() {
    # Cloud environments
    if [[ -n "${CODESPACE_NAME:-}" ]] || [[ -n "${GITHUB_CODESPACES:-}" ]]; then
        echo "codespaces"
    elif [[ -n "${GITPOD_WORKSPACE_ID:-}" ]]; then
        echo "gitpod"
    elif [[ -n "${COLAB_GPU:-}" ]]; then
        echo "colab"
    elif [[ -n "${JUPYTER_SERVER_ROOT:-}" ]]; then
        echo "jupyter"
    # Local environments
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
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

is_container_environment() {
    local env=$(detect_environment)
    case $env in
        codespaces|gitpod|colab|jupyter)
            return 0
            ;;
        *)
            # Additional container detection
            if [[ -f /.dockerenv ]] || [[ -f /run/.containerenv ]]; then
                return 0
            fi
            return 1
            ;;
    esac
}

# Auto-install missing prerequisites
auto_install_prerequisites() {
    print_header "CHECKING AND INSTALLING PREREQUISITES"
    
    local env=$(detect_environment)
    print_info "Detected environment: $env"
    
    local need_r=false
    local need_git=false
    
    # Check what's missing
    if ! command_exists git; then
        need_git=true
        print_warning "Git is not installed"
    fi
    
    if ! command_exists R; then
        need_r=true
        print_warning "R is not installed"
    fi
    
    # Auto-install for supported environments
    if [[ "$need_git" == true ]] || [[ "$need_r" == true ]]; then
        case $env in
            codespaces|ubuntu|debian|gitpod)
                if command_exists apt-get; then
                    print_step "Auto-installing prerequisites for $env..."
                    
                    # Update package list with proper error handling
                    print_info "Updating package list..."
                    sudo apt-get update -qq || {
                        print_warning "Package list update failed, but continuing..."
                    }
                    
                    local packages_to_install=()
                    
                    if [[ "$need_git" == true ]]; then
                        packages_to_install+=("git")
                    fi
                    
                    if [[ "$need_r" == true ]]; then
                        packages_to_install+=("r-base" "r-base-dev")
                    fi
                    
                    if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                        print_info "Installing: ${packages_to_install[*]}"
                        
                        # Install with better error handling to prevent freezing
                        timeout 300 sudo apt-get install -y "${packages_to_install[@]}" || {
                            print_error "Auto-installation failed or timed out"
                            print_info "Please install manually:"
                            [[ "$need_git" == true ]] && print_info "  sudo apt-get install git"
                            [[ "$need_r" == true ]] && print_info "  sudo apt-get install r-base r-base-dev"
                            return 1
                        }
                        
                        print_success "Prerequisites auto-installed successfully"
                    fi
                fi
                ;;
            fedora|centos|rhel)
                if command_exists dnf; then
                    print_step "Auto-installing prerequisites for $env..."
                    local packages_to_install=()
                    
                    if [[ "$need_git" == true ]]; then
                        packages_to_install+=("git")
                    fi
                    
                    if [[ "$need_r" == true ]]; then
                        packages_to_install+=("R" "R-devel")
                    fi
                    
                    if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                        sudo dnf install -y "${packages_to_install[@]}" || {
                            print_warning "Some packages failed to install"
                        }
                    fi
                fi
                ;;
            macos)
                print_info "macOS detected - please install missing prerequisites manually:"
                [[ "$need_git" == true ]] && print_info "  Git: Install Xcode Command Line Tools"
                [[ "$need_r" == true ]] && print_info "  R: Download from https://cran.r-project.org/"
                return 1
                ;;
            *)
                print_warning "Cannot auto-install on $env environment"
                print_info "Please install missing prerequisites manually"
                return 1
                ;;
        esac
    fi
    
    return 0
}

# Enhanced system library detection
check_system_library() {
    local lib_name="$1"
    local header_file="$2"
    local pkg_config_name="$3"
    
    # Method 1: Check via pkg-config
    if command_exists pkg-config && [[ -n "$pkg_config_name" ]]; then
        if pkg-config --exists "$pkg_config_name" 2>/dev/null; then
            return 0
        fi
    fi
    
    # Method 2: Check for header file
    if [[ -n "$header_file" ]]; then
        local search_paths=(
            "/usr/include"
            "/usr/local/include"
            "/opt/homebrew/include"
            "/opt/local/include"
            "/usr/include/x86_64-linux-gnu"
        )
        
        for path in "${search_paths[@]}"; do
            if [[ -f "$path/$header_file" ]]; then
                return 0
            fi
        done
    fi
    
    # Method 3: OS-specific checks
    local env=$(detect_environment)
    case $env in
        macos)
            if command_exists brew && brew list "$lib_name" &>/dev/null; then
                return 0
            fi
            ;;
        ubuntu|debian|codespaces|gitpod)
            if dpkg -l | grep -q "lib${lib_name}.*-dev"; then
                return 0
            fi
            ;;
        fedora|centos|rhel)
            if rpm -qa | grep -q "${lib_name}.*-devel"; then
                return 0
            fi
            ;;
    esac
    
    return 1
}

# Enhanced dependency installation with individual choice
install_enhanced_dependencies() {
    print_header "SYSTEM DEPENDENCIES INSTALLATION"
    
    local env=$(detect_environment)
    print_info "Environment: $env"
    
    case $env in
        ubuntu|debian|codespaces|gitpod)
            if command_exists apt-get; then
                print_step "Available Ubuntu/Debian dependencies"
                sudo apt-get update -qq || print_warning "Package update failed"
                
                # Core dependencies (always install)
                local core_packages=("libssl-dev" "libcurl4-openssl-dev")
                print_info "Installing core dependencies: ${core_packages[*]}"
                sudo apt-get install -y "${core_packages[@]}" || {
                    print_error "Failed to install core dependencies"
                    exit 1
                }
                
                # Optional dependencies with descriptions
                declare -A optional_packages=(
                    ["libxml2-dev"]="XML library - Optional for XML processing (xml2 package)"
                    ["libfontconfig1-dev"]="Font configuration - Optional for text rendering"
                    ["libcairo2-dev"]="Cairo graphics - Optional for high-quality graphics"
                    ["libharfbuzz-dev"]="Text shaping - Optional for complex text layout"
                    ["libfribidi-dev"]="Bidirectional text - Optional for right-to-left text"
                    ["libfreetype6-dev"]="Font rendering - Optional for custom fonts"
                    ["libpng-dev"]="PNG support - Optional for PNG processing"
                    ["libjpeg-dev"]="JPEG support - Optional for JPEG processing"
                )
                
                local selected_packages=()
                
                echo
                print_info "Optional dependencies (choose individually):"
                for pkg in "${!optional_packages[@]}"; do
                    echo -e "${CYAN}Package:${NC} $pkg"
                    echo -e "${YELLOW}Description:${NC} ${optional_packages[$pkg]}"
                    read -p "Install $pkg? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        selected_packages+=("$pkg")
                        echo -e "${GREEN}✓ Will install $pkg${NC}"
                    else
                        echo -e "${YELLOW}⚬ Skipping $pkg${NC}"
                    fi
                    echo
                done
                
                if [[ ${#selected_packages[@]} -gt 0 ]]; then
                    print_info "Installing selected packages: ${selected_packages[*]}"
                    sudo apt-get install -y "${selected_packages[@]}" || {
                        print_warning "Some optional packages failed to install"
                    }
                fi
            fi
            ;;
            
        fedora|centos|rhel)
            if command_exists dnf; then
                local core_packages=("openssl-devel" "libcurl-devel")
                print_info "Installing core dependencies: ${core_packages[*]}"
                sudo dnf install -y "${core_packages[@]}"
                
                declare -A optional_packages=(
                    ["libxml2-devel"]="XML2 library"
                    ["fontconfig-devel"]="Font configuration"
                    ["cairo-devel"]="Cairo graphics"
                    ["harfbuzz-devel"]="Text shaping"
                    ["fribidi-devel"]="Bidirectional text"
                    ["freetype-devel"]="Font rendering"
                    ["libpng-devel"]="PNG support"
                    ["libjpeg-turbo-devel"]="JPEG support"
                )
                
                # Similar selection process for optional packages
                local selected_packages=()
                echo
                for pkg in "${!optional_packages[@]}"; do
                    echo -e "${CYAN}Package:${NC} $pkg - ${optional_packages[$pkg]}"
                    read -p "Install $pkg? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        selected_packages+=("$pkg")
                    fi
                done
                
                if [[ ${#selected_packages[@]} -gt 0 ]]; then
                    sudo dnf install -y "${selected_packages[@]}"
                fi
            fi
            ;;
            
        macos)
            if command_exists brew; then
                local core_packages=("openssl" "curl")
                print_info "Installing core dependencies: ${core_packages[*]}"
                brew install "${core_packages[@]}"
                
                declare -A optional_packages=(
                    ["libxml2"]="XML2 library"
                    ["cairo"]="Cairo graphics"
                    ["harfbuzz"]="Text shaping"
                    ["fribidi"]="Bidirectional text"
                    ["freetype"]="Font rendering"
                    ["libpng"]="PNG support"
                    ["jpeg"]="JPEG support"
                )
                
                # Similar selection process
                local selected_packages=()
                echo
                for pkg in "${!optional_packages[@]}"; do
                    echo -e "${CYAN}Package:${NC} $pkg - ${optional_packages[$pkg]}"
                    read -p "Install $pkg? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        selected_packages+=("$pkg")
                    fi
                done
                
                if [[ ${#selected_packages[@]} -gt 0 ]]; then
                    brew install "${selected_packages[@]}"
                fi
            else
                print_warning "Homebrew not found. Install from: https://brew.sh"
            fi
            ;;
    esac
}

# Prerequisites check with auto-installation
check_prerequisites() {
    print_header "CHECKING PREREQUISITES"
    
    # Attempt auto-installation first
    auto_install_prerequisites || {
        print_warning "Auto-installation failed or not supported"
        print_info "Continuing with manual prerequisite check..."
    }
    
    local missing_deps=()
    
    # Check Git
    if ! command_exists git; then
        missing_deps+=("git")
        print_error "Git is not installed"
    else
        print_success "Git found: $(git --version)"
    fi
    
    # Check R with better error handling
    if ! command_exists R; then
        missing_deps+=("R")
        print_error "R is not installed"
    else
        # Safer R version check
        local r_version_output
        r_version_output=$(timeout 10 R --version 2>/dev/null | head -n1) || {
            print_warning "Could not determine R version (R may be installed but not responding)"
            r_version_output="R (version check failed)"
        }
        print_success "R found: $r_version_output"
        
        if ! check_r_version 2>/dev/null; then
            print_warning "R version should be 4.4+ for optimal compatibility"
        fi
    fi
    
    # Check disk space
    local available_space=$(df . | tail -1 | awk '{print $4}')
    local space_gb=$((available_space / 1024 / 1024))
    
    if [[ $space_gb -lt 2 ]]; then
        print_warning "Available disk space: ${space_gb}GB (recommend 2GB+)"
    else
        print_success "Available disk space: ${space_gb}GB"
    fi
    
    # Check internet connectivity
    if ping -c 1 github.com >/dev/null 2>&1; then
        print_success "Internet connectivity confirmed"
    else
        # Don't fail in container environments
        if is_container_environment; then
            print_warning "Limited internet connectivity detected (common in containers)"
        else
            missing_deps+=("internet")
            print_error "No internet connectivity to GitHub"
        fi
    fi
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        print_error "Missing dependencies: ${missing_deps[*]}"
        print_info "Please install missing dependencies and run the script again"
        exit 1
    fi
    
    print_success "All prerequisites satisfied"
}

# Repository setup (unchanged)
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

# Enhanced renv setup with dependency-aware installation
setup_enhanced_renv_environment() {
    print_header "SETTING UP R ENVIRONMENT"
    
    print_step "Installing renv package manager"
    
    # Install renv with timeout to prevent freezing
    timeout 120 R --slave --no-restore --no-save -e "
        if (!requireNamespace('renv', quietly = TRUE)) {
            cat('Installing renv globally...\n')
            install.packages('renv', repos = 'https://cloud.r-project.org/')
        } else {
            cat('renv already available globally\n')
        }
    " || {
        print_error "Failed to install renv (timeout or error)"
        exit 1
    }
    
    print_success "renv is available"
    
    # Check system library availability
    local cairo_available=FALSE
    local xml2_available=FALSE
    
    if check_system_library "cairo" "cairo/cairo.h" "cairo"; then
        cairo_available=TRUE
        print_info "Cairo system library: Available"
    else
        print_info "Cairo system library: Not available"
    fi
    
    if check_system_library "xml2" "libxml/parser.h" "libxml-2.0"; then
        xml2_available=TRUE
        print_info "XML2 system library: Available"
    else
        print_info "XML2 system library: Not available"
    fi
    
    print_step "Setting up environment with intelligent package selection"
    
    # Create enhanced restoration script
    cat > renv_enhanced_restore.R << EOF
# Enhanced renv restoration script with dependency awareness

cat("=== VIVID VOLCANO ENHANCED INSTALLATION ===\n")
cat("Starting at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set options
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 300)
options(download.file.method = "auto")

# System library availability
CAIRO_AVAILABLE <- ${cairo_available}
XML2_AVAILABLE <- ${xml2_available}

cat("System library status:\n")
cat("  Cairo available:", CAIRO_AVAILABLE, "\n")
cat("  XML2 available:", XML2_AVAILABLE, "\n\n")

# Activate renv
cat("1. Activating renv environment...\n")
source("renv/activate.R")
cat("   ✓ renv environment activated\n")

# Enhanced package installation
install_enhanced_packages <- function() {
    cat("\n2. Installing packages with dependency awareness...\n")
    
    library(renv)
    
    # Core packages (always install)
    core_packages <- c(
        "shiny", "shinyjs", "htmltools", "htmlwidgets", "httpuv",
        "dplyr", "tidyr", "readr", "data.table", "magrittr",
        "tibble", "purrr", "stringr", "lubridate",
        "ggplot2", "ggtext", "ggrepel", "scales", "gridExtra",
        "RColorBrewer", "colourpicker", "viridisLite",
        "DT", "reactable",
        "shiny.semantic", "semantic.dashboard", "semantic.assets",
        "jsonlite", "digest", "rlang", "cli", "glue",
        "lifecycle", "vctrs", "pillar", "fansi", "utf8",
        "httr", "curl", "mime", "base64enc", "markdown",
        "shinyalert", "arrow"
    )
    
    # Conditional packages based on system libraries
    conditional_packages <- list()
    
    if (CAIRO_AVAILABLE) {
        conditional_packages[["Cairo"]] <- "Cairo graphics device"
        conditional_packages[["webshot2"]] <- "Web screenshots"
    }
    
    if (XML2_AVAILABLE) {
        conditional_packages[["xml2"]] <- "XML processing"
        conditional_packages[["gt"]] <- "Grammar of tables"
    }
    
    # Optional packages (try but don't fail)
    optional_packages <- c("plotly")
    
    # Install core packages
    cat("   Installing", length(core_packages), "core packages...\n")
    failed_core <- c()
    for (pkg in core_packages) {
        cat("   Installing", pkg, "... ")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat("✓\n")
            TRUE
        }, error = function(e) {
            cat("✗\n")
            failed_core <<- c(failed_core, pkg)
            FALSE
        })
    }
    
    # Install conditional packages
    if (length(conditional_packages) > 0) {
        cat("\n   Installing conditional packages...\n")
        failed_conditional <- c()
        for (pkg in names(conditional_packages)) {
            cat("   Installing", pkg, "... ")
            result <- tryCatch({
                renv::install(pkg, prompt = FALSE)
                cat("✓\n")
                TRUE
            }, error = function(e) {
                cat("✗\n")
                failed_conditional <<- c(failed_conditional, pkg)
                FALSE
            })
        }
    }
    
    # Try optional packages
    cat("\n   Installing optional packages...\n")
    failed_optional <- c()
    for (pkg in optional_packages) {
        cat("   Installing", pkg, "... ")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat("✓\n")
            TRUE
        }, error = function(e) {
            cat("⚠\n")
            failed_optional <<- c(failed_optional, pkg)
            FALSE
        })
    }
    
    cat("\n   Installation summary:\n")
    cat("   ✓ Core packages:", length(core_packages) - length(failed_core), "/", length(core_packages), "\n")
    if (exists("failed_conditional")) {
        cat("   ✓ Conditional packages:", length(conditional_packages) - length(failed_conditional), "/", length(conditional_packages), "\n")
    }
    cat("   ⚠ Failed optional:", length(failed_optional), "\n")
    
    success_rate <- (length(core_packages) - length(failed_core)) / length(core_packages)
    return(success_rate >= 0.8)
}

# Verification and testing functions
verify_installation <- function() {
    cat("\n3. Verifying installation...\n")
    essential <- c("shiny", "dplyr", "ggplot2", "DT", "shiny.semantic")
    missing <- c()
    
    for (pkg in essential) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            missing <- c(missing, pkg)
            cat("   ✗", pkg, "\n")
        } else {
            cat("   ✓", pkg, "\n")
        }
    }
    
    return(length(missing) == 0)
}

test_basic_app <- function() {
    cat("\n4. Testing app components...\n")
    tryCatch({
        library(shiny, quietly = TRUE)
        library(dplyr, quietly = TRUE)
        library(ggplot2, quietly = TRUE)
        cat("   ✓ Core libraries load successfully\n")
        
        if (file.exists("app.R")) {
            parsed <- parse("app.R")
            cat("   ✓ app.R syntax is valid\n")
        }
        return(TRUE)
    }, error = function(e) {
        cat("   ✗ Error:", e\$message, "\n")
        return(FALSE)
    })
}

# Main execution
main <- function() {
    install_success <- install_enhanced_packages()
    verify_success <- verify_installation()
    test_success <- test_basic_app()
    
    cat("\n=== INSTALLATION SUMMARY ===\n")
    if (install_success && verify_success && test_success) {
        cat("Status: ✅ READY TO USE\n")
        return("READY")
    } else if (verify_success) {
        cat("Status: ⚠ CORE FUNCTIONALITY AVAILABLE\n")
        return("PARTIAL")
    } else {
        cat("Status: ❌ INSTALLATION INCOMPLETE\n")
        return("INCOMPLETE")
    }
}

result <- main()
cat("Installation result:", result, "\n")
quit(status = if(result %in% c("READY", "PARTIAL")) 0 else 1)
EOF

    # Execute the enhanced restoration
    R --no-restore --no-save < renv_enhanced_restore.R
    
    local exit_code=$?
    
    # Clean up
    rm -f renv_enhanced_restore.R
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "R environment setup completed successfully"
    else
        print_warning "Setup completed with some limitations"
    fi
}

# Launch application with environment detection
launch_application() {
    print_header "LAUNCHING VIVID VOLCANO"
    
    if [[ ! -f "app.R" ]]; then
        print_error "app.R not found in current directory"
        return 1
    fi
    
    # Detect environment for proper configuration
    local env=$(detect_environment)
    local host="127.0.0.1"
    local port="8080"
    local launch_browser="TRUE"
    
    # Container environment configuration
    if is_container_environment; then
        host="0.0.0.0"
        port="3000"
        launch_browser="FALSE"
        
        case $env in
            codespaces)
                print_info "🌋 Starting Vivid Volcano for GitHub Codespaces..."
                print_info "📱 Access through VS Code 'Ports' tab (port 3000)"
                ;;
            gitpod)
                print_info "🌋 Starting Vivid Volcano for Gitpod..."
                print_info "📱 Gitpod will automatically open the app in a new tab"
                ;;
            *)
                print_info "🌋 Starting Vivid Volcano for container environment..."
                print_info "📱 Access through port 3000"
                ;;
        esac
        print_info "🔗 Make port public to share with colleagues"
    else
        print_info "🌋 Starting Vivid Volcano..."
        print_info "📱 The app will open in your default browser"
    fi
    
    print_info "🛑 To stop the app, press Ctrl+C"
    
    # Launch the application
    R --slave --no-restore --no-save -e "
        source('renv/activate.R')
        
        essential <- c('shiny', 'dplyr', 'ggplot2', 'DT')
        missing <- c()
        
        for (pkg in essential) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                missing <- c(missing, pkg)
            }
        }
        
        if (length(missing) > 0) {
            cat('❌ Missing essential packages:', paste(missing, collapse = ', '), '\n')
            quit(status = 1)
        }
        
        cat('✅ Environment verified\n')
        cat('🚀 Launching Vivid Volcano...\n\n')
        
        library(shiny)
        runApp('app.R', host = '$host', port = $port, launch.browser = $launch_browser)
    "
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_info "👋 Vivid Volcano session ended"
    else
        print_warning "Application ended with issues"
        print_info "Manual launch command:"
        print_info "  R -e \"source('renv/activate.R'); shiny::runApp('app.R', host='$host', port=$port)\""
    fi
}

# Main installation function
main() {
    local start_time=$(date +%s)
    
    print_header "VIVID VOLCANO UNIVERSAL INSTALLER v4.2"
    print_info "Repository: https://github.com/DatViseR/Vivid-Volcano"
    print_info "Author: DatViseR"
    print_info "Environment: $(detect_environment)"
    print_info "Started at: $(date)"
    print_info ""
    print_info "💡 Note: This is a self-hosted alternative."
    print_info "   For instant access, visit: https://vivid-volcano.com"
    
    # Ask user about system dependencies
    echo
    if is_container_environment; then
        print_info "Container environment detected - will install essential dependencies"
        read -p "Proceed with installation? (Y/n): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Nn]$ ]]; then
            print_info "Installation cancelled"
            exit 0
        fi
    else
        print_info "System dependencies will be presented for individual selection"
        read -p "Proceed with system dependency installation? (Y/n): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Nn]$ ]]; then
            print_info "Skipping system dependencies"
        else
            install_enhanced_dependencies || {
                print_warning "System dependencies installation had issues, but continuing..."
            }
        fi
    fi
    
    # Execute installation steps
    check_prerequisites
    setup_repository
    setup_enhanced_renv_environment
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local minutes=$((duration / 60))
    local seconds=$((duration % 60))
    
    print_header "INSTALLATION COMPLETED!"
    print_success "Total time: ${minutes}m ${seconds}s"
    print_info "Location: $(pwd)"
    
    echo
    print_success "🎉 Vivid Volcano is ready! 🌋"
    
    # Environment-specific usage instructions
    if is_container_environment; then
        print_info ""
        print_info "🌐 CONTAINER USAGE:"
        print_info "  - App will run on port 3000"
        case $(detect_environment) in
            codespaces)
                print_info "  - Access via VS Code 'Ports' tab"
                print_info "  - Make port public to share with colleagues"
                ;;
            gitpod)
                print_info "  - Gitpod will automatically handle port forwarding"
                ;;
            *)
                print_info "  - Access via your container's port forwarding"
                ;;
        esac
    fi
    
    # Ask if user wants to launch now
    echo
    read -p "Would you like to launch Vivid Volcano now? (Y/n): " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
        echo
        launch_application
    else
        echo
        print_info "To launch later:"
        print_info "  cd $(pwd)"
        if is_container_environment; then
            print_info "  R -e \"source('renv/activate.R'); shiny::runApp('app.R', host='0.0.0.0', port=3000)\""
        else
            print_info "  R -e \"source('renv/activate.R'); shiny::runApp()\""
        fi
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