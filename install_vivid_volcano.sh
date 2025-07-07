#!/bin/bash

# ====================================================================
# Vivid Volcano - Universal Installation Script 
# ====================================================================
# Author: DatViseR
# Date: 2025-01-08
# Description: Production-ready universal installer with enhanced macOS dependency handling
# Repository: https://github.com/DatViseR/Vivid-Volcano
# Works on: macOS, Linux, Codespaces, Gitpod, and other containers
# ====================================================================

set -e  # Exit on any error

# Colors and formatting
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Logging functions
print_header() {
    echo -e "\n${PURPLE}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${PURPLE}${BOLD} $1${NC}"
    echo -e "${PURPLE}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
}

print_step() {
    echo -e "${BLUE}▶${NC} $1"
}

print_info() {
    echo -e "${CYAN}ℹ${NC} $1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Enhanced progress indicator with spinner
show_progress() {
    local message="$1"
    local duration="${2:-60}"
    local pid=$!
    
    local spinner='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
    local delay=0.1
    local i=0
    
    echo -ne "${CYAN}⏳${NC} $message "
    
    while kill -0 $pid 2>/dev/null; do
        local char="${spinner:$i:1}"
        echo -ne "\r${CYAN}⏳${NC} $message ${char}"
        sleep $delay
        i=$(( (i+1) % ${#spinner} ))
    done
    
    wait $pid
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo -e "\r${GREEN}✓${NC} $message - completed successfully"
    else
        echo -e "\r${RED}✗${NC} $message - failed"
    fi
    
    return $exit_code
}

# Enhanced command execution with progress
execute_with_progress() {
    local command="$1"
    local message="$2"
    
    eval "$command" >/dev/null 2>&1 &
    show_progress "$message"
}

# Utility functions
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

has_timeout() {
    command_exists timeout
}

detect_environment() {
    if [[ -n "${CODESPACE_NAME:-}" ]] || [[ -n "${GITHUB_CODESPACES:-}" ]]; then
        echo "codespaces"
    elif [[ -n "${GITPOD_WORKSPACE_ID:-}" ]]; then
        echo "gitpod"
    elif [[ -n "${COLAB_GPU:-}" ]]; then
        echo "colab"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if command_exists lsb_release; then
            lsb_release -si | tr '[:upper:]' '[:lower:]'
        elif [[ -f /etc/os-release ]]; then
            grep '^ID=' /etc/os-release | cut -d= -f2 | tr -d '"' | tr '[:upper:]' '[:lower:]'
        else
            echo "linux"
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    else
        echo "unknown"
    fi
}

is_container_environment() {
    local env=$(detect_environment)
    case $env in
        codespaces|gitpod|colab)
            return 0
            ;;
        *)
            if [[ -f /.dockerenv ]] || [[ -f /run/.containerenv ]]; then
                return 0
            fi
            return 1
            ;;
    esac
}

# Enhanced R version check with error handling
check_r_version() {
    if ! command_exists R; then
        return 1
    fi
    
    local r_version
    if has_timeout; then
        r_version=$(timeout 10 R --version 2>/dev/null | head -n1 | grep -o 'R version [0-9]\+\.[0-9]\+\.[0-9]\+' | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' 2>/dev/null) || return 1
    else
        r_version=$(R --version 2>/dev/null | head -n1 | grep -o 'R version [0-9]\+\.[0-9]\+\.[0-9]\+' | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' 2>/dev/null) || return 1
    fi
    
    local major minor
    major=$(echo "$r_version" | cut -d. -f1)
    minor=$(echo "$r_version" | cut -d. -f2)
    
    if [[ $major -gt 4 ]] || [[ $major -eq 4 && $minor -ge 4 ]]; then
        return 0
    else
        return 1
    fi
}

# Enhanced system library detection (especially for macOS Cairo)
check_system_library() {
    local lib_name="$1"
    local header_file="$2"
    local pkg_config_name="$3"
    
    # Method 1: pkg-config check
    if command_exists pkg-config && [[ -n "$pkg_config_name" ]]; then
        if pkg-config --exists "$pkg_config_name" 2>/dev/null; then
            return 0
        fi
    fi
    
    # Method 2: Header file search with enhanced macOS paths
    if [[ -n "$header_file" ]]; then
        local search_paths=(
            "/usr/include"
            "/usr/local/include"
            "/opt/homebrew/include"              # Apple Silicon Homebrew
            "/usr/local/opt/cairo/include"       # Intel Homebrew Cairo
            "/opt/homebrew/opt/cairo/include"    # Apple Silicon Homebrew Cairo
            "/usr/local/opt/libxml2/include"     # Intel Homebrew libxml2
            "/opt/homebrew/opt/libxml2/include"  # Apple Silicon Homebrew libxml2
            "/opt/local/include"                 # MacPorts
            "/usr/include/x86_64-linux-gnu"     # Linux multiarch
            "/usr/include/cairo"                 # Direct Cairo path
            "/usr/include/libxml2"               # Direct libxml2 path
        )
        
        for path in "${search_paths[@]}"; do
            if [[ -f "$path/$header_file" ]]; then
                return 0
            fi
            # Also check without subdirectory for some cases
            if [[ "$header_file" == *"/"* ]]; then
                local base_header
                base_header=$(basename "$header_file")
                if [[ -f "$path/$base_header" ]]; then
                    return 0
                fi
            fi
        done
    fi
    
    # Method 3: OS-specific package manager checks
    local env
    env=$(detect_environment)
    case $env in
        macos)
            if command_exists brew; then
                # Check multiple possible formula names for macOS
                local brew_names=("$lib_name")
                case $lib_name in
                    cairo)
                        brew_names=("cairo" "libcairo")
                        ;;
                    xml2)
                        brew_names=("libxml2" "xml2")
                        ;;
                    ssl)
                        brew_names=("openssl" "openssl@3" "openssl@1.1")
                        ;;
                esac
                
                for brew_name in "${brew_names[@]}"; do
                    if brew list "$brew_name" &>/dev/null; then
                        return 0
                    fi
                done
            fi
            ;;
        ubuntu|debian|codespaces|gitpod)
            if command_exists dpkg; then
                if dpkg -l 2>/dev/null | grep -q "lib${lib_name}.*-dev"; then
                    return 0
                fi
            fi
            ;;
        fedora|centos|rhel)
            if command_exists rpm; then
                if rpm -qa 2>/dev/null | grep -q "${lib_name}.*-devel"; then
                    return 0
                fi
            fi
            ;;
    esac
    
    return 1
}

# Prerequisites check and auto-installation
check_and_install_prerequisites() {
    print_header "PREREQUISITES"
    
    local env
    env=$(detect_environment)
    local missing_prereqs=false
    
    # Check and auto-install Git
    if ! command_exists git; then
        print_warning "Git not found"
        case $env in
            codespaces|ubuntu|debian|gitpod)
                if command_exists apt-get; then
                    execute_with_progress "sudo apt-get update -qq && sudo apt-get install -y git" "Installing Git"
                fi
                ;;
            macos)
                print_info "Install Git via Xcode Command Line Tools: xcode-select --install"
                missing_prereqs=true
                ;;
        esac
    else
        print_success "Git available"
    fi
    
    # Check and auto-install R
    if ! command_exists R; then
        print_warning "R not found"
        case $env in
            codespaces|ubuntu|debian|gitpod)
                if command_exists apt-get; then
                    execute_with_progress "sudo apt-get update -qq && sudo apt-get install -y r-base r-base-dev" "Installing R"
                fi
                ;;
            macos)
                print_info "Download R from: https://cran.r-project.org/"
                missing_prereqs=true
                ;;
        esac
    else
        print_success "R available"
        if ! check_r_version; then
            print_warning "R version <4.4 detected (4.4+ recommended)"
        fi
    fi
    
    # macOS specific checks
    if [[ "$env" == "macos" ]]; then
        if ! xcode-select -p >/dev/null 2>&1; then
            print_warning "Xcode Command Line Tools not installed"
            print_info "Run: xcode-select --install"
            missing_prereqs=true
        fi
        
        if ! command_exists brew; then
            print_warning "Homebrew not found (recommended for dependencies)"
            print_info "Install from: https://brew.sh"
        fi
    fi
    
    # Final prerequisite check
    if [[ "$missing_prereqs" == true ]]; then
        print_error "Please install missing prerequisites and rerun the script"
        exit 1
    fi
    
    # Verify we have what we need
    if ! command_exists git || ! command_exists R; then
        print_error "Required prerequisites still missing after installation attempt"
        exit 1
    fi
    
    # Check disk space
    local available_space
    available_space=$(df . | tail -1 | awk '{print $4}')
    local space_gb=$((available_space / 1024 / 1024))
    
    if [[ $space_gb -lt 2 ]]; then
        print_warning "Available space: ${space_gb}GB (2GB+ recommended)"
    else
        print_success "Sufficient disk space: ${space_gb}GB"
    fi
    
    # Check connectivity
    if ping -c 1 github.com >/dev/null 2>&1; then
        print_success "Internet connectivity confirmed"
    else
        if is_container_environment; then
            print_warning "Limited connectivity (common in containers)"
        else
            print_error "No GitHub connectivity - check internet connection"
            exit 1
        fi
    fi
    
    print_success "All prerequisites satisfied"
}

# System dependencies installation with proper flow
install_system_dependencies() {
    print_header "SYSTEM DEPENDENCIES"
    
    local env
    env=$(detect_environment)
    print_info "Environment: $env"
    
    case $env in
        ubuntu|debian|codespaces|gitpod)
            print_step "Installing system libraries for Ubuntu/Debian"
            
            # Essential dependencies (no choice)
            local essential_packages=("libssl-dev" "libcurl4-openssl-dev")
            print_info "Installing essential libraries (required)"
            execute_with_progress "sudo apt-get update -qq && sudo apt-get install -y ${essential_packages[*]}" "Installing essential libraries"
            
            # Optional dependencies with clear explanations
            echo
            print_step "Optional system libraries (choose which features you want)"
            echo
            
            local optional_deps=(
                "libxml2-dev:XML data processing support:Enables xml2 R package for XML/HTML parsing"
                "libcairo2-dev:High-quality graphics rendering:Enables Cairo R package for publication-quality plots"
                "libfontconfig1-dev:Advanced font support:Better text rendering in plots"
                "libharfbuzz-dev:Complex text layout:Support for complex scripts and typography"
                "libfribidi-dev:Bidirectional text:Right-to-left text support (Arabic, Hebrew)"
                "libfreetype6-dev:Font rendering engine:Enhanced font display"
                "libpng-dev:PNG image support:PNG file processing capabilities"
                "libjpeg-dev:JPEG image support:JPEG file processing capabilities"
            )
            
            local selected_packages=()
            for pkg_info in "${optional_deps[@]}"; do
                local pkg="${pkg_info%%:*}"
                local feature="${pkg_info#*:}"
                local description="${feature#*:}"
                feature="${feature%%:*}"
                
                echo -e "${CYAN}Feature:${NC} $feature"
                echo -e "${YELLOW}Package:${NC} $pkg"
                echo -e "${BLUE}Description:${NC} $description"
                read -p "Install this feature? (Y/n): " -n 1 -r
                echo
                if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                    selected_packages+=("$pkg")
                    print_success "Will install $pkg"
                else
                    print_info "Skipping $pkg"
                fi
                echo
            done
            
            if [[ ${#selected_packages[@]} -gt 0 ]]; then
                execute_with_progress "sudo apt-get install -y ${selected_packages[*]}" "Installing selected optional libraries"
            fi
            ;;
            
        macos)
            print_step "Installing system libraries for macOS via Homebrew"
            
            if ! command_exists brew; then
                print_error "Homebrew is required for macOS system dependencies"
                print_info "Install Homebrew first: https://brew.sh"
                print_info "Then rerun this script"
                exit 1
            fi
            
            # Essential dependencies for macOS
            local essential_packages=("openssl" "curl")
            print_info "Installing essential libraries"
            for pkg in "${essential_packages[@]}"; do
                if ! brew list "$pkg" &>/dev/null; then
                    execute_with_progress "brew install $pkg" "Installing $pkg"
                else
                    print_success "$pkg already installed"
                fi
            done
            
            # Optional dependencies with Cairo focus
            echo
            print_step "Optional Homebrew packages (choose which features you want)"
            echo
            
            local optional_deps=(
                "libxml2:XML data processing:Enables xml2 R package for XML/HTML parsing"
                "cairo:High-quality graphics rendering:Enables Cairo R package - HIGHLY RECOMMENDED for publication plots"
                "harfbuzz:Complex text layout:Advanced typography support"
                "fribidi:Bidirectional text:Right-to-left text support"
                "freetype:Font rendering engine:Enhanced font display"
                "libpng:PNG image support:PNG file processing"
                "jpeg:JPEG image support:JPEG file processing"
            )
            
            local selected_packages=()
            for pkg_info in "${optional_deps[@]}"; do
                local pkg="${pkg_info%%:*}"
                local feature="${pkg_info#*:}"
                local description="${feature#*:}"
                feature="${feature%%:*}"
                
                echo -e "${CYAN}Feature:${NC} $feature"
                echo -e "${YELLOW}Homebrew package:${NC} $pkg"
                echo -e "${BLUE}Description:${NC} $description"
                
                # Special recommendation for Cairo on macOS
                if [[ "$pkg" == "cairo" ]]; then
                    echo -e "${GREEN}⭐ RECOMMENDED:${NC} Cairo fixes common macOS graphics issues"
                fi
                
                read -p "Install this feature? (Y/n): " -n 1 -r
                echo
                if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                    selected_packages+=("$pkg")
                    print_success "Will install $pkg"
                else
                    print_info "Skipping $pkg"
                fi
                echo
            done
            
            for pkg in "${selected_packages[@]}"; do
                if ! brew list "$pkg" &>/dev/null; then
                    execute_with_progress "brew install $pkg" "Installing $pkg"
                else
                    print_success "$pkg already installed"
                fi
            done
            ;;
            
        fedora|centos|rhel)
            print_step "Installing system libraries for Red Hat/Fedora"
            
            if command_exists dnf; then
                local essential_packages=("openssl-devel" "libcurl-devel")
                execute_with_progress "sudo dnf install -y ${essential_packages[*]}" "Installing essential libraries"
                
                local optional_deps=(
                    "libxml2-devel:XML processing"
                    "cairo-devel:Graphics rendering"
                    "fontconfig-devel:Font configuration"
                    "harfbuzz-devel:Text shaping"
                    "fribidi-devel:Bidirectional text"
                    "freetype-devel:Font rendering"
                    "libpng-devel:PNG support"
                    "libjpeg-turbo-devel:JPEG support"
                )
                
                echo
                print_step "Optional packages"
                local selected_packages=()
                
                for pkg_desc in "${optional_deps[@]}"; do
                    local pkg="${pkg_desc%%:*}"
                    local desc="${pkg_desc##*:}"
                    echo -e "${CYAN}$pkg${NC} - $desc"
                    read -p "Install? (Y/n): " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        selected_packages+=("$pkg")
                    fi
                done
                
                if [[ ${#selected_packages[@]} -gt 0 ]]; then
                    execute_with_progress "sudo dnf install -y ${selected_packages[*]}" "Installing selected packages"
                fi
            fi
            ;;
            
        *)
            print_warning "Unknown environment - skipping system dependencies"
            print_info "You may need to install development libraries manually"
            ;;
    esac
    
    print_success "System dependencies installation completed"
}

# Repository setup
setup_repository() {
    print_header "REPOSITORY SETUP"
    
    local repo_url="https://github.com/DatViseR/Vivid-Volcano.git"
    local repo_dir="Vivid-Volcano"
    
    if [[ -d "$repo_dir" ]]; then
        print_info "Found existing Vivid-Volcano directory"
        cd "$repo_dir"
        
        if [[ -d ".git" ]]; then
            execute_with_progress "git pull origin master" "Updating repository"
        fi
    else
        execute_with_progress "git clone $repo_url $repo_dir" "Cloning Vivid Volcano repository"
        cd "$repo_dir"
    fi
    
    # Verify essential files
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
    
    print_success "Repository ready at: $(pwd)"
}

# R environment setup with proper dependency detection
setup_r_environment() {
    print_header "R ENVIRONMENT SETUP"
    
    # Install renv
    print_step "Installing renv package manager"
    local renv_cmd='R --slave --no-restore --no-save -e "
        if (!requireNamespace(\"renv\", quietly = TRUE)) {
            cat(\"Installing renv...\\n\")
            install.packages(\"renv\", repos = \"https://cloud.r-project.org/\")
        } else {
            cat(\"renv already available\\n\")
        }"'
    
    if has_timeout; then
        renv_cmd="timeout 180 $renv_cmd"
    fi
    
    eval "$renv_cmd" &
    show_progress "Installing renv package manager"
    
    # Detect system libraries with R-compatible booleans
    print_step "Detecting available system libraries"
    
    local cairo_available="FALSE"
    local xml2_available="FALSE"
    
    if check_system_library "cairo" "cairo/cairo.h" "cairo"; then
        cairo_available="TRUE"
        print_success "Cairo graphics library detected"
    else
        print_info "Cairo graphics library not found"
        if [[ "$(detect_environment)" == "macos" ]]; then
            print_info "Install with: brew install cairo"
        fi
    fi
    
    if check_system_library "xml2" "libxml/parser.h" "libxml-2.0"; then
        xml2_available="TRUE"
        print_success "XML2 processing library detected"
    else
        print_info "XML2 processing library not found"
    fi
    
    # Create R package installation script
    print_step "Installing R packages based on available system libraries"
    
    cat > install_r_packages.R << 'R_SCRIPT_EOF'
# Vivid Volcano R Package Installation
cat("=== R PACKAGE INSTALLATION ===\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Configuration
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 300)
options(download.file.method = "auto")
options(install.packages.compile.from.source = "never")

# System library availability (will be replaced by script)
CAIRO_AVAILABLE <- CAIRO_PLACEHOLDER
XML2_AVAILABLE <- XML2_PLACEHOLDER

cat("System library status:\n")
cat("  Cairo available:", CAIRO_AVAILABLE, "\n")
cat("  XML2 available:", XML2_AVAILABLE, "\n\n")

# Activate renv environment
cat("Activating renv environment...\n")
source("renv/activate.R")

# Package installation function
install_packages <- function() {
    library(renv)
    
    # Core packages (always installed - essential for app)
    core_packages <- c(
        # Shiny framework
        "shiny", "shinyjs", "htmltools", "htmlwidgets",
        # Data manipulation
        "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble",
        # Visualization
        "ggplot2", "ggrepel", "ggtext", "scales",
        # UI components
        "DT", "shiny.semantic", "RColorBrewer",
        # Utilities
        "jsonlite", "digest", "rlang", "httr", "curl",
        "markdown", "shinyalert", "arrow"
    )
    
    # Conditional packages (only if system libraries available)
    conditional_packages <- list()
    if (CAIRO_AVAILABLE) {
        conditional_packages$Cairo <- "High-quality graphics rendering"
        conditional_packages$webshot2 <- "Web page screenshots"
    }
    if (XML2_AVAILABLE) {
        conditional_packages$xml2 <- "XML/HTML processing"
        conditional_packages$gt <- "Advanced table formatting"
    }
    
    # Optional packages (nice to have)
    optional_packages <- c("plotly")
    
    # Install core packages
    cat("Installing", length(core_packages), "core packages...\n")
    failed_core <- character(0)
    
    for (pkg in core_packages) {
        cat("  Installing", pkg, "...")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat(" ✓\n")
            TRUE
        }, error = function(e) {
            cat(" ✗\n")
            failed_core <<- c(failed_core, pkg)
            FALSE
        })
    }
    
    # Install conditional packages
    if (length(conditional_packages) > 0) {
        cat("\nInstalling conditional packages (based on system libraries)...\n")
        failed_conditional <- character(0)
        
        for (pkg in names(conditional_packages)) {
            cat("  Installing", pkg, "(", conditional_packages[[pkg]], ") ...")
            result <- tryCatch({
                renv::install(pkg, prompt = FALSE)
                cat(" ✓\n")
                TRUE
            }, error = function(e) {
                cat(" ✗\n")
                failed_conditional <<- c(failed_conditional, pkg)
                FALSE
            })
        }
    }
    
    # Install optional packages
    cat("\nInstalling optional packages...\n")
    failed_optional <- character(0)
    
    for (pkg in optional_packages) {
        cat("  Installing", pkg, "...")
        tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat(" ✓\n")
        }, error = function(e) {
            cat(" ⚠ (optional)\n")
            failed_optional <<- c(failed_optional, pkg)
        })
    }
    
    # Installation summary
    cat("\n=== INSTALLATION SUMMARY ===\n")
    cat("Core packages:", length(core_packages) - length(failed_core), "/", length(core_packages), "\n")
    if (exists("failed_conditional")) {
        cat("Conditional packages:", length(conditional_packages) - length(failed_conditional), "/", length(conditional_packages), "\n")
    }
    cat("Optional packages:", length(optional_packages) - length(failed_optional), "/", length(optional_packages), "\n")
    
    if (length(failed_core) > 0) {
        cat("\nFailed core packages:", paste(failed_core, collapse = ", "), "\n")
    }
    
    # Return success if most core packages installed
    success_rate <- (length(core_packages) - length(failed_core)) / length(core_packages)
    return(success_rate >= 0.8)
}

# Verification
verify_installation <- function() {
    cat("\n=== VERIFICATION ===\n")
    essential <- c("shiny", "dplyr", "ggplot2", "DT")
    missing <- character(0)
    
    for (pkg in essential) {
        if (requireNamespace(pkg, quietly = TRUE)) {
            cat("✓", pkg, "\n")
        } else {
            cat("✗", pkg, "(missing)\n")
            missing <- c(missing, pkg)
        }
    }
    
    return(length(missing) == 0)
}

# Test basic app functionality
test_app <- function() {
    cat("\n=== APP TEST ===\n")
    tryCatch({
        library(shiny, quietly = TRUE)
        library(dplyr, quietly = TRUE)
        library(ggplot2, quietly = TRUE)
        
        if (file.exists("app.R")) {
            parse("app.R")
            cat("✓ app.R syntax is valid\n")
        }
        
        cat("✓ Core components working\n")
        return(TRUE)
        
    }, error = function(e) {
        cat("✗ Error:", e$message, "\n")
        return(FALSE)
    })
}

# Main execution
install_success <- install_packages()
verify_success <- verify_installation()
test_success <- test_app()

if (install_success && verify_success && test_success) {
    cat("\n🎉 R environment ready!\n")
    quit(status = 0)
} else if (verify_success) {
    cat("\n⚠ R environment ready with some limitations\n")
    quit(status = 0)
} else {
    cat("\n❌ R environment setup incomplete\n")
    quit(status = 1)
}
R_SCRIPT_EOF

    # Replace placeholders with actual boolean values
    if command_exists sed; then
        sed -i.bak "s/CAIRO_PLACEHOLDER/$cairo_available/g" install_r_packages.R
        sed -i.bak "s/XML2_PLACEHOLDER/$xml2_available/g" install_r_packages.R
        rm -f install_r_packages.R.bak
    else
        # Fallback for systems without sed
        local temp_file
        temp_file=$(mktemp)
        awk -v cairo="$cairo_available" -v xml2="$xml2_available" '
            {gsub(/CAIRO_PLACEHOLDER/, cairo); gsub(/XML2_PLACEHOLDER/, xml2); print}
        ' install_r_packages.R > "$temp_file"
        mv "$temp_file" install_r_packages.R
    fi
    
    # Execute R package installation
    R --no-restore --no-save < install_r_packages.R &
    show_progress "Installing R packages (this may take several minutes)"
    
    local r_exit_code=$?
    
    # Cleanup
    rm -f install_r_packages.R
    
    if [[ $r_exit_code -eq 0 ]]; then
        print_success "R environment configured successfully"
    else
        print_warning "R environment setup completed with some issues"
        print_info "Core functionality should still be available"
    fi
}

# Application launch with proper instructions
launch_application() {
    print_header "LAUNCH VIVID VOLCANO"
    
    if [[ ! -f "app.R" ]]; then
        print_error "app.R not found in current directory"
        return 1
    fi
    
    local env
    env=$(detect_environment)
    local host="127.0.0.1"
    local port="8080"
    local launch_browser="TRUE"
    
    # Configure for container environments
    if is_container_environment; then
        host="0.0.0.0"
        port="3000"
        launch_browser="FALSE"
        
        print_step "Configuring for container environment"
        case $env in
            codespaces)
                print_info "🌋 GitHub Codespaces detected"
                print_info "📱 Access: VS Code 'Ports' tab → port 3000 → 'Open in Browser'"
                print_info "🌐 Share: Right-click port 3000 → 'Port Visibility' → 'Public'"
                ;;
            gitpod)
                print_info "🌋 Gitpod detected"
                print_info "📱 Gitpod will automatically open the app in a new tab"
                ;;
            *)
                print_info "🌋 Container environment detected"
                print_info "📱 Access the app via port 3000"
                ;;
        esac
    else
        print_info "🌋 Local environment detected"
        print_info "📱 App will open in your default browser"
    fi
    
    echo
    print_info "🛑 To stop the app: Press Ctrl+C in this terminal"
    print_info "💾 Your data stays private - nothing is uploaded to external servers"
    echo
    
    # Create launch script
    cat > launch_vivid_volcano.R << LAUNCH_EOF
# Vivid Volcano Launch Script
suppressMessages(source('renv/activate.R'))

# Verify essential packages
essential <- c('shiny', 'dplyr', 'ggplot2', 'DT')
missing <- character(0)

for (pkg in essential) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        missing <- c(missing, pkg)
    }
}

if (length(missing) > 0) {
    cat('✗ Missing essential packages:', paste(missing, collapse = ', '), '\n')
    cat('Please rerun the installation script.\n')
    quit(status = 1)
}

cat('✓ Environment verified - all essential packages available\n')
cat('🚀 Starting Vivid Volcano...\n\n')

# Load and launch
suppressMessages(library(shiny))
runApp('app.R', host = '$host', port = $port, launch.browser = $launch_browser)
LAUNCH_EOF

    # Launch the application
    print_step "Starting Vivid Volcano application"
    R --no-restore --no-save < launch_vivid_volcano.R
    
    local app_exit_code=$?
    
    # Cleanup
    rm -f launch_vivid_volcano.R
    
    echo
    if [[ $app_exit_code -eq 0 ]]; then
        print_info "👋 Vivid Volcano session ended normally"
    else
        print_warning "Application ended unexpectedly"
    fi
    
    return $app_exit_code
}

# Main installation function
main() {
    local start_time
    start_time=$(date +%s)
    
    # Header
    print_header "VIVID VOLCANO INSTALLER v4.4"
    echo -e "${CYAN}Repository:${NC} https://github.com/DatViseR/Vivid-Volcano"
    echo -e "${CYAN}Author:${NC} DatViseR"
    echo -e "${CYAN}Environment:${NC} $(detect_environment)"
    echo
    echo -e "${YELLOW}💡 Cloud Alternative:${NC} For instant access, visit ${BLUE}https://vivid-volcano.com${NC}"
    echo -e "${CYAN}   No installation required, works immediately in any browser${NC}"
    echo
    
    # Installation flow confirmation
    echo -e "${BOLD}This installer will:${NC}"
    echo "  1. Check and install prerequisites (Git, R)"
    echo "  2. Install system libraries (with your choices)"
    echo "  3. Download Vivid Volcano repository"
    echo "  4. Set up R environment and packages"
    echo "  5. Optionally launch the application"
    echo
    
    read -p "Proceed with installation? (Y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Nn]$ ]]; then
        print_info "Installation cancelled"
        exit 0
    fi
    
    # Installation steps
    check_and_install_prerequisites
    
    # System dependencies (optional step)
    echo
    print_step "System dependencies provide enhanced functionality"
    print_info "You can skip this step - the app will work with basic features"
    read -p "Install system dependencies with feature selection? (Y/n): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
        install_system_dependencies
    else
        print_info "Skipping system dependencies - basic features will be available"
    fi
    
    setup_repository
    setup_r_environment
    
    # Installation summary
    local end_time duration minutes seconds
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    minutes=$((duration / 60))
    seconds=$((duration % 60))
    
    print_header "INSTALLATION COMPLETE"
    print_success "Total installation time: ${minutes}m ${seconds}s"
    print_success "Vivid Volcano location: $(pwd)"
    
    # Post-installation instructions
    echo
    echo -e "${BOLD}🎉 Vivid Volcano is ready to use!${NC}"
    echo
    echo -e "${BOLD}Features available:${NC}"
    echo "  ✓ Upload CSV/TSV omics data"
    echo "  ✓ Generate publication-ready volcano plots"
    echo "  ✓ Perform GO enrichment analysis"
    echo "  ✓ Interactive data filtering and exploration"
    echo "  ✓ Download high-quality plots and results"
    
    # Enhanced features based on what was installed
    if check_system_library "cairo" "cairo/cairo.h" "cairo"; then
        echo "  ✓ High-quality Cairo graphics (enhanced plot rendering)"
    fi
    if check_system_library "xml2" "libxml/parser.h" "libxml-2.0"; then
        echo "  ✓ XML data processing capabilities"
    fi
    
    echo
    echo -e "${BOLD}Manual launch commands:${NC}"
    if is_container_environment; then
        echo -e "${CYAN}  cd $(pwd)${NC}"
        echo -e "${CYAN}  R -e \"source('renv/activate.R'); shiny::runApp('app.R', host='0.0.0.0', port=3000)\"${NC}"
    else
        echo -e "${CYAN}  cd $(pwd)${NC}"
        echo -e "${CYAN}  R -e \"source('renv/activate.R'); shiny::runApp()\"${NC}"
    fi
    
    # Launch option with clear choice
    echo
    echo -e "${BOLD}Launch Options:${NC}"
    echo "  [Y] Launch Vivid Volcano now"
    echo "  [N] Complete installation (launch manually later)"
    echo
    read -p "Your choice (Y/n): " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
        echo
        launch_application
    else
        echo
        print_success "🌋 Installation complete!"
        print_info "Use the manual launch commands above when ready"
    fi
}

# Error handling
cleanup() {
    print_error "Installation interrupted"
    # Clean up any temporary files
    rm -f install_r_packages.R launch_vivid_volcano.R setup_environment.R 2>/dev/null || true
    exit 1
}

trap cleanup INT TERM

# Execute main function
main "$@"
exit 0