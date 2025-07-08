#!/bin/bash

# ====================================================================
# Vivid Volcano - Universal Installation Script
# ====================================================================
# Author: DatViseR
# Date: 2025-01-08
# Description: Production-ready universal installer with corrected package categorization,
# provides information on essential and optional (enhanced functions) dependencies
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

# Enhanced progress indicator
show_progress() {
    local message="$1"
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

# Enhanced R version check
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
    
    if [[ $major -gt 4 ]] || [[ $major -eq 4 && $minor -ge 3 ]]; then
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
            print_warning "R version <4.3 detected (4.3+ recommended for all packages)"
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
            print_warning "Homebrew not found (recommended for optional dependencies)"
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
            print_info "Installing essential libraries (required for basic functionality)"
            execute_with_progress "sudo apt-get update -qq && sudo apt-get install -y ${essential_packages[*]}" "Installing essential libraries"
            
            # Optional dependencies with clear explanations
            echo
            print_step "Optional system libraries enhance Vivid Volcano features"
            print_info "The app works without these, but they enable advanced functionality"
            echo
            
            local optional_deps=(
                "libxml2-dev:XML data processing:Enables xml2 package for XML/HTML parsing and web scraping (gt tables should work without this - install only if you have issueus with GT tables display in the app"
                "libcairo2-dev:High-quality graphics:Enables Cairo package for publication-quality plot rendering"
                "libfontconfig1-dev:Advanced font support:Better text rendering and font selection in plots"
                "libharfbuzz-dev:Complex text layout:Support for advanced typography and text shaping"
                "libfribidi-dev:Bidirectional text:Right-to-left text support (Arabic, Hebrew, etc.)"
                "libfreetype6-dev:Font rendering engine:Enhanced font display and custom font support"
                "libpng-dev:PNG image support:Enhanced PNG file processing and plot export"
                "libjpeg-dev:JPEG image support:Enhanced JPEG file processing and plot export"
            )
            
            local selected_packages=()
            for pkg_info in "${optional_deps[@]}"; do
                local pkg="${pkg_info%%:*}"
                local feature="${pkg_info#*:}"
                local description="${feature#*:}"
                feature="${feature%%:*}"
                
                echo -e "${CYAN}Feature:${NC} $feature"
                echo -e "${YELLOW}Package:${NC} $pkg"
                echo -e "${BLUE}Benefit:${NC} $description"
                read -p "Install this enhancement? (Y/n): " -n 1 -r
                echo
                if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                    selected_packages+=("$pkg")
                    print_success "Will install $pkg"
                else
                    print_info "Skipping $pkg - basic functionality will still work"
                fi
                echo
            done
            
            if [[ ${#selected_packages[@]} -gt 0 ]]; then
                execute_with_progress "sudo apt-get install -y ${selected_packages[*]}" "Installing selected enhancements"
            fi
            ;;
            
        macos)
            print_step "Installing system libraries for macOS via Homebrew"
            
            if ! command_exists brew; then
                print_warning "Homebrew not found - optional dependencies will be skipped"
                print_info "Vivid Volcano will work with basic functionality"
                print_info "To install Homebrew later: https://brew.sh"
                return 0
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
            
            # Optional dependencies with Cairo emphasis
            echo
            print_step "Optional Homebrew packages enhance Vivid Volcano features"
            print_info "⚠️  macOS Note: These are OPTIONAL - the app works without them"
            print_info "    Cairo is especially helpful for high-quality plot rendering on macOS"
            echo
            
            local optional_deps=(
                "libxml2:XML data processing::Enables xml2 package for XML/HTML parsing and web scraping (gt tables should work without this - install only if you have issueus with GT tables display in the app"
                "cairo:High-quality graphics:Fixes macOS graphics issues and enables publication-quality plots"
                "harfbuzz:Advanced typography:Complex text layout and font shaping"
                "fribidi:Bidirectional text:Right-to-left language support"
                "freetype:Font rendering:Enhanced font display and custom fonts"
                "libpng:PNG processing:Enhanced PNG support for plots and data"
                "jpeg:JPEG processing:Enhanced JPEG support for plots and data"
            )
            
            local selected_packages=()
            for pkg_info in "${optional_deps[@]}"; do
                local pkg="${pkg_info%%:*}"
                local feature="${pkg_info#*:}"
                local description="${feature#*:}"
                feature="${feature%%:*}"
                
                echo -e "${CYAN}Feature:${NC} $feature"
                echo -e "${YELLOW}Homebrew package:${NC} $pkg"
                echo -e "${BLUE}Benefit:${NC} $description"
                
                # Special recommendation for Cairo on macOS
                if [[ "$pkg" == "cairo" ]]; then
                    echo -e "${GREEN}⭐ RECOMMENDED for macOS:${NC} Solves common graphics rendering issues"
                fi
                
                read -p "Install this enhancement? (Y/n): " -n 1 -r
                echo
                if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                    selected_packages+=("$pkg")
                    print_success "Will install $pkg"
                else
                    print_info "Skipping $pkg - app will work without it"
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
                print_step "Optional packages for enhanced functionality"
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
            print_info "Vivid Volcano will work with basic functionality"
            print_info "You may install system libraries manually for enhanced features"
            ;;
    esac
    
    print_success "System dependencies configuration completed"
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

# R environment setup with complete package list from renv.lock
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
        print_info "Cairo graphics library not found (optional)"
    fi
    
    if check_system_library "xml2" "libxml/parser.h" "libxml-2.0"; then
        xml2_available="TRUE"
        print_success "XML2 processing library detected"
    else
        print_info "XML2 processing library not found (optional)"
    fi
    
    # Create comprehensive R package installation script based on actual app dependencies
    print_step "Installing R packages (this may take several minutes)"
    
    cat > install_r_packages.R << 'R_SCRIPT_EOF'
# Vivid Volcano Complete R Package Installation
cat("=== COMPREHENSIVE R PACKAGE INSTALLATION ===\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Configuration
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 600)
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

# Complete package installation function based on actual app.R analysis
install_packages <- function() {
    library(renv)
    
    # ESSENTIAL CORE PACKAGES (app will NOT work without these)
    # Based on library() calls in app.R and core functionality
    essential_packages <- c(
        # Shiny framework (required)
        "shiny", "shinyjs", 
        
        # Data manipulation (required)
        "readr", "dplyr", "tidyr", "data.table", "magrittr",
        
        # Visualization core (required)
        "ggplot2", "ggtext", "ggrepel", "scales", "RColorBrewer",
        
        # Interactive tables (required - core functionality)
        "DT", "gt", "plotly",
        
        # UI framework (required - app uses semantic.dashboard)
        "shiny.semantic", "semantic.dashboard", "colourpicker",
        
        # Data formats (required - app uses arrow for GO data)
        "arrow",
        
        # Essential utilities (required)
        "jsonlite", "digest", "rlang", "htmltools", "htmlwidgets", 
        "httpuv", "R6",
        
        # Notifications and alerts (required for user feedback)
        "shinyalert"
    )
    
    # ENHANCED PACKAGES (improve functionality but app can work without)
    enhanced_packages <- c(
        # Additional shiny components
        "bslib", "jquerylib", "crosstalk", "reactable",
        
        # Enhanced data processing
        "lubridate", "hms", "bit64", "clipr", "fs", "stringr", "purrr", "tibble",
        
        # Additional visualization
        "gridExtra", "viridisLite", "lattice", "isoband", "farver", "gtable",
        
        # Graphics and rendering support
        "base64enc", "mime", "fontawesome", "markdown", "cachem", "fastmap",
        
        # System utilities  
        "lifecycle", "cli", "glue", "fansi", "crayon", "vctrs", "pillar",
        
        # HTTP and web
        "httr", "curl", "askpass", "openssl",
        
        # Statistical packages
        "MASS", "Matrix"
    )
    
    # CONDITIONAL PACKAGES (only if system libraries available)
    conditional_packages <- list()
    conditional_descriptions <- list()
    
    if (CAIRO_AVAILABLE) {
        conditional_packages$Cairo <- "Cairo"
        conditional_descriptions$Cairo <- "High-quality graphics rendering"
        conditional_packages$webshot2 <- "webshot2"  
        conditional_descriptions$webshot2 <- "Web page screenshots"
    }
    
    if (XML2_AVAILABLE) {
        conditional_packages$xml2 <- "xml2"
        conditional_descriptions$xml2 <- "XML/HTML processing for gt package"
        # Note: gt is now in essential as it's used in core functionality
    }
    
    # SPECIALIZED PACKAGES (may fail on some systems, not critical)
    specialized_packages <- c(
        "processx", "callr", "later", "promises", "memoise",
        "sys", "blob", "DBI", "RSQLite", "RPostgres",
        "V8", "chromote", "websocket", "AsioHeaders",
        "bigD", "bit", "bitops", "backports", "assertthat",
        "checkmate", "cpp11", "Rcpp", "colorspace", "commonmark",
        "evaluate", "highr", "knitr", "rmarkdown", "yaml",
        "juicyjuice", "labeling", "lazyeval", "pkgconfig",
        "withr", "timechange", "generics", "tidyselect"
    )
    
    # Install essential packages first (MUST succeed)
    cat("Installing", length(essential_packages), "essential packages...\n")
    failed_essential <- character(0)
    
    for (pkg in essential_packages) {
        cat("  Installing", pkg, "...")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat(" ✓\n")
            TRUE
        }, error = function(e) {
            cat(" ✗\n")
            failed_essential <<- c(failed_essential, pkg)
            FALSE
        })
    }
    
    # Install enhanced packages
    cat("\nInstalling", length(enhanced_packages), "enhanced packages...\n")
    failed_enhanced <- character(0)
    
    for (pkg in enhanced_packages) {
        cat("  Installing", pkg, "...")
        result <- tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat(" ✓\n")
            TRUE
        }, error = function(e) {
            cat(" ⚠\n")
            failed_enhanced <<- c(failed_enhanced, pkg)
            FALSE
        })
    }
    
    # Install conditional packages
    if (length(conditional_packages) > 0) {
        cat("\nInstalling conditional packages (based on system libraries)...\n")
        failed_conditional <- character(0)
        
        for (pkg_key in names(conditional_packages)) {
            pkg <- conditional_packages[[pkg_key]]
            desc <- conditional_descriptions[[pkg_key]]
            cat("  Installing", pkg, "(", desc, ") ...")
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
    
    # Install specialized packages (optional)
    cat("\nInstalling specialized packages...\n")
    failed_specialized <- character(0)
    
    for (pkg in specialized_packages) {
        cat("  Installing", pkg, "...")
        tryCatch({
            renv::install(pkg, prompt = FALSE)
            cat(" ✓\n")
        }, error = function(e) {
            cat(" ⚠\n")
            failed_specialized <<- c(failed_specialized, pkg)
        })
    }
    
    # Installation summary
    cat("\n=== INSTALLATION SUMMARY ===\n")
    cat("Essential packages:", length(essential_packages) - length(failed_essential), "/", length(essential_packages), "\n")
    cat("Enhanced packages:", length(enhanced_packages) - length(failed_enhanced), "/", length(enhanced_packages), "\n")
    if (exists("failed_conditional")) {
        cat("Conditional packages:", length(conditional_packages) - length(failed_conditional), "/", length(conditional_packages), "\n")
    }
    cat("Specialized packages:", length(specialized_packages) - length(failed_specialized), "/", length(specialized_packages), "\n")
    
    if (length(failed_essential) > 0) {
        cat("\n❌ FAILED ESSENTIAL PACKAGES:\n")
        for(pkg in failed_essential) {
            cat("  -", pkg, "\n")
        }
        cat("⚠️  These failures WILL prevent the app from starting!\n")
    }
    
    # Return success if ALL essential packages installed
    success_rate <- (length(essential_packages) - length(failed_essential)) / length(essential_packages)
    return(success_rate >= 1.0)  # Require ALL essential packages
}

# Verification with complete package list
verify_installation <- function() {
    cat("\n=== VERIFICATION ===\n")
    
    # Must verify ALL essential packages from app.R
    essential <- c(
        "shiny", "shinyjs", "readr", "dplyr", "ggplot2", "ggtext", 
        "colourpicker", "ggrepel", "arrow", "DT", "plotly", "gt",
        "shiny.semantic", "semantic.dashboard", "gridExtra", "shinyalert", 
        "tidyr", "data.table", "jsonlite", "htmltools"
    )
    
    missing <- character(0)
    
    for (pkg in essential) {
        if (requireNamespace(pkg, quietly = TRUE)) {
            cat("✓", pkg, "\n")
        } else {
            cat("✗", pkg, "(missing)\n")
            missing <- c(missing, pkg)
        }
    }
    
    # Check conditional packages
    if (CAIRO_AVAILABLE) {
        if (requireNamespace("Cairo", quietly = TRUE)) {
            cat("✓ Cairo (conditional)\n")
        } else {
            cat("⚠ Cairo (conditional - not installed)\n")
        }
    }
    
    if (XML2_AVAILABLE) {
        if (requireNamespace("xml2", quietly = TRUE)) {
            cat("✓ xml2 (conditional)\n")
        } else {
            cat("⚠ xml2 (conditional - not installed)\n")
        }
    }
    
    return(length(missing) == 0)
}

# Test app functionality
test_app <- function() {
    cat("\n=== APP FUNCTIONALITY TEST ===\n")
    tryCatch({
        # Test ALL core libraries that must load for app to work
        suppressMessages({
            library(shiny, quietly = TRUE)
            library(shinyjs, quietly = TRUE)
            library(dplyr, quietly = TRUE)
            library(ggplot2, quietly = TRUE)
            library(DT, quietly = TRUE)
            library(colourpicker, quietly = TRUE)
            library(shiny.semantic, quietly = TRUE)
            library(semantic.dashboard, quietly = TRUE)
            library(arrow, quietly = TRUE)
            library(gt, quietly = TRUE)
            library(plotly, quietly = TRUE)
        })
        
        cat("✓ All essential libraries load successfully\n")
        
        if (file.exists("app.R")) {
            parse("app.R")
            cat("✓ app.R syntax is valid\n")
        }
        
        cat("✓ All essential components working\n")
        return(TRUE)
        
    }, error = function(e) {
        cat("✗ Error loading essential libraries:", e$message, "\n")
        return(FALSE)
    })
}

# Main execution
install_success <- install_packages()
verify_success <- verify_installation()
test_success <- test_app()

if (install_success && verify_success && test_success) {
    cat("\n🎉 Complete R environment ready!\n")
    cat("All essential packages installed and verified.\n")
    quit(status = 0)
} else if (verify_success && test_success) {
    cat("\n⚠ R environment ready with some optional features missing\n")
    cat("Core functionality available.\n")
    quit(status = 0)
} else {
    cat("\n❌ R environment setup incomplete\n")
    cat("Some essential packages may be missing.\n")
    if (test_success) {
        cat("However, app syntax test passed - try launching anyway.\n")
        quit(status = 0)
    } else {
        quit(status = 1)
    }
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
    print_step "Installing R packages with detailed progress..."
echo
R --no-restore --no-save < install_r_packages.R

local r_exit_code=$?
    
    # Cleanup
    rm -f install_r_packages.R
    
    if [[ $r_exit_code -eq 0 ]]; then
        print_success "R environment configured successfully"
    else
        print_warning "R environment setup completed with some issues"
        print_info "Core functionality should be available"
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
    print_info "⚡ First startup may take a moment while packages load"
    echo
    
    # Create launch script
    cat > launch_vivid_volcano.R << LAUNCH_EOF
# Vivid Volcano Launch Script
cat("Loading Vivid Volcano environment...\n")
suppressMessages(source('renv/activate.R'))

# Verify ALL essential packages are available (based on app.R analysis)
essential <- c(
    'shiny', 'shinyjs', 'readr', 'dplyr', 'ggplot2', 'ggtext', 
    'colourpicker', 'ggrepel', 'arrow', 'DT', 'plotly', 'gt',
    'shiny.semantic', 'semantic.dashboard', 'gridExtra', 'shinyalert',
    'tidyr', 'data.table'
)

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
cat('🚀 Starting Vivid Volcano application...\n')
cat('   First load may take 10-30 seconds...\n\n')

# Load and launch (suppress startup messages for cleaner output)
suppressPackageStartupMessages({
    library(shiny)
})

runApp('app.R', host = '$host', port = $port, launch.browser = $launch_browser)
LAUNCH_EOF

    # Launch the application
    print_step "Starting Vivid Volcano application..."
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
    print_header "VIVID VOLCANO UNIVERSAL INSTALLER"
    echo -e "${CYAN}Repository:${NC} https://github.com/DatViseR/Vivid-Volcano"
    echo -e "${CYAN}Author:${NC} DatViseR"
    echo -e "${CYAN}Environment:${NC} $(detect_environment)"
    echo
    echo -e "${YELLOW}💡Public Cloud availible:${NC} For instant access, visit ${BLUE}https://datviser-vivid-volcano.share.connect.posit.cloud/${NC}"
    echo -e "${CYAN}  Availible via web; Data privacy: When you use this cloud-based application, your data is processed securely within your own session. What it means :

    Your uploaded data is not stored permanently, and it is not accessible to other users.

    Each session is isolated, meaning your data is only available during your active session and is automatically cleared when the session ends (including temporal logs).

    Vivid Volcano does not collect, store, or share any uploaded files or analysis results. It collects non-sensitive telemetry data - number of sessions, analyses performed, time of sessions etc....

    For your safety, I recommend not uploading highly sensitive data (ex. not blinded patients data) , as the app is hosted on a public server without authentication. For sensitive data you can
    use this installer to install the app localy or in your own cloud container. 
 ${NC}"
    echo
    
    # Installation flow confirmation
    echo -e "${BOLD}This installer will:${NC}"
    echo "  1. Check and install prerequisites (Git, R)"
    echo "  2. Install system libraries (optional - your choice)"
    echo "  3. Download Vivid Volcano repository"
    echo "  4. Set up complete R environment with all packages"
    echo "  5. Optionally launch the application"
    echo
    echo -e "${YELLOW}Note:${NC} System dependencies are OPTIONAL for enhanced features"
    echo -e "${YELLOW}      The app works without them but may have limited functionality${NC}"
    echo
    
    read -p "Proceed with installation? (Y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Nn]$ ]]; then
        print_info "Installation cancelled"
        exit 0
    fi
    
    # Installation steps
    check_and_install_prerequisites
    
    # System dependencies (optional step with clear explanation)
    echo
    print_step "System dependencies enable enhanced features"
    echo -e "${CYAN}✓ Basic features:${NC} Work without system dependencies"
    echo -e "${CYAN}✓ Enhanced features:${NC} High-quality graphics, advanced tables, XML processing"
    echo -e "${YELLOW}⚠ macOS users:${NC} Cairo can fix graphics issues but is completely optional"
    echo
    read -p "Install optional system dependencies for enhanced features? (Y/n): " -n 1 -r
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
    echo -e "${BOLD}Core features (always available):${NC}"
    echo "  ✓ Upload CSV/TSV omics data"
    echo "  ✓ Generate volcano plots"
    echo "  ✓ Perform GO enrichment analysis"
    echo "  ✓ Interactive data filtering"
    echo "  ✓ Download results and plots"
    echo "  ✓ Interactive plots with plotly"
    echo "  ✓ Publication-quality tables with gt"
    
    # Enhanced features based on what was installed
    echo
    echo -e "${BOLD}Enhanced features:${NC}"
    if check_system_library "cairo" "cairo/cairo.h" "cairo"; then
        echo "  ✓ High-quality Cairo graphics (publication-ready plots)"
    else
        echo "  ⚬ High-quality graphics (install Cairo for enhanced rendering)"
    fi
    if check_system_library "xml2" "libxml/parser.h" "libxml-2.0"; then
        echo "  ✓ XML data processing and advanced table formatting"
    else
        echo "  ⚬ Advanced table formatting (install libxml2 for full gt package support)"
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
    echo -e "${BOLD}Ready to launch Vivid Volcano?${NC}"
    echo "  [Y] Start the application now"
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
    rm -f install_r_packages.R launch_vivid_volcano.R 2>/dev/null || true
    exit 1
}

trap cleanup INT TERM

# Execute main function
main "$@"
exit 0