![](www/Vivid_volcano_logo.png)

### Vivid-Volcano is a Shiny application designed for the visualization and exploration of preprocessed omics data. It enables easy creation of highly customizable, publication-ready volcano plots, comprehensive data exploration, and gene ontology (GO) enrichment analysis. The primary goal of the app is to provide an average biologist, with little to no bioinformatics background, an intuitive tool for exploring and visualizing their preprocessed omics data. Users can download variously formatted publication-ready plots and neatly formatted tables that adhere to scientific standards.

**Current status:** • After closed beta testing, pre-released and
waiting for the peer review from proffessional scientific software
reviewers. Please find the link to the journal article pre-print in the
sections below.

#### **Key Features:**

• Upload user data in TSV/CSV format or use the provided demo data\
• Interactive preview of the uploaded data table\
• Server-friendly binary format for gene ontology data\
• User-friendly GSEA analysis with publication-ready plot and table
outputs\
• Five methods for p-value adjustment with different statistical
thresholds\
• Highlighting and counting of regulated genes with custom-colored
labels\
• Visualization of GO categories on the plot with custom-colored labels\
• Customization of arbitrary gene labels\
• Optional trimming of multiplied gene names\
• Labeling of selected genes of interest\
• Server-side processing and browsing of more than 8,000 GO categories
from a binary file\
• Customization of axis and title labels\
• Creation of gene ontology enrichment analyses with results in a
professionally formatted table (optional PDF output)\
• Table of gene lists for each GO category, enabling easy identification
of detected and regulated genes within a specific category\
• Process log system for user sessions, useful for checking data
processing outcomes, debugging, and analyzing app functionality

## **The application is available in the cloud or can be easily installed locally or on private server container:**

### **Vivid Volcano live at POSIT Connect Cloud**

[**Vivid volcano cloud
version**](https://datviser-vivid-volcano.share.connect.posit.cloud/)

**Data Privacy Notice for the version deployed in the public cloud:**\
When you use this cloud-based application, your data is processed
securely within your own session. What it means :

-   **Your uploaded data is not stored permanently**, and it is **not
    accessible to other users**.

-   Each session is isolated, meaning your data is **only available
    during your active session** and is **automatically cleared when the
    session ends (including temporal logs)**.

-   Vivid Volcano does **not collect, store, or share** any uploaded
    files or analysis results. It collects non-sensitive telemetry
    data - number of sessions, analyses performed, time of sessions
    etc....

-   For your safety, I recommend not uploading highly sensitive data
    (ex. not blinded patients data) , as the app is hosted on a public
    server without authentication.

**The live demo of the first beta version is linked below. Now the app
is after closed beta testing, pre-released and waiting for professional
peer review from the scientific community.**

[**Vivid volcano features- demo
movie**](https://drive.google.com/file/d/1b7IzoJnTdTEW1VTk-L8TeDV9CMX0qKEz/view?usp=drive_link)

### Instructions for running the app locally or installing inside a container on the private server:

Vivid Volcano uses **renv** (R environment management) to ensure
reproducible package dependencies across different systems. This creates
an isolated R environment with the exact package versions specified in
`renv.lock`, preventing conflicts with your system-wide R packages.

#### For UNIX Systems (Linux/macOS/cloud containers)

1.  Automated installation

Download the **automated installation script** written in bash :

``` bash
# Download the installation script
curl -fsSL -o install_vivid_volcano.sh https://raw.githubusercontent.com/DatViseR/Vivid-Volcano/master/install_vivid_volcano.sh
```

\- This script that handles optional system dependencies, renv setup,
cloning the repository and should enable working with Vivid Volcano
locally or in linux based cloud container with just few clicks. The full
installation may take around 5 min.

``` bash
# Run the automated installer in the bash terminal
chmod +x install_vivid_volcano.sh
./install_vivid_volcano.sh
```

**What the installation script does:**

-   Checks prerequisites (R 4.4+, Git, system dependencies)

-   Offers selective installation of optional system libraries(such as
    Cairo that enhances graphical outputs)

-   Sets up renv environment with core packages

-   If users accepts run the app in the browser

Next time if you want to run the app from the UNIX terminal, you have
two options:

``` bash
# Make sure you are in Vivid-Volcano folder (cd Vivid-Volcano)

# Enhanced launcher (recommended for macOS)
./launch_app.sh

# R launcher with improved browser support
R -f launch_app.R

# Basic R command
R -e "shiny::runApp('app.R')"
```

or run it from R GUI, R Studio or other R IDE

2.  Manual installation

If the installation script fails or you prefer full manual control over
the installation process use the following code to start using Vivid
Volcano locally

``` bash
# Clone and enter directory
git clone https://github.com/DatViseR/Vivid-Volcano.git
cd Vivid-Volcano

# Install system dependencies (Ubuntu/Debian example) - OPTIONAL for enhance feuteres and developers who wish to modify the app (enabling pdf gt tables output) 
sudo apt-get update
sudo apt-get install libssl-dev libcurl4-openssl-dev libxml2-dev

# Setup R environment
R -e "install.packages('renv')"
R -e "renv::restore()"

# Launch the app
R -e "shiny::runApp('app.R')"
```

### For Windows users

**Step-by-step manual installation:**

1.  **Install Prerequisites**

-   Install R (4.4+ recommended): <https://cran.r-project.org/>

-   Install Git: <https://git-scm.com/>

-   Install Rtools (for package compilation):
    <https://cran.r-project.org/bin/windows/Rtools/>

2.  **Clone the Repository**

-   Open Git Bash or Command Prompt

``` bash
git clone https://github.com/DatViseR/Vivid-Volcano.git
cd Vivid-Volcano
```

3.  Set Up R Environment Open R or RStudio and run:

``` r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Activate the project environment
renv::activate()

# Restore packages from lockfile
renv::restore()
```

4.  Handle Potential Issues If renv package installation fails - install
    essential core packages from CRAN repository:

``` r

core_packages <- c(
  "shiny",             # Core web application framework
  "shinyjs",           # JavaScript integration for Shiny
  "shinyalert",        # Modal alerts in Shiny
  "shiny.semantic",    # Semantic UI theming
  "semantic.dashboard",# Semantic UI dashboards
  "dplyr",             # Data manipulation
  "tidyr",             # Data tidying
  "data.table",        # High-performance data manipulation
  "readr",             # Fast data import
  "arrow",             # Parquet/Feather file support
  "ggplot2",           # Core plotting
  "ggrepel",           # Better text labels in ggplot2
  "ggtext",            # Rich text in ggplot2
  "DT",                # Interactive tables
  "gt",                # Modern HTML tables
  "plotly",            # Interactive plots
  "colourpicker",      # Flexible color pickers
  "gridExtra"          # Arrange multiple grid-based plots
  )
install.packages(core_packages)
```

5.  **Launch the App**

``` r
# Run the app
shiny::runApp("app.R")
```

The app will launch in your default web browser, typically at
<http://127.0.0.1:3838> or similar.

#### Why renv?

-   **Reproducibility**: Ensures everyone uses the same package versions

-   **Isolation**: Doesn't interfere with your global R package library

-   **Stability**: Prevents version conflicts that could break the app

-   **Portability**: Makes the project work consistently across
    different systems

### **Troubleshooting**

#### Browser Issues on macOS

If you're experiencing issues with the app not opening in your browser
on macOS, here are several solutions:

**Problem**: Running `R -e "shiny::runApp()"` shows "Listening on [URL]"
but doesn't open browser, and terminal appears frozen.

**Solutions** (in order of recommendation):

1.  **Use the Enhanced Launcher (Recommended)**

    ``` bash
    ./launch_app.sh
    ```

    This script provides robust browser launching with multiple fallback
    methods specifically designed for macOS.

2.  **Use the R Launcher Script**

    ``` r
    # From R console or terminal
    R -f launch_app.R
    ```

    This provides enhanced browser support with platform detection.

3.  **Manual Browser Opening**

    ``` r
    # Run this and manually open the URL shown
    R -e "shiny::runApp('app.R', launch.browser = FALSE)"
    ```

    Then copy the displayed URL (typically `http://127.0.0.1:3838` or
    similar) into your browser.

4.  **Browser-Specific Solutions**

    If automatic launching fails, try these macOS-specific commands:

    ``` bash
    # For Safari
    open -a Safari http://127.0.0.1:3838

    # For Chrome  
    open -a "Google Chrome" http://127.0.0.1:3838

    # For Firefox
    open -a Firefox http://127.0.0.1:3838
    ```

**Why This Happens**: macOS sometimes has security restrictions or
session context issues that prevent automatic browser launching from R,
especially when run from certain terminal environments.

**Prevention**: Use the enhanced launchers (`./launch_app.sh` or
`R -f launch_app.R`) which include multiple fallback methods and better
error handling for macOS.

### **Contributions are welcome !**

Please check the CONTRIBUTIING.md file in .github folder

## How to cite Vivid Volcano?

The scientific article on Vivid Volcano was sent for review and is
currently under consideration. In the meantime, please cite the
application as follows:

**If you want to cite the pre-print of a journal article:**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15119893.svg)](https://doi.org/10.5281/zenodo.15119893)

Stępkowski, T. M. (2025). Vivid Volcano: Empowering
Non-Bioinformaticians to Analyze Pre-Processed Omics Data. Zenodo.
<https://doi.org/10.5281/zenodo.15119893>

**If you want to cite the source code image from Zenodo:**

[![DOI]([https://zenodo.org/badge/DOI/10.5281/zenodo.15119703.svg)](https://doi.org/10.5281/zenodo.15119703](https://doi.org/10.5281/zenodo.15119702))

Tomasz Stępkowski. (2025). DatViseR/Vivid-Volcano: Vivid Volcano v1.0 - an intuitive tool that helps experimental scientists with no bioinformatics background explore and analyze pre-processed omics data. (v1.0.0). Zenodo.
<https://doi.org/10.5281/zenodo.16742973>
