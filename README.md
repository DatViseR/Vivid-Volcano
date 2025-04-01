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

**The application is available in the cloud:**

[**Vivid volcano live at POSIT Connect
Cloud**](https://datviser-vivid-volcano.share.connect.posit.cloud//)

**The live demo of the first beta version linked below. Now the app is
after closed beta testing, pre-released and waiting for professional
peer review from the scientific community.**

[**Vivid volcano features- demo
movie**](https://drive.google.com/file/d/1b7IzoJnTdTEW1VTk-L8TeDV9CMX0qKEz/view?usp=drive_link)

To run the app locally follow the Instructions:

1.  Clone this repository:\
    git clone <https://github.com/DatViseR/Vivid-Volcano.git>

2.  Change directory:\
    cd Vivid-Volcano

3.  Run the app in R:\
    Rscript -e "shiny::runApp()"

**Contributions are welcome!** Please fork the repository and submit a
pull request.

### How to cite Vivid Volcano?

The scientific article on Vivid Volcano was sent for review and is
currently under consideration. In the meantime, please cite the
application as follows:

**If you want to cite the pre-print of a journal article:**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15119893.svg)](https://doi.org/10.5281/zenodo.15119893)

Stępkowski, T. M. (2025). Vivid Volcano: Empowering
Non-Bioinformaticians to Analyze Pre-Processed Omics Data. Zenodo.
<https://doi.org/10.5281/zenodo.15119893>

**If you want to cite the source code image from Zenodo:**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15119703.svg)](https://doi.org/10.5281/zenodo.15119703)

Tomasz Stępkowski. (2025). DatViseR/Vivid-Volcano - an intuitive tool
that helps experimental scientists with no bioinformatics background
explore and analyze pre-processed omics data. Pre-release. (v0.1.0).
Zenodo. <https://doi.org/10.5281/zenodo.15119703>
