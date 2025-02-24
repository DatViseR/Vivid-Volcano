![](www/Vivid_volcano_logo.png)

### Vivid-Volcano is a Shiny application designed for the visualization and exploration of preprocessed omics data. It enables easy creation of highly customizable, publication-ready volcano plots, comprehensive data exploration, and gene ontology (GO) enrichment analysis. The primary goal of the app is to provide an average biologist, with little to no bioinformatics background, an intuitive tool for exploring and visualizing their preprocessed omics data. Users can download variously formatted publication-ready plots and neatly formatted tables that adhere to scientific standards.

#### Key Features:

**Currently under development:** 
• Preparation for publication of the application and further developments in 
response to feedback from beta users. 

**In the master version the following features are available:**  
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

The application is available in the cloud:

[Vivid volcano live - beta
version](https://datviser-vivid-volcano.share.connect.posit.cloud//)

The live demo of the first beta version - now the app is in the third round of beta testing:

[Vivid volcano features- demo
movie](https://drive.google.com/file/d/1b7IzoJnTdTEW1VTk-L8TeDV9CMX0qKEz/view?usp=drive_link)

To run the app locally follow the Instructions:

1.  Clone this repository:\
    git clone <https://github.com/DatViseR/Vivid-Volcano.git>

2.  Change directory:\
    cd Vivid-Volcano

3.  Run the app in R:\
    Rscript -e "shiny::runApp()"

Contributions are welcome! Please fork the repository and submit a pull
request.

# 
