# # Load required libraries
# if (!requireNamespace("biomaRt", quietly = TRUE)) {
#   install.packages("biomaRt")
# }
# library(biomaRt)
# 
# # Initialize the Ensembl biomart
# ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")  # replace with relevant species if needed
# 
# # Define the ontology groups: BP, MF, CC
# ontologies <- c("biological_process", "molecular_function", "cellular_component")
# 
# # Function to fetch GO terms for a specific ontology group
# fetch_go_terms <- function(ontology_group) {
#   # Query biomaRt for GO terms and associated gene names
#   go_data <- getBM(attributes = c("go_id", "external_gene_name"),
#                    filters = "go_parent_term",
#                    values = ontology_group,
#                    mart = ensembl)
#   
#   # Rename columns
#   colnames(go_data) <- c("GO_Tag", "Gene_Name")
#   
#   return(go_data)
# }
# 
# # Fetch data for each ontology group and store in separate dataframes
# go_bp <- fetch_go_terms("biological_process")
# go_mf <- fetch_go_terms("molecular_function")
# go_cc <- fetch_go_terms("cellular_component")
# 
# # Save the datasets to CSV files (optional)
# write.csv(go_bp, "GO_Biological_Process.csv", row.names = FALSE)
# write.csv(go_mf, "GO_Molecular_Function.csv", row.names = FALSE)
# write.csv(go_cc, "GO_Cellular_Component.csv", row.names = FALSE)
# 
# # Display the first few rows of each dataset
# head(go_bp)
# head(go_mf)
# head(go_cc)
