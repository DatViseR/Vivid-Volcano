# # This script create a datatable and a parquet file with all the GO annotations matched to genes and 
# This script performs the following tasks:
#   
#   Load Gene Ontology Annotation (GAF) File:
#   Reads the GAF file goa_human.gaf and filters relevant columns (GO_ID, Gene, Aspect).
# Splits the filtered data into biological process, molecular function, and cellular component aspects.
# 
# Install and Load Required Packages:
#   Installs and loads the BiocManager and GSEABase packages.
# Loads GO annotations from the go-basic.obo file into GO_non_parsed.
# 
# Process GO Annotations:
#   Extracts a subset of GO_non_parsed and writes it to GO_non_parsed.txt.
# Filters for rows containing "name" and pivots the data to create GO_numbers_with_names2.
# 
# Clean and Prepare Data:
#   Removes the first row and second column from GO_numbers_with_names2.
# Renames columns to "id", "name", and "ontology".
# Writes GO_numbers_with_names2 to GO_numbers_with_names2.txt.
# 
# Join GO Data with GAF Data:
#   Renames columns of gaf_filtered to match GO_numbers_with_names2.
# Joins gaf_filtered with GO_numbers_with_names2 on "id" to create GO_names_and_genes.
# Writes GO_names_and_genes to GO_names_and_genes.txt.
# 
# This script processes and combines Gene Ontology (GO) data with gene annotation data to produce comprehensive tables of GO terms and their associated genes.




#Download gene ontology annotation 
library(data.table)
# Load GAF file
gaf_file <- "goa_human.gaf"  # Ensure you've downloaded the file to this path
gaf <- fread(gaf_file, skip = "!")  # Skip header lines starting with '!'
# Select the relevant columns
gaf_filtered <- gaf[, .(GO_ID = V5, Gene = V3, Aspect = V9)]

# Install required packages if not already installed
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GSEABase")

library(GSEABase)

# Load the GO annotations
go_obo <- getOBOCollection("go-basic.obo")

GO_non_parsed <- go_obo@.kv

GO_non_parsed_subset <- GO_non_parsed[1:2000, ]

#write GO_non_parsed_subset to a file
write.table(GO_non_parsed,"GO_non_parsed.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# filter the GO_non_parsed_subset for name
GO_numbers_with_names <- GO_non_parsed[grepl("name", GO_non_parsed$key), ]

# move the values in key column into separate columns
GO_numbers_with_names2 <- GO_numbers_with_names |>
  tidyr::pivot_wider(names_from = key, values_from = value)


#remove first raw
GO_numbers_with_names2 <- GO_numbers_with_names2[-1, ] 
#remove second column
GO_numbers_with_names2 <- GO_numbers_with_names2[, -2]

# View the result
print(GO_numbers_with_names2)

# change column names to "id", "name", "ontology"
GO_numbers_with_names2 <- setNames(GO_numbers_with_names2, c("id", "name", "ontology"))

#write GO_numbers_with_names2 to a file
write.table(GO_numbers_with_names2,"GO_numbers_with_names2.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

#change the colnames of gaf_filtered to match the GO_numbers_with_names2
colnames(gaf_filtered) <- c("id", "gene", "ontology")

# join the gaf_filtered and GO_numbers_with_names2 on id
GO_names_and_genes <- dplyr::left_join(gaf_filtered, GO_numbers_with_names2, by = "id")

GO_names_and_genes<-GO_names_and_genes |> dplyr::select(id,name,gene, ontology = ontology.x)

#save GO_names_and_genes to a file
write.table(GO_names_and_genes,"GO_names_and_genes.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
library(arrow)

# Read the data once again
library(readr)
To_parquet <- read_delim("GO_names_and_genes.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
str(To_parquet)


# Write to Parquet file for Vivid-Volcano application 
write_parquet(To_parquet, "GO.parquet")
