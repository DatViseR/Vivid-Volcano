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
# remove the obsolete GO terms from the obo file 
# Renames columns to "id", "name", and "ontology".
# Writes GO_numbers_with_names2 to GO_numbers_with_names2.txt.
# 
# Join GO Data with GAF Data:
# filters the gaf file to remove the annotations with obsolete terms 
#   Renames columns of gaf_filtered to match GO_numbers_with_names2.
# Joins gaf_filtered with GO_numbers_with_names2 on "id" to create GO_names_and_genes.
# Writes GO_names_and_genes to GO_names_and_genes.txt.
# Leaves the GO terms that have at least 6 gene annotations present

# 
# This script processes and combines Gene Ontology (GO) data with gene annotation data to produce comprehensive tables of GO terms and their associated genes.


library(dplyr)

#Download gene ontology annotation 
library(data.table)
# Load GAF file 2024-11-03 release
#EBI Gene Ontology Annotation Database (goa) 	protein 	782871 	goa_human.gaf (gzip)
gaf_file <- "goa_human2.gaf"  # Ensure you've downloaded the file to this path
gaf <- fread(gaf_file, skip = "!")  # Skip header lines starting with '!'
# Select the relevant columns - filter pseudogenes out of the data # Filter for protein-coding genes
gaf_filtered <- gaf %>% filter(V12 == "protein") %>% select(V3, V5, V9) 


# Install required packages if not already installed
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GSEABase")

library(GSEABase)

# Load the GO annotations
go_obo <- getOBOCollection("go-basic2.obo")

GO_non_parsed <- go_obo@.kv

# Extract obsolete terms
obsolete_terms <- GO_non_parsed[GO_non_parsed$key == "is_obsolete" & GO_non_parsed$value == "true", ]

# Filter out rows with obsolete GO IDs from the gaf_filtered data
gaf_filtered <- gaf_filtered[!gaf_filtered$V5 %in% obsolete_terms$stanza_id]

# there are no obsolete terms in the gaf file
table(!gaf_filtered$V5 %in% obsolete_terms$stanza_id)

# leave the GO terms that have at least 4 gene annotations present in the gaf file
gaf_filtered <- gaf_filtered %>%
  group_by(V5) %>%
  filter(n() >= 6) %>%
  ungroup()


GO_non_parsed_subset <- GO_non_parsed[1:2000, ]

#write GO_non_parsed_subset to a file
write.table(GO_non_parsed,"GO_non_parsed.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# filter the GO_non_parsed_subset for name
GO_numbers_with_names <- GO_non_parsed[grepl("name", GO_non_parsed$key), ]

# move the values in key column into separate columns
GO_numbers_with_names <- GO_numbers_with_names |>
  tidyr::pivot_wider(names_from = key, values_from = value)


#remove first raw
GO_numbers_with_names <- GO_numbers_with_names2[-1, ] 
#remove second column
GO_numbers_with_names <- GO_numbers_with_names2[, -2]

# removes the hits with "obsolete" in the name
GO_numbers_with_names <- GO_numbers_with_names2[!grepl("obsolete", GO_numbers_with_names2$name), ]


# View the result
print(GO_numbers_with_names2)

# change column names to "id", "name", "ontology"
GO_numbers_with_names <- setNames(GO_numbers_with_names, c("id", "name", "ontology"))

#write GO_numbers_with_names2 to a file
write.table(GO_numbers_with_names,"GO_numbers_with_names2.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

#change the colnames of gaf_filtered to match the GO_numbers_with_names2
colnames(gaf_filtered) <- c("gene", "id", "ontology")




# join the gaf_filtered and GO_numbers_with_names2 on id
GO_names_and_genes <- dplyr::left_join(gaf_filtered, GO_numbers_with_names, by = "id")

GO_names_and_genes<- GO_names_and_genes |> dplyr::select(id,name,gene, ontology = ontology.x)

# show the number of unique GO terms
length(unique(GO_names_and_genes$id))



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
write_parquet(To_parquet, "GO.parquet2")




############################## Estimation of n of level 3 hierarchy terms##########################


# Estimate the number of non-redundant no overlapping GO categories at level 3

library(GSEABase)
library(data.table)

# Step 1: Convert the `kv` slot to a `data.table` for fast processing
kv_dt <- as.data.table(go_obo@.kv)

# Step 2: Filter for `is_a` relationships
is_a_relations <- kv_dt[key == "is_a"]

# Step 3: Create a parent-child map as a `data.table` for fast lookups
parent_child_map <- is_a_relations[, .(parent = value), by = stanza_id]
setnames(parent_child_map, "stanza_id", "child")

# Step 4: Initialize levels as a named vector and set root terms to level 1
go_ids <- go_obo@ids
levels <- integer(length(go_ids))  # Using integers is more memory efficient
names(levels) <- go_ids
levels[] <- NA

# Identify root terms (terms with no parents)
root_terms <- setdiff(go_ids, parent_child_map$parent)
levels[root_terms] <- 1

# Step 5: Queue-based traversal for level assignment
queue <- root_terms  # Start with root terms
while (length(queue) > 0) {
  current_term <- queue[1]
  queue <- queue[-1]  # Dequeue the current term
  
  # Get child terms for the current term using data.table join
  child_terms <- parent_child_map[parent == current_term, child]
  
  # Assign levels to child terms and add them to the queue if not already leveled
  new_children <- child_terms[is.na(levels[child_terms])]
  levels[new_children] <- levels[current_term] + 1
  queue <- c(queue, new_children)  # Enqueue only new children
}

# Step 6: Filter terms at level 3 and count them
level_3_terms <- names(levels[levels == 3])

# Final count of level 3 terms
n_level_3_terms <- length(level_3_terms)
n_level_3_terms  # is 15306  - the real number is around 1160??
