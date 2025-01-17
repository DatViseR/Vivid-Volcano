#gemerates mock data for file size input limit testing :


# Function to generate random gene symbols (short strings)
generate_gene_symbol <- function() {
  paste0(
    sample(LETTERS, 3, replace = TRUE),
    sample(0:9, 2, replace = TRUE),
    collapse = ""
  )
}

# Function to generate random protein names (short phrases)
generate_protein_name <- function() {
  words <- c("Protein", "Factor", "Enzyme", "Receptor", "Kinase", "Channel", "Transport", "Binding", "Signal", "Growth")
  paste(sample(words, sample(1:5, 1)), collapse = " ")
}

# Function to create mock data with specified approximate file size
create_mock_data <- function(target_size_mb) {
  # Calculate approximate rows needed for target size
  # Estimate: each row ~100 bytes (varies with actual data)
  target_bytes <- target_size_mb * 1024 * 1024
  estimated_rows <- ceiling(target_bytes / 100)
  
  # Create data frame
  data <- data.frame(
    # Numeric columns
    pvalue = runif(estimated_rows, 0, 1),
    log2fold = rnorm(estimated_rows, 0, 2),
    intensity = rnorm(estimated_rows, 1000, 200),
    abundance = abs(rnorm(estimated_rows, 500, 100)),
    
    # Character columns
    gene_symbol = replicate(estimated_rows, generate_gene_symbol()),
    protein_name = replicate(estimated_rows, generate_protein_name()),
    condition = sample(c("Control", "Treatment A", "Treatment B"), estimated_rows, replace = TRUE),
    replicate = paste0("Rep_", sample(1:5, estimated_rows, replace = TRUE))
  )
  
  return(data)
}

# Generate 10MB file
data_10mb <- create_mock_data(10)
write.csv(data_10mb, "mock_data_10mb.csv", row.names = FALSE)

# Generate 15MB file
data_15mb <- create_mock_data(15)
write.csv(data_15mb, "mock_data_15mb.csv", row.names = FALSE)

# Print file sizes to verify
file_size_10mb <- file.size("mock_data_10mb.csv") / (1024^2)
file_size_15mb <- file.size("mock_data_15mb.csv") / (1024^2)

cat(sprintf("10MB file actual size: %.2f MB\n", file_size_10mb))
cat(sprintf("15MB file actual size: %.2f MB\n", file_size_15mb))
