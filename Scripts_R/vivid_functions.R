##################---CRUCIAL CUSTOM FUNCTION DEFINITIONS----###################

# 
#' 
#' 
#' @title Create Logger
#' @description The logger factory function that creates a logger function for a specific session 
#' that formats and stores log messages with timestamps and session identifiers. 
#' The logger maintains a consistent format and can output messages both to
#' a reactive value and the console.
#' 
#' @param session A Shiny session object used to create unique session identifiers
#' 
#' @return A function that takes three parameters:
#'   \itemize{
#'     \item log_messages_rv: A reactive value to store log messages
#'     \item message: The message to log
#'     \item type: The type of log message (default: "INFO")
#'   }
#' 
#' @details The returned logging function creates formatted log entries with:
#'   \itemize{
#'     \item Timestamp in YYYY-MM-DD HH:MM:SS format
#'     \item Shortened session identifier (6-character hash)
#'     \item Message type (e.g., "INFO", "ERROR", "WARNING")
#'     \item The actual log message
#'   }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   logger(log_messages, "Application started", "INFO")
#' }
#' }
#' 
#' @importFrom digest digest
#' @importFrom shiny is.reactive isolate
#' @export

create_logger <- function(session) {
  # Create session-specific identifiers
  session_start_time <- Sys.time()
  # Extract only the hash part for display
  session_hash <- substr(digest::digest(session$token), 1, 6)
  
  # Return the configured log_event function
  function(log_messages_rv, message, type = "INFO") {
    if (!shiny::is.reactive(log_messages_rv)) {
      stop("log_messages_rv must be a reactiveVal or reactive expression")
    }
    
    # Format the log entry with simplified session information
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- sprintf("[%s][Session:%s] %s: %s\n", 
                             timestamp, 
                             session_hash,  # Using only the hash part
                             type, 
                             message)
    
    # Update the log using isolate
    isolate({
      current_log <- log_messages_rv()
      log_messages_rv(paste0(current_log, formatted_msg))
    })
    
    # Also print to console
    cat(formatted_msg)
  }
}


#' Create Structure Logger
#' 
#' @title Create Structure Logger
#' @description # The structure logging helper function -
#'  it can be used to log the structure of any R object or modified to 
# log other properties of the of R objects using dplyr's glimpse function. 
# This logger extends the basic logger functionality to 
# include detailed object structure information.
#' 
#' @param session A Shiny session object used to create unique session identifiers
#' 
#' @return A function that takes four parameters:
#'   \itemize{
#'     \item log_messages_rv: A reactive value to store log messages
#'     \item obj: The R object whose structure should be logged
#'     \item message: Custom message prefix (default: "Object structure:")
#'     \item type: The type of log message (default: "INFO")
#'   }
#' 
#' @details The returned function combines the functionality of the basic logger with
#' structure inspection capabilities. It:
#'   \itemize{
#'     \item Uses dplyr::glimpse to capture object structure
#'     \item Combines custom message with structure information
#'     \item Maintains consistent logging format with session tracking
#'   }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   structure_logger <- create_structure_logger(session)
#'   df <- data.frame(x = 1:3, y = letters[1:3])
#'   structure_logger(log_messages, df, "Data frame structure:")
#' }
#' }
#' 
#' @importFrom dplyr glimpse
#' @seealso \code{\link{create_logger}} for the basic logging functionality
#' @export


create_structure_logger <- function(session) {
  # Get the basic logger
  log_event <- create_logger(session)
  
  # Return the configured log_structure function
  function(log_messages_rv, obj, message = "Object structure:", type = "INFO") {
    # Capture the structure output
    structure_info <- paste(capture.output(dplyr::glimpse(obj)), collapse = "\n")
    
    # Combine the message and structure info
    full_message <- paste0(message, "\n", structure_info)
    
    # Use the session-aware log_event
    log_event(log_messages_rv, full_message, type)
  }
}


#' Diagnose and Remove NA Values from Input Columns
#'
#' @description
#' Analyzes specified columns in a data frame for NA values, removes rows with NAs,
#' removes columns that are fully NA, and provides comprehensive statistics 
#' and user feedback about the cleaning process.
#'
#' @param df A data frame containing the input data
#' @param pvalue_col Character string specifying the name of the p-value column
#' @param fold_col Character string specifying the name of the fold change column
#' @param annotation_col Character string specifying the name of the annotation column
#' @param log_messages_rv Optional logging object for event logging
#' @param log_event Optional logging function for event logging
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item cleaned_data: Data frame with NA values removed
#'   \item statistics: List containing:
#'   \itemize{
#'     \item original_rows: Number of rows in original data
#'     \item new_rows: Number of rows after cleaning
#'     \item dropped_rows: Number of rows removed
#'     \item na_counts: Named vector of NA counts per column
#'     \item empty_columns_removed: Number of fully NA columns removed
#'   }
#' }
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input data and column names
#' 2. Counts NA values in specified columns
#' 3. Removes rows with NA values
#' 4. Removes columns that are fully NA
#' 5. Generates statistics about the cleaning process
#' 6. Displays a visual alert with cleaning summary
#'
#' @examples
#' \dontrun{
#' results <- diagnose_input_columns_and_remove_NA(
#'   df = my_data,
#'   pvalue_col = "pvalue",
#'   fold_col = "log2FC",
#'   annotation_col = "gene_id"
#' )
#' 
#' # Access cleaned data
#' cleaned_data <- results$cleaned_data
#' 
#' # Access statistics
#' stats <- results$statistics
#' }
#'
#' @importFrom dplyr drop_na %>%
#' @importFrom shinyalert shinyalert
#' @importFrom htmltools HTML
#'
#' @export
#'
#' @author DatViseR
#'
#' @note 
#' This function is part of the Vivid-Volcano package, designed for 
#' custom analyses of preprocessed omics data.
#'
#' @seealso 
#' \code{\link[dplyr]{drop_na}}, \code{\link[shinyalert]{shinyalert}}
#'

diagnose_input_columns_and_remove_NA <- function(df, pvalue_col, fold_col, annotation_col, log_messages_rv = NULL, log_event = NULL) {
  
  # Add debug logging at function start
  if (!is.null(log_messages_rv) && !is.null(log_event)) {
    log_event(log_messages_rv,
              sprintf("Function called at %s with params: pvalue_col=%s, fold_col=%s, annotation_col=%s",
                      format(Sys.time(), "%H:%M:%S.%OS3"),
                      pvalue_col, fold_col, annotation_col),
              "DEBUG from diagnose_input_columns_and_remove_NA")
  }
  
  
  # Input validation
  if (is.null(df) || !is.data.frame(df)) {
    stop("Input must be a valid data frame")
  }
  
  if (!all(c(pvalue_col, fold_col, annotation_col) %in% colnames(df))) {
    missing_cols <- setdiff(c(pvalue_col, fold_col, annotation_col), colnames(df))
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Store initial state
  original_rows <- nrow(df)
  original_cols <- ncol(df)
  
  # Log initial state
  if (!is.null(log_messages_rv)) {
    log_event(log_messages_rv, 
              sprintf("Initial number of rows: %d, columns: %d", original_rows, original_cols), 
              "INFO")
  }
  
  # Count NA values in specified columns using tryCatch for safer evaluation
  na_counts <- tryCatch({
    sapply(df[c(pvalue_col, fold_col, annotation_col)], 
           function(x) sum(is.na(x)))
  }, error = function(e) {
    stop(sprintf("Error counting NA values: %s", e$message))
  })
  
  # Log NA counts
  if (!is.null(log_messages_rv)) {
    log_event(log_messages_rv, 
              sprintf("NA counts in columns before filtering:\n%s", 
                      paste(names(na_counts), na_counts, sep = ": ", collapse = "\n")), 
              "INFO")
  }
  
  # Remove rows with NA values in specified columns using tryCatch
  df_cleaned <- tryCatch({
    df %>% 
      drop_na(all_of(c(pvalue_col, fold_col, annotation_col)))
  }, error = function(e) {
    stop(sprintf("Error removing NA values: %s", e$message))
  })
  
  # Remove columns that are fully NA
  empty_columns <- colnames(df_cleaned)[colSums(is.na(df_cleaned)) == nrow(df_cleaned)]
  df_cleaned <- df_cleaned[, !colnames(df_cleaned) %in% empty_columns, drop = FALSE]
  empty_columns_removed <- length(empty_columns)
  
  # Log empty columns removal
  if (!is.null(log_messages_rv)) {
    if (empty_columns_removed > 0) {
      log_event(log_messages_rv, 
                sprintf("Removed %d empty columns: %s", 
                        empty_columns_removed, paste(empty_columns, collapse = ", ")), 
                "INFO")
    } else {
      log_event(log_messages_rv, 
                "No empty columns were removed", 
                "INFO")
    }
  }
  
  # Calculate statistics
  new_rows <- nrow(df_cleaned)
  dropped_rows <- original_rows - new_rows
  new_cols <- ncol(df_cleaned)
  
  # Log results
  if (!is.null(log_messages_rv)) {
    if (dropped_rows > 0) {
      log_event(log_messages_rv,
                sprintf("Removed %d rows with missing values (%d -> %d rows)",
                        dropped_rows, original_rows, new_rows),
                "INFO")
    } else {
      log_event(log_messages_rv,
                "No rows were removed - no missing values found",
                "INFO")
    }
  }
  
  # Create alert message with safe HTML creation
  alert_message <- tryCatch({
    HTML(sprintf(
      "<div style='text-align: left;'>
            <strong>Data Processing Summary:</strong><br><br>
            Initial number of rows: %d<br>
            Initial number of columns: %d<br><br>
            <strong>Missing values found:</strong><br>
            %s<br>
            <strong>Rows removed:</strong> %d<br>
            <strong>Remaining rows:</strong> %d<br>
            <strong>Empty columns removed:</strong> %d<br>
            <strong>Remaining columns:</strong> %d
            </div>",
      original_rows,
      original_cols,
      paste(names(na_counts), na_counts, sep = ": ", collapse = "<br>"),
      dropped_rows,
      new_rows,
      empty_columns_removed,
      new_cols
    ))
  }, error = function(e) {
    stop(sprintf("Error creating alert message: %s", e$message))
  })
  
  # Show alert with error handling
  tryCatch({
    shinyalert(
      title = "Data Processing Information",
      text = alert_message,
      type = "warning",
      html = TRUE,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      showConfirmButton = TRUE,
      confirmButtonText = "Continue",
      timer = 0
    )
  }, error = function(e) {
    warning(sprintf("Error showing alert: %s", e$message))
  })
  
  # Return results as a list
  return(list(
    cleaned_data = df_cleaned,
    statistics = list(
      original_rows = original_rows,
      new_rows = new_rows,
      dropped_rows = dropped_rows,
      na_counts = na_counts,
      empty_columns_removed = empty_columns_removed,
      original_cols = original_cols,
      new_cols = new_cols
    )
  ))
}





#' Clean and Standardize Gene Names
#' 
#' @title Clean Gene Names
#' @description Standardizes and cleans gene names by removing special characters, 
#' converting to uppercase, and ensuring uniqueness. This function handles common 
#' issues in gene name formatting such as multiple entries, special characters, 
#' and inconsistent spacing.
#' 
#' @param genes A vector of gene names to be cleaned
#' @param log_messages_rv Optional reactive value for storing log messages
#' @param log_event Optional logging function created by create_logger()
#' 
#' @return A vector of cleaned, unique gene names with the following modifications:
#'   \itemize{
#'     \item Removed NA values
#'     \item Removed text after delimiters (;,|)
#'     \item Trimmed whitespace
#'     \item Converted to uppercase
#'     \item Removed special characters
#'     \item Removed duplicates
#'   }
#' 
#' @details The function performs the following cleaning steps in order:
#'   1. Removes NA values
#'   2. Keeps only the first part of compound names (before ;,| or space)
#'   3. Removes leading and trailing whitespace
#'   4. Removes empty strings
#'   5. Converts to uppercase
#'   6. Removes all characters except letters and numbers
#'   7. Removes duplicates
#' 
#' @examples
#' \dontrun{
#' # Basic usage without logging
#' genes <- c("Gene1", "gene1;alt", "GENE-2", NA, "Gene1", "gene_3")
#' cleaned <- clean_gene_names(genes)
#' print(cleaned)  # Returns: c("GENE1", "GENE2", "GENE3")
#' 
#' # Usage with logging in Shiny
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   genes <- c("Gene1", "gene1;alt", "GENE-2", NA)
#'   cleaned <- clean_gene_names(genes, log_messages, logger)
#' }
#' }
#' 
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' 
#' @seealso 
#' \code{\link{create_logger}} for creating the logging function
#' 
#' @export
clean_gene_names <- function(genes, 
                             log_messages_rv = NULL,  # Optional: for logging messages
                             log_event = NULL) {      # Optional: logging function
  
  # Only log if both logging parameters are provided
  if (!is.null(log_messages_rv) && !is.null(log_event)) {
    log_event(log_messages_rv,
              sprintf("Starting gene names cleaning:\n- Input length: %d",
                      length(genes)),
              "INFO from clean_gene_names()")
  }
  
  # Clean gene names using pipe
  cleaned_genes <- genes %>%
    na.omit() %>%
    sub(pattern = "[;|,\\s].*", replacement = "") %>%
    trimws() %>%
    .[. != ""] %>%
    toupper() %>%
    gsub(pattern = "[^A-Z0-9]", replacement = "") %>%
    unique()
  
  # Only log if both logging parameters are provided
  if (!is.null(log_messages_rv) && !is.null(log_event)) {
    log_event(log_messages_rv,
              sprintf("Gene names cleaning completed:\n- Input genes: %d\n- Cleaned unique genes: %d\n- Removed genes: %d",
                      length(genes),
                      length(cleaned_genes),
                      length(genes) - length(cleaned_genes)),
              "SUCCESS from clean_gene_names()")
  }
  
  return(cleaned_genes)
}



# GO tag enrichment analysis is based on the hypergeometric test which is a statistical test used to determine if the
# number of successes in a sample drawn from a population is significantly different from the number of successes in the
# population as a whole. The function below performs the hypergeometric test and logs the results. 

perform_hypergeometric_test <- function(log_messages_rv, population_size, success_population_size, sample_size, sample_success_size, log_event) {
  # Log the input parameters
  log_event(log_messages_rv, 
            sprintf("Performing hypergeometric test with parameters: \npopulation_size: %d \nsuccess_population_size: %d \nsample_size: %d \nsample_success_size: %d",
                    population_size, 
                    success_population_size, 
                    sample_size, 
                    sample_success_size), 
            "INFO from perform_hypergeometric_test()")
  
  # Calculate the result
  result <- phyper(sample_success_size - 1, 
                   success_population_size, 
                   population_size - success_population_size, 
                   sample_size, 
                   lower.tail = FALSE)
  
  # Log the result
  log_event(log_messages_rv, 
            sprintf("Hypergeometric test result: %g", result), 
            "INFO from perform_hypergeometric_test()")
  
  return(result)
}

# silent version with logging supressed - aimed to run inside the lapply loop for testing each GO tag 
# which are in tousands so logging needs to be supressed to avoid cluttering the log. 

# First create a silent version of the hypergeometric test

# Only this function definition should be in global scope
perform_hypergeometric_test_silent <- function(population_size, success_population_size, 
                                               sample_size, sample_success_size) {
  # Just calculate and return the result, no logging
  phyper(sample_success_size - 1, 
         success_population_size, 
         population_size - success_population_size, 
         sample_size, 
         lower.tail = FALSE)
}


#' Calculate Gene Ontology Enrichment Analysis
#' 
#' @title Calculate GO Enrichment
#' @description Performs Gene Ontology (GO) enrichment analysis using hypergeometric testing
#' on a set of input genes. Includes comprehensive logging of the analysis process and
#' applies Bonferroni correction for multiple testing. This function is used in the Draw VOlcano module,
#' but not in the GSEA module which uses identify_top_enriched_GO_enrichment instead.
#' 
#' @param genes Vector or list of gene identifiers to analyze
#' @param go_categories Vector of GO category names to test for enrichment
#' @param go_data Data frame containing GO annotations with columns 'name' (GO category) 
#'        and 'gene' (gene identifier)
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A data frame containing enrichment analysis results with columns:
#'   \itemize{
#'     \item GO_Category: Name of the GO category
#'     \item P_Value: Unadjusted p-value from hypergeometric test
#'     \item Adjusted_P_Value: Bonferroni-corrected p-value (n=1160)
#'     \item Population_Size: Total number of genes in background (19689 human coding genes)
#'     \item Success_Population_Size: Number of genes in the GO category
#'     \item Sample_Size: Number of input genes after cleaning
#'     \item Sample_Success_Size: Number of input genes found in the GO category
#'   }
#' 
#' @details
#' The function performs these steps:
#' 1. Cleans and standardizes input gene names
#' 2. Processes each GO category separately
#' 3. Performs hypergeometric testing for enrichment
#' 4. Applies Bonferroni correction for multiple testing
#' 
#' The background population is set to 19,689 genes (human coding genes excluding pseudogenes).
#' Bonferroni correction uses n=1160 (estimate for level 4 hierarchy GO tags).
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example input data
#'   genes <- c("BRCA1", "BRCA2", "TP53", "PTEN")
#'   go_cats <- c("DNA repair", "cell cycle", "apoptosis")
#'   go_data <- data.frame(
#'     name = c("DNA repair", "DNA repair", "cell cycle"),
#'     gene = c("BRCA1", "BRCA2", "TP53")
#'   )
#'   
#'   # Calculate enrichment
#'   results <- calculate_go_enrichment(
#'     genes, 
#'     go_cats, 
#'     go_data, 
#'     log_messages, 
#'     logger
#'   )
#' }
#' }
#' 
#' @importFrom dplyr filter pull bind_rows
#' @importFrom stats p.adjust
#' @importFrom magrittr %>%
#' 
#' @seealso 
#' \code{\link{clean_gene_names}} for gene name preprocessing
#' \code{\link{perform_hypergeometric_test}} for statistical testing
#' 
#' @note 
#' The function uses a fixed background population size of 19,689 genes and 
#' applies Bonferroni correction with n=1160. These values may need adjustment
#' based on your specific analysis context. This function is used in the Draw VOlcano module,
#' but not in the GSEA module which uses identify_top_enriched_GO_enrichmentinstead.
#' 
#' @export
calculate_go_enrichment <- function(genes, go_categories, go_data, log_messages_rv, log_event) {
  # Initial gene processing logging
  log_event(log_messages_rv, 
            sprintf("Starting gene preprocessing with %d raw entries", length(unlist(genes))),
            "INFO from calculate_go_enrichment()")
  
  # Use clean_gene_names function instead of direct pipe
  genes <- clean_gene_names(genes, log_messages_rv, log_event)                   
  
  # Log gene cleaning results
  log_event(log_messages_rv, 
            sprintf("Gene preprocessing complete: %d unique genes after cleaning", length(genes)),
            "INFO from calculate_go_enrichment()")
  
  # Check if the gene list is empty
  if (length(genes) == 0) {
    log_event(log_messages_rv, 
              "No valid genes found after preprocessing. Returning empty data frame",
              "WARNING from calculate_go_enrichment()")
    return(data.frame())
  }
  
  # Log start of enrichment analysis
  log_event(log_messages_rv, 
            sprintf("Starting GO enrichment analysis for %d genes across %d GO categories", 
                    length(genes), length(go_categories)),
            "INFO from calculate_go_enrichment()")
  
  enrichment_results <- lapply(go_categories, function(go_category) {
    go_genes <- go_data %>% filter(name == go_category) %>% pull(gene) %>% toupper() %>% unique()
    
    # Log category processing
    log_event(log_messages_rv, 
              sprintf("Processing GO category: %s (%d genes in category)", 
                      go_category, length(go_genes)),
              "INFO from calculate_go_enrichment()")
    
    population_size <- 19689  # Number of human coding genes after pseudogene exclusion
    success_population_size <- length(go_genes)
    sample_size <- length(genes) # analyze the  # "clean gene names" section to understand the cleaning process of gene names
    sample_success_size <- sum(sapply(genes, function(gene) {
      cleaned_gene <- gsub("[c\\(\\)\";]", "", gene)
      gene_parts <- toupper(unlist(strsplit(cleaned_gene, "[ ,;:]+")))
      any(gene_parts %in% go_genes)  
    }))
    
    # Log hypergeometric test parameters
    log_event(log_messages_rv, 
              sprintf("GO category %s statistics:\n Population: %d\n Category size: %d\n Sample size: %d\n Matches: %d", 
                      go_category, population_size, success_population_size, sample_size, sample_success_size),
              "INFO from calculate_go_enrichment()")
    
    p_value <- perform_hypergeometric_test(log_messages_rv, population_size, success_population_size, sample_size, sample_success_size, log_event)
    
    # Log p-value result
    log_event(log_messages_rv, 
              sprintf("GO category %s: p-value = %g", go_category, p_value),
              "INFO from calculate_go_enrichment()")
    
    data.frame(
      GO_Category = go_category,
      P_Value = p_value,
      Population_Size = population_size,
      Success_Population_Size = success_population_size,
      Sample_Size = sample_size,
      Sample_Success_Size = sample_success_size
    )
  })
  
  enrichment_results <- bind_rows(enrichment_results) # Combine results into a single data frame
  
  # Check if enrichment_results is empty
  if (nrow(enrichment_results) == 0) {
    log_event(log_messages_rv, 
              "No enrichment results found. Returning empty data frame",
              "WARNING from calculate_go_enrichment()")
    return(data.frame(GO_Category = character(),
                      P_Value = numeric(),
                      Population_Size = numeric(),
                      Success_Population_Size = numeric(),
                      Sample_Size = numeric(),
                      Sample_Success_Size = numeric()))
  }
  
  # Log p-value adjustment
  log_event(log_messages_rv, 
            "Applying Bonferroni correction n = 1160 (estimate for level 4 hierarchy GO tags)", "INFO from calculate_go_enrichment()")
  
  # This is based on the number of level 4 hierarchy GO tags - a more suitable approach may exists but 
  # would involve complex bioinformatic calculations taking into account the hierarchy structure of the GO tags tree
  enrichment_results$Adjusted_P_Value <- p.adjust(enrichment_results$P_Value, method = "bonferroni", n = 1160)
  
  # Log final results summary
  log_event(log_messages_rv, 
            sprintf("GO enrichment analysis complete: processed %d categories, found %d significant results (p < 0.05)", 
                    nrow(enrichment_results),
                    sum(enrichment_results$Adjusted_P_Value < 0.05)),
            "INFO from calculate_go_enrichment()")
  
  return(enrichment_results)
}


#' Identify top enriched GO categories for multiple sets of regulated genes
#'
#' @description
#' Performs Gene Ontology (GO) enrichment analysis using hypergeometric testing for multiple
#' sets of regulated genes (up-regulated, down-regulated, and bidirectionally regulated).
#' The function implements multiple testing correction and various filtering criteria to
#' identify statistically significant GO terms.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input parameters and GO annotations
#' 2. Calculates background statistics for GO terms
#' 3. Processes each gene set (up/down/bidirectional) separately
#' 4. Performs hypergeometric testing for enrichment
#' 5. Adjusts p-values for multiple testing
#' 6. Filters results based on significance criteria
#' 7. Returns both top significant terms and complete analysis results
#'
#' @param detected_genes A character vector of all genes detected in the experiment (background set).
#' @param regulated_sets A named list of character vectors containing regulated genes.
#'        Names should be "up", "down", and/or "bidirectional".
#'        Example: list(up = c("gene1", "gene2"), down = c("gene3", "gene4"))
#' @param go_filtered A data frame containing filtered GO annotations with columns:
#'        \itemize{
#'          \item name - GO term name/ID
#'          \item gene - Gene identifier
#'          \item ontology - GO category (P: Process, F: Function, C: Component)
#'        }
#' @param ontology Character string specifying the GO category to analyze ("P", "F", or "C").
#' @param p_adj_method Method for p-value adjustment. Default: "BH" (Benjamini-Hochberg).
#'        See ?p.adjust for available methods.
#' @param alpha Significance level threshold after p-value adjustment. Default: 0.05
#' @param max_categories Maximum number of top enriched categories to return. Default: 10
#' @param min_genes_in_term Minimum number of genes required in a GO term. Default: 5
#' @param max_genes_in_term Maximum number of genes allowed in a GO term. Default: 500
#' @param min_fold_enrichment Minimum fold enrichment required. Default: 1.5
#' @param log_messages_rv Optional reactiveValues object for Shiny app logging.
#' @param log_event Optional function for logging events in Shiny app.
#'
#' @return A list containing four elements:
#' \describe{
#'   \item{top_results}{A list of data frames (one per gene set) containing significantly
#'         enriched GO terms that pass all filters, sorted by adjusted p-value}
#'   \item{all_results}{A list of data frames (one per gene set) containing all tested
#'         GO terms and their statistics}
#'   \item{top10_results}{A list of data frames (one per gene set) containing the top 10
#'         GO terms sorted by significance, regardless of filtering criteria}
#'   \item{missing_genes}{Character vector of input genes without GO annotations}
#' }
#'
#' Each results data frame contains the following columns:
#' \describe{
#'   \item{name}{GO term name/ID}
#'   \item{gene_set}{Name of the gene set (up/down/bidirectional)}
#'   \item{total_count}{Total number of genes associated with the GO term}
#'   \item{genes_in_term}{Semicolon-separated list of all genes in the term}
#'   \item{regulated_count}{Number of regulated genes in the term}
#'   \item{regulated_genes}{Semicolon-separated list of regulated genes in the term}
#'   \item{expected_count}{Expected number of genes by chance}
#'   \item{fold_enrichment}{Observed/Expected ratio}
#'   \item{p_value}{Raw p-value from hypergeometric test}
#'   \item{p_adj}{Adjusted p-value}
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare input data
#' detected <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' regulated <- list(
#'   up = c("gene1", "gene2"),
#'   down = c("gene3", "gene4")
#' )
#' go_data <- data.frame(
#'   name = c("GO:0001", "GO:0002"),
#'   gene = c("gene1", "gene2"),
#'   ontology = c("P", "P")
#' )
#'
#' # Run enrichment analysis
#' results <- identify_top_go_enrichment(
#'   detected_genes = detected,
#'   regulated_sets = regulated,
#'   go_filtered = go_data,
#'   ontology = "P"
#' )
#' }
#'
#' @seealso
#' \code{\link[stats]{p.adjust}} for p-value adjustment methods
#'
#' @importFrom dplyr group_by summarise filter inner_join mutate arrange %>%
#' @importFrom stats p.adjust phyper
#'
#' @export
identify_top_go_enrichment <- function(detected_genes,
                                       regulated_sets,
                                       go_filtered,
                                       ontology,
                                       p_adj_method = "BH",
                                       alpha = 0.05,
                                       max_categories = 10,
                                       min_genes_in_term = 5,
                                       max_genes_in_term = 500,
                                       min_fold_enrichment = 1.5,
                                       log_messages_rv = NULL,
                                       log_event = NULL) {
  
  # Initialize empty result structure with correct column types
  empty_result <- data.frame(
    name = character(),
    gene_set = character(),
    total_count = integer(),
    genes_in_term = character(),
    regulated_count = integer(),
    regulated_genes = character(),
    expected_count = numeric(),
    fold_enrichment = numeric(),
    p_value = numeric(),
    p_adj = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Initialize results storage with empty structures
  all_results <- list(
    up = empty_result,
    down = empty_result,
    bidirectional = empty_result
  )
  top_results <- list(
    up = empty_result,
    down = empty_result,
    bidirectional = empty_result
  )
  
  # Input validation with safe returns
  if (is.null(detected_genes) || length(detected_genes) == 0) {
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      log_event(log_messages_rv, "No detected genes provided", "ERROR")
    }
    return(list(
      top_results = top_results,
      all_results = all_results,
      missing_genes = character(0)
    ))
  }
  
  if (is.null(regulated_sets) || length(regulated_sets) == 0) {
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      log_event(log_messages_rv, "No regulated gene sets provided", "ERROR")
    }
    return(list(
      top_results = top_results,
      all_results = all_results,
      missing_genes = character(0)
    ))
  }
  
  if (is.null(go_filtered) || nrow(go_filtered) == 0) {
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      log_event(log_messages_rv, "No GO annotations provided", "ERROR")
    }
    return(list(
      top_results = top_results,
      all_results = all_results,
      missing_genes = character(0)
    ))
  }
  
  # Start logging
  if (!is.null(log_messages_rv) && !is.null(log_event)) {
    log_event(log_messages_rv,
              sprintf("Starting GO enrichment analysis:\n- Detected genes: %d\n- Ontology: %s\n- Alpha: %g\n- Max categories: %d\n- Adjustment method: %s\n- Gene size filters: %d-%d\n- Min fold enrichment: %.2f",
                      length(detected_genes), ontology, alpha, max_categories, 
                      p_adj_method, min_genes_in_term, max_genes_in_term, min_fold_enrichment),
              "START from identify_top_go_enrichment()")
  }
  
  # Pre-calculate background statistics with size filtering and error handling
  go_term_stats <- tryCatch({
    stats <- go_filtered %>%
      group_by(name) %>%
      summarise(
        total_count = n_distinct(gene),
        genes_in_term = paste(sort(unique(gene)), collapse = ";"),
        .groups = 'drop'
      ) %>%
      filter(total_count >= min_genes_in_term,
             total_count <= max_genes_in_term)
    
    if (nrow(stats) == 0) {
      return(NULL)
    }
    stats
  }, error = function(e) {
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      log_event(log_messages_rv,
                sprintf("Error in background statistics calculation: %s", e$message),
                "ERROR")
    }
    return(NULL)
  })
  
  if (is.null(go_term_stats)) {
    return(list(
      top_results = top_results,
      all_results = all_results,
      missing_genes = character(0)
    ))
  }
  
  # Process each gene set
  for (set_name in names(regulated_sets)) {
    regulated_genes <- regulated_sets[[set_name]]
    
    if (length(regulated_genes) == 0) {
      if (!is.null(log_messages_rv) && !is.null(log_event)) {
        log_event(log_messages_rv,
                  sprintf("Skipping analysis for empty %s gene set", set_name),
                  "INFO")
      }
      all_results[[set_name]] <- empty_result
      top_results[[set_name]] <- empty_result
      next
    }
    
    # Calculate regulated gene statistics with error handling
    regulated_stats <- tryCatch({
      stats <- go_filtered %>%
        filter(gene %in% regulated_genes) %>%
        group_by(name) %>%
        summarise(
          regulated_count = n_distinct(gene),
          regulated_genes = paste(sort(unique(gene)), collapse = ";"),
          .groups = 'drop'
        )
      
      if (nrow(stats) == 0) {
        return(NULL)
      }
      stats
    }, error = function(e) {
      if (!is.null(log_messages_rv) && !is.null(log_event)) {
        log_event(log_messages_rv,
                  sprintf("Error in regulated gene statistics for %s: %s", set_name, e$message),
                  "ERROR")
      }
      return(NULL)
    })
    
    if (is.null(regulated_stats)) {
      all_results[[set_name]] <- empty_result
      top_results[[set_name]] <- empty_result
      next
    }
    
    # Combine statistics and calculate enrichment with error handling
    set_results <- tryCatch({
      results <- go_term_stats %>%
        inner_join(regulated_stats, by = "name") %>%
        mutate(
          gene_set = set_name,
          expected_count = as.numeric(total_count) * length(regulated_genes) / length(detected_genes)
        ) %>%
        mutate(
          fold_enrichment = as.numeric(regulated_count) / expected_count,
          p_value = pmax(mapply(function(rc, tc, rg, dg) {
            if (is.na(rc) || is.na(tc) || is.na(rg) || is.na(dg) || 
                rc > tc || tc == 0 || rg > dg) {
              return(1)
            }
            phyper(q = rc - 1, m = tc, n = dg - tc, k = rg, lower.tail = FALSE)
          }, regulated_count, total_count, length(regulated_genes), length(detected_genes)),
          .Machine$double.xmin)
        )
      
      if (nrow(results) == 0) {
        return(NULL)
      }
      results
    }, error = function(e) {
      if (!is.null(log_messages_rv) && !is.null(log_event)) {
        log_event(log_messages_rv,
                  sprintf("Error in enrichment calculation for %s: %s", set_name, e$message),
                  "ERROR")
      }
      return(NULL)
    })
    
    if (is.null(set_results)) {
      all_results[[set_name]] <- empty_result
      top_results[[set_name]] <- empty_result
      next
    }
    
    # Adjust p-values with error handling
    set_results$p_adj <- tryCatch({
      p.adjust(set_results$p_value, method = p_adj_method)
    }, error = function(e) {
      if (!is.null(log_messages_rv) && !is.null(log_event)) {
        log_event(log_messages_rv,
                  sprintf("Error in p-value adjustment for %s: %s", set_name, e$message),
                  "ERROR")
      }
      rep(1, nrow(set_results))
    })
    
    # Store full results
    all_results[[set_name]] <- set_results
    
    # Filter and store significant results with error handling
    significant_results <- tryCatch({
      filtered_results <- set_results %>%
        filter(!is.na(p_adj) & as.numeric(p_adj) < alpha,
               !is.na(fold_enrichment) & as.numeric(fold_enrichment) >= min_fold_enrichment) %>%
        arrange(p_adj) %>%
        head(max_categories)
      
      if (nrow(filtered_results) == 0) {
        empty_result
      } else {
        filtered_results
      }
    }, error = function(e) {
      if (!is.null(log_messages_rv) && !is.null(log_event)) {
        log_event(log_messages_rv,
                  sprintf("Error in filtering significant results for %s: %s", set_name, e$message),
                  "ERROR")
      }
      empty_result
    })
    
    top_results[[set_name]] <- significant_results
    
    # Log results with error handling
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      tryCatch({
        log_event(log_messages_rv,
                  sprintf("%s gene set analysis:\n- Terms tested: %d\n- Raw p < 0.05: %d\n- Adjusted p < %g: %d\n- Terms passing fold enrichment ≥ %.1f: %d\n- Final top terms: %d",
                          set_name,
                          nrow(set_results),
                          sum(!is.na(set_results$p_value) & set_results$p_value < 0.05),
                          alpha,
                          sum(!is.na(set_results$p_adj) & set_results$p_adj < alpha),
                          min_fold_enrichment,
                          sum(!is.na(set_results$fold_enrichment) & set_results$fold_enrichment >= min_fold_enrichment),
                          nrow(significant_results)),
                  "INFO")
      }, error = function(e) {
        log_event(log_messages_rv,
                  sprintf("Error in logging results for %s: %s", set_name, e$message),
                  "ERROR")
      })
    }
  }
  
  # Calculate missing genes with error handling
  missing_genes <- tryCatch({
    unique(unlist(lapply(regulated_sets, function(genes) {
      setdiff(genes, unique(go_filtered$gene))
    })))
  }, error = function(e) {
    if (!is.null(log_messages_rv) && !is.null(log_event)) {
      log_event(log_messages_rv,
                sprintf("Error in calculating missing genes: %s", e$message),
                "ERROR")
    }
    character(0)
  })
  
  # Final logging
  if (!is.null(log_messages_rv) && !is.null(log_event)) {
    log_event(log_messages_rv,
              sprintf("GO enrichment analysis completed.\nMissing genes: %d",
                      length(missing_genes)),
              "SUCCESS")
  }
  top10_results <- list(
    up = tryCatch({
      all_results$up %>%
        # First sort significant results (p_adj < 1)
        mutate(
          sorting_value = case_when(
            p_adj < 1 ~ p_adj,
            TRUE ~ p_value + 1  # Add 1 to ensure p_adj < 1 results come first
          )
        ) %>%
        arrange(sorting_value) %>%
        slice_head(n = 10) %>%
        select(-sorting_value)  # Remove the temporary sorting column
    }, error = function(e) { empty_result }),
    
    down = tryCatch({
      all_results$down %>%
        mutate(
          sorting_value = case_when(
            p_adj < 1 ~ p_adj,
            TRUE ~ p_value + 1
          )
        ) %>%
        arrange(sorting_value) %>%
        slice_head(n = 10) %>%
        select(-sorting_value)
    }, error = function(e) { empty_result }),
    
    bidirectional = tryCatch({
      all_results$bidirectional %>%
        mutate(
          sorting_value = case_when(
            p_adj < 1 ~ p_adj,
            TRUE ~ p_value + 1
          )
        ) %>%
        arrange(sorting_value) %>%
        slice_head(n = 10) %>%
        select(-sorting_value)
    }, error = function(e) { empty_result })
  )
  
  return(list(
    top_results = top_results,
    all_results = all_results,
    top10_results = top10_results,  # Add this to the return list
    missing_genes = missing_genes
  ))
}


#' @title Calculate GO Enrichment Tables
#' @description Performs Gene Ontology (GO) enrichment analysis separately for 
#' upregulated, downregulated, and all regulated genes based on provided significance 
#' thresholds and fold change values. Includes comprehensive error handling and logging.
#' 
#' @param df Data frame containing differential expression analysis results
#' @param annotation_col Character, name of the column containing gene annotations
#' @param go_categories Vector of GO category names to test for enrichment
#' @param go_data Data frame containing GO annotations with columns 'name', 'id', and 'gene'
#' @param alpha Numeric, significance threshold for adjusted p-values
#' @param fold_col Character, name of the column containing fold change values
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' @param log_structure Structure logging function created by create_structure_logger()
#' 
#' @return A list containing three elements:
#'   \itemize{
#'     \item upregulated: List with 'data' containing GO enrichment results for upregulated genes
#'     \item downregulated: List with 'data' containing GO enrichment results for downregulated genes
#'     \item regulated: List with 'data' containing GO enrichment results for all regulated genes
#'   }
#' Each 'data' element is a data frame with columns:
#'   \itemize{
#'     \item GO_Category: Name of the GO category
#'     \item P_Value: Unadjusted p-value
#'     \item Adjusted_P_Value: Bonferroni-corrected p-value
#'     \item Population_Size: Total number of background genes
#'     \item Success_Population_Size: Number of genes in GO category
#'     \item Sample_Size: Number of input genes
#'     \item Sample_Success_Size: Number of matches
#'     \item id: GO term identifier
#'   }
#' 
#' @details 
#' The function performs these steps:
#' 1. Filters genes based on significance (alpha) and fold change direction
#' 2. Performs separate GO enrichment analyses for each gene group
#' 3. Handles cases with no regulated genes by returning empty data frames
#' 4. Adds GO term identifiers to results
#' 5. Implements comprehensive error handling and logging
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   structure_logger <- create_structure_logger(session)
#'   
#'   # Example input data
#'   df <- data.frame(
#'     gene = c("BRCA1", "TP53", "PTEN"),
#'     log2FC = c(2.1, -1.5, 1.8),
#'     adjusted_pvalues = c(0.01, 0.03, 0.02)
#'   )
#'   
#'   go_cats <- c("DNA repair", "cell cycle")
#'   go_data <- data.frame(
#'     name = c("DNA repair", "cell cycle"),
#'     id = c("GO:0006281", "GO:0007049"),
#'     gene = c("BRCA1", "TP53")
#'   )
#'   
#'   results <- calculate_go_enrichment_table(
#'     df = df,
#'     annotation_col = "gene",
#'     go_categories = go_cats,
#'     go_data = go_data,
#'     alpha = 0.05,
#'     fold_col = "log2FC",
#'     log_messages,
#'     logger,
#'     structure_logger
#'   )
#' }
#' }
#' 
#' @importFrom dplyr filter pull distinct left_join sym
#' @importFrom rlang .data !!
#' 
#' @seealso 
#' \code{\link{calculate_go_enrichment}} for the core enrichment calculation
#' \code{\link{create_logger}} for logging functionality
#' \code{\link{create_structure_logger}} for structure logging
#' 
#' @note 
#' The function assumes the presence of an 'adjusted_pvalues' column in the input
#' data frame. Empty results are handled gracefully with appropriate structure
#' and NA values.
#' 
#' @export
calculate_go_enrichment_table <- function(df, annotation_col, go_categories, go_data, alpha, fold_col, log_messages_rv, log_event, log_structure) {
  # Log start of analysis with parameters
  log_event(log_messages_rv,
            sprintf("Starting GO enrichment analysis with parameters:\n Alpha: %g\n Fold change column: %s\n Annotation column: %s",
                    alpha, fold_col, annotation_col),
            "INFO from calculate_go_enrichment_table()")
  
  # Get regulated genes
  upregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% 
    pull(!!sym(annotation_col))
  
  downregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% 
    pull(!!sym(annotation_col))
  
  regulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha) %>% 
    pull(!!sym(annotation_col))
  
  # Log gene counts
  log_event(log_messages_rv,
            sprintf("Filtered gene counts:\n Upregulated: %d\n Downregulated: %d\n Total regulated: %d",
                    length(upregulated_genes),
                    length(downregulated_genes),
                    length(regulated_genes)),
            "INFO from calculate_go_enrichment_table()")
  
  # Check if there are any regulated genes
  if (length(regulated_genes) == 0) {
    log_event(log_messages_rv,
              "No regulated genes found at the current significance threshold",
              "WARNING from calculate_go_enrichment_table()")
    
    # Create empty enrichment results with proper structure
    empty_enrichment <- data.frame(
      GO_Category = go_categories,
      P_Value = NA_real_,
      Population_Size = NA_integer_,
      Success_Population_Size = NA_integer_,
      Sample_Size = 0,
      Sample_Success_Size = 0,
      Adjusted_P_Value = NA_real_
    )
    
    # Get GO IDs for categories even if no enrichment
    go_ids <- go_data %>% 
      filter(name %in% go_categories) %>% 
      distinct(name, id)
    
    empty_enrichment <- empty_enrichment %>%
      left_join(go_ids, by = c("GO_Category" = "name"))
    
    # Return results list with empty data frames but proper structure
    return(list(
      upregulated = list(data = empty_enrichment),
      downregulated = list(data = empty_enrichment),
      regulated = list(data = empty_enrichment)
    ))
  }
  
  # Get GO IDs for categories
  go_ids <- go_data %>% 
    filter(name %in% go_categories) %>% 
    distinct(name, id)
  
  # Log GO categories information
  log_event(log_messages_rv,
            sprintf("Processing %d GO categories for enrichment analysis",
                    nrow(go_ids)),
            "INFO from calculate_go_enrichment_table()")
  
  # Calculate enrichment with added GO IDs
  # Wrap each enrichment calculation in tryCatch to handle potential errors
  upregulated_enrichment <- tryCatch({
    if (length(upregulated_genes) > 0) {
      log_event(log_messages_rv, "Starting upregulated genes enrichment analysis", 
                "INFO from calculate_go_enrichment_table()")
      calculate_go_enrichment(upregulated_genes, go_categories, go_data, log_messages_rv, log_event) %>%
        left_join(go_ids, by = c("GO_Category" = "name"))
    } else {
      log_event(log_messages_rv, "No upregulated genes found", 
                "INFO from calculate_go_enrichment_table()")
      empty_enrichment
    }
  }, error = function(e) {
    log_event(log_messages_rv, 
              sprintf("Error in upregulated enrichment: %s", e$message),
              "ERROR from calculate_go_enrichment_table()")
    empty_enrichment
  })
  
  downregulated_enrichment <- tryCatch({
    if (length(downregulated_genes) > 0) {
      log_event(log_messages_rv, "Starting downregulated genes enrichment analysis", 
                "INFO from calculate_go_enrichment_table()")
      calculate_go_enrichment(downregulated_genes, go_categories, go_data, log_messages_rv, log_event) %>%
        left_join(go_ids, by = c("GO_Category" = "name"))
    } else {
      log_event(log_messages_rv, "No downregulated genes found", 
                "INFO from calculate_go_enrichment_table()")
      empty_enrichment
    }
  }, error = function(e) {
    log_event(log_messages_rv, 
              sprintf("Error in downregulated enrichment: %s", e$message),
              "ERROR from calculate_go_enrichment_table()")
    empty_enrichment
  })
  
  regulated_enrichment <- tryCatch({
    if (length(regulated_genes) > 0) {
      log_event(log_messages_rv, "Starting all regulated genes enrichment analysis", 
                "INFO from calculate_go_enrichment_table()")
      calculate_go_enrichment(regulated_genes, go_categories, go_data, log_messages_rv, log_event) %>%
        left_join(go_ids, by = c("GO_Category" = "name"))
    } else {
      log_event(log_messages_rv, "No regulated genes found", 
                "INFO from calculate_go_enrichment_table()")
      empty_enrichment
    }
  }, error = function(e) {
    log_event(log_messages_rv, 
              sprintf("Error in regulated enrichment: %s", e$message),
              "ERROR from calculate_go_enrichment_table()")
    empty_enrichment
  })
  
  # Create results list
  enrichment_results_list <- list(
    upregulated = list(data = upregulated_enrichment),
    downregulated = list(data = downregulated_enrichment),
    regulated = list(data = regulated_enrichment)
  )
  
  # Log summary of results
  log_event(log_messages_rv,
            sprintf("GO enrichment analysis completed:\n Upregulated categories: %d\n Downregulated categories: %d\n Total regulated categories: %d\n",
                    nrow(upregulated_enrichment),
                    nrow(downregulated_enrichment),
                    nrow(regulated_enrichment)),
            "INFO from calculate_go_enrichment_table()")
  
  log_structure(log_messages_rv, enrichment_results_list, "Final enrichment results structure", 
                "INFO from calculate_go_enrichment_table()")
  
  return(enrichment_results_list)
}



#' @title Create Publication Plot
#' @description Converts a ggplot object into a publication-ready format by adjusting 
#' dimensions, point sizes, text sizes, and margins according to standardized journal 
#' figure specifications. Includes comprehensive logging of all modifications.
#' 
#' @param base_plot A ggplot2 object to be converted to publication format
#' @param width_mm Numeric, desired width in millimeters
#' @param height_mm Numeric, desired height in millimeters
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A modified ggplot2 object with publication-ready formatting
#' 
#' @details 
#' Supports five standardized size configurations:
#' \itemize{
#'   \item 85x85mm (single column square)
#'   \item 114x114mm (1.5 column square)
#'   \item 114x65mm (1.5 column rectangle)
#'   \item 174x174mm (double column square)
#'   \item 174x98mm (double column rectangle)
#' }
#' 
#' Point size configurations for each format:
#' \describe{
#'   \item{85x85mm}{base: 0.6, highlight: 0.9, annotation: 1.5}
#'   \item{114x114mm}{base: 0.8, highlight: 1.2, annotation: 2.0}
#'   \item{114x65mm}{base: 0.7, highlight: 1.05, annotation: 1.75}
#'   \item{174x174mm}{base: 1.0, highlight: 1.5, annotation: 2.5}
#'   \item{174x98mm}{base: 0.9, highlight: 1.35, annotation: 2.25}
#' }
#' 
#' Layout configurations:
#' \describe{
#'   \item{Compact (85x85mm, 114x65mm)}{
#'     \itemize{
#'       \item Title size: 10pt
#'       \item Text size: 6pt
#'       \item Margins: top=10pt, right=10pt, bottom=5pt, left=5pt
#'     }
#'   }
#'   \item{Standard (other sizes)}{
#'     \itemize{
#'       \item Title size: 12pt
#'       \item Text size: 8pt
#'       \item Margins: top=30pt, right=85pt, bottom=10pt, left=10pt
#'     }
#'   }
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Create base volcano plot
#'   base_plot <- ggplot(data, aes(x = log2FC, y = -log10(pvalue))) +
#'     geom_point()
#'   
#'   # Convert to single-column publication format
#'   pub_plot <- create_publication_plot(
#'     base_plot = base_plot,
#'     width_mm = 85,
#'     height_mm = 85,
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#'   
#'   # Save the plot
#'   ggsave("publication_plot.pdf", pub_plot, 
#'          width = 85 * 0.0393701, 
#'          height = 85 * 0.0393701)
#' }
#' }
#' 
#' @importFrom ggplot2 theme element_text margin
#' @importFrom rlang %||%
#' 
#' @section Plot Layer Modifications:
#' The function automatically detects and modifies different types of layers:
#' \itemize{
#'   \item Point geometries: Adjusts sizes based on highlight status
#'   \item Text and label geometries: Adjusts sizes based on annotation status
#'   \item Theme elements: Updates text sizes and margins based on plot dimensions
#' }
#' 
#' @note 
#' - Dimensions should be provided in millimeters
#' - The function automatically converts measurements to inches for ggsave compatibility
#' - Highlighted points are identified by their color (darkgreen or red)
#' - Annotations are identified by infinite x and y coordinates
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' \code{\link[ggplot2]{theme}} for theme modifications
#' 
#' @export
create_publication_plot <- function(base_plot, width_mm, height_mm, log_messages_rv, log_event) {
  # Log start of plot creation with dimensions
  log_event(log_messages_rv,
            sprintf("Creating publication plot with dimensions: %dmm x %dmm", width_mm, height_mm),
            "INFO from create_publication_plot()")
  
  # Convert mm to inches for ggsave compatibility
  width_in <- width_mm * 0.0393701
  height_in <- height_mm * 0.0393701
  
  # Log conversion to inches
  log_event(log_messages_rv,
            sprintf("Converted dimensions to inches: %.2f\" x %.2f\"", width_in, height_in),
            "INFO from create_publication_plot()")
  
  # Define fixed point sizes for different plot dimensions
  point_sizes <- list(
    "85x85" = list(base = 0.6, highlight = 0.9, annotation = 1.5),
    "114x114" = list(base = 0.8, highlight = 1.2, annotation = 2),
    "114x65" = list(base = 0.7, highlight = 1.05, annotation = 1.75),
    "174x174" = list(base = 1, highlight = 1.5, annotation = 2.5),
    "174x98" = list(base = 0.9, highlight = 1.35, annotation = 2.25)
  )
  
  # Determine which size configuration to use
  plot_key <- paste0(width_mm, "x", height_mm)
  point_size <- point_sizes[[plot_key]] %||% point_sizes[["85x85"]]
  
  # Log selected size configuration
  log_event(log_messages_rv,
            sprintf("Using size configuration for '%s' (base: %.2f, highlight: %.2f, annotation: %.2f)",
                    plot_key,
                    point_size$base,
                    point_size$highlight,
                    point_size$annotation),
            "INFO from create_publication_plot()")
  
  # Adjust text sizes and margins based on plot dimensions
  if (plot_key %in% c("85x85", "114x65")) {
    title_size <- 10
    text_size <- 6
    plot_margin <- margin(t = 10, r = 10, b = 5, l = 5, unit = "pt")
    log_event(log_messages_rv,
              sprintf("Using compact layout (title: %d, text: %d)", title_size, text_size),
              "INFO from create_publication_plot()")
  } else {
    title_size <- 12
    text_size <- 8
    plot_margin <- margin(t = 30, r = 85, b = 10, l = 10, unit = "pt")
    log_event(log_messages_rv,
              sprintf("Using standard layout (title: %d, text: %d)", title_size, text_size),
              "INFO from create_publication_plot()")
  }
  
  # Modify the plot with publication-ready settings
  publication_plot <- base_plot +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size),
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      plot.margin = plot_margin
    )
  
  # Log layer modifications
  log_event(log_messages_rv,
            sprintf("Modifying %d plot layers", length(publication_plot$layers)),
            "INFO from create_publication_plot()")
  
  # Update all layers
  layer_modifications <- list()
  for (i in seq_along(publication_plot$layers)) {
    layer <- publication_plot$layers[[i]]
    
    # Handle point geometries
    if (inherits(layer$geom, "GeomPoint")) {
      is_highlight <- !is.null(layer$aes_params$color) &&
        layer$aes_params$color %in% c("darkgreen", "red")
      new_size <- if (is_highlight) point_size$highlight else point_size$base
      layer$aes_params$size <- new_size
      layer_modifications[[i]] <- sprintf("Layer %d: Point geometry %s", 
                                          i, 
                                          if(is_highlight) "highlighted" else "base")
    }
    
    # Handle text and label annotations
    if (inherits(layer$geom, "GeomText") ||
        inherits(layer$geom, "GeomTextRepel") ||
        inherits(layer$geom, "GeomLabelRepel") ||
        inherits(layer$geom, "GeomLabel")) {
      
      is_annotation <- all(
        !is.null(layer$data),
        "x" %in% names(layer$data),
        "y" %in% names(layer$data),
        all(is.infinite(layer$data$x) & is.infinite(layer$data$y))
      )
      
      if (is_annotation) {
        layer$aes_params$size <- point_size$annotation
        layer_modifications[[i]] <- sprintf("Layer %d: Annotation text", i)
      } else {
        layer$aes_params$size <- text_size * 0.25
        layer_modifications[[i]] <- sprintf("Layer %d: Regular text label", i)
      }
    }
  }
  
  # Log layer modification summary
  if (length(layer_modifications) > 0) {
    log_event(log_messages_rv,
              sprintf("Layer modifications completed:\n%s", 
                      paste(unlist(layer_modifications), collapse = "\n")),
              "INFO from create_publication_plot()")
  }
  
  log_event(log_messages_rv,
            "Publication plot creation completed",
            "INFO from create_publication_plot()")
  
  return(publication_plot)
}


#' Build Gene Set Enrichment Analysis (GSEA) Plots
#' 
#' @title Build GSEA Plots
#' @description Creates publication-ready Gene Set Enrichment Analysis visualizations 
#' for up-regulated, down-regulated, and bidirectionally-regulated genes. Supports 
#' different ontology types and includes options for displaying non-significant results.
#' 
#' @param enrichment_results_list List containing enrichment analysis results with 
#'        components $top_results and $top10_results, each containing $bidirectional, 
#'        $up, and $down data frames
#' @param ontology Character, type of ontology being analyzed (e.g., "Biological Process")
#' @param show_not_significant Logical, whether to show non-significant terms (default: FALSE)
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' @param plotOntologyValue Character, one of "P" (Biological Process), 
#'        "F" (Molecular Function), or "C" (Cellular Component)
#' 
#' @return A list containing three ggplot objects:
#'   \itemize{
#'     \item bidirectional: Plot for bidirectionally-regulated genes
#'     \item up: Plot for up-regulated genes
#'     \item down: Plot for down-regulated genes
#'   }
#' Each plot is NULL if no data is available for that category.
#' 
#' @details 
#' Plot Features:
#' \itemize{
#'   \item Horizontal bar charts showing fold enrichment
#'   \item Color gradient indicating significance (-log10 adjusted p-value)
#'   \item Term labels including regulated/detected gene counts
#'   \item Grey bars for non-significant terms (when show_not_significant = TRUE)
#'   \item Customized semantic UI blue color scheme
#' }
#' 
#' Color Scheme:
#' \itemize{
#'   \item Light blue (#E6F3FF) for low significance
#'   \item Semantic UI Blue (#2185D0) for medium significance
#'   \item Navy blue (#084B8A) for high significance
#'   \item Grey (grey90) for non-significant terms
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example enrichment results
#'   enrichment_results <- list(
#'     top_results = list(
#'       bidirectional = data.frame(
#'         name = c("GO:Term1", "GO:Term2"),
#'         p_adj = c(0.01, 0.03),
#'         regulated_count = c(10, 15),
#'         total_count = c(100, 150),
#'         fold_enrichment = c(2.5, 1.8)
#'       ),
#'       up = data.frame(...),
#'       down = data.frame(...)
#'     ),
#'     top10_results = list(...)
#'   )
#'   
#'   plots <- build_gsea_plots(
#'     enrichment_results_list = enrichment_results,
#'     ontology = "Biological Process",
#'     show_not_significant = FALSE,
#'     log_messages = log_messages,
#'     log_event = logger,
#'     plotOntologyValue = "P"
#'   )
#' }
#' }
#' 
#' 
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_gradient2 labs 
#'             theme_minimal theme element_markdown element_text element_blank 
#'             element_rect margin
#' @importFrom dplyr mutate arrange
#' @importFrom rlang if_else
#' 
#' @section Plot Customization:
#' The function includes several visual customizations:
#' \itemize{
#'   \item Font sizes: 11pt for axis text and legends, 12pt for titles
#'   \item Compact margins: 5pt on all sides
#'   \item Legend position: Bottom right corner with white background
#'   \item Line height adjustment for multi-line term labels
#'   \item Removed vertical gridlines for cleaner appearance
#' }
#' 
#' @note 
#' - Required data frame columns: name, p_adj, regulated_count, total_count, fold_enrichment
#' - Assumes adjusted p-value threshold of 0.05 for significance
#' - Uses markdown formatting for term labels
#' 
#' # Process the data with fixed label format using colon separator - previously it was / but some ratios
# were not rendered properly 

# Issue explanation: The original problem occurred because ggplot2's element_markdown()
# was interpreting single-digit fractions (like 1/5, 1/7, 1/9) as special fraction notations
# when both numerator and denominator were single digits. This caused these specific
# number combinations to be rendered incorrectly or become invisible. The solution was
# to use a colon instead of a forward slash to prevent the fraction interpretation
# while maintaining a clear ratio representation.
#' 
#' 
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' 
#' @export
build_gsea_plots <- function(enrichment_results_list, ontology = "Biological Process", 
                             show_not_significant = FALSE, log_messages_rv, log_event,
                             plotOntologyValue) {  
  
  # 1. Initial logging
  log_event(log_messages_rv,
            sprintf("Starting build_gsea_plots with ontology = %s, show_not_significant = %s", 
                    ontology, show_not_significant),
            "DEBUG from build_gsea_plots")
  
  # 2. Data preparation: Check if each category exists in the input data
  # Checks both top10_results and top_results for data availability
  has_bidirectional <- if (show_not_significant) {
    !is.null(enrichment_results_list$top10_results$bidirectional) && 
      nrow(enrichment_results_list$top10_results$bidirectional) > 0
  } else {
    !is.null(enrichment_results_list$top_results$bidirectional) && 
      nrow(enrichment_results_list$top_results$bidirectional) > 0
  }
  
  has_up <- if (show_not_significant) {
    !is.null(enrichment_results_list$top10_results$up) && 
      nrow(enrichment_results_list$top10_results$up) > 0
  } else {
    !is.null(enrichment_results_list$top_results$up) && 
      nrow(enrichment_results_list$top_results$up) > 0
  }
  
  has_down <- if (show_not_significant) {
    !is.null(enrichment_results_list$top10_results$down) && 
      nrow(enrichment_results_list$top10_results$down) > 0
  } else {
    !is.null(enrichment_results_list$top_results$down) && 
      nrow(enrichment_results_list$top_results$down) > 0
  }
  
  # Store availability information
  enrichment_results_list$has_bidirectional <- has_bidirectional
  enrichment_results_list$has_up <- has_up
  enrichment_results_list$has_down <- has_down
  
  # Select appropriate results based on show_not_significant parameter
  results_to_use <- list(
    bidirectional = if (has_bidirectional) {
      if (show_not_significant) enrichment_results_list$top10_results$bidirectional 
      else enrichment_results_list$top_results$bidirectional
    } else NULL,
    up = if (has_up) {
      if (show_not_significant) enrichment_results_list$top10_results$up 
      else enrichment_results_list$top_results$up
    } else NULL,
    down = if (has_down) {
      if (show_not_significant) enrichment_results_list$top10_results$down 
      else enrichment_results_list$top_results$down
    } else NULL
  )
  
  
  # 3. Early validation of data sources
  log_event(log_messages_rv,
            sprintf("Checking data availability - Bidirectional: %s, Up: %s, Down: %s",
                    !is.null(results_to_use$bidirectional),
                    !is.null(results_to_use$up),
                    !is.null(results_to_use$down)),
            "DEBUG from build_gsea_plots")
  
  # 4. Prepare plot titles using the plotOntologyValue instead of ontology
  current_ontology <- plotOntologyValue  # Get the stored ontology value
  
  plot_titles <- list(
    bidirectional = sprintf("%s\nBidirectionally-regulated Genes\n%s",
                            switch(current_ontology,
                                   "P" = "Biological Processes",
                                   "F" = "Molecular Functions",
                                   "C" = "Cellular Compartments",
                                   current_ontology),  # fallback to original value if not P,F,C
                            if(show_not_significant) 
                              "Top 10 terms\n(significant in color)" 
                            else 
                              "Top 10 significant terms"),
    up = sprintf("%s\nUp-regulated Genes\n%s",
                 switch(current_ontology,
                        "P" = "Biological Processes",
                        "F" = "Molecular Functions",
                        "C" = "Cellular Compartments",
                        current_ontology),
                 if(show_not_significant) 
                   "Top 10 terms\n(significant in color)" 
                 else 
                   "Top 10 significant terms"),
    down = sprintf("%s\nDown-regulated Genes\n%s",
                   switch(current_ontology,
                          "P" = "Biological Processes",
                          "F" = "Molecular Functions",
                          "C" = "Cellular Compartments",
                          current_ontology),
                   if(show_not_significant) 
                     "Top 10 terms\n(significant in color)" 
                   else 
                     "Top 10 significant terms")
  )
  
  # 5. Define helper function for plot creation
  create_single_plot <- function(results_data, plot_title, log_messages_rv) {
    # Input validation
    if (is.null(results_data) || nrow(results_data) == 0) {
      log_event(log_messages_rv,
                sprintf("No data available for %s plot", plot_title),
                "WARNING from build_gsea_plots")
      return(NULL)
    }
    
    # Data processing
    # Note: Using colon instead of slash to avoid markdown fraction interpretation,
    # (needed because of element_markdown() from ggtext)
    plot_data <- results_data %>%
      mutate(
        term_label = sprintf("<b>%s</b><br>%d:%d [regulated:detected]", 
                             name, regulated_count, total_count),
        neg_log10_padj = -log10(p_adj),
        is_significant = p_adj < 0.05
      ) %>%
      arrange(desc(fold_enrichment)) %>%
      mutate(
        term_label = factor(term_label, levels = rev(term_label))
      )
    
    # Log processed data info
    log_event(log_messages_rv,
              sprintf("Created plot data with %d rows, %d significant", 
                      nrow(plot_data),
                      sum(plot_data$is_significant)),
              "DEBUG from build_gsea_plots")
    
    # Plot creation
    plot <- ggplot(plot_data, aes(x = fold_enrichment, y = term_label)) +
      geom_bar(aes(fill = if_else(is_significant, neg_log10_padj, NA_real_)), 
               stat = "identity",
               width = 0.3) +
      scale_fill_gradient2(
        name = "-log10(adj.P)",
        low = "#E6F3FF",      # Light blue
        mid = "#2185D0",      # Semantic UI Blue
        high = "#084B8A",     # Navy blue
        limits = c(1.30, max(plot_data$neg_log10_padj, na.rm = TRUE)),  # From -log10(0.05) to max value
        midpoint = (1.30 + max(plot_data$neg_log10_padj, na.rm = TRUE))/2,  # Set midpoint halfway
        na.value = "grey90"
      )+
      labs(
        title = plot_title,
        x = "Fold Enrichment",
        y = NULL
      ) +
      scale_y_discrete(expand = c(0, 0)) +
      theme_minimal() +
      theme(
        axis.text = element_markdown(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        plot.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        axis.text.y.left = element_text(lineheight = 0.9),
        legend.position = c(0.95, 0.05),  # x=0.95 and y=0.05 positions (bottom right)
        legend.justification = c(1, 0),    # Anchor point at bottom right
        legend.box.just = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.background = element_rect(fill = "white", color = NA)
      )
    
    return(plot)
  }
  
  # 6. Create all three plots
  plots <- list(
    bidirectional = create_single_plot(results_to_use$bidirectional, plot_titles$bidirectional, log_messages_rv),
    up = create_single_plot(results_to_use$up, plot_titles$up, log_messages_rv),
    down = create_single_plot(results_to_use$down, plot_titles$down, log_messages_rv)
  )
  
  # 7. Final logging and return
  log_event(log_messages_rv, "All plots created successfully", "DEBUG from build_gsea_plots")
  return(plots)
}

#' Build GSEA Results Table using GT Package
#' 
#' @title Build GSEA GT Table
#' @description Creates a formatted GT (Grammar of Tables) table displaying Gene Set 
#' Enrichment Analysis (GSEA) results. The table includes separate sections for 
#' up-regulated, down-regulated, and bidirectionally-regulated genes with custom 
#' styling and informative footnotes.
#' 
#' @param enrichment_results_list List containing GSEA results with structure:
#'   \itemize{
#'     \item top_results
#'       \itemize{
#'         \item bidirectional: Data frame of bidirectionally regulated terms
#'         \item up: Data frame of up-regulated terms
#'         \item down: Data frame of down-regulated terms
#'       }
#'   }
#' @param color_highlight Character vector of length 2 containing hex color codes:
#'   \itemize{
#'     \item [1]: Color for down-regulated genes section
#'     \item [2]: Color for up-regulated genes section
#'   }
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A GT table object containing the formatted GSEA results, or a simple 
#' message table if no significant results are found.
#' 
#' @details 
#' Table Structure:
#' \itemize{
#'   \item Title: "Gene Set Enrichment Analysis Results"
#'   \item Subtitle: "Significantly enriched Gene Ontology terms"
#'   \item Columns:
#'     \itemize{
#'       \item GO Term: Name of the GO term
#'       \item Detected Genes in Term: Total number of genes in the GO term
#'       \item Regulated Genes in Term: Number of regulated genes in the term
#'       \item Fold Enrichment: Enrichment ratio (2 decimal places)
#'       \item P-value: Raw p-value (<0.001 or 3 decimal places)
#'       \item Adjusted P-value: BH-adjusted p-value (<0.001 or 3 decimal places)
#'     }
#' }
#' 
#' Styling Features:
#' \itemize{
#'   \item Custom background colors for regulation groups
#'   \item Bold column headers and group labels
#'   \item 12px font size
#'   \item Full-width table layout
#'   \item Informative footnotes for statistical measures
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example enrichment results
#'   enrichment_results <- list(
#'     top_results = list(
#'       bidirectional = data.frame(
#'         name = c("DNA repair", "Cell cycle"),
#'         p_value = c(0.001, 0.01),
#'         p_adj = c(0.005, 0.03),
#'         regulated_count = c(10, 15),
#'         total_count = c(100, 150),
#'         fold_enrichment = c(2.5, 1.8)
#'       ),
#'       up = data.frame(...),
#'       down = data.frame(...)
#'     )
#'   )
#'   
#'   # Create table with custom colors
#'   gsea_table <- build_gsea_gt_table(
#'     enrichment_results_list = enrichment_results,
#'     color_highlight = c("#FFE6E6", "#E6FFE6"),  # Light red and light green
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#' }
#' }
#' 
#' @importFrom gt gt tab_header cols_label fmt_number tab_options tab_style 
#'             tab_footnote cells_column_labels cells_row_groups cell_text cell_fill pct px
#' @importFrom dplyr mutate select bind_rows
#' @importFrom tibble tibble
#' 
#' @section Footnotes:
#' The table includes detailed footnotes explaining:
#' \itemize{
#'   \item Adjusted p-value calculation using Benjamini-Hochberg method
#'   \item Raw p-value calculation using hypergeometric test
#'   \item Interpretation of fold enrichment values
#' }
#' 
#' @note 
#' - Required columns in input data frames: name, p_value, p_adj, regulated_count, 
#'   total_count, fold_enrichment
#' - Returns a simple message table if no significant results are found
#' - Group styling uses bidirectional (gray), up-regulated (user-defined), and 
#'   down-regulated (user-defined) colors
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' 
#' @export

build_gsea_gt_table <- function(enrichment_results_list, color_highlight, log_messages_rv, log_event) {
  # Log function start
  log_event(log_messages_rv,
            sprintf("Starting GSEA GT table build with parameters:\n Colors: %s, %s",
                    color_highlight[1], color_highlight[2]),
            "INFO from build_gsea_gt_table()")
  
  down_color <- color_highlight[1]
  up_color <- color_highlight[2]
  
  # Early return if no results or all empty
  if (is.null(enrichment_results_list) || 
      is.null(enrichment_results_list$top_results) || 
      all(sapply(enrichment_results_list$top_results, function(x) nrow(x) == 0))) {
    
    return(
      tibble(Message = "No significant functional group of genes is overrepresented in this set") %>%
        gt() %>%
        tab_options(
          table.width = pct(100),
          table.font.size = px(12),
          column_labels.visible = FALSE
        )
    )
  }
  
  # Process results for each regulation type
  process_results <- function(df, regulation_type) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df %>%
      mutate(
        Regulation = regulation_type,
        Genes_Ratio = sprintf("%d/%d", regulated_count, total_count),
        Fold_Enrichment = round(fold_enrichment, 2),
        P_Value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
        Adjusted_P_Value = ifelse(p_adj < 0.001, "<0.001", sprintf("%.3f", p_adj))
      ) %>%
      select(
        Regulation,
        GO_Term = name,
        Genes_Total = total_count,
        Genes_Found = regulated_count,
        Fold_Enrichment,
        P_Value,
        Adjusted_P_Value
      )
  }
  
  # Process each regulation type
  regulated_df <- process_results(enrichment_results_list$top_results$bidirectional, "Bidirectional")
  upregulated_df <- process_results(enrichment_results_list$top_results$up, "Up-regulated")
  downregulated_df <- process_results(enrichment_results_list$top_results$down, "Down-regulated")
  
  # Combine results
  combined_df <- bind_rows(
    regulated_df,
    upregulated_df,
    downregulated_df
  )
  
  # Create GT table
  gt_table <- combined_df %>%
    gt(groupname_col = "Regulation") %>%
    tab_header(
      title = "Gene Set Enrichment Analysis Results",
      subtitle = "Significantly enriched Gene Ontology terms"
    ) %>%
    cols_label(
      GO_Term = "GO Term",
      Genes_Total = "Detected Genes in Term",
      Genes_Found = "Regulated Genes in Term",
      Fold_Enrichment = "Fold Enrichment",
      P_Value = "P-value",
      Adjusted_P_Value = "Adjusted P-value"
    ) %>%
    fmt_number(
      columns = Fold_Enrichment,
      decimals = 2
    ) %>%
    tab_footnote(
      footnote = "Adjusted p-values were calculated using the Benjamini-Hochberg (BH) 
      method to control the False Discovery Rate (FDR). Note that BH treats all
      GO terms as independent, which may lead to over-adjustment (conservative results)
      for broad terms with many dependent child terms or under-adjustment (liberal results) 
      for terms whose significance is driven by overlapping child terms
      see https://github.com/DatViseR/Vivid-Volcano/Notes on GSEA statistics.md 
      for details examples and explanation"
      ,
      locations = cells_column_labels("Adjusted_P_Value")
    ) %>%
    tab_footnote(
      footnote = "The raw p-value is calculated using the hypergeometric test
      ,which assesses the probability of observing at least the given number of
      regulated genes in a GO term by chance, given the total number of detected
      genes, the total count of genes in the term, and the number of
      regulated genes in the dataset."
      ,
      locations = cells_column_labels("P_Value")
      
    )%>%
    tab_footnote(
      footnote = "represents how many times more frequent the GO term 
      appears among the regulated gene set compared to what would be expected by chance"
      ,
      locations = cells_column_labels("Fold_Enrichment")
    )%>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    )
  
  # Style row groups
  if (!is.null(regulated_df) && nrow(regulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "#D3D3D3")
        ),
        locations = cells_row_groups(groups = "Bidirectional")
      )
  }
  
  if (!is.null(upregulated_df) && nrow(upregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = up_color)
        ),
        locations = cells_row_groups(groups = "Up-regulated")
      )
  }
  
  if (!is.null(downregulated_df) && nrow(downregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = down_color)
        ),
        locations = cells_row_groups(groups = "Down-regulated")
      )
  }
  
  log_event(log_messages_rv,
            "GSEA GT table build completed successfully",
            "INFO from build_gsea_gt_table()")
  
  return(gt_table)
}

#' Filter Gene Set Enrichment Analysis Results
#' 
#' @title Filter GSEA Results
#' @description Filters Gene Set Enrichment Analysis (GSEA) results based on a pattern 
#' match, keeping the most significant matching term for each category while 
#' preserving the original structure and non-matching terms.
#' 
#' @param enrichment_results_list List containing GSEA results with structure:
#'   \itemize{
#'     \item all_results: Complete results for each regulation type
#'     \item top_results: Significant results (p_adj < 0.05, fold enrichment >= 1.5)
#'     \item top10_results: Top 10 results regardless of significance
#'     \item missing_genes: List of genes not found in analysis
#'   }
#' Each results component contains three sub-lists: up, down, and bidirectional
#' 
#' @param filter_pattern Character string containing the pattern to match against GO terms
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A list with the same structure as the input, but filtered:
#'   \itemize{
#'     \item all_results: Filtered complete results
#'     \item top_results: Rebuilt top significant results from filtered data
#'     \item top10_results: Rebuilt top 10 results from filtered data
#'     \item missing_genes: Unchanged from input
#'   }
#' 
#' @details 
#' Filtering Process:
#' \enumerate{
#'   \item For each regulation type (up, down, bidirectional):
#'     \itemize{
#'       \item Finds terms matching the pattern (case-insensitive)
#'       \item Keeps only the most significant matching term
#'       \item Preserves all non-matching terms
#'       \item Maintains original sorting by adjusted p-value
#'     }
#'   \item Rebuilds result categories:
#'     \itemize{
#'       \item top_results: Significant terms (p_adj < 0.05, fold enrichment >= 1.5)
#'       \item top10_results: Top 10 terms by significance
#'     }
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example GSEA results
#'   enrichment_results <- list(
#'     all_results = list(
#'       up = data.frame(
#'         name = c("DNA repair", "DNA replication", "cell cycle"),
#'         p_adj = c(0.01, 0.02, 0.03),
#'         p_value = c(0.005, 0.01, 0.015),
#'         fold_enrichment = c(2.5, 2.0, 1.8)
#'       ),
#'       down = data.frame(...),
#'       bidirectional = data.frame(...)
#'     ),
#'     top_results = list(...),
#'     top10_results = list(...)
#'   )
#'   
#'   # Filter for DNA-related terms
#'   filtered_results <- filter_gsea_results(
#'     enrichment_results_list = enrichment_results,
#'     filter_pattern = "DNA",
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#' }
#' }
#' 
#' @section Filtering Rules:
#' \itemize{
#'   \item Pattern matching is case-insensitive
#'   \item Empty data frames are returned unchanged
#'   \item If no matches are found, original data is returned
#'   \item For multiple matches, only the most significant term is kept
#'   \item Non-matching terms are preserved
#' }
#' 
#' @note 
#' Required columns in input data frames:
#' \itemize{
#'   \item name: GO term name
#'   \item p_adj: Adjusted p-value
#'   \item p_value: Raw p-value
#'   \item fold_enrichment: Enrichment ratio
#' }
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export

filter_gsea_results <- function(enrichment_results_list, filter_pattern, log_messages_rv, log_event) {
  log_event(log_messages_rv, 
            sprintf("Starting GSEA filtering with pattern '%s'", filter_pattern), 
            "INFO from filter_gsea_results")
  
  filter_all_results <- function(df, pattern) {
    if (nrow(df) == 0) {
      log_event(log_messages_rv, 
                "Encountered empty data frame; returning as is.", 
                "INFO from filter_gsea_results")
      return(df)
    }
    
    # Split into matching and non-matching results
    matching_idx <- grepl(pattern, df$name, ignore.case = TRUE)
    
    if (!any(matching_idx)) {
      log_event(log_messages_rv, 
                "No matching entries found; returning original data frame.", 
                "INFO from filter_gsea_results")
      return(df)
    }
    
    # Get matching results and sort by adjusted p-value
    matching_results <- df[matching_idx, , drop = FALSE]
    matching_results <- matching_results[order(matching_results$p_adj), ]
    
    log_event(log_messages_rv, 
              sprintf("Found %d matching entries; keeping most significant (p_adj: %g)", 
                      nrow(matching_results), matching_results$p_adj[1]), 
              "INFO from filter_gsea_results")
    
    # Keep only the top matching result with all its original data
    top_match <- matching_results[1, , drop = FALSE]
    
    # Get non-matching results preserving all original data
    non_matching_results <- df[!matching_idx, , drop = FALSE]
    
    # Combine and sort while preserving all columns
    filtered_results <- rbind(top_match, non_matching_results)
    filtered_results <- filtered_results[order(filtered_results$p_adj), ]
    
    return(filtered_results)
  }
  
  rebuild_top_results <- function(all_results_df, alpha = 0.05, min_fold_enrichment = 1.5, max_categories = 10) {
    if (nrow(all_results_df) == 0) return(all_results_df[0, ])
    
    # Filter significant results while preserving all columns
    significant_results <- all_results_df[
      !is.na(all_results_df$p_adj) & 
        all_results_df$p_adj < alpha &
        !is.na(all_results_df$fold_enrichment) & 
        all_results_df$fold_enrichment >= min_fold_enrichment, 
    ]
    
    if (nrow(significant_results) == 0) return(all_results_df[0, ])
    
    # Sort and take top results while preserving all data
    significant_results[order(significant_results$p_adj)[1:min(max_categories, nrow(significant_results))], ]
  }
  
  rebuild_top10_results <- function(all_results_df) {
    if (nrow(all_results_df) == 0) return(all_results_df[0, ])
    
    # Create temporary sorting value
    sorting_values <- ifelse(all_results_df$p_adj < 1, 
                             all_results_df$p_adj, 
                             all_results_df$p_value + 1)
    
    # Sort and take top 10 while preserving all original data
    all_results_df[order(sorting_values)[1:min(10, nrow(all_results_df))], ]
  }
  
  categories <- c("up", "down", "bidirectional")
  filtered_top_results <- list()
  filtered_all_results <- list()
  filtered_top10_results <- list()
  
  for (cat in categories) {
    # Get original data safely
    original_all <- enrichment_results_list$all_results[[cat]]
    
    # Filter all_results first
    filtered_all_results[[cat]] <- filter_all_results(original_all, filter_pattern)
    
    # Rebuild other results from filtered all_results
    filtered_top_results[[cat]] <- rebuild_top_results(filtered_all_results[[cat]])
    filtered_top10_results[[cat]] <- rebuild_top10_results(filtered_all_results[[cat]])
    
    # Get the original top10 for comparison
    original_top10 <- enrichment_results_list$top10_results[[cat]]
    
    # Enhanced logging with safe comparison
    changed_terms <- if (!is.null(original_top10) && nrow(original_top10) > 0) {
      sum(!filtered_top10_results[[cat]]$name %in% original_top10$name)
    } else {
      NA
    }
    
    log_event(log_messages_rv, 
              sprintf(paste0(
                "Category '%s' processed:\n",
                "- All results: %d (from %d)\n",
                "- Significant (p_adj < 0.05): %d\n",
                "- Top matching term: '%s' (p_adj: %g)\n",
                "- Top10 content changes: %s"
              ),
              cat,
              nrow(filtered_all_results[[cat]]),
              nrow(original_all),
              sum(!is.na(filtered_all_results[[cat]]$p_adj) & 
                    filtered_all_results[[cat]]$p_adj < 0.05),
              filtered_top10_results[[cat]]$name[1],
              filtered_top10_results[[cat]]$p_adj[1],
              if(is.na(changed_terms)) "N/A" else sprintf("%d terms", changed_terms)),
              "INFO from filter_gsea_results")
  }
  
  log_event(log_messages_rv, "GSEA filtering completed.", "SUCCESS from filter_gsea_results")
  
  filtered_gsea_results <- list(
    top_results = filtered_top_results,
    all_results = filtered_all_results,
    top10_results = filtered_top10_results,
    missing_genes = enrichment_results_list$missing_genes
  )
  
  return(filtered_gsea_results)
}




#' Build Formatted GT Table for Gene Ontology Enrichment Results
#' 
#' @title Build GO Enrichment GT Table
#' @description Creates a formatted GT (Grammar of Tables) table displaying Gene 
#' Ontology enrichment results with separate sections for bidirectionally regulated, 
#' up-regulated, and down-regulated genes. Includes enrichment ratios, statistical 
#' tests, and comprehensive footnotes.
#' 
#' @param enrichment_results_list List containing enrichment results with structure:
#'   \itemize{
#'     \item regulated$data: Data frame for bidirectionally regulated genes
#'     \item upregulated$data: Data frame for up-regulated genes
#'     \item downregulated$data: Data frame for down-regulated genes
#'   }
#' Each data frame must contain columns: GO_Category, id, Success_Population_Size, 
#' Sample_Size, Sample_Success_Size, P_Value, Adjusted_P_Value
#' 
#' @param upregulated_count Integer, number of up-regulated genes
#' @param downregulated_count Integer, number of down-regulated genes
#' @param color_highlight Character vector of length 2:
#'   \itemize{
#'     \item [1]: Color for down-regulated section
#'     \item [2]: Color for up-regulated section
#'   }
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A GT table object containing formatted enrichment results with:
#'   \itemize{
#'     \item Color-coded sections for each regulation type
#'     \item Enrichment ratios for detected and regulated genes
#'     \item Statistical test results with formatted p-values
#'     \item Explanatory footnotes
#'   }
#' 
#' @details 
#' Table Features:
#' \itemize{
#'   \item Columns:
#'     \itemize{
#'       \item GO name: Gene Ontology term name
#'       \item GO id: Gene Ontology identifier
#'       \item Detected genes enrichment: Ratio of detected genes in GO term to total background
#'       \item Regulated genes enrichment: Ratio of regulated genes in GO term to total regulated
#'       \item Statistical tests: Raw and Bonferroni-adjusted p-values
#'       \item Gene counts: Numbers of detected and regulated genes in each category
#'     }
#'   \item Formatting:
#'     \itemize{
#'       \item P-values: "<0.001" for small values, 3 decimal places otherwise
#'       \item Enrichment ratios: 3 decimal places
#'       \item 12px font size
#'       \item Bold column headers
#'       \item Full-width table layout
#'     }
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example enrichment results
#'   enrichment_results <- list(
#'     regulated = list(
#'       data = data.frame(
#'         GO_Category = c("DNA repair", "Cell cycle"),
#'         id = c("GO:0006281", "GO:0007049"),
#'         Success_Population_Size = c(100, 150),
#'         Sample_Size = c(50, 50),
#'         Sample_Success_Size = c(10, 15),
#'         P_Value = c(0.001, 0.01),
#'         Adjusted_P_Value = c(0.005, 0.03)
#'       )
#'     ),
#'     upregulated = list(data = data.frame(...)),
#'     downregulated = list(data = data.frame(...))
#'   )
#'   
#'   # Create table
#'   gt_table <- build_gt_table(
#'     enrichment_results_list = enrichment_results,
#'     upregulated_count = 30,
#'     downregulated_count = 20,
#'     color_highlight = c("#FFE6E6", "#E6FFE6"),  # Light red and light green
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#' }
#' }
#' 
#' @importFrom gt gt tab_header fmt_markdown cols_label tab_footnote 
#'             cells_column_labels cols_hide tab_options tab_row_group 
#'             tab_style cells_row_groups cell_text cell_fill pct px
#' @importFrom dplyr mutate select bind_rows
#' 
#' @section Footnotes:
#' The table includes detailed footnotes explaining:
#' \itemize{
#'   \item Bonferroni correction methodology (n=1160 for level 4 GO hierarchy)
#'   \item Detected gene enrichment calculation and interpretation
#'   \item Regulated gene enrichment calculation and interpretation
#' }
#' 
#' @note 
#' - P-values less than 0.001 are displayed as "<0.001"
#' - Bidirectionally regulated section uses light gray (#D3D3D3) background
#' - Sample_Size column is hidden but used for group labels
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export

build_gt_table <- function(enrichment_results_list, upregulated_count, downregulated_count, color_highlight, log_messages_rv, log_event) {
  
  
  # Log function start and parameters
  log_event(log_messages_rv,
            sprintf("Starting GT table build with parameters:\n Colors: %s, %s\n Counts - Up: %d, Down: %d",
                    color_highlight[1], color_highlight[2],
                    upregulated_count, downregulated_count),
            "INFO from build_gt_table()")
  
  
  down_color <- color_highlight[1]
  up_color <- color_highlight[2]
  
  # Log data preparation start
  log_event(log_messages_rv,
            "Processing regulated genes data",
            "INFO from build_gt_table()")
  
  # Prepare data frames with rounded values and formatted p-values - this is done to ensure consistent formatting
  # in the table output and to avoid scientific notation
  regulated_df <- enrichment_results_list$regulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.3f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.3f", Adjusted_P_Value)),
      P_Value = as.character(P_Value),
      Adjusted_P_Value = as.character(Adjusted_P_Value)
    )
  
  upregulated_df <- enrichment_results_list$upregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value)),
      P_Value = as.character(P_Value),
      Adjusted_P_Value = as.character(Adjusted_P_Value)
    )
  
  downregulated_df <- enrichment_results_list$downregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value)),
      P_Value = as.character(P_Value),
      Adjusted_P_Value = as.character(Adjusted_P_Value)
    )
  
  # Combine data frames
  log_event(log_messages_rv,
            "Combining data frames and formatting table",
            "INFO from build_gt_table()")
  
  
  # Combine all data frames and remove "Population_Size" column
  combined_df <- bind_rows(
    regulated_df,
    upregulated_df,
    downregulated_df
  ) %>%
    select(-Population_Size) %>%  # Removes "Number of human genes" column
    mutate(
      P_Value = as.character(P_Value),
      Adjusted_P_Value = as.character(Adjusted_P_Value)
    )
  
  # Create GT table
  log_event(log_messages_rv,
            sprintf("Creating GT table with %d total rows", nrow(combined_df)),
            "INFO from build_gt_table()")
  
  
  # Create the gt table with formatted numbers
  gt_table <- combined_df %>%
    gt() %>%
    tab_header(
      title = "Gene Ontology Enrichment Results",
      subtitle = paste(
        "three groups of regulated proteins and",
        nrow(regulated_df),
        "chosen GO terms"
      )
    ) %>%
    fmt_markdown(
      columns = c(
        "Population_Enrichment_Ratio",
        "Subpopulation_Enrichment_Ratio",
        "P_Value",
        "Adjusted_P_Value"
      )
    ) %>%
    cols_label(
      GO_Category = "GO name",
      id = "GO id",
      Population_Enrichment_Ratio = "Detected genes enrichment",
      Subpopulation_Enrichment_Ratio = "Regulated genes enrichment",
      P_Value = "Hypergeometric test p-value",
      Adjusted_P_Value = "Bonferroni adj-p value",
      Success_Population_Size = "Detected genes in GO category",
      Sample_Size = "Number of regulated genes",
      Sample_Success_Size = "Regulated genes in GO category"
    ) %>%
    # Adding footnotes
    
    tab_footnote(
      footnote = "Bonferroni correction based on the estimated number of level 4 hierarchy GO tags n=1160",
      locations = cells_column_labels("Adjusted_P_Value")
    ) %>%
    tab_footnote(
      footnote = "Detected gene enrichment is defined as the ratio of detected genes annotated to the GO term
      to the total number of detected background genes,
      indicating the prevalence of the GO term within the analyzed gene set.",
      locations = cells_column_labels("Population_Enrichment_Ratio")
    ) %>%
    tab_footnote(
      footnote = "Regulated genes enrichment is calculated as the ratio of 
      regulated genes in the GO term to the total number of regulated genes
      in the analysis, reflecting the relative enrichment within the regulated subset.",
      locations = cells_column_labels("Subpopulation_Enrichment_Ratio")
    ) %>%
    
    # Hide the Sample_Size column
    cols_hide(columns = "Sample_Size") %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    )
  
  # Add colored row groups
  if (nrow(regulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Bidirectionally regulated n = ", regulated_df$Sample_Size[1]),
        rows = 1:nrow(regulated_df)
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "#D3D3D3")  # Light gray color
        ),
        locations = cells_row_groups(groups = paste0("Bidirectionally regulated n = ", regulated_df$Sample_Size[1]))
      )
  }
  
  if (nrow(upregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Upregulated n = ", upregulated_df$Sample_Size[1]),
        rows = (nrow(regulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = up_color)  # Use up_color from input
        ),
        locations = cells_row_groups(groups = paste0("Upregulated n = ", upregulated_df$Sample_Size[1]))
      )
  }
  
  if (nrow(downregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Downregulated n = ", downregulated_df$Sample_Size[1]),
        rows = (nrow(regulated_df) + nrow(upregulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df) + nrow(downregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = down_color)  # Use down_color from input
        ),
        locations = cells_row_groups(groups = paste0("Downregulated n = ", downregulated_df$Sample_Size[1]))
      )
  }
  
  log_event(log_messages_rv,
            "GT table build completed successfully",
            "INFO from build_gt_table()")
  
  
  return(gt_table)
}


#' @title Build GO Category Gene Lists Table
#' @description Creates a formatted GT (Grammar of Tables) table showing gene lists 
#' for selected GO categories, with color-coded display of up-regulated and 
#' down-regulated genes, and visual distinction between detected and non-detected genes.
#' 
#' @param df Data frame containing differential expression analysis results
#' @param annotation_col Character, name of the column containing gene annotations
#' @param chosen_go Character vector of selected GO category names
#' @param go_data Data frame containing GO annotations with columns:
#'   \itemize{
#'     \item name: GO category name
#'     \item id: GO identifier
#'     \item gene: Gene identifier
#'   }
#' @param alpha Numeric, significance threshold for adjusted p-values
#' @param fold_col Character, name of the column containing fold change values
#' @param color_highlight Character vector of length 2:
#'   \itemize{
#'     \item [1]: Color for down-regulated genes
#'     \item [2]: Color for up-regulated genes
#'   }
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return A GT table object with four columns:
#'   \itemize{
#'     \item GO Category: GO term name and ID
#'     \item All Genes: Complete gene list with visual formatting
#'     \item Downregulated Genes: List of down-regulated genes
#'     \item Upregulated Genes: List of up-regulated genes
#'   }
#' 
#' @details 
#' Gene Classification:
#' \itemize{
#'   \item Detected genes: All genes present in the input data
#'   \item Regulated genes: Genes with adjusted p-value < alpha
#'   \item Up-regulated: Regulated genes with positive fold change
#'   \item Down-regulated: Regulated genes with negative fold change
#' }
#' 
#' Visual Formatting:
#' \itemize{
#'   \item Bold: Detected genes
#'   \item Color (up): Up-regulated genes
#'   \item Color (down): Down-regulated genes
#'   \item Plain: Non-detected genes
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example data
#'   df <- data.frame(
#'     gene = c("BRCA1", "TP53", "PTEN"),
#'     log2FC = c(2.1, -1.5, 1.8),
#'     adjusted_pvalues = c(0.01, 0.03, 0.02)
#'   )
#'   
#'   go_data <- data.frame(
#'     name = c("DNA repair", "DNA repair", "cell cycle"),
#'     id = c("GO:0006281", "GO:0006281", "GO:0007049"),
#'     gene = c("BRCA1", "TP53", "PTEN")
#'   )
#'   
#'   # Create gene lists table
#'   gene_table <- build_gt_gene_lists(
#'     df = df,
#'     annotation_col = "gene",
#'     chosen_go = c("DNA repair", "cell cycle"),
#'     go_data = go_data,
#'     alpha = 0.05,
#'     fold_col = "log2FC",
#'     color_highlight = c("#FFE6E6", "#E6FFE6"),
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#' }
#' }
#' 
#' @importFrom gt gt tab_header cols_label tab_style fmt_markdown tab_options 
#'             cells_body cell_text pct px
#' @importFrom dplyr filter pull bind_rows sym
#' @importFrom rlang !!
#' 
#' @section Table Features:
#' \itemize{
#'   \item Responsive width (100%)
#'   \item 12px base font size
#'   \item Bold column headers
#'   \item Top-aligned text in GO Category column
#'   \item Markdown formatting support for gene lists
#'   \item Color-coded regulation status
#' }
#' 
#' @note 
#' - Requires clean_gene_names() function for gene name standardization
#' - Uses HTML spans for color formatting in the All Genes column
#' - Displays "No genes found" messages when appropriate
#' - GO Category column combines term name and ID with newline
#' 
#' @seealso 
#' \code{\link{clean_gene_names}} for gene name preprocessing
#' \code{\link{create_logger}} for logging functionality
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export
build_gt_gene_lists <- function(df, annotation_col, chosen_go, go_data, alpha, fold_col, color_highlight, log_messages_rv, log_event) {
  # Debug color inputs
  cat("\n==== Color Values Received ====\n")
  cat("Down-regulated color:", color_highlight[1], "\n")
  cat("Up-regulated color:", color_highlight[2], "\n")
  cat("================================\n\n")
  
  # Log function start with key parameters
  log_event(log_messages_rv,
            sprintf("Building gene lists table\n Alpha: %g\nChosen GO terms: %d\nAnnotation column: %s",
                    alpha,
                    length(chosen_go),
                    annotation_col),
            "INFO from build_gt_gene_lists()")
  
  # Extract colors
  down_color <- color_highlight[1]
  up_color <- color_highlight[2]
  
  # Get gene lists - corrected classification
  detected_genes <- df[[annotation_col]] %>%  # All genes in the experiment
    clean_gene_names(., log_messages_rv, log_event)
  
  regulated_genes <- df %>%  # All significantly regulated genes
    filter(adjusted_pvalues < alpha) %>%
    pull(!!sym(annotation_col)) %>%
    clean_gene_names(., log_messages_rv, log_event)
  
  downregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% 
    pull(!!sym(annotation_col)) %>%
    clean_gene_names(., log_messages_rv, log_event)
  
  upregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% 
    pull(!!sym(annotation_col)) %>%
    clean_gene_names(., log_messages_rv, log_event)
  
  # Log gene counts
  log_event(log_messages_rv,
            sprintf("Gene counts:\nTotal detected: %d\nTotal regulated: %d\nUpregulated: %d\nDownregulated: %d",
                    length(detected_genes),
                    length(regulated_genes),
                    length(upregulated_genes),
                    length(downregulated_genes)),
            "INFO from build_gt_gene_lists()")
  
  # Process GO categories
  log_event(log_messages_rv,
            sprintf("Processing %d GO categories", length(chosen_go)),
            "INFO from build_gt_gene_lists()")
  
  
  # Process each GO category and prepare data for color-coded display
  table_data <- lapply(chosen_go, function(category) {
    category_data <- go_data %>% 
      filter(name == category)
    
    go_genes <- category_data %>%
      pull(gene) %>%
      clean_gene_names(., log_messages_rv, log_event)
    
    go_id <- unique(category_data$id)[1]  # Get the GO ID
    
    # Create separate strings for different regulation states
    down_genes <- intersect(downregulated_genes, go_genes)
    up_genes <- intersect(upregulated_genes, go_genes)
    detected_only_genes <- setdiff(intersect(detected_genes, go_genes), c(down_genes, up_genes))
    non_detected_genes <- setdiff(go_genes, detected_genes)
    
    all_genes_formatted <- c(
      # Format downregulated genes
      if(length(down_genes) > 0) {
        paste0("<span style='color: ", down_color, "'><strong>", down_genes, "</strong></span>")
      },
      # Format upregulated genes
      if(length(up_genes) > 0) {
        paste0("<span style='color: ", up_color, "'><strong>", up_genes, "</strong></span>")
      },
      # Format detected but not regulated genes
      if(length(detected_only_genes) > 0) {
        paste0("<strong>", detected_only_genes, "</strong>")
      },
      # Add non-detected genes without formatting
      if(length(non_detected_genes) > 0) {
        non_detected_genes
      }
    )
    
    data.frame(
      GO_Category = paste0(category,"\n",  go_id),  # Combined name and ID
      All_Genes = if(length(all_genes_formatted) > 0) {
        paste(all_genes_formatted, collapse = ", ")
      } else {
        "No genes found"
      },
      Downregulated = if(length(down_genes) > 0) {
        paste(down_genes, collapse = ", ")
      } else {
        "No downregulated genes found"
      },
      Upregulated = if(length(up_genes) > 0) {
        paste(up_genes, collapse = ", ")
      } else {
        "No upregulated genes found"
      },
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  
  # Log table creation
  log_event(log_messages_rv,
            sprintf("Creating GT table with %d rows", nrow(table_data)),
            "INFO from build_gt_gene_lists()")
  
  # Create GT table with styling
  gt_table_genes <- table_data %>%
    gt() %>%
    tab_header(
      title = "Gene Lists by GO Category",
      subtitle = paste("Analysis includes", length(detected_genes), "detected genes,",
                       "of which", length(regulated_genes), "are regulated")
    ) %>%
    cols_label(
      GO_Category = "GO Category",
      All_Genes = "All Genes (detected in bold, detected and regulated in color [if color option enabled])",
      Downregulated = "Downregulated Genes",
      Upregulated = "Upregulated Genes"
    ) %>%
    # Style for downregulated genes
    tab_style(
      style = list(
        cell_text(color = down_color, weight = "bold", v_align = "top")
      ),
      locations = cells_body(
        columns = "Downregulated",
        rows = !grepl("No downregulated genes found", table_data$Downregulated)
      )
    ) %>%
    # Style for upregulated genes
    tab_style(
      style = list(
        cell_text(color = up_color, weight = "bold", v_align = "top")
      ),
      locations = cells_body(
        columns = "Upregulated",
        rows = !grepl("No upregulated genes found", table_data$Upregulated)
      )
    ) %>%
    # Align text in GO_Category column to the top
    tab_style(
      style = cell_text(v_align = "top"),
      locations = cells_body(
        columns = "GO_Category"
      )
    ) %>%
    fmt_markdown(columns = "All_Genes") %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    )
  
  log_event(log_messages_rv,
            "Gene lists table creation completed successfully",
            "SUCCESS from build_gt_gene_lists()")
  
  return(gt_table_genes)
  
}



##' Check and Unlog P-values
#' 
#' @title Check and Unlog P-values
#' @description Validates p-values in a data frame and attempts to reverse -log10 
#' transformation if detected. Performs comprehensive range checking and handles NA values.
#' 
#' @param df Data frame containing p-values
#' @param pvalue_col Character, name of the column containing p-values
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' 
#' @return The input data frame with potentially transformed p-values if -log10 
#' transformation was detected and reversed. Original data frame is returned if no 
#' transformation was needed or possible.
#' 
#' @details 
#' Validation Steps:
#' \enumerate{
#'   \item Checks for NA values
#'   \item Identifies values outside valid p-value range [0,1]
#'   \item Detects potential -log10 transformation
#'   \item Attempts to reverse -log10 transformation if detected
#'   \item Verifies successful transformation
#' }
#' 
#' Detection Criteria:
#' \itemize{
#'   \item Values between -50 and 50 suggest -log10 transformation
#'   \item Negative values or values > 1 indicate invalid p-values
#'   \item NA values are counted but preserved
#' }
#' 
#' @section Logging Messages:
#' The function generates several types of log messages:
#' \describe{
#'   \item{PVALUE}{Basic process information}
#'   \item{WARNING}{Issues with data quality}
#'   \item{ERROR}{Critical problems with values}
#'   \item{SUCCESS}{Successful transformations}
#'   \item{VALIDATION}{Confirmation of valid values}
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   
#'   # Example with regular p-values
#'   df1 <- data.frame(
#'     pvalue = c(0.01, 0.05, 0.001, NA)
#'   )
#'   result1 <- check_and_unlog_pvalues(
#'     df = df1,
#'     pvalue_col = "pvalue",
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#'   
#'   # Example with -log10 transformed p-values
#'   df2 <- data.frame(
#'     pvalue = c(2, 1.30, 3, NA)  # -log10(0.01), -log10(0.05), -log10(0.001)
#'   )
#'   result2 <- check_and_unlog_pvalues(
#'     df = df2,
#'     pvalue_col = "pvalue",
#'     log_messages = log_messages,
#'     log_event = logger
#'   )
#' }
#' }
#' 
#' @section Edge Cases:
#' \itemize{
#'   \item NA values are preserved and counted
#'   \item Values outside [-50, 50] are considered invalid and not transformed
#'   \item Zero p-values are allowed but may indicate potential issues
#'   \item Extremely small p-values may result from unlogging large negative values
#' }
#' 
#' @note 
#' - The function assumes -log10 transformation if values are in [-50, 50]
#' - Takes absolute values before unlogging to handle negative -log10 values
#' - Original column is overwritten if transformation is performed
#' - NA handling is passive (preserved without modification)
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export

check_and_unlog_pvalues <- function(df, pvalue_col, log_messages_rv, log_event) {
  log_event(log_messages_rv, "Starting p-value range check", "PVALUE")
  pvalues <- as.numeric(df[[pvalue_col]])
  
  # Check for NAs
  na_count <- sum(is.na(pvalues))
  if (na_count > 0) {
    log_event(log_messages_rv, 
              sprintf("WARNING: Found %d NA values in p-value column", na_count), 
              "WARNING")
  }
  
  # Check value ranges
  negative_values <- sum(pvalues < 0, na.rm = TRUE)
  above_one_values <- sum(pvalues > 1, na.rm = TRUE)
  total_abnormal <- negative_values + above_one_values
  
  if (total_abnormal > 0) {
    log_event(log_messages_rv, 
              sprintf("Found %d abnormal p-values (%d negative, %d above 1)", 
                      total_abnormal, negative_values, above_one_values), 
              "WARNING")
    
    # Check if values appear to be -log10 transformed
    if (all(pvalues >= -50 & pvalues <= 50, na.rm = TRUE)) {
      log_event(log_messages_rv, 
                "Values appear to be -log10 transformed. Attempting to unlog...", 
                "PVALUE")
      pvalues <- 10^(-abs(pvalues))
      df[[pvalue_col]] <- pvalues
      
      # Verify transformation worked
      new_invalid <- sum(pvalues < 0 | pvalues > 1, na.rm = TRUE)
      if (new_invalid == 0) {
        log_event(log_messages_rv, 
                  "P-value unlogging completed successfully. All values now in [0,1] range", 
                  "SUCCESS")
      } else {
        log_event(log_messages_rv, 
                  sprintf("WARNING: %d values still outside [0,1] range after unlogging", 
                          new_invalid), 
                  "ERROR")
      }
    } else {
      log_event(log_messages_rv, 
                "Values outside normal range and do not appear to be -log10 transformed", 
                "ERROR")
    }
  } else {
    log_event(log_messages_rv, 
              sprintf("All %d p-values are in correct range [0,1]", 
                      sum(!is.na(pvalues))), 
              "VALIDATION")
  }
  
  return(df)
}


#' Diagnose Expression Columns for Suspicious Values
#' 
#' @title Diagnose Expression Data Columns
#' @description Identifies and handles potentially problematic values in expression-related 
#' columns (e.g., log2 fold changes, expression levels). Provides interactive alerts 
#' for user confirmation before removing suspicious data points.
#' 
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' @param uploaded_df Reactive function returning the data frame to analyze
#' 
#' @return Invisibly returns NULL. Function operates through side effects:
#'   \itemize{
#'     \item Updates the uploaded_df reactive value if suspicious values are removed
#'     \item Generates log messages
#'     \item Shows interactive alerts
#'   }
#' 
#' @details 
#' Detection Process:
#' \enumerate{
#'   \item Identifies expression-related columns using keywords:
#'     \itemize{
#'       \item "log2"
#'       \item "expression"
#'       \item "fold"
#'     }
#'   \item Checks for values where abs(value) > 10
#'   \item Presents findings to user via shinyalert
#'   \item Removes flagged rows upon user confirmation
#' }
#' 
#' @section Alert Features:
#' The function generates an HTML-formatted alert showing:
#' \itemize{
#'   \item Number of suspicious values per column
#'   \item Column names with issues
#'   \item Option to proceed with removal
#'   \item Cannot be dismissed without user action
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   uploaded_df <- reactiveVal(data.frame(
#'     gene = c("A", "B", "C", "D"),
#'     log2_fc = c(2, 15, -1, -20),
#'     expression = c(1, 2, 11, 4)
#'   ))
#'   
#'   # Call the diagnostic function
#'   observeEvent(input$analyze, {
#'     diagnose_expression_column(
#'       log_messages_rv = log_messages,
#'       log_event = logger,
#'       uploaded_df = uploaded_df
#'     )
#'   })
#' }
#' }
#' 
#' @section Logging Messages:
#' The function generates several types of log messages:
#' \describe{
#'   \item{Column Detection}{Reports found expression-related columns}
#'   \item{Value Analysis}{Reports presence/absence of suspicious values}
#'   \item{User Actions}{Records user's decision about removing rows}
#'   \item{Data Modification}{Reports number of rows removed if applicable}
#' }
#' 
#' @note 
#' - Assumes log2-transformed expression values
#' - Values |x| > 10 are considered suspicious
#' - Case-insensitive column name matching
#' - Modifies data only with user confirmation
#' - Alert cannot be dismissed by clicking outside or pressing ESC
#' 
#' @importFrom shinyalert shinyalert
#' @importFrom htmltools HTML
#' 
#' @section Dependencies:
#' Requires the following packages:
#' \itemize{
#'   \item shinyalert: For interactive alerts
#'   \item htmltools: For HTML formatting in alerts
#' }
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' \code{\link[shinyalert]{shinyalert}} for alert creation
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export
diagnose_expression_column <- function(log_messages_rv, log_event, uploaded_df) {
  # Get the current dataframe from the reactive value
  df <- uploaded_df()
  
  # Define keywords for identifying expression-like columns
  keywords <- c("log2", "expression", "fold")
  expression_cols <- grep(
    paste(keywords, collapse = "|"), 
    colnames(df), 
    value = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(expression_cols) == 0) {
    log_event(log_messages_rv, "No expression-like columns detected.", "INFO from diagnose_expression_column")
    return(invisible(NULL))
  }
  
  # Log detected columns
  log_event(log_messages_rv, sprintf("Detected expression-like columns: %s", paste(expression_cols, collapse = ", ")), 
            "INFO from diagnose_expression_column")
  
  # Check for suspicious values in expression-like columns
  suspicious_rows <- lapply(expression_cols, function(col) {
    which(abs(df[[col]]) > 10)  # Identify rows with absolute values > 10
  })
  names(suspicious_rows) <- expression_cols
  
  # Filter columns with suspicious values
  suspicious_cols <- names(suspicious_rows)[sapply(suspicious_rows, length) > 0]
  if (length(suspicious_cols) == 0) {
    log_event(log_messages_rv, "No suspicious values detected in expression columns.", "INFO from diagnose_expression_column")
    return(invisible(NULL))
  }
  
  # Prepare alert message
  alert_text <- paste0(
    "<strong>Suspicious Values Detected:</strong><br/>",
    paste(sapply(suspicious_cols, function(col) {
      sprintf("%d values in column '%s' exceed abs(10)", length(suspicious_rows[[col]]), col)
    }), collapse = "<br/>"),
    "<br/><br/>Click 'Proceed' to remove these rows."
  )
  
  # Show alert and handle user response
  shinyalert::shinyalert(
    title = "Data Warning",
    text = HTML(alert_text),
    type = "warning",
    html = TRUE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "Proceed",
    callbackR = function(proceed) {
      if (isTRUE(proceed)) {
        log_event(log_messages_rv, "User opted to remove suspicious rows.", "INFO from diagnose_expression_column")
        
        # Remove rows with suspicious values
        rows_to_remove <- unique(unlist(suspicious_rows[suspicious_cols]))
        df <- df[-rows_to_remove, ]
        
        # Update the reactive value
        uploaded_df(df)
        
        log_event(log_messages_rv, sprintf("Removed %d rows with suspicious values.", length(rows_to_remove)), 
                  "INFO from diagnose_expression_column")
      } else {
        log_event(log_messages_rv, "User canceled the removal of suspicious rows.", "INFO from diagnose_expression_column")
      }
    }
  )
  
  return(invisible(NULL))
}


#' Diagnose and Clean Data for Omics Analysis
#' 
#' @title Diagnose and Clean Numeric Data
#' @description Performs automated diagnosis and cleaning of data frames containing
#' omics data, focusing on numeric columns and fold change values. Provides 
#' interactive user feedback and logging of all modifications.
#' 
#' @param df Data frame to be diagnosed and cleaned
#' @param log_messages_rv Reactive value for storing log messages
#' @param log_event Logging function created by create_logger()
#' @param log_structure Structure logging function for data frame updates
#' 
#' @return Clean data frame with:
#'   \itemize{
#'     \item Non-numeric values removed from numeric-like columns
#'     \item Extreme fold change values (|x| > 10) removed
#'     \item Columns coerced to appropriate numeric types
#'   }
#' 
#' @details 
#' Cleaning Process:
#' \enumerate{
#'   \item Identifies numeric-like columns using keywords:
#'     \itemize{
#'       \item log, fold, pvalue, padj, mean, std, variance
#'       \item count, value, diff, change, ratio, score, rank
#'     }
#'   \item Removes non-numeric values from numeric columns
#'   \item Coerces columns to numeric type
#'   \item Identifies and removes extreme fold changes (|x| > 10)
#'   \item Provides interactive confirmation via shinyalert
#' }
#' 
#' @section Alert Features:
#' The function generates a styled HTML alert showing:
#' \itemize{
#'   \item Non-numeric value removal details
#'   \item Extreme value removal details
#'   \item Summary statistics (initial/final row counts)
#'   \item Warning note about data processing
#'   \item Options to proceed or cancel changes
#' }
#' 
#' @examples
#' \dontrun{
#' # In Shiny server context
#' server <- function(input, output, session) {
#'   log_messages <- reactiveVal("")
#'   logger <- create_logger(session)
#'   structure_logger <- create_structure_logger(session)
#'   
#'   # Example data with issues
#'   df <- data.frame(
#'     gene = c("A", "B", "C", "D"),
#'     log2_fc = c(2, "NA", -1, 15),
#'     pvalue = c(0.01, "invalid", 0.05, 0.03),
#'     stringsAsFactors = FALSE
#'   )
#'   
#'   # Clean data
#'   cleaned_df <- diagnose_and_clean_data(
#'     df = df,
#'     log_messages_rv = log_messages,
#'     log_event = logger,
#'     log_structure = structure_logger
#'   )
#' }
#' }
#' 
#' @section CSS Styling:
#' The alert includes custom CSS classes:
#' \describe{
#'   \item{.cleaning-section}{Main content sections with blue border}
#'   \item{.summary-section}{Summary statistics with gray border}
#'   \item{.warning-note}{Warning message with yellow border}
#'   \item{code}{Inline code formatting}
#' }
#' 
#' @section Logging Messages:
#' The function generates several types of log messages:
#' \describe{
#'   \item{INFO}{Process steps and counts}
#'   \item{WARN}{Detection of suspicious values}
#'   \item{SUCCESS}{User acceptance of changes}
#' }
#' 
#' @note 
#' - Case-insensitive column name matching
#' - Modifies data only with user confirmation
#' - Preserves original data if user cancels
#' - Alert cannot be dismissed without user action
#' - Returns original data frame if no cleaning needed
#' 
#' @importFrom shinyalert shinyalert
#' @importFrom htmltools HTML
#' 
#' @section Dependencies:
#' Required packages:
#' \itemize{
#'   \item shinyalert: For interactive alerts
#'   \item htmltools: For HTML formatting
#' }
#' 
#' #' @section Numeric Column Detection:
#' The function uses a comprehensive keyword system to identify potentially numeric columns:
#' 
#' Primary Keywords (Statistical):
#' \describe{
#'   \item{log}{Identifies log-transformed data (e.g., log2_fc, log10_expr)}
#'   \item{fold}{Captures fold changes (e.g., fold_change, foldChange)}
#'   \item{pvalue}{Detects significance values (e.g., pvalue, p.value, pval)}
#'   \item{padj}{Finds adjusted p-values (e.g., padj, p_adjusted)}
#' }
#' 
#' Statistical Measures:
#' \describe{
#'   \item{mean}{Sample means and averages}
#'   \item{std}{Standard deviations}
#'   \item{variance}{Variance measurements}
#' }
#' 
#' Quantitative Measures:
#' \describe{
#'   \item{count}{Read counts or occurrence frequencies}
#'   \item{value}{Generic numeric values}
#'   \item{diff}{Differences or changes}
#'   \item{change}{Alternative to diff/fold}
#'   \item{ratio}{Proportions and ratios}
#' }
#' 
#' Ranking/Scoring:
#' \describe{
#'   \item{score}{Numerical scores or ratings}
#'   \item{rank}{Ranked values or positions}
#' }
#' 
#' Detection Process:
#' \enumerate{
#'   \item Column name matching:
#'     \itemize{
#'       \item Case-insensitive pattern matching
#'       \item Handles various separator styles (., _, -)
#'       \item Matches partial words (e.g., "log" matches "log2fc")
#'     }
#'   \item Content validation:
#'     \itemize{
#'       \item Checks if values can be coerced to numeric
#'       \item Identifies suspicious non-numeric entries
#'       \item Preserves NA values as valid missing data
#'     }
#'   \item Special cases:
#'     \itemize{
#'       \item Handles scientific notation (e.g., 1e-10)
#'       \item Recognizes common numeric strings ("-1.23")
#'       \item Identifies potentially misformatted numbers
#'     }
#' }
#' 
#' Example Column Matches:
#' ```r
#' # Will be detected as numeric:
#' log2_fold_change
#' Log2FC
#' pvalue_adjusted
#' mean_expression
#' count_normalized
#' diff_score
#' 
#' # Won't be detected as numeric:
#' gene_name
#' sample_id
#' condition
#' ```
#' 
#' @note 
#' Column Detection Details:
#' \itemize{
#'   \item Keywords are applied using regex pattern matching
#'   \item Detection is performed before any data modification
#'   \item False positives are possible but rare due to keyword specificity
#'   \item Column detection results are logged for verification
#'   \item Users can review detected columns via the interactive alert
#' }
#' 
#' Common Edge Cases:
#' \itemize{
#'   \item Mixed numeric/character columns are flagged for cleaning
#'   \item Percentage values with '%' symbols are identified
#'   \item Currency values with symbols are detected
#'   \item Date-like numeric columns are typically excluded
#'   \item Special characters in column names are handled
#' }
#' 
#' 
#' @seealso 
#' \code{\link{create_logger}} for logging functionality
#' \code{\link{create_structure_logger}} for structure logging
#' \code{\link[shinyalert]{shinyalert}} for alert creation
#' 
#' @author Tomasz Stępkowski <dzdzstepel@gmail.com>
#' @export
diagnose_and_clean_data <- function(df, log_messages_rv, log_event, log_structure) {
  # Initialize variables at the start
  cleaned_df <- df
  cleaning_messages <- character(0)
  any_cleaning_needed <- FALSE
  initial_rows <- nrow(df)
  
  # Step 1: Check for numeric-like columns
  keywords_numeric <- c("log", "fold", "pvalue", "padj", "mean", "std", "variance", 
                        "count", "value", "diff", "change", "ratio", "score", "rank")
  
  potential_numeric_cols <- grep(
    paste(keywords_numeric, collapse = "|"), 
    colnames(cleaned_df), 
    value = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(potential_numeric_cols) == 0) {
    log_event(log_messages_rv, "No numeric-like columns detected.", "INFO from diagnose_and_clean_data")
    return(df)
  }
  
  # Phase 1: Handle non-numeric values
  invalid_rows_list <- lapply(potential_numeric_cols, function(col) {
    if (!is.numeric(cleaned_df[[col]])) {
      suspicious_rows <- which(!is.na(df[[col]]) & is.na(suppressWarnings(as.numeric(df[[col]]))))
      if (length(suspicious_rows) > 0) {
        log_event(
          log_messages_rv,
          paste0("Column '", col, "' has ", length(suspicious_rows), " suspicious (non-numeric) entries."),
          "WARN from diagnose_and_clean_data"
        )
        return(list(
          rows = suspicious_rows,
          count = length(suspicious_rows),
          column = col
        ))
      }
    }
    return(NULL)
  })
  
  invalid_rows_list <- Filter(function(x) !is.null(x), invalid_rows_list)
  non_numeric_rows <- unique(unlist(lapply(invalid_rows_list, function(x) x$rows)))
  
  # Create detailed message for non-numeric values
  if (length(invalid_rows_list) > 0) {
    column_details <- sapply(invalid_rows_list, function(x) {
      sprintf("<li><code>%s</code>: %d entries</li>", x$column, x$count)
    })
    non_numeric_message <- sprintf(
      "<div class='cleaning-section'>
       <h4>⚠️Non-numeric values found and removed from columns
       expected to be purely numeric.  

        </h4>
       <p>Total rows affected: %d</p>
       <ul>%s</ul>
       </div>",
      length(non_numeric_rows),
      paste(column_details, collapse = ""))
  }
  
  if (length(non_numeric_rows) > 0) {
    any_cleaning_needed <- TRUE
    log_event(
      log_messages_rv,
      sprintf("Removing %d rows with non-numeric values", length(non_numeric_rows)),
      "INFO from diagnose_and_clean_data"
    )
    cleaned_df <- cleaned_df[-non_numeric_rows, ]
    cleaning_messages <- c(cleaning_messages, non_numeric_message)
    
    # Coerce columns to numeric
    for (col in potential_numeric_cols) {
      if (!is.numeric(cleaned_df[[col]])) {
        cleaned_df[[col]] <- suppressWarnings(as.numeric(cleaned_df[[col]]))
      }
    }
    log_event(log_messages_rv, "Columns coerced to numeric", "INFO from diagnose_and_clean_data")
    log_structure(log_messages_rv, cleaned_df, "Data structure after numeric coercion:", "INFO from diagnose_and_clean_data")
  }
  
  # Phase 2: Check for extreme values
  fold_cols <- grep("log2.*fold|log2.*fc|l2fc|fold.*change", 
                    colnames(cleaned_df), value = TRUE, ignore.case = TRUE)
  
  if (length(fold_cols) > 0) {
    extreme_info <- lapply(fold_cols, function(col) {
      extreme_indices <- which(abs(cleaned_df[[col]]) > 10)
      if (length(extreme_indices) > 0) {
        return(list(
          column = col,
          rows = extreme_indices,
          count = length(extreme_indices)
        ))
      }
      return(NULL)
    })
    
    extreme_info <- Filter(function(x) !is.null(x), extreme_info)
    
    if (length(extreme_info) > 0) {
      any_cleaning_needed <- TRUE
      extreme_rows <- unique(unlist(lapply(extreme_info, function(x) x$rows)))
      
      extreme_details <- sapply(extreme_info, function(x) {
        sprintf("<li><code>%s</code>: %d rows</li>", x$column, x$count)
      })
      
      extreme_message <- sprintf(
        "<div class='cleaning-section'>
         <h4>📊 Potentialy improper extreme log2 fold values found and removed</h4>
         <p>Rows with log<sub>2</sub>fold change > +-10</p>
         <ul>%s</ul>
         </div>",
        paste(extreme_details, collapse = ""))
      
      log_event(
        log_messages_rv,
        paste(extreme_details, collapse = "\n"),
        "INFO from diagnose_and_clean_data"
      )
      
      cleaned_df <- cleaned_df[-extreme_rows, ]
      cleaning_messages <- c(cleaning_messages, extreme_message)
      
      log_structure(log_messages_rv, cleaned_df, 
                    "Data structure after removing extreme values:", 
                    "INFO from diagnose_and_clean_data")
    }
  }
  
  # Show alert only if any cleaning was performed
  if (any_cleaning_needed) {
    final_rows <- nrow(cleaned_df)
    rows_removed <- initial_rows - final_rows
    
    alert_text <- sprintf(
      "<div style='text-align: left; font-family: Arial, sans-serif;'>
        <style>
          .cleaning-section { 
            margin-bottom: 20px; 
            padding: 15px;
            border-left: 3px solid #007bff;
            background: #f8f9fa;
            border-radius: 4px;
          }
          .cleaning-section h4 { 
            color: #007bff; 
            margin: 0 0 10px 0;
            font-size: 1.1em;
          }
          .cleaning-section p {
            margin: 8px 0;
            color: #495057;
          }
          .cleaning-section ul {
            margin: 8px 0;
            padding-left: 20px;
            list-style-type: none;
          }
          .cleaning-section li {
            margin: 4px 0;
            color: #495057;
          }
          .summary-section {
            background: #e9ecef;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
            border-left: 3px solid #6c757d;
          }
          code {
            background: #fff;
            padding: 2px 6px;
            border-radius: 3px;
            color: #d63384;
            font-size: 0.9em;
            border: 1px solid #dee2e6;
          }
          .warning-note {
            margin-top: 20px;
            padding: 15px;
            border-left: 3px solid #ffc107;
            background: #fff3cd;
            color: #856404;
            font-size: 0.95em;
            line-height: 1.4;
            border-radius: 4px;
          }
        </style>
        
        %s
        
        <div class='summary-section'>
          <strong>📈 Summary:</strong><br>
          Initial rows: %d<br>
          Rows removed: %d<br>
          Final rows: %d
        </div>

        <div class='warning-note'>
          Automatic data processing is performed due to potentially improper data format in part of the input, 
          cancel to check the data input settings (separator and decimal point) or to check your data, 
          proceed to continue with automatically cleaned data.
        </div>
      </div>",
      paste(cleaning_messages, collapse = "\n"),
      initial_rows,
      rows_removed,
      final_rows
    )
    
    shinyalert::shinyalert(
      title = "🔍Improper input structure, data processing triggered",
      text = alert_text,
      type = "info",
      html = TRUE,
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Proceed",
      callbackR = function(user_proceeded) {
        if (isTRUE(user_proceeded)) {
          log_event(log_messages_rv, "User accepted data cleaning changes", "SUCCESS from diagnose_and_clean_data")
          return(cleaned_df)
        } else {
          log_event(log_messages_rv, "User cancelled data cleaning", "INFO from diagnose_and_clean_data")
          return(df)
        }
      }
    )
  }
  
  return(cleaned_df)
}
