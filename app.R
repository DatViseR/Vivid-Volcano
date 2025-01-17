


####################### LIBRARY SETUP ############################

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(ggrepel)
library(arrow)
library(DT)
library(plotly)
library(Cairo)
library(gt)
library(shiny.semantic)
library(semantic.dashboard)
library(gridExtra)
library(webshot2)
library(shinyalert)  
library(tidyr)

# Loading the GO data once globally
# The preparation of this file is described in https://github.com/DatViseR/Vivid-GO-data and in the script
# Parquet_GO_source_data_preparation_script.R
# The file is also available in the data folder of this repository
# This newfile contains around 8000 non-obsolete unique GO categories with at least 6 annotated genes in the category
GO <- arrow::read_parquet("GO.parquet2")




##################---CRUCIAL CUSTOM FUNCTION DEFINITIONS---- ###########################

# The logger factory function that creates a logger function for a specific session  

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

# The structure logging helper function - it can be used to log the structure of any R object or modified to 
# log other properties of the object
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

calculate_go_enrichment <- function(genes, go_categories, go_data, log_messages_rv, log_event) {
  # Initial gene processing logging
  log_event(log_messages_rv, 
            sprintf("Starting gene preprocessing with %d raw entries", length(unlist(genes))),
            "INFO from calculate_go_enrichment()")
  
  # Clean gene names
  genes <- unlist(strsplit(genes, "[;|,\\s]+"))  
  genes <- trimws(genes)                         
  genes <- genes[genes != ""]                    
  genes <- toupper(genes)                        
  genes <- gsub("[^A-Z0-9]", "", genes)          
  genes <- unique(genes)                         
  
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


# Creates a GO enrichment table for upregulated, downregulated, and all regulated genes based on the input data frame

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

# Creates a publication-ready plot for pdf outputs with custom dimensions and text sizes based on the input base plot and 
# defined output

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

# Creates a GT table with color-coded row groups for regulated genes based on the input enrichment results list 

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
    select(-Population_Size) %>%  # Remove "Number of human genes" column
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
      Population_Enrichment_Ratio = "Genomic enrichment",
      Subpopulation_Enrichment_Ratio = "Regulated genes enrichment",
      P_Value = "Hypergeometric test p-value",
      Adjusted_P_Value = "Bonferroni adj-p value",
      Success_Population_Size = "Genes in GO category",
      Sample_Size = "Number of regulated genes",
      Sample_Success_Size = "Regulated genes in GO category"
    ) %>%
    # Add footnote for Adjusted_P_Value column
    tab_footnote(
      footnote = "Bonferroni correction based on the estimated number of level 4 hierarchy GO tags n=1160",
      locations = cells_column_labels("Adjusted_P_Value")
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

#This function creates GT tables for each GO category with columns for genes in the GO category,
#downregulated genes, and upregulated genes, with detected genes in bold and regulated in color.

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
    toupper() %>%
    gsub("[^A-Z0-9]", "", .) %>%
    unique() %>%
    .[. != ""]
  
  regulated_genes <- df %>%  # All significantly regulated genes
    filter(adjusted_pvalues < alpha) %>%
    pull(!!sym(annotation_col)) %>%
    toupper() %>%
    gsub("[^A-Z0-9]", "", .) %>%
    unique()
  
  downregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% 
    pull(!!sym(annotation_col)) %>%
    toupper() %>%
    gsub("[^A-Z0-9]", "", .) %>%
    unique()
  
  upregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% 
    pull(!!sym(annotation_col)) %>%
    toupper() %>%
    gsub("[^A-Z0-9]", "", .) %>%
    unique()
  
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
      toupper() %>%
      gsub("[^A-Z0-9]", "", .) %>%
      unique()
    
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
      All_Genes = "All Genes (detected in bold, detected and regulated in color)",
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






# Checks p-values in a data frame column to find out if they are logged or not and unlogs them if necessary

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

# data preprocesing function detecting non-numeric values in columns supposed to be numeric 
# the function analyze observations with suspicious values - logs the summary of those values 
# and remove the rows tha may result in coercion to character values instead of numeric 
#' Diagnose and trim numeric-like columns, then display a summary alert.
#'
#' This function scans columns in a dataframe for non-numeric values in columns
#' that are likely meant to be numeric. If suspicious entries are found, a summary
#' alert is displayed using shinyalert. When the user clicks "Continue", those rows
#' with non-numeric entries are removed, and the final trimmed dataframe is

diagnose_dataframe_v4 <- function(log_messages_rv, log_event, uploaded_df) {
  # Get the current dataframe from the reactive value
  df <- uploaded_df()
  
  # Define keywords for identifying numeric-like columns
  keywords <- c("log", "fold", "pvalue", "padj", "mean", "std", "variance", 
                "count", "value", "diff", "change", "ratio", "score", "rank")
  
  potential_numeric_cols <- grep(
    paste(keywords, collapse = "|"), 
    colnames(df), 
    value = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(potential_numeric_cols) == 0) {
    log_event(log_messages_rv, "No numeric-like columns detected.", "INFO from diagnose_dataframe_v4")
    return(invisible(NULL))
  }
  
  # Modified to store both rows and counts
  invalid_rows_list <- lapply(potential_numeric_cols, function(col) {
    suspicious_rows <- which(!is.na(df[[col]]) & is.na(suppressWarnings(as.numeric(df[[col]]))))
    if (length(suspicious_rows) > 0) {
      log_event(
        log_messages_rv,
        paste0("Column '", col, "' has ", length(suspicious_rows), " suspicious (non-numeric) entries."),
        "WARN from diagnose_dataframe_v4"
      )
      list(
        rows = suspicious_rows,
        count = length(suspicious_rows)
      )
    } else {
      NULL
    }
  })
  names(invalid_rows_list) <- potential_numeric_cols
  
  # Filter out NULL entries and get all suspicious rows
  valid_entries <- !sapply(invalid_rows_list, is.null)
  problematic_cols <- names(invalid_rows_list)[valid_entries]
  invalid_rows_list <- invalid_rows_list[valid_entries]
  
  all_suspicious_rows <- unique(unlist(lapply(invalid_rows_list, function(x) x$rows)))
  
  if (length(all_suspicious_rows) == 0) {
    log_event(log_messages_rv, "No suspicious non-numeric and not NA entries found in numeric-like columns.", "INFO from diagnose_dataframe_v4")
    return(invisible(NULL))
  }
  
  # Create detailed message for each problematic column
  column_messages <- sapply(seq_along(problematic_cols), function(i) {
    sprintf("• %s suspicious non-numeric and not NA values found in column '%s' that is supposed to be numeric",
            invalid_rows_list[[i]]$count,
            problematic_cols[i])
  })
  
  # Build the complete alert text
  alert_text <- paste0(
    "<div style='text-align: left;'>",
    "<strong>Data Processing Warning:</strong><br/><br/>",
    paste(column_messages, collapse = "<br/>"),
    "<br/><br/>",
    "Total rows to be removed: ", length(all_suspicious_rows),
    "<br/><br/>",
    "Click 'Proceed' to remove these rows and convert columns to numeric format.",
    "</div>"
  )
  
  shinyalert::shinyalert(
    title = "Suspicious Values Detected",
    text = HTML(alert_text),
    type = "warning",
    html = TRUE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "Proceed",
    callbackR = function(user_proceeded) {
      if (isTRUE(user_proceeded)) {
        log_event(
          log_messages_rv,
          paste0("User proceeded. Removing ", length(all_suspicious_rows), " suspicious rows."),
          "INFO from diagnose_dataframe_v4"
        )
        
        # Create cleaned version of the dataframe
        cleaned_df <- df[-all_suspicious_rows, ]
        
        # Coerce problematic columns to numeric
        for (col_name in potential_numeric_cols) {
          cleaned_df[[col_name]] <- suppressWarnings(as.numeric(cleaned_df[[col_name]]))
        }
        
        # Update the reactive value with cleaned data
        uploaded_df(cleaned_df)
      }
    }
  )
  
  return(invisible(NULL))
}

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


# one diagnostic and cleaning function that

# Checks for columns that should be numeric but have suspicious non numeric and not NA values - log event

# If there are such values in columns supposed to be numeric remove those observations - log event

# Coerce those columns to numeric - log event and structure of df after step

# Search for column with log2 fold

# remove  values > abs(10) - log event

# log structure after second cleaning step

# show nicely formated shiny alert with information on the performed data processing and information to the user

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









################################### ----UI---#################################


ui <- semanticPage(

  # Includes custom CSS
  tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css", 
              href = paste0("custom.css?v=", Sys.time())),

  tags$script(HTML(
    "
    // Send the current client width to the server
    $(document).on('shiny:connected', function() {
      Shiny.setInputValue('clientWidth', window.innerWidth);
    });
    $(window).on('resize', function() {
      Shiny.setInputValue('clientWidth', window.innerWidth);
    });
    "
  ))
    
    
  ),
  
  segment(class = "navbar",
        img(src = "Vivid_volcano_logo.png", alt = "Logo", class = "logo"),
      div(class = "left-section",
          h1("Vivid Volcano", class = "title"),
          h4("Publication-ready volcano plots and GO analysis with ease", class = "subtitle")
      ),
      div(class = "social-buttons",
          h5("Source Code", class = "icon-header"),  
          a(href = "https://github.com/DatViseR",
            icon("github big"),
            class = "github",
            target = "_blank"
          ),
          h5("Developer", class = "icon-header"),
          a(href = "https://www.linkedin.com/in/tomasz-st%C4%99pkowski/",
            icon("linkedin big"),
            class = "linkedin",
            target = "_blank"
          ),
          h5("Buy me a coffee?", class = "icon-header"),
          a(href = "https://buymeacoffee.com/datviser",
            icon("fa-solid fa-mug-hot"),
            class = "coffe",
            target = "_blank"
          ),
          
      )
  ),
  
  div(class = "ui fluid container",
      # Main layout using sidebar
      sidebar_layout(
        # Sidebar panel with controls
        sidebar_panel(
                      width = 3,
                      # Data Upload Card
                      div(class = "ui raised segment",
                          header(title = "Upload your data", description = "", icon = "upload"),
                          div(class = "ui grey ribbon label", "Upload a CSV or TSV file"),
                          div(class = "ui file input", 
                              file_input("file1", label = NULL, accept = c(".csv", ".tsv"))
                          ),
                          
                          # Add download link for demo data
                          tags$a(
                            href = "demo_data.csv",  # This will be served from www/demo_data.csv
                            download = NA,
                            class = "ui small labeled icon button",
                            tags$i(class = "download icon"),
                            "Download tab separated demo data for upload"
                          ),
                          # Form layout for checkbox and radio buttons
                          div(class = "ui form",
                              div(class = "three fields",
                                  # Header Checkbox
                                  div(class = "field",
                                      div(style = "display: flex; flex-direction: column; gap: 5px;",
                                          div(style = "font-weight: bold;", "Header"),  # Styled label
                                          toggle("header", "", is_marked = TRUE)
                                      )
                                  ),
                                  # Separator Radio Buttons
                                  div(class = "field",
                                      multiple_radio(class = "radio", "sep", "Separator", 
                                                     choices = list("Comma" , 
                                                                    "Semicolon", 
                                                                    "Tab"), 
                                                     choices_value = c(",", ";", "\t"),
                                                     selected = ",")
                                  ),
                                  # Decimal Point Radio Buttons
                                  div(class = "field",
                                      multiple_radio("dec", "Decimal Point", 
                                                     choices = list("Dot" , 
                                                                    "Comma")
                                                     , choices_value = c(".", ","),
                                                     selected = ".")
                                  )
                              )
                          ),
                          
                          actionButton("upload", label = HTML('<i class="upload icon"></i> Upload'), 
                                       class = "ui primary button")
                      ),
                      
                      
                      uiOutput("column_select_ui"),
                      
                      
                      
                      # Analysis Options Card
                      div( class = "ui raised segment",
                           # ribbon
                           header(title = "Analysis Options", description = "Customize volcano plot",icon = "cogs"),
                           div(class = "ui grey ribbon label", "Customize p value adjustment"),
                           
                           dropdown_input("adj",
                                          choices = c("None",
                                                      "Bonferroni",
                                                      "Hochberg",
                                                      "Benjamini-Hochberg",
                                                      "Benjamini-Yekutieli"),
                                          
                                          choices_value = c("none", "bonferroni", "hochberg", "BH", "BY"),
                                          value = "BH"),
                           numericInput("alpha", "Significance Threshold", value = 0.05, min = 0.0001, max = 1, step = 0.001),
                           
                           # Plot Options Card
                           div(class = "ui grey ribbon label", "Customize annotations"),
                           toggle("color_highlight", "Highlight Significant Hits", FALSE),
                           uiOutput("color_highlight_ui"),
                           toggle("show_go_category", "Visualize GO Categories", FALSE),
                           uiOutput("go_category_ui"),
                           uiOutput("color_picker_ui"),
                           numericInput("num_labels", "Number of Gene Labels (0-100)", 
                                        value = 10, min = 0, max = 100),
                           toggle("trim_gene_names", "Trim Multiplied Gene Names to First Occurrence", TRUE),
                           toggle("select_custom_labels", "Label your choosen genes", FALSE),
                           uiOutput("custom_gene_labels_ui"),
                           div(class = "ui grey ribbon label", "Customize plot title"),
                           textInput("plot_title", "", "Vivid Volcano"),
                           div(class = "ui grey ribbon label", "Customize X -axis label"),
                           textInput("x_axis_label", "", 
                                     "Log2 Fold Change (Condition X vs. Condition Y)"),
                           actionButton("draw_volcano", "Draw Volcano Plot", 
                                        class = "ui primary button", 
                                        icon = icon("chart line icon")),
                           uiOutput("download_log_ui")
                      ),
        ),
        
        main_panel(
          width = 12,
          # Dataset preview
          segment(
            class = "raised",
            div(class = "ui grey ribbon label", "Dataset Preview"),
            semantic_DTOutput("dataset_summary", height = "auto")
          ),
          segment(
            class = "placeholder",
            header(title = "Results", description = "", icon = "fa-solid fa-square-poll-vertical"),
            
            tabset(
              tabs = list(
                list(
                  menu = "Static Volcano Plot and GO enrichment table",
                  content = div(
                    div(class = "ui two column grid",
                        # First column (50%) - Plot and Downloads
                        div(class = "column",
                            segment(
                              class = "basic",
                              plotOutput("volcano_plot", width = "100%", height = "600px")
                            ),
                            segment(
                              class = "basic",
                              h4(class = "ui header", "Download Plots"),
                              div(
                                class = "ui tiny fluid buttons",
                                downloadButton("download_plot1", "85x85mm (1 col)", class = "ui button"),
                                downloadButton("download_plot2", "114x114mm (1.5 col)", class = "ui button"),
                                downloadButton("download_plot3", "114x65mm (landscape)", class = "ui button")
                              ),
                              div(
                                style = "margin-top: 10px;",
                                class = "ui tiny fluid buttons",
                                downloadButton("download_plot4", "174x174mm (square)", class = "ui button"),
                                downloadButton("download_plot5", "174x98mm (landscape)", class = "ui button")
                              )
                            )
                        ),
                        # Second column (50%) - GO Table
                        div(class = "column",
                            segment(
                              class = "basic",
                              h4(class = "ui header", "Download GO Enrichment Table"),
                              div(
                                class = "ui tiny fluid buttons",
                                downloadButton("download_go_enrichment", "Download GO enrichment table", class = "ui button")
                              ),
                              gt_output("go_enrichment_gt")
                            )
                        )
                    )
                  )
                ),
                list(
                  menu = "Interactive Volcano Plot",
                  content = div(
                    plotlyOutput("volcano_plotly", width = "800px", height = "740px")
                  )
                ),
                list(
                  menu = "GO Category Details",
                  content = div(
                    segment(
                      class = "basic",
                      h4(class = "ui header", "Download GO Gene List Table"),
                      div(
                        class = "ui tiny fluid buttons",
                        downloadButton("download_go_gene_list", "Download GO gene lists", class = "ui button")
                      ),
                      gt_output("go_gene_list_gt")
                    )
                  )
                )        
              )
            )
          ),
      
        )
      )
  )
)



##########################-----SERVER----####################################


server <- function(input, output, session) {
  uploaded_df <- reactiveVal()
  volcano_plot_rv <- reactiveVal()
  log_messages <- reactiveVal("")
  is_mobile <- reactiveVal(FALSE)
  
# Creates session variables at server start
  session_id <- substr(digest::digest(session$token), 1, 6)
  session_start_time <- Sys.time()
  
  # Creates the logging functions with session context
  log_event <- create_logger(session)
  log_structure <- create_structure_logger(session)
  
  
  
  
  # Immediate logging of initial display state
  observeEvent(input$clientWidth, {
    req(input$clientWidth)  # Ensure the value is available
    current_is_mobile <- input$clientWidth <= 800
    is_mobile(current_is_mobile)
    log_event(log_messages, 
              sprintf("Browser window size: %dpx (%s view)", 
                      input$clientWidth,
                      if(current_is_mobile) "MOBILE" else "DESKTOP"), 
              "INFO display initialization")
  }, once = TRUE)
  
  # Monitor for changes in window size
  observeEvent(input$clientWidth, {
    req(input$clientWidth)
    current_is_mobile <- input$clientWidth <= 800
    previous_is_mobile <- isolate(is_mobile())
    if (current_is_mobile != previous_is_mobile) {
      is_mobile(current_is_mobile)
      log_event(log_messages, 
                sprintf("Display changed to %s view (width: %dpx)", 
                        if(current_is_mobile) "MOBILE" else "DESKTOP",
                        input$clientWidth), 
                "INFO display change")
    }
  })
  
 
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    df <- read_delim(in_file$datapath, delim = input$sep, col_names = input$header, locale = locale(decimal_mark = input$dec))
    uploaded_df(df)
    # create log event for successful initialization of reactive values
    
    log_event(log_messages, "Reactive value uploaded_df initialized successfully", "INFO from upload observer")   
    
    # Log the structure of the uploaded dataset
    log_structure(log_messages, df, "The structure of the uploaded dataset is:", "INFO from upload observer")
    log_event(log_messages, "Dataset uploaded successfully", "SUCCESS from upload observer")
 
       
    # Call diagnose_dataframe_v4 with just the reactive value
    # Run combined diagnostic and cleaning
    df_cleaned <- diagnose_and_clean_data(
      df = df,
      log_messages_rv = log_messages,
      log_event = log_event,
      log_structure = log_structure
    )
    
    # Update if cleaning returns result
    if (!is.null(df_cleaned)) {
      uploaded_df(df_cleaned)
    }
    
   log_structure(log_messages, df, "The structure of the uploaded dataset after 1st diagnostic preprocesing:", "INFO from upload observer")
    

    output$column_select_ui <- renderUI({
      if (is.null(df)) return(NULL)
      # Log event to indicate that the UI has been rendered
      log_event(log_messages, "Reactive UI for column selection rendered", "INFO from output$column_select_ui")
      div(class = "ui raised segment",
          div(class = "ui grey ribbon label", "Select Data"),
          selectInput("pvalue_col", "Select p-value column", choices = names(df)),
          selectInput("fold_col", "Select regulation column - log2(fold)", choices = names(df)),
          selectInput("annotation_col", "Select human gene symbols column", choices = names(df))
      )
    })
    
 
    
    output$dataset_summary <- renderDT({
      log_event(log_messages, "Rendering dataset summary table", "INFO from output$dataset_summary")
      
      table <- semantic_DT(
        data.frame(df, check.names = FALSE), # Convert to data.frame if not already
        options = list(
          responsive = TRUE,
          pageLength = 3,
          dom = 'lftp',
          lengthMenu = list(c(1, 3, 5, 10), c('1','3', '5', '10')),
          rownames = FALSE,
          scrollX = TRUE,
          columnDefs = list(list(
            targets = '_all',  # Apply to all columns
            className = 'dt-nowrap'  # Add nowrap class
          ))
        ),
        style = "semanticui",
        class = "ui small compact table",
        selection = 'none'  # Disable row selection if needed
      )
      
      if (!is.null(table)) {
        log_event(log_messages, "Dataset summary table created successfully", "INFO from output$dataset_summary")
        # check the structure of the table  
        log_structure(log_messages, table, "The structure of the dataset summary table is:\n")
      } else {
        log_event(log_messages, "Failed to create dataset summary table", "ERROR from output$dataset_summary")
      }
      
      table
    })
      
  })
  
  # Observe changes to the color_highlight input
  observeEvent(input$color_highlight, {
    if (input$color_highlight) {
      log_event(log_messages, "Color highlighting enabled", "INFO from input$color_highlight")
    } else {
      log_event(log_messages, "Color highlighting disabled", "INFO from input$color_highlight")
    }
  })
  
  # Dynamic UI for color highlight options
  output$color_highlight_ui <- renderUI({
    if (input$color_highlight) {
      tagList(
        colourInput("up_color", "Up-regulated color", value = "#FF7081"),
        colourInput("down_color", "Down-regulated color", value = "#7973FA")
      )
    }
  })
  
  # Observe changes to the show_go_category input
  observeEvent(input$show_go_category, {
    if (input$show_go_category) {
      log_event(log_messages, "GO categories selection enabled", "INFO from input$show_go_category")
    } else {
      log_event(log_messages, "GO categories selection disabled", "INFO from input$show_go_category")
    }
  })
  
  # Render the UI based on the toggle
  output$go_category_ui <- renderUI({
    if (input$show_go_category) {
      selectizeInput("go_category", "Select from ~8000 unique GO categories", choices = NULL, multiple = TRUE)
    }
  })
  
  observe({
    if (input$show_go_category) {
      log_event(log_messages, "GO categories updated", "INFO from input$show_go_category")
      updateSelectizeInput(session, "go_category", choices = unique(GO$name), server = TRUE)
      
    }
  })
  
  # Reactive expression to track chosen GO categories
  chosen_go <- reactive({
    log_event(log_messages, "Reactive to track choosen GO categories initialized - input should be null", "INFO")
    input$go_category
    
  })
  
  
  color_palette <- c("#440154FF", "darkblue","gold","darkorange","darkcyan","deeppink","black") 
  
  
  
  # Dynamic UI for additional color pickers
  output$color_picker_ui <- renderUI({
    if (!input$show_go_category) {
      log_event(log_messages, "Color pickers for GO categories are disabled", "INFO from output$color_picker_ui")
      return(NULL)
    }
    
    req(chosen_go())
    chosen <- chosen_go()
    cat("Chosen GO categories: ", paste(chosen, collapse = ", "), "\n")  # Debug statement
    log_event(log_messages, "Creating color inputs for GO categories", "INFO from output$color_picker_ui")
    
    # Iterate over indices, not values, to correctly access both `chosen[i]` and `color_palette[i]`
    color_inputs <- lapply(seq_along(chosen), function(i) {
      go <- chosen[i]
      sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go)  # Sanitize ID
      color_value <- color_palette[(i - 1) %% length(color_palette) + 1]  # Cycle through colors using index `i`
      cat("Creating color input for: ", go, " with ID: ", sanitized_id, " and color: ", color_value, "\n")  # Debug statement
      
      colourInput(paste0("color_", sanitized_id), paste("Color for", go), value = color_value)
    })
    
    do.call(tagList, color_inputs)
  })
  
  observe({
    print(str(chosen_go()))
    log_structure(log_messages, input$go_category, "Chosen GO categories modified:") 
  
  })
  
  
  # Observe changes to the select_custom_labels input
  observeEvent(input$select_custom_labels, {
    state <- if (input$select_custom_labels) "ENABLED" else "DISABLED"
    log_event(log_messages, paste("Custom gene labels selection", state), "INFO input$select_custom_labels")
  })
  
  # First, create the UI with empty choices
  output$custom_gene_labels_ui <- renderUI({
    if (input$select_custom_labels) {
      req(uploaded_df(), input$annotation_col)
      selectizeInput(
        "custom_gene_labels", 
        "Select gene names to label", 
        choices = NULL,  # Start with no choices
        multiple = TRUE
      )
    }
  })
  
  # Then, update it with server-side processing - this server side processing was suggested by R console warning message that
  # appeared in the previous version with client side processing and no updataSelectizeInput function
  observe({
    req(input$select_custom_labels, uploaded_df(), input$annotation_col)
    gene_names <- unique(uploaded_df()[[input$annotation_col]])
    updateSelectizeInput(
      session,
      "custom_gene_labels",
      choices = gene_names,
      server = TRUE  
    )
  })
  
  # Reactive expression to track chosen custom gene labels
  custom_genes <- reactive({
    log_event(log_messages, "Reactive to track choosen custom gene labels initialized or modified", "INFO")
    input$custom_gene_labels
  })
  
  
  observeEvent(input$draw_volcano, {
    req(uploaded_df(), input$pvalue_col, input$fold_col, input$annotation_col, input$adj)
    
    # Get the original data
    df <- uploaded_df()
    original_rows <- nrow(df)
    
    # Log the initial state
    log_event(log_messages, sprintf("Initial number of rows: %d", original_rows), "INFO")
    
    # Add error checking for NA values before dropping
    na_counts <- sapply(df[c(input$pvalue_col, input$fold_col, input$annotation_col)], function(x) sum(is.na(x)))
    log_event(log_messages, sprintf("NA counts in columns before filtering:\n%s", 
                                    paste(names(na_counts), na_counts, sep = ": ", collapse = "\n")), "INFO")
    
    # Drop NA values and capture the new data frame
    df <- df %>% drop_na(!!sym(input$pvalue_col), !!sym(input$fold_col), !!sym(input$annotation_col))
    new_rows <- nrow(df)
    
    # Calculate and log the difference
    dropped_rows <- original_rows - new_rows
    if (dropped_rows > 0) {
      log_event(log_messages, 
                sprintf("Removed %d rows with missing values (%d -> %d rows)", 
                        dropped_rows, original_rows, new_rows), "INFO")
    } else {
      log_event(log_messages, "No rows were removed - no missing values found", "INFO")
    }
    
    # Update the reactive value
    uploaded_df(df)
    
    # Create alert message
    alert_message <- HTML(sprintf(
      "<div style='text-align: left;'>
        <strong>Data Processing Summary:</strong><br><br>
        Initial number of rows: %d<br><br>
        <strong>Missing values found:</strong><br>
        %s<br>
        <strong>Rows removed:</strong> %d<br>
        <strong>Remaining rows:</strong> %d
        </div>",
      original_rows,
      paste(names(na_counts), na_counts, sep = ": ", collapse = "<br>"),
      dropped_rows,
      new_rows
    ))
    
    # Show alert with the information
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
    
    
   
    log_event(log_messages, "Starting volcano plot generation", "INFO input$draw_volcano")
    log_structure(log_messages, df, "The structure of the uploaded_df before creating volcano plot is:\n","INFO")
    
    # Checks if input$pvalue_col and input$fold_col are numeric and breaks the code with a warning allert to the user if not 
    # informs the user to double check the decimal point in the upload section - the wrong decimal point resulted in coercing
    # the vectors to character instead of numeric
    
    if (!is.numeric(df[[input$pvalue_col]]) || !is.numeric(df[[input$fold_col]])) {
      log_event(log_messages, "Error: p-value and fold change columns are not numeric", "ERROR - draw volcano observer checks")
      shinyalert(
        title = "Data Processing Error",
        text = "The p-value and fold change columns must be numeric. Please double-check the decimal point in the upload section",
        type = "error",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        timer = 0
      )
      return(NULL)
    }
    
    
    # Check and unlog p-values
    df <- check_and_unlog_pvalues(df, input$pvalue_col, log_messages, log_event)
    uploaded_df(df)  # Update the reactive value with unlogged p-values
    log_structure(log_messages, df, "The structure of the uploaded dataset after unlogging p-values is:", "INFO pvalues module")
    
    # Adjust p-values
    pvalues <- df[[input$pvalue_col]]
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    log_event(log_messages, paste(input$adj, "method choosen for p-value adjustment", "INFO adjusting pvalues"))
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)  # Ensure reactive value is updated
    log_event(log_messages, "Unlogging and adjusting p-values completed", "SUCCESS")
    log_structure(log_messages, df, "The structure of the uploaded dataset after adjusting p-values is:", "INFO")
    
   
    
    
    
    # Only perform GO enrichment calculations if the toggle is on
    # GO enrichment and gene lists section
    if (input$show_go_category && length(input$go_category) > 0) {
     
       # Calculate enrichment results
      enrichment_results_list <- calculate_go_enrichment_table(
        df = df,
        annotation_col = input$annotation_col,
        go_categories = input$go_category,
        go_data = GO,
        alpha = input$alpha,
        fold_col = input$fold_col,
        log_messages_rv = log_messages, 
        log_event = log_event,
        log_structure = log_structure
      )
      
      # Render GO gene list table
      output$go_gene_list_gt <- render_gt({
        # First check if color highlighting is enabled
        req(input$color_highlight)
        log_event(log_messages, "Rendering GO gene list table", "INFO from output$go_gene_list_gt")
        
        # Get the colors from the proper inputs when color highlighting is enabled
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        
        # Debug the actual colors being passed
        cat("\n==== Color Values Being Passed ====\n")
        cat("Down-regulated color:", colors_to_use[1], "\n")
        cat("Up-regulated color:", colors_to_use[2], "\n")
        cat("================================\n\n")
        
        build_gt_gene_lists(
          df = df,  # Use the actual df from the parent scope
          annotation_col = input$annotation_col,
          chosen_go = input$go_category,
          go_data = GO,
          alpha = input$alpha,
          fold_col = input$fold_col,
          color_highlight = colors_to_use,
          log_messages_rv = log_messages, 
          log_event = log_event
        )
      })
      
      # Render GO enrichment table
      output$go_enrichment_gt <- render_gt({
        req(enrichment_results_list)
        
        # Get the colors from the proper inputs when color highlighting is enabled
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        
        
        build_gt_table(
          enrichment_results_list,
          upregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
          downregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0)),
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
      })
    } else {
      # Handle both outputs in the else block
      log_event(log_messages, "GO categories not selected or no categories chosen", "INFO output$go_enrichment_gt")
      output$go_gene_list_gt <- render_gt({
        gt(data.frame(Message = if(!input$show_go_category) {
          "Enable GO category visualization to see results"
        } else {
          "Select at least one GO category to see results"
        }))
      })
      
      output$go_enrichment_gt <- render_gt({
        log_event(log_messages, "GO categories not selected or no categories chosen", "INFO output$go_enrichment_gt")
        gt(data.frame(Message = if(!input$show_go_category) {
          "Enable GO category visualization to see results"
        } else {
          "Select at least one GO category to see results"
        }))
      })
    }
 
    
    # Draw the volcano plot
    if (!"adjusted_pvalues" %in% names(df)) {
      print("Error: adjusted_pvalues column is missing.")
      return(NULL)
    }
    
    # limits for y slighly bigger to have space for annotation
    limits_y <- c(0, max(-log10(as.numeric(df[[input$pvalue_col]])) + 1))
    
    abs_min <- min(abs(df[[input$fold_col]]), na.rm = TRUE)
    abs_max <- max(abs(df[[input$fold_col]]), na.rm = TRUE)
    limit_for_x_scale <- ifelse(abs_max > abs_min, abs_max, abs_min)
    cat(paste("Limits for x scale are:", limit_for_x_scale, "\n"))
   
    log_event(log_messages, "Creating volcano plot", "INFO input$draw_volcano")
    volcano_plot <- ggplot(df, aes(x = round(!!sym(input$fold_col), 4), 
                                   y = -log10(!!sym(input$pvalue_col)),
                                   text = paste("Gene:", !!sym(input$annotation_col),
                                                "\nP-value:", round(!!sym(input$pvalue_col), 4),
                                                "<br>log2 Fold Change:", round(!!sym(input$fold_col), 3),
                                                "<br>Adjusted P-value:", round(adjusted_pvalues, 4)))) +
                                                
      geom_point(size = 1.8, alpha = 0.5, color = "gray70") +
      theme_classic() +
      labs(title = input$plot_title, x = input$x_axis_label, y = "-Log10 P-Value") +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            aspect.ratio = 0.75,
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 16, color = "black", face = "bold"),
            axis.text = element_text(size = 14, color = "black", face = "bold"),
             ) +  
      geom_hline(yintercept = -log10(input$alpha), linetype = "dashed", color = "red") +
      scale_x_continuous(
        limits = c(-limit_for_x_scale, limit_for_x_scale)
      ) +
      scale_y_continuous(
        limits = limits_y
      )
      
   
    # Generate subtitle based on the input settings
    subtitle <- NULL
    
    if (input$color_highlight) {
      log_event(log_messages, "Color highlighting enabled", "INFO input$draw_volcano")
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      volcano_plot <- volcano_plot +
        annotate("text", x = -Inf, y = Inf, label = paste0("Upregulated n= ", upregulated_count), color = input$up_color, hjust = -0.1 ,vjust = 2, size = 5.5 ) +
        annotate("text", x = -Inf, y = Inf, label = paste0("Downregulated n= ", downregulated_count), color = input$down_color, hjust = -0.1, vjust = 1, size = 5.5)
      # Detailed debug analysis of input$color_highlight
      cat("\n==== Color Input Debug Analysis ====\n")
      cat("Current Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("\n1. Basic Information:\n")
      cat("- Is input$color_highlight NULL?:", is.null(input$color_highlight), "\n")
      cat("- Class of input$color_highlight:", class(input$color_highlight), "\n")
      cat("- Length of input$color_highlight:", length(input$color_highlight), "\n")
      
      cat("\n2. Value Details:\n")
      cat("- Raw value of input$color_highlight:\n")
      print(input$color_highlight)  # Using print() for structured output
      
      cat("\n3. Individual Elements:\n")
      cat("- First element:", input$color_highlight[1], 
          " (class:", class(input$color_highlight[1]), ")\n")
      cat("- Second element:", input$color_highlight[2],
          " (class:", class(input$color_highlight[2]), ")\n")
      
      cat("\n4. Structure Information:\n")
      str(input$color_highlight)
      
      cat("\n================================\n\n")
      
          }
    
    # Add annotations for chosen GO categories
    if (input$show_go_category) {
      chosen <- chosen_go()
      selected_GO <- GO %>% filter(name %in% chosen)
      
      if ("id" %in% colnames(selected_GO)) {
        go_details <- paste0(paste(chosen, unique(selected_GO$id), collapse = "\n"))
      } else {
        go_details <- paste0("GO: ", paste(chosen, collapse = ", "), "\nID: Not available")
        cat("Warning: 'id' column not found in selected_GO\n")  
        cat(go_details)  
      }
      
      for (i in seq_along(chosen)) {
        go <- chosen[i]
        color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
        go_detail <- paste0(go, ": ", unique(selected_GO$id[selected_GO$name == go]))
        volcano_plot <- volcano_plot +
          annotate("text", x = Inf, y = Inf, label = go_detail, color = color, hjust = 1.1, vjust = 1 + i*1.2, size = 5.5)
      }
    }  
     
    # Add subtitle to the plot
    if (!is.null(subtitle)) {
      volcano_plot <- volcano_plot + labs(subtitle = subtitle)
    }
  
    
    if (input$color_highlight) {
      log_event(log_messages, "Adding color point layers from color_highlight input", "INFO input$draw_volcano")
      volcano_plot <- volcano_plot +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0), aes(color = "Up"), size = 2, color = input$up_color, alpha = 0.5) +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0), aes(color = "Down"), size = 2, color = input$down_color, alpha = 0.5)
    }
    
    # Highlighting genes belonging to chosen GO categories
    if (!is.null(chosen_go())) {
      log_event(log_messages, "Coloring genes belonging to chosen GO categories", "INFO input$draw_volcano")
      selected_GO <- GO %>% filter(name %in% chosen_go())
      for (go in chosen_go()) {
        color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
        genes <- selected_GO %>% filter(name == go) %>% pull(gene)
        
        # Convert all gene names to uppercase in one step
        genes <- toupper(genes)
        
        # print a message if any genes were modified
        non_human_genes <- genes[genes != toupper(genes)]
        if (length(non_human_genes) > 0) {
          cat(sprintf("\n %d non-human genes detected - converted to uppercase\n", 
                      length(non_human_genes)))
          cat("\n the following genes were converted to uppercase: \n")
          print(non_human_genes)
        }
        
        volcano_plot <- volcano_plot +
          geom_point(
            data = df %>% filter(toupper(!!sym(input$annotation_col)) %in% genes),
            aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col))),
            size = 1.8, color = color, alpha = 0.7
          )
      }
    }
    
    # Create a new column for trimmed labels if input$trim_gene_names is TRUE
    if (input$trim_gene_names) {
      log_event(log_messages, "Trimming gene names to first occurrence", "INFO input$draw_volcano")
      df$trimmed_labels <- sapply(df[[input$annotation_col]], function(x) {
        strsplit(as.character(x), "[,; :]+")[[1]][1]
      })
    } else {
      # If not trimming, use the original annotation column for labels
      df$trimmed_labels <- df[[input$annotation_col]]
    }
    
    # Select top hits for labeling and assign colors to labels
    if (input$num_labels > 0) {
      top_hits <- df %>% arrange(adjusted_pvalues, desc(abs(!!sym(input$fold_col)))) %>% head(input$num_labels)
      
      # Assign Default Label Color
      top_hits$label_color <- "black"  # Default label color
      if (!is.null(chosen_go())) {
        selected_GO <- GO %>% filter(name %in% chosen_go())
        for (go in chosen_go()) {
          color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
          genes <- selected_GO %>% filter(name == go) %>% pull(gene)
          top_hits$label_color[top_hits[[input$annotation_col]] %in% genes] <- color
        }
      }
      
      volcano_plot <- volcano_plot + 
        geom_text_repel(data = top_hits, aes(label = trimmed_labels, color = label_color), size = 4, max.overlaps = Inf, nudge_y = 0.2) +
        scale_color_identity()  # Use identity scale to apply the colors directly
    }
    
    # Add custom gene labels if feature is enabled
    if (input$select_custom_labels && !is.null(custom_genes())) {
      log_event(log_messages, "Adding custom gene labels", "INFO input$draw_volcano")
      custom_label_data <- df %>% filter(!!sym(input$annotation_col) %in% custom_genes())
      
      # Use trimmed labels if applicable
      if (input$trim_gene_names) {
        custom_label_data$trimmed_labels <- sapply(custom_label_data[[input$annotation_col]], function(x) {
          strsplit(as.character(x), "[,; :]+")[[1]][1]
        })
      } else {
        custom_label_data$trimmed_labels <- custom_label_data[[input$annotation_col]]
      }
      
      volcano_plot <- volcano_plot +
        geom_label_repel(
          data = custom_label_data,
          aes(label = trimmed_labels),
          size = 4,
          color = "black",
          fill = "white",
          max.overlaps = Inf,
          nudge_y = 0.3,
          alpha = 0.7
        )
    }
    
    if (!is_mobile()) {
      volcano_plot_rv(volcano_plot)
    } else {
      # Calculate scaling factors based on screen width
      width <- input$clientWidth
      text_scale <- max(0.6, min(1, width / 768))
      point_scale <- max(0.8, min(1.2, width / 768))
      
      mobile_plot <- volcano_plot +
        # Adjust theme elements for mobile
        theme(
          # Reduce text sizes
          plot.title = element_text(size = 18 * text_scale),
          axis.title = element_text(size = 16 * text_scale),
          axis.text = element_text(size = 14 * text_scale),
          
          # Adjust margins for mobile
          plot.margin = margin(
            t = 5 * text_scale,
            r = 5 * text_scale,
            b = 5 * text_scale,
            l = 5 * text_scale
          ),
          
          # Modify aspect ratio for better mobile viewing
          aspect.ratio = 0.85,
          
          # Ensure plot uses full width
          panel.spacing = unit(1, "mm")
        )
      
      # Modify all existing layers for mobile optimization
      mobile_plot$layers <- lapply(mobile_plot$layers, function(l) {
        # Adjust point sizes
        if (inherits(l$geom, "GeomPoint")) {
          l$aes_params$size <- l$aes_params$size * point_scale
          l$aes_params$alpha <- min(l$aes_params$alpha + 0.1, 1) # Slightly increase visibility
        }
        # Adjust text labels
        else if (inherits(l$geom, "GeomTextRepel")) {
          l$aes_params$size <- 4 * text_scale
          l$aes_params$max.overlaps <- 10  # Reduce overlaps on mobile
          l$aes_params$box.padding <- 0.35
          l$aes_params$point.padding <- 0.25
          l$aes_params$force <- 2
        }
        else if (inherits(l$geom, "GeomLabelRepel")) {
          l$aes_params$size <- 4 * text_scale
          l$aes_params$max.overlaps <- 10  # Reduce overlaps on mobile
          l$aes_params$box.padding <- 0.35
          l$aes_params$point.padding <- 0.25
          l$aes_params$force <- 2
        }
        
        
        
        # Adjust annotations
        else if (inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel")) {
          l$aes_params$size <- l$aes_params$size * text_scale
        }
        return(l)
      })
      
      log_event(log_messages, 
                sprintf("Plot optimized for mobile view (width: %dpx, text scale: %.2f, point scale: %.2f)", 
                        width, text_scale, point_scale), 
                "INFO mobile optimization")
      # Store the plot in reactive value
      volcano_plot_rv(mobile_plot)
    }
    
      
    
    
    output$volcano_plot <- renderPlot({ 
      log_event(log_messages, "Rendering static volcano plot", "INFO output$volcano_plot")
      req(volcano_plot_rv())
      print(volcano_plot_rv()) 
    })
    cat("\n=== Debug Information for Volcano Plot ===\n")
    cat("1. Object Class:", class(volcano_plot), "\n")
    # cat("2. Structure:\n")
    # print(str(volcano_plot))
    cat("2. structure of the plot: output disabled \n")
    cat("3. Plot Layers:", length(volcano_plot$layers), "\n")
    cat("4. Plot Data Dimensions:", dim(volcano_plot$data), "\n")
    cat("================================\n")
    
    
    output$volcano_plotly <- renderPlotly({ 
      req(volcano_plot_rv())
      log_event(log_messages, "Rendering interactive volcano plot", "INFO output$volcano_plotly")
      # Convert to plotly with proper layout
      p <- ggplotly(volcano_plot_rv(), tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 50, r = 50, b = 50, t = 50),
          showlegend = TRUE
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          responsive = TRUE
        )
      
      # Adds try-catch for error handling
      tryCatch({
        return(p)
      }, error = function(e) {
        # Log the error and return a blank plot
        message("Error in plotly rendering: ", e$message)
        plot_ly() %>% 
          add_annotations(
            text = "Unable to render interactive plot. Please check your inputs.",
            showarrow = FALSE
          )
      })
      # Renders the download button
      output$download_log_button <- renderUI({
        downloadButton("download_log", "Download Analysis Log", class = "ui gray button")
      })
      
      # Renders the download button only after draw_volcano is clicked
      outputOptions(output, "download_log_button", suspendWhenHidden = FALSE)
      
      
      
    })
    
    

    output$download_plot1 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_85x85_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 85, 85, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 85, height = 85, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot2 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_114x114_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 114, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 114, height = 114, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot3 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_114x65_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 65, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 114, height = 65, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot4 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_174x174_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 174,log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 174, height = 174, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot5 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_174x98_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 98, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 174, height = 98, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    # Download handler for GO enrichment table
    output$download_go_enrichment <- downloadHandler(
      filename = function() {
        paste0("GO_Enrichment_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        req(enrichment_results_list, input$color_highlight)
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        gt_table <- build_gt_table(
          enrichment_results_list,
          upregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
          downregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0)),
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
        log_event(log_messages, "GO enrichment table created successfully and ready for saving as formatted html", "SUCCESS from output$download_go_enrichment")
        gtsave(gt_table, file, inline_css = TRUE)
      }
    )
    
    output$download_go_gene_list <- downloadHandler(
      filename = function() {
        paste0("GO_Gene_List_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        req(input$color_highlight, enrichment_results_list)
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        gt_table <- build_gt_gene_lists(
          df = uploaded_df(),  
          annotation_col = input$annotation_col,
          chosen_go = input$go_category,
          go_data = GO,
          alpha = input$alpha,
          fold_col = input$fold_col,
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
        
        # Save as HTML with inline CSS
        log_event(log_messages, "GO gene list table created successfully and ready for saving as formatted html", "SUCCESS from output$download_go_gene_list")
        gt::gtsave(gt_table, file, inline_css = TRUE)
      }
    )
    
    # Add the download log UI
    output$download_log_ui <- renderUI({
      if (!is.null(uploaded_df())) {
        tags$div(
          style = "margin-top: 20px;",
          downloadButton("download_log", "Download Process Log")
        )
      }
      
    })
    
    
    
    output$download_log <- downloadHandler(
      filename = function() {
        paste0("vivid_volcano_log_", session_id, ".txt")
      },
      content = function(file) {
        # Add session information header
        session_info <- sprintf(
          "Log File for Vivid Volcano Analysis\nSession ID: %s\nSession Start: %s\nDownload Time: %s\n----------------------------------------\n\n",
          session_id,
          format(session_start_time),
          format(Sys.time())
        )
        
        # Combine session info with logs
        complete_log <- paste0(session_info, log_messages())
        writeLines(complete_log, file)
      }
    )
    
    # Clear logs when session ends
    session$onSessionEnded(function() {
      log_event(log_messages, "Session ended", "INFO")
      isolate(log_messages(""))  # Clear logs
    })
    
  
})
}
shinyApp(ui = ui, server = server)
