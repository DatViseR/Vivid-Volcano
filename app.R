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

# Load the GO data once globally
# The preparation of this file is described in https://github.com/DatViseR/Vivid-GO-data  and in the script
# Parquet_GO_source_data_preparation_script.R
# The file is also available in the data folder of this repository
# This newfile contains around 8000 non-obsolete unique GO categories with at least 6 annotated genes in the category
GO <- arrow::read_parquet("GO.parquet2")




##################---CRUCIAL CUSTOM FUNCTION DEFINITIONS---- ###########################

perform_hypergeometric_test <- function(population_size, success_population_size, sample_size, sample_success_size) {
  cat("Performing hypergeometric test with parameters:\n")
  cat("population_size:", population_size, "success_population_size:", success_population_size, "sample_size:", sample_size, "sample_success_size:", sample_success_size, "\n")
  phyper(sample_success_size - 1, success_population_size, population_size - success_population_size, sample_size, lower.tail = FALSE)
}

calculate_go_enrichment <- function(genes, go_categories, go_data) {
  # Clean gene names
  genes <- unlist(strsplit(genes, "[;|,\\s]+"))  # Split on semicolons, commas, or whitespace
  genes <- trimws(genes)                         # Remove leading/trailing whitespace
  genes <- genes[genes != ""]                    # Remove empty strings
  genes <- toupper(genes)                        # Convert to uppercase
  genes <- gsub("[^A-Z0-9]", "", genes)          # Remove any remaining special characters
  genes <- unique(genes)                         # Remove duplicates
  
  # Check if the gene list is empty
  if (length(genes) == 0) {
    warning("No valid genes provided for GO enrichment.")
    return(data.frame())  # Return an empty data frame
  }
  
  cat("Calculating GO enrichment for \n", "n=" , length(genes), "detected genes" ,     "\n")
  enrichment_results <- lapply(go_categories, function(go_category) {
    go_genes <- go_data %>% filter(name == go_category) %>% pull(gene)%>% toupper()%>% unique()
    cat("GO category:", go_category, "GO genes:", paste(go_genes, collapse = ", "), "\n")
    
    population_size <- 19689  # Number of human coding genes after pseudogene exclusion
    success_population_size <- length(go_genes)
    sample_size <- length(genes)
    # Check if each gene name has at least one exact match after cleaning and splitting
    sample_success_size <- sum(sapply(genes, function(gene) {
      # Remove special characters commonly surrounding genes
      cleaned_gene <- gsub("[c\\(\\)\";]", "", gene)
      
      # Split by spaces, commas, semicolons, or colons
      gene_parts <- toupper(unlist(strsplit(cleaned_gene, "[ ,;:]+")) )
      
      # Check if any of the cleaned parts match exactly with go_genes
      any(gene_parts %in% go_genes)
    }))
    
    cat("Computed values - population_size:", population_size, "success_population_size:", success_population_size,
        "sample_size:", sample_size, "sample_success_size:", sample_success_size, "\n")
    
    p_value <- perform_hypergeometric_test(population_size, success_population_size, sample_size, sample_success_size)
    cat("Calculated p_value for category", go_category, "is:", p_value, "\n")
    data.frame(
      GO_Category = go_category,
      P_Value = p_value,
      Population_Size = population_size,
      Success_Population_Size = success_population_size,
      Sample_Size = sample_size,
      Sample_Success_Size = sample_success_size
    )
  })
  
  enrichment_results <- bind_rows(enrichment_results)
  #This is only adjusting pvalue for the p values for the picked categories - i need to change this 
  # changed to bonferroni using n = 1160 which is an estimate for the number of GO categories at level 3 of hierarchy
  
  enrichment_results <- bind_rows(enrichment_results)
  
  # Check if enrichment_results is empty
  if (nrow(enrichment_results) == 0) {
    warning("No enrichment results found. Returning an empty data frame.")
    return(data.frame(GO_Category = character(),
                      P_Value = numeric(),
                      Population_Size = numeric(),
                      Success_Population_Size = numeric(),
                      Sample_Size = numeric(),
                      Sample_Success_Size = numeric()))
  }
  
  # Adjust p-values
  enrichment_results$Adjusted_P_Value <- p.adjust(enrichment_results$P_Value, method = "bonferroni", n = 1160)
  cat("The structure of the enrichment_results is: \n" , str(enrichment_results), "\n")
  return(enrichment_results)
}


calculate_go_enrichment_table <- function(df, annotation_col, go_categories, go_data, alpha, fold_col) {
  cat("Calculating GO enrichment table with alpha:", alpha, "fold_col:", fold_col, "\n")
  
  # Get genes
  upregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% 
    pull(!!sym(annotation_col))
  
  downregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% 
    pull(!!sym(annotation_col))
  
  regulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha) %>% 
    pull(!!sym(annotation_col))
  
  # Get GO IDs for categories
  go_ids <- go_data %>% 
    filter(name %in% go_categories) %>% 
    distinct(name, id)
  
  # Calculate enrichment with added GO IDs
  upregulated_enrichment <- calculate_go_enrichment(upregulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  downregulated_enrichment <- calculate_go_enrichment(downregulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  regulated_enrichment <- calculate_go_enrichment(regulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  enrichment_results_list <- list(
    upregulated = list(
      data = upregulated_enrichment
    ),
    downregulated = list(
      data = downregulated_enrichment
    ),
    regulated = list(
      data = regulated_enrichment
    )
  )
  
  cat("--------------\n", "Structure of the enrichment_results_list \n", str(enrichment_results_list),"\n", "--------------\n")
  return(enrichment_results_list)
  
}



create_publication_plot <- function(base_plot, width_mm, height_mm) {
  # Convert mm to inches for ggsave compatibility
  width_in <- width_mm * 0.0393701
  height_in <- height_mm * 0.0393701
  
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
  
  # Adjust text sizes and margins based on plot dimensions
  if (plot_key %in% c("85x85", "114x65")) {
    title_size <- 10
    text_size <- 6
    plot_margin <- margin(t = 10, r = 10, b = 5, l = 5, unit = "pt")
  } else {
    title_size <- 12
    text_size <- 8
    plot_margin <- margin(t = 30, r = 85, b = 10, l = 10, unit = "pt")
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
  
  # Update all layers
  for (i in seq_along(publication_plot$layers)) {
    layer <- publication_plot$layers[[i]]
    
    # Handle point geometries
    if (inherits(layer$geom, "GeomPoint")) {
      is_highlight <- !is.null(layer$aes_params$color) &&
        layer$aes_params$color %in% c("darkgreen", "red")
      new_size <- if (is_highlight) point_size$highlight else point_size$base
      layer$aes_params$size <- new_size
    }
    
    # Handle text and label annotations
    if (inherits(layer$geom, "GeomText") ||
        inherits(layer$geom, "GeomTextRepel") ||
        inherits(layer$geom, "GeomLabelRepel") ||
        inherits(layer$geom, "GeomLabel")) {
      
      # Check if this is an annotation text (positioned at Inf, Inf)
      is_annotation <- all(
        !is.null(layer$data),
        "x" %in% names(layer$data),
        "y" %in% names(layer$data),
        all(is.infinite(layer$data$x) & is.infinite(layer$data$y))
      )
      
      if (is_annotation) {
        # This is an annotation (e.g., counts or GO details)
        layer$aes_params$size <- point_size$annotation
      } else {
        # This is a regular text label (e.g., gene names)
        layer$aes_params$size <- text_size * 0.25
      }
    }
  }
  
  return(publication_plot)
}

build_gt_table <- function(enrichment_results_list, upregulated_count, downregulated_count, color_highlight) {
  down_color <- color_highlight[1]
  up_color <- color_highlight[2]
  
  # Prepare data frames with rounded values and formatted p-values
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
  
  return(gt_table)
}

#This function creates GT tables for each GO category with columns for genes in the GO category,
#downregulated genes, and upregulated genes, with detected genes in bold.

build_gt_gene_lists <- function(df, annotation_col, chosen_go, go_data, alpha, fold_col, color_highlight) {
  # Debug color inputs
  cat("\n==== Color Values Received ====\n")
  cat("Down-regulated color:", color_highlight[1], "\n")
  cat("Up-regulated color:", color_highlight[2], "\n")
  cat("================================\n\n")
  
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
      GO_Category = paste0(category, " GO:", go_id),  # Combined name and ID
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
      All_Genes = "All Genes",
      Downregulated = "Downregulated Genes",
      Upregulated = "Upregulated Genes"
    ) %>%
    # Style for downregulated genes
    tab_style(
      style = list(
        cell_text(color = down_color, weight = "bold")
      ),
      locations = cells_body(
        columns = "Downregulated",
        rows = !grepl("No downregulated genes found", table_data$Downregulated)
      )
    ) %>%
    # Style for upregulated genes
    tab_style(
      style = list(
        cell_text(color = up_color, weight = "bold")
      ),
      locations = cells_body(
        columns = "Upregulated",
        rows = !grepl("No upregulated genes found", table_data$Upregulated)
      )
    ) %>%
    fmt_markdown(columns = "All_Genes") %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    )
  
  return(gt_table_genes)
}




################################### ----UI---#################################


ui <- semanticPage(
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css", 
              href = paste0("custom.css?v=", Sys.time()))
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
          a(href = "https://www.linkedin.com/in/yourusername",
            icon("linkedin big"),
            class = "linkedin",
            target = "_blank"
          )
      )
  ),
  
  div(class = "ui fluid container",
      # Main layout using sidebar
      sidebar_layout(
        # Sidebar panel with controls
        sidebar_panel(class = "custom-sidebar",
                      width = 3,
                      # Data Upload Card
                      div(class = "ui raised segment",
                          header(title = "Upload your data", description = "", icon = "upload"),
                          div(class = "ui grey ribbon label", "Upload a CSV or TSV file"),
                          div(class = "ui file input", 
                              file_input("file1", label = NULL, accept = c(".csv", ".tsv"))
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
                           numericInput("alpha", "Significance Threshold", value = 0.05),
                           
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
                                        icon = icon("chart line icon"))
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
                                downloadButton("download_go_enrichment", "Download GO Table as PDF", class = "ui button")
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
                        downloadButton("download_go_gene_list", "Download GO Gene List as PDF", class = "ui button")
                      ),
                      gt_output("go_gene_list_gt")
                    )
                  )
                )        
              )
            )
          )
        )
      )
  )
)

##########################-----SERVER----####################################


server <- function(input, output, session) {
  
  uploaded_df <- reactiveVal()
  volcano_plot_rv <- reactiveVal()  # Create a reactive value to store the plot
  
  
  # Function to check and unlog p-values
  check_and_unlog_pvalues <- function(df, pvalue_col) {
    pvalues <- as.numeric(df[[pvalue_col]])
    if (all(pvalues >= 0 & pvalues <= 1) == FALSE) {
      cat("P-values appear to be -log10 transformed. Unlogging...\n")
      pvalues <- 10^(-abs(pvalues))
      df[[pvalue_col]] <- pvalues
    }
    return(df)
  }
  
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    df <- read_delim(in_file$datapath, delim = input$sep, col_names = input$header, locale = locale(decimal_mark = input$dec))
    uploaded_df(df)
    #show structure of the uploaded dataset
    cat("\n The structure of the uploaded dataset is: \n")
    str(df)
    
    output$column_select_ui <- renderUI({
      if (is.null(df)) return(NULL)
      
      div(class = "ui raised segment",
          div(class = "ui grey ribbon label", "Select Data"),
          selectInput("pvalue_col", "Select p-value column", choices = names(df)),
          selectInput("fold_col", "Select regulation column - log2(fold)", choices = names(df)),
          selectInput("annotation_col", "Select human gene symbols column", choices = names(df))
      )
    })
    
    
    output$dataset_summary <- renderDT({
      cat("The following columns were uploaded: \n\n")
      semantic_DT(
        data.frame(df, check.names = FALSE),  # Convert to data.frame if not already
        options = list(
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
    })
      
  })
  
  # Dynamic UI for color highlight options
  output$color_highlight_ui <- renderUI({
    if (input$color_highlight) {
      tagList(
        colourInput("up_color", "Up-regulated color", value = "darkgreen"),
        colourInput("down_color", "Down-regulated color", value = "red")
      )
    }
  })
  
  # Dynamic UI for GO Category Input
  output$go_category_ui <- renderUI({
    if (input$show_go_category) {
      selectizeInput("go_category", "Select from 18777 unique GO categories", choices = NULL, multiple = TRUE)
    }
  })
  
  observe({
    if (input$show_go_category) {
      updateSelectizeInput(session, "go_category", choices = unique(GO$name), server = TRUE)
    }
  })
  
  # Reactive expression to track chosen GO categories
  chosen_go <- reactive({
    input$go_category
  })
  
  
  color_palette <- c("#440154FF", "darkblue","gold","darkorange","darkcyan","deeppink","black") 
  
  
  
  # Dynamic UI for additional color pickers
  output$color_picker_ui <- renderUI({
    if (!input$show_go_category) {
      return(NULL)
    }
    
    req(chosen_go())
    chosen <- chosen_go()
    cat("Chosen GO categories: ", paste(chosen, collapse = ", "), "\n")  # Debug statement
    
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
    print(str(chosen_go()))  # This will help verify that categories are selected correctly
  })
  
  
# Dynamic UI for custom gene labels input
  output$custom_gene_labels_ui <- renderUI({
    if (input$select_custom_labels) {
      req(uploaded_df(), input$annotation_col)
      gene_names <- unique(uploaded_df()[[input$annotation_col]])
      selectizeInput("custom_gene_labels", "Select gene names to label", choices = gene_names, multiple = TRUE)
    }
  })
  
  # Reactive expression to track chosen custom gene labels
  custom_genes <- reactive({
    input$custom_gene_labels
  })
  
  
  observeEvent(input$draw_volcano, {
    req(uploaded_df(), input$pvalue_col, input$fold_col, input$annotation_col, input$adj)
    df <- uploaded_df()
    
    # Check and unlog p-values
    df <- check_and_unlog_pvalues(df, input$pvalue_col)
    uploaded_df(df)  # Update the reactive value with unlogged p-values
    
    # Adjust p-values
    pvalues <- df[[input$pvalue_col]]
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)  # Ensure reactive value is updated
    
    cat("----------------\n", "Structure of the uploaded dataset after unlogging and adjusting p-values \n", str(df), "\n", "----------------\n")
    
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
        fold_col = input$fold_col
      )
      
      # Render GO gene list table
      output$go_gene_list_gt <- render_gt({
        # First check if color highlighting is enabled
        req(input$color_highlight)
        
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
          color_highlight = colors_to_use
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
          downregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0))
        )
      })
    } else {
      # Handle both outputs in the else block
      output$go_gene_list_gt <- render_gt({
        gt(data.frame(Message = if(!input$show_go_category) {
          "Enable GO category visualization to see results"
        } else {
          "Select at least one GO category to see results"
        }))
      })
      
      output$go_enrichment_gt <- render_gt({
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
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      volcano_plot <- volcano_plot +
        annotate("text", x = -Inf, y = Inf, label = paste0("Upregulated n= ", upregulated_count), color = input$up_color, hjust = -0.1 ,vjust = 2, size = 5.5 ) +
        annotate("text", x = -Inf, y = Inf, label = paste0("Downregulated n= ", downregulated_count), color = input$down_color, hjust = -0.2, vjust = 1, size = 5.5)
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
        cat("Warning: 'id' column not found in selected_GO\n")  # Debug warning if 'id' column is missing
        cat(go_details)  # Debug statement
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
      volcano_plot <- volcano_plot +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0), aes(color = "Up"), size = 2, color = input$up_color, alpha = 0.5) +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0), aes(color = "Down"), size = 2, color = input$down_color, alpha = 0.5)
    }
    
    # Highlighting genes belonging to chosen GO categories
    if (!is.null(chosen_go())) {
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
            size = 1.8, color = color, alpha = 0.5
          )
      }
    }
    
    # Create a new column for trimmed labels if input$trim_gene_names is TRUE
    if (input$trim_gene_names) {
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
    
    # NEW: Add custom gene labels if feature is enabled
    if (input$select_custom_labels && !is.null(custom_genes())) {
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
    
    volcano_plot_rv(volcano_plot)  # Store the plot in reactive value
    
    output$volcano_plot <- renderPlot({ 
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
      
      # Add try-catch for error handling
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
    })
    
    

    output$download_plot1 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_85x85_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 85, 85)
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
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 114)
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
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 65)
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
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 174)
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
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 98)
        ggsave(file, publication_plot, width = 174, height = 98, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    # Download handler for GO enrichment table
    output$download_go_enrichment <- downloadHandler(
      filename = function() {
        paste0("GO_Enrichment_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(enrichment_results_list)
        gt_table <- build_gt_table(
          enrichment_results_list,
          upregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
          downregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0))
        )
        gtsave(gt_table, file)
      }
    )
    
    # Download handler for GO gene list table
    output$download_go_gene_list <- downloadHandler(
      filename = function() {
        paste0("GO_Gene_List_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
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
          df = uploaded_df(),  # Use the actual df from the parent scope
          annotation_col = input$annotation_col,
          chosen_go = input$go_category,
          go_data = GO,
          alpha = input$alpha,
          fold_col = input$fold_col,
          color_highlight = colors_to_use
        )
        gtsave(gt_table, file)
      }
    )
    
    
})
}
shinyApp(ui = ui, server = server)
