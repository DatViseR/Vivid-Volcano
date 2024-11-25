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

# Load the GO data once globally
GO <- arrow::read_parquet("GO.parquet")

##################---CRUCIAL FUNCTION DEFINITIONS---- ###########################

perform_hypergeometric_test <- function(population_size, success_population_size, sample_size, sample_success_size) {
  cat("Performing hypergeometric test with parameters:\n")
  cat("population_size:", population_size, "success_population_size:", success_population_size, "sample_size:", sample_size, "sample_success_size:", sample_success_size, "\n")
  phyper(sample_success_size - 1, success_population_size, population_size - success_population_size, sample_size, lower.tail = FALSE)
}

calculate_go_enrichment <- function(genes, go_categories, go_data) {
  cat("Calculating GO enrichment for genes:\n", paste(genes, collapse = ", "), "\n")
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
  
  enrichment_results$Adjusted_P_Value <- p.adjust(enrichment_results$P_Value, method = "bonferroni", n = 1160)
  
  cat("Enrichment results:\n")
  print(enrichment_results)
  
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
  
  return(enrichment_results_list)
}



create_publication_plot <- function(base_plot, width_mm, height_mm) {
  # Convert mm to inches for ggsave compatibility
  width_in <- width_mm * 0.0393701
  height_in <- height_mm * 0.0393701
  
  # Define fixed base sizes for different plot elements
  title_size <- 12
  text_size <- 8
  
  # Define fixed point sizes for different plot dimensions
  point_sizes <- list(
    "85x85" = list(base = 0.6, highlight = 0.9, annotation = 1.5),
    "114x114" = list(base = 0.8, highlight = 1.2, annotation = 2),
    "114x65" = list(base = 0.7, highlight = 1.05, annotation = 1.75),
    "174x174" = list(base = 1, highlight = 1.5,annotation = 2.5),
    "174x98" = list(base = 0.9, highlight = 1.35, annotation = 2.25)) 
  
  # Determine which size configuration to use
  plot_key <- paste0(width_mm, "x", height_mm)
  point_size <- point_sizes[[plot_key]] %||% point_sizes[["85x85"]]
  
  # Modify the plot with publication-ready settings
  publication_plot <- base_plot +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size),
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      plot.margin = margin(t = 30, r = 85, b = 10, l = 10, unit = "pt")
    )
  
  # Update all layers
  for(i in seq_along(publication_plot$layers)) {
    layer <- publication_plot$layers[[i]]
    
    # Handle point geometries
    if(inherits(layer$geom, "GeomPoint")) {
      is_highlight <- !is.null(layer$aes_params$color) &&
        layer$aes_params$color %in% c("darkgreen", "red")
      new_size <- if(is_highlight) point_size$highlight else point_size$base
      layer$aes_params$size <- new_size
    }
    
    # Handle text annotations
    if(inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomTextRepel")) {
      # Check if this is an annotation text (positioned at Inf,Inf)
      is_annotation <- all(
        !is.null(layer$data),
        "x" %in% names(layer$data),
        "y" %in% names(layer$data),
        all(is.infinite(layer$data$x) & is.infinite(layer$data$y))
      )
      
      if(is_annotation) {
        # This is an annotation text (upregulated/downregulated counts or GO details)
        layer$aes_params$size <- point_size$annotation
      } else {
        # This is a regular text label (gene names)
        layer$aes_params$size <- text_size * 0.25
      }
    }
  }
  
  return(publication_plot)
}

build_gt_table <- function(enrichment_results_list, upregulated_count, downregulated_count) {
  # Prepare data frames with rounded values and formatted p-values
  regulated_df <- enrichment_results_list$regulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.3f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.3f", Adjusted_P_Value))
    )
  
  upregulated_df <- enrichment_results_list$upregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value))
    )
  
  downregulated_df <- enrichment_results_list$downregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value))
    )
  
  # Combine all data frames and remove "Population_Size" column
  combined_df <- bind_rows(
    regulated_df,
    upregulated_df,
    downregulated_df
  ) %>%
    select(-Population_Size)   # 1. Remove "Number of human genes" column
  #  select(-Sample_Size)   # 2. Remove "Number of regulated genes" column
  
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
        "Subpopulation_Enrichment_Ratio"
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
    )%>%
    # Hide the Sample_Size column
    cols_hide(columns = "Sample_Size")
  
  # Add colored row groups
  if(nrow(regulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Bidirectionally regulated n = ", regulated_df$Sample_Size[1]),
        rows = 1:nrow(regulated_df)
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#D3D3D3") # Light gray color
        ),
        locations = cells_row_groups(groups = paste0("Bidirectionally regulated n = ", regulated_df$Sample_Size[1]))
      )
  }
  
  if(nrow(upregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Upregulated n = ", upregulated_df$Sample_Size[1]),
        rows = (nrow(regulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#ADD8E6") # Light blue color
        ),
        locations = cells_row_groups(groups = paste0("Upregulated n = ",  upregulated_df$Sample_Size[1]))
      )
  }
  
  if(nrow(downregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Downregulated n = ",  downregulated_df$Sample_Size[1]),
        rows = (nrow(regulated_df) + nrow(upregulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df) + nrow(downregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#FFC0CB") # Light pink color
        ),
        locations = cells_row_groups(groups = paste0("Downregulated n = ", downregulated_df$Sample_Size[1]))
      )
  }
  
  return(gt_table)
}






################################### ----UI---#################################


  ui <- semanticPage(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Navbar
    div(class = "navbar",
            
            # Left Section: Title and Subtitle
          
            img(src = "Vivid_volcano_logo3.svg", alt = "Logo", class = "logo"),
            
              div(class = "left-section",
                    
                    
                    h1("Vivid Volcano", class = "title"),
                    h4("Publication-ready volcano plots and GO analysis with ease", class = "subtitle")
            ),
            
            
           
           
            div(class = "social-buttons",
                h5("Source Code", class = "icon-header"),  
                    a(
                      href = "https://github.com/DatViseR",
                      icon("github big"),
                      class = "github",
                      target = "_blank"
                    ),
                h5("Developer", class = "icon-header"),
                    a(
                      href = "https://www.linkedin.com/in/yourusername",
                      icon("linkedin big"),
                      class = "linkedin",
                      target = "_blank"
                    )
            )
    
    ),

    
  # Main grid layout
  div(class = "ui grid container",
      # Sidebar (4 columns)
      div(class = "six wide column",
          # Upload controls
          segment(class = "raised",
                  h3(class = "ui header", "Data Upload"),
                  div(class = "ui form",
                      fileInput("file1", "Upload a CSV or TSV file", accept = c(".csv", ".tsv")),
                      checkboxInput("header", "Header", TRUE),
                     multiple_radio("sep", "Separator", 
                                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), 
                                   selected = ","),
                      radioButtons("dec", "Decimal Point", 
                                   choices = c(Dot = ".", Comma = ","), 
                                   selected = "."),
                      actionButton("upload", "Upload", class = "ui primary button")
                  )
          ),
          
          # Analysis options
          segment(class = "raised",
                  h3(class = "ui header", "Analysis Options"),
                  div(class = "ui form",
                      uiOutput("column_select_ui"),
                      multiple_radio("adj", "P-value Adjustment",
                                   choices = c(
                                     None = "none",
                                     Bonferroni = "bonferroni",
                                     Hochberg = "hochberg",
                                     `Benjamini-Hochberg` = "BH",
                                     `Benjamini-Yekutieli` = "BY"
                                   ), selected = "BH"),
                      numericInput("alpha", "Significance Threshold", value = 0.05)
                  )
          ),
          
          # Plot options
          segment(class = "raised",
                  h3(class = "ui header", "Plot Options"),
                  div(class = "ui form",
                      checkboxInput("color_highlight", "Highlight Significant Hits", FALSE),
                      uiOutput("color_highlight_ui"),
                      checkboxInput("show_go_category", "Visualize GO Categories", FALSE),
                      uiOutput("go_category_ui"),
                      uiOutput("color_picker_ui"),
                      numericInput("num_labels", "Number of Labels (0-100)", 
                                   value = 10, min = 0, max = 100),
                      checkboxInput("trim_gene_names", "Trim Gene Names to First Occurrence", FALSE),
                      textInput("plot_title", "Plot Title", "Vivid Volcano"),
                      textInput("x_axis_label", "X Axis Label", 
                                "Log2 Fold Change (Condition X vs. Condition Y)"),
                      actionButton("draw_volcano", "Draw Volcano Plot", 
                                   class = "ui primary button")
                  )
          )
      ),
      
      # Main content (12 columns)
      div(class = "twelve wide column",
          # Dataset preview
          segment(class = "raised",
                  h3(class = "ui header", "Dataset Preview"),
                  DT::dataTableOutput("dataset_summary", height = "auto")
          ),
          
          # Tabset for plots and results
          segment(class = "raised",
                  tabset(
                    tabs = list(
                      list(
                        menu = "Static Volcano Plot and GO enrichment table",
                        content = div(
                          plotOutput("volcano_plot", height = "350px"),
                          segment(class = "basic",
                                  h4(class = "ui header", "Download Publication-Ready Plots"),
                                  div(class = "ui buttons",
                                      downloadButton("download_plot1", "85x85mm (1 column)", 
                                                     class = "ui button"),
                                      downloadButton("download_plot2", "114x114mm (1.5 column)", 
                                                     class = "ui button"),
                                      downloadButton("download_plot3", "114x65mm (1.5 column landscape)", 
                                                     class = "ui button"),
                                      downloadButton("download_plot4", "174x174mm (square)", 
                                                     class = "ui button"),
                                      downloadButton("download_plot5", "174x98mm (landscape)", 
                                                     class = "ui button")
                                  )
                          ),
                          segment(class = "basic",
                                  h4(class = "ui header", "GO Enrichment Results"),
                                  gt_output("go_enrichment_gt")
                          )
                        )
                      ),
                      list(
                        menu = "Interactive Volcano Plot",
                        content = div(
                          plotlyOutput("volcano_plotly", height = "350px")
                        )
                      )
                    )
                  )
          )
      )
  ),
  
  # CSS inclusion
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)





##########################-----SERVER----####################################


server <- function(input, output, session) {
  
  uploaded_df <- reactiveVal()
  volcano_plot_rv <- reactiveVal()  # Create a reactive value to store the plot
  
  
  # Function to check and unlog p-values
  check_and_unlog_pvalues <- function(df, pvalue_col) {
    pvalues <- df[[pvalue_col]]
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
      colnames <- names(df)
      tagList(
        selectInput("pvalue_col", "Select p-value column", choices = colnames),
        selectInput("fold_col", "Select regulation column - log2(fold)", choices = colnames),
        selectInput("annotation_col", "Select human gene symbols column", choices = colnames)
      )
    })
    
    
    output$dataset_summary <- DT::renderDataTable({ 
      cat("The following columns were uploaded: \n\n")
      #show only first 3 entries
     datatable(df, rownames = FALSE, options = list(pageLength = 3))
    })
      output$uploaded_dataset_ui <- renderUI({
       tagList(
         h3("Uploaded Dataset Preview"),
         DT::dataTableOutput("dataset_summary")
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
      selectizeInput("go_category", "Browse 18777 unique GO categories", choices = NULL, multiple = TRUE)
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
    
    # Render the adjusted p-values summary
    output$pvalue_distribution <- renderPrint({ 
      req(uploaded_df())  # Ensure output recalculates when df updates
      cat("Summary of the adjusted p-values: \n\n")
      summary(adjusted_pvalues)
    })
    
    # Render the significant genes/proteins
    output$significant_genes <- renderPrint({
      req(uploaded_df())  # Ensure output recalculates when df updates
      cat("Significantly regulated genes/proteins: \n\n")
      significant_genes <- df %>% filter(adjusted_pvalues < input$alpha)
      non_significant_genes <- df %>% filter(adjusted_pvalues >= input$alpha)
      cat("Number of significant genes/proteins: ", nrow(significant_genes), "\n")
      cat("Number of non-significant genes/proteins: ", nrow(non_significant_genes), "\n")
    })
    
    # Render the final data frame structure
    output$df_structure <- renderPrint({
      req(uploaded_df())  # Ensure output recalculates when df updates
      cat("Final Data Frame Structure: \n")
      updated_df <- uploaded_df()  # Re-fetch the updated dataframe
      str(updated_df)  # Reflect the updated dataframe
    })
    
    # Calculate GO tag enrichment
    # modify so the arguments are described in the function
    enrichment_results_list <- calculate_go_enrichment_table(df, input$annotation_col, input$go_category, GO, input$alpha, input$fold_col)
    
    cat("Structure of enrichment_results_list:\n")
    str(enrichment_results_list)
    
    # Render the GO enrichment table
    output$go_enrichment_gt <- render_gt({
      req(enrichment_results_list)
      build_gt_table(
        enrichment_results_list,
        upregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
        downregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0))
      )
    })
    
    
    
    # Draw the volcano plot
    if (!"adjusted_pvalues" %in% names(df)) {
      print("Error: adjusted_pvalues column is missing.")
      return(NULL)
    }
    
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
            axis.title = element_text(size = 16, color = "navy", face = "bold"),
            axis.text = element_text(size = 14, color = "navy", face = "bold"),
             ) +  
      geom_hline(yintercept = -log10(input$alpha), linetype = "dashed", color = "red") +
      scale_x_continuous(limits = c(-max(abs(df[[input$fold_col]])), max(abs(df[[input$fold_col]]))))  # Set x-axis limits
   
    # Generate subtitle based on the input settings
    subtitle <- NULL
    
    if (input$color_highlight) {
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      volcano_plot <- volcano_plot +
        annotate("text", x = Inf, y = Inf, label = paste0("Upregulated n= ", upregulated_count), color = input$up_color, hjust = 1.1, vjust = 2, size = 6, fontface = "italic") +
        annotate("text", x = Inf, y = Inf, label = paste0("Downregulated n= ", downregulated_count), color = input$down_color, hjust = 1.1, vjust = 3, size = 6, fontface = "italic")
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
          annotate("text", x = Inf, y = Inf, label = go_detail, color = color, hjust = 1.1, vjust = 3 + i, size = 6, fontface = "italic")
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
    
    volcano_plot_rv(volcano_plot)  # Store the plot in reactive value
    
    output$volcano_plot <- renderPlot({ 
      req(volcano_plot_rv())
      print(volcano_plot_rv()) 
    })
    
    output$volcano_plotly <- renderPlotly({ 
      req(volcano_plot_rv())
      ggplotly(volcano_plot_rv(), tooltip = "text") 
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
  
})
}
shinyApp(ui = ui, server = server)
