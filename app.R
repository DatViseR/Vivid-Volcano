library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(ggrepel)
library(arrow)
library(DT)

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
    go_genes <- go_data %>% filter(name == go_category) %>% pull(gene)
    cat("GO category:", go_category, "GO genes:", paste(go_genes, collapse = ", "), "\n")
    
    population_size <- 19689  # Number of human coding genes after pseudogene exclusion
    success_population_size <- length(go_genes)
    sample_size <- length(genes)
    # Check if each gene name has at least one exact match after cleaning and splitting
    sample_success_size <- sum(sapply(genes, function(gene) {
      # Remove special characters commonly surrounding genes
      cleaned_gene <- gsub("[c\\(\\)\";]", "", gene)
      
      # Split by spaces, commas, semicolons, or colons
      gene_parts <- unlist(strsplit(cleaned_gene, "[ ,;:]+"))
      
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
  upregulated_genes <- df %>% filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% pull(!!sym(annotation_col))
  downregulated_genes <- df %>% filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% pull(!!sym(annotation_col))
  regulated_genes <- df %>% filter(adjusted_pvalues < alpha) %>% pull(!!sym(annotation_col))
  
  cat("Upregulated genes:", paste(upregulated_genes, collapse = ", "), "\n")
  cat("Downregulated genes:", paste(downregulated_genes, collapse = ", "), "\n")
  cat("All regulated genes:", paste(regulated_genes, collapse = ", "), "\n")
  
  upregulated_enrichment <- calculate_go_enrichment(upregulated_genes, go_categories, go_data)
  downregulated_enrichment <- calculate_go_enrichment(downregulated_genes, go_categories, go_data)
  regulated_enrichment <- calculate_go_enrichment(regulated_genes, go_categories, go_data)
  
  enrichment_results_list <- list(
    upregulated = upregulated_enrichment,
    downregulated = downregulated_enrichment,
    regulated = regulated_enrichment
  )
  
  cat("Structure of enrichment_results_list:\n")
  str(enrichment_results_list)
  
  return(enrichment_results_list)
}
################################### ----UI---#################################

ui <- fluidPage(
  titlePanel("Vivid Volcano Controls"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload a CSV or TSV file", accept = c(".csv", ".tsv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      radioButtons("dec", "Decimal Point", choices = c(Dot = ".", Comma = ","), selected = "."),
      actionButton("upload", "Upload"),
      h4("Select columns for analysis"),
      uiOutput("column_select_ui"),
      radioButtons("adj", "pvalue adjustment", choices = c(None = "none", Bonferroni = "bonferroni", Hochberg = "hochberg", Benjamini_Hochberg = "BH", Benjamini_Yekutieli = "BY"), selected = "BH"),
      numericInput("alpha", "Significance threshold", value = 0.05),
      h4("Volcano Plot Options"),
      checkboxInput("color_highlight", "Highlight significant hits", FALSE),
      uiOutput("color_highlight_ui"),
      checkboxInput("show_go_category", "I want to visualise GO categories", FALSE),
      uiOutput("go_category_ui"),  # Placeholder for dynamic UI
      uiOutput("color_picker_ui"),  # Placeholder for dynamic color pickers
      numericInput("num_labels", "Number of labels (0-100)", value = 10, min = 0, max = 100),
      checkboxInput("trim_gene_names", "Trim multiple gene names to first occurrence", FALSE),
      textInput("plot_title", "Plot Title", "Vivid Volcano"),
      textInput("x_axis_label", "X Axis Label", "log2 fold condition X vs. condition Y"),
      actionButton("draw_volcano", "Draw Volcano Plot")
    ),
    mainPanel(
      uiOutput("uploaded_dataset_ui"),
      DT::dataTableOutput("dataset_summary"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("pvalue_distribution"),
      verbatimTextOutput("significant_genes"),
      verbatimTextOutput("df_structure"),
      plotOutput("volcano_plot"),
      h3("GO Enrichment for Regulated Genes"),
      tableOutput("go_enrichment_regulated"),
      h3("GO Enrichment for Upregulated Genes"),
      tableOutput("go_enrichment_upregulated"),
      h3("GO Enrichment for Downregulated Genes"),
      tableOutput("go_enrichment_downregulated")
    )
  )
)

server <- function(input, output, session) {
  
  uploaded_df <- reactiveVal()
  
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
    
    output$contents <- renderTable({ head(df) })
    
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
  
  # Dynamic UI for additional color pickers
  output$color_picker_ui <- renderUI({
    if (!input$show_go_category) {
      return(NULL)
    }
    req(chosen_go())
    chosen <- chosen_go()
    cat("Chosen GO categories: ", paste(chosen, collapse = ", "), "\n")  # Debug statement
    color_inputs <- lapply(chosen, function(go) {
      sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go)  # this is needed because spaces in GO categories causes bug
      cat("Creating color input for: ", go, " with ID: ", sanitized_id, "\n")  # Debug statement
      colourInput(paste0("color_", sanitized_id), paste("Color for", go), value = "blue")
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
    enrichment_results_list <- calculate_go_enrichment_table(df, input$annotation_col, input$go_category, GO, input$alpha, input$fold_col)
    
    cat("Structure of enrichment_results_list:\n")
    str(enrichment_results_list)
    
    output$go_enrichment_regulated <- renderTable({
      if (nrow(enrichment_results_list$regulated) > 0) {
        enrichment_results_list$regulated
      } else {
        data.frame(Message = "No data available for regulated enrichment.")
      }
    })
    
    output$go_enrichment_upregulated <- renderTable({
      if (nrow(enrichment_results_list$upregulated) > 0) {
        enrichment_results_list$upregulated
      } else {
        data.frame(Message = "No data available for upregulated enrichment.")
      }
    })
    
    output$go_enrichment_downregulated <- renderTable({
      if (nrow(enrichment_results_list$downregulated) > 0) {
        enrichment_results_list$downregulated
      } else {
        data.frame(Message = "No data available for downregulated enrichment.")
      }
    })
    
    
    
    # Draw the volcano plot
    if (!"adjusted_pvalues" %in% names(df)) {
      print("Error: adjusted_pvalues column is missing.")
      return(NULL)
    }
    
    volcano_plot <- ggplot(df, aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col)))) +
      geom_point(size = 1.8, alpha = 0.5, color = "gray70") +
      theme_minimal() +
      labs(title = input$plot_title, x = input$x_axis_label, y = "-Log10 P-Value") +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()
            )+
      geom_hline(yintercept = -log10(input$alpha), linetype = "dashed", color = "red") 
   
    # Generate subtitle based on the input settings
    subtitle <- NULL
    
    if (input$color_highlight) {
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      subtitle <- paste0("Upregulated n= ", upregulated_count, "\n" , "\n", "Downregulated n= ", downregulated_count, "\n")
    }
    
    if (input$show_go_category) {
      chosen <- chosen_go()
      go_details <- paste0("GO: ", paste(chosen, collapse = ", "))
      if (!is.null(subtitle)) {
        subtitle <- paste(subtitle, go_details, sep = "\n")
      } else {
        subtitle <- go_details
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
        volcano_plot <- volcano_plot +
          geom_point(
            data = df %>% filter(!!sym(input$annotation_col) %in% genes),
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
        geom_text_repel(data = top_hits, aes(label = trimmed_labels, color = label_color), size = 3, max.overlaps = Inf, nudge_y = 0.2) +
        scale_color_identity()  # Use identity scale to apply the colors directly
    }
    
    output$volcano_plot <- renderPlot({ print(volcano_plot) })
  })
}
shinyApp(ui = ui, server = server)
