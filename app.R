library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(ggrepel)
library(arrow)

# Load the GO data once globally
GO <- arrow::read_parquet("GO.parquet")

ui <- fluidPage(
  titlePanel("Vivid Volcano Controls"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload a CSV or TSV file", accept = c(".csv", ".tsv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      radioButtons("dec", "Decimal Point", choices = c(Dot = ".", Comma = ","), selected = "."),
      actionButton("upload", "Upload")
    ),
    mainPanel(
      verbatimTextOutput("dataset_summary"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("pvalue_distribution"),
      verbatimTextOutput("significant_genes"),
      verbatimTextOutput("df_structure"),
      plotOutput("volcano_plot")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Select columns and calculate adjusted p-values"),
      uiOutput("column_select_ui"),
      radioButtons("adj", "pvalue adjustment", choices = c(None = "none", Bonferroni = "bonferroni", Hochberg = "hochberg", Benjamini_Hochberg = "BH", Benjamini_Yekutieli = "BY"), selected = "BH"),
      numericInput("alpha", "Significance threshold", value = 0.05),
      h4("Volcano Plot Options"),
      checkboxInput("color_highlight", "Highlight significant hits", FALSE),
      uiOutput("color_highlight_ui"),
      checkboxInput("show_go_category", "I want to visualise GO categories", FALSE),
      uiOutput("go_category_ui"),  # Placeholder for dynamic UI # chose from 18777 unique categories
      uiOutput("color_picker_ui"),  # Placeholder for dynamic color pickers
      
      numericInput("num_labels", "Number of labels (0-100)", value = 10, min = 0, max = 100),
      actionButton("draw_volcano", "Draw Volcano Plot")
    ),
    mainPanel(
      verbatimTextOutput("dataset_summary"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("pvalue_distribution"),
      verbatimTextOutput("significant_genes"),
      verbatimTextOutput("df_structure"),
      plotOutput("volcano_plot")
    )
  )
)

server <- function(input, output, session) {
  
  uploaded_df <- reactiveVal()
  
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
        selectInput("pvalue_col", "Select pvalue column", choices = colnames),
        selectInput("fold_col", "Select fold column", choices = colnames),
        selectInput("annotation_col", "Select annotation column", choices = colnames)
      )
    })
    
    output$dataset_summary <- renderPrint({ 
      cat("The following columns were uploaded: \n\n")
      dplyr::glimpse(df)
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
      sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go) # this is needed because spaces in GO categories causes bug
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
    
    # Adjust p-values
    pvalues <- df[[input$pvalue_col]]
    req(pvalues)  # Ensure pvalues are not NULL
    if(length(pvalues) == 0) {
      print("Error: P-values column is empty or not found.")
      return(NULL)
    }
    
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    req(adjusted_pvalues)  # Ensure adjusted_pvalues are not NULL
    if(length(adjusted_pvalues) == 0) {
      print("Error: Adjusted p-values vector is empty.")
      return(NULL)
    }
    
    # Check if the p-value column is log-transformed
    pvalues <- df[[input$pvalue_col]]
    if (all(pvalues >= 0 & pvalues <= 1) == FALSE) {
      cat("P-values appear to be -log10 transformed. Unlogging...\n")
      pvalues <- 10^(-abs(pvalues))
      df[[input$pvalue_col]] <- pvalues
      uploaded_df(df)  # Update the reactive value with unlogged p-values
      
      # Show summary distribution of the unlogged p-values
      output$pvalue_distribution <- renderPrint({ 
        req(uploaded_df())  # Ensure output recalculates when df updates
        cat("Summary of the unlogged p-values: \n\n")
        summary(pvalues)
      })
    }
    
    # Add adjusted p-values to dataframe and update reactive value
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
    
    # Draw the volcano plot
    req(df$adjusted_pvalues)  # Ensure adjusted_pvalues column is present
    if(!"adjusted_pvalues" %in% names(df)) {
      print("Error: adjusted_pvalues column is missing.")
      return(NULL)
    }
    
    volcano_plot <- ggplot(df, aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col)))) +
      geom_point(color = "gray50", size = 1.5) +
      theme_minimal() +
      labs(title = "Volcano Plot", x = "Log2 Fold Change", y = "-Log10 P-Value")
    
    if (input$color_highlight) {
      volcano_plot <- volcano_plot +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0), aes(color = "Up"), size = 1.5, color = input$up_color) +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0), aes(color = "Down"), size = 1.5, color = input$down_color)
    }
    
    # Highlighting genes belonging to chosen GO categories
    if (!is.null(chosen_go())) {
      selected_GO <- GO %>% filter(name %in% chosen_go())
      req(selected_GO)  # Ensure selected_GO is not NULL
      for (go in chosen_go()) {
        color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
        genes <- selected_GO %>% filter(name == go) %>% pull(gene)
        req(genes)  # Ensure genes are not NULL
        volcano_plot <- volcano_plot +
          geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$annotation_col) %in% genes),
                     aes(color = !!sym(input$annotation_col)), size = 1.5, color = color)
      }
    }
    
    if (input$num_labels > 0 & input$color_highlight ) {
      top_hits <- df %>% arrange(adjusted_pvalues, desc(abs(!!sym(input$fold_col)))) %>% head(input$num_labels)
      req(top_hits)  # Ensure top_hits are not NULL
      volcano_plot <- volcano_plot + geom_text_repel(data = top_hits, aes(label = !!sym(input$annotation_col)), size = 3, max.overlaps = Inf, nudge_y = 0.3)
    }
    
    output$volcano_plot <- renderPlot({ 
      req(volcano_plot)  # Ensure volcano_plot is not NULL
      print(volcano_plot) 
    })
  })
}
shinyApp(ui = ui, server = server)