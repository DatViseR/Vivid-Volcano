
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Advanced Data Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload a CSV or TSV file", accept = c(".csv", ".tsv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      radioButtons("dec", "Decimal Point", choices = c(Dot = ".", Comma = ","), selected = "."),
      actionButton("upload", "Upload"),
      h4("Select columns to build a volcano plot"),
      uiOutput("column_select_ui"),
      actionButton("save_columns", "Save Columns"),
      h4("Find significantly regulated genes/proteins"),
      radioButtons("adj", "pvalue adjustment", choices = c(None = "none", Bonferroni = "bonferroni", Hochberg = "hochberg", Benjamini_Hochberg  = "BH", Benjamini_Yekutieli = "BY"), selected = "BH"),
      numericInput("alpha", "Significance treshold", value = 0.05),
      actionButton("adjust_pvalues", "calculate adjusted p values"),
      h4("Volcano Plot Options"),
      checkboxInput("color_highlight", "Highlight significant hits", FALSE),
      checkboxInput("GO_highlight", "Highlight GO terms", FALSE),
      colourInput("up_color", "Up-regulated color", value = "darkgreen"),
      colourInput("down_color", "Down-regulated color", value = "red"),
      numericInput("num_labels", "Number of labels (0-100)", value = 10, min = 0, max = 100),
      actionButton("draw_volcano", "Draw Volcano Plot")
    ),
    mainPanel(
      verbatimTextOutput("dataset_summary"),
      verbatimTextOutput("column_info"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("pvalue_distribution"),
      verbatimTextOutput("significant_genes"),
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
    output$dataset_summary <- renderPrint({ cat("The following columns were uploaded \n \n"); dplyr::glimpse(df) })
  })
  
  observeEvent(input$save_columns, {
    req(input$pvalue_col, input$fold_col, input$annotation_col)
    output$column_info <- renderPrint({ cat("The following columns were selected to build a volcano plot \n \n"); list(pvalue = input$pvalue_col, fold = input$fold_col, annotation = input$annotation_col) })
    output$column_structure <- renderPrint({
      cat("The following columns structure was selected to build a volcano plot \n \n")
      df <- uploaded_df()
      selected_columns <- df[, c(input$pvalue_col, input$fold_col, input$annotation_col)]
      uploaded_df(selected_columns)
      str(selected_columns)
    })
  })
  
  observeEvent(input$adjust_pvalues, {
    req(uploaded_df(), input$pvalue_col)
    df <- uploaded_df()
    pvalues <- df[[input$pvalue_col]]
    if(length(pvalues) == 0) {
      print("Error: P-values column is empty or not found.")
      return(NULL)
    }
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    if(length(adjusted_pvalues) == 0) {
      print("Error: Adjusted p-values vector is empty.")
      return(NULL)
    }
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)
    output$pvalue_distribution <- renderPrint({ cat("Summary of the adjusted p-values \n \n"); summary(adjusted_pvalues) })
    output$significant_genes <- renderPrint({
      cat("Significantly regulated genes/proteins \n \n")
      significant_genes <- df %>% filter(adjusted_pvalues < input$alpha)
      non_significant_genes <- df %>% filter(adjusted_pvalues >= input$alpha)
      cat("Number of significant genes/proteins: ", nrow(significant_genes), "\n")
      cat("Number of non-significant genes/proteins: ", nrow(non_significant_genes), "\n")
    })
  })
  
  
}

shinyApp(ui = ui, server = server)