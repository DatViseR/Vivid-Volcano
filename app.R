
library(shiny)
library(readr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Advanced Data Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload a CSV or TSV file",
                accept = c(".csv", ".tsv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("dec", "Decimal Point",
                   choices = c(Dot = ".",
                               Comma = ","),
                   selected = "."),
      actionButton("upload", "Upload"),
      h4("Select columns to build a volcano plot"),
      uiOutput("column_select_ui"),
      actionButton("save_columns", "Save Columns"),
      h4("Find significantly regulated genes/proteins"),
      radioButtons("adj", "pvalue adjustment",
                   choices = c(None = "none",
                               Bonferroni = "bonferroni",
                               Hochberg = "hochberg",
                               Benjamini_Hochberg  = "BH",
                               Benjamini_Yekutieli = "BY",
                               None = "none"),
                   selected = "BH"),
      numericInput("alpha", "Significance treshold", value = 0.05),
      actionButton("adjust_pvalues", "calculate adjusted p values")
    ),
    mainPanel(
      verbatimTextOutput("dataset_summary"),
      verbatimTextOutput("column_info"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("pvalue_distribution"),
      verbatimTextOutput("significant_genes")
    )
  )
)
 


server <- function(input, output, session) {
  
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    
    # Use readr package to read the uploaded dataset
    df <- read_delim(in_file$datapath, delim = input$sep, 
                     col_names = input$header, locale = locale(decimal_mark = input$dec))
    
    output$contents <- renderTable({
      head(df)
    })
    
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
      #output a message to the user "Check if the summary of the data is appropriate "
      cat("The following columns were uploaded \n \n" )
      dplyr::glimpse(df)
    })
  })
  
  observeEvent(input$save_columns, {
    req(input$pvalue_col, input$fold_col, input$annotation_col)
    output$column_info <- renderPrint({
      cat("The following columns were selected to build a volcano plot \n \n")
        list(
        pvalue = input$pvalue_col,
        fold = input$fold_col,
        annotation = input$annotation_col
      )
    })
    
    output$column_structure <- renderPrint({
      cat("The following columns structure was selected to build a volcano plot \n \n")
      
      req(input$file1)
      in_file <- input$file1
      
      df <- read_delim(in_file$datapath, delim = input$sep, 
                       col_names = input$header, locale = locale(decimal_mark = input$dec))
      
      selected_df <- df[, c(input$pvalue_col, input$fold_col, input$annotation_col)]
      str(selected_df)
    })
  })
}

shinyApp(ui = ui, server = server)