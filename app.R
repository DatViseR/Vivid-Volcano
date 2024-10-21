#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(readr)

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
      uiOutput("column_select_ui"),
      actionButton("save_columns", "Save Columns")
    ),
    mainPanel(
      tableOutput("contents"),
      verbatimTextOutput("column_info"),
      verbatimTextOutput("column_structure"),
      verbatimTextOutput("dataset_summary")
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
      summary(df)
    })
  })
  
  observeEvent(input$save_columns, {
    req(input$pvalue_col, input$fold_col, input$annotation_col)
    output$column_info <- renderPrint({
      list(
        pvalue = input$pvalue_col,
        fold = input$fold_col,
        annotation = input$annotation_col
      )
    })
    
    output$column_structure <- renderPrint({
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