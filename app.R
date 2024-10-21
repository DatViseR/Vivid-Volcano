#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(titlePanel("Upload CSV File"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      "file1",
                      "Upload a csv file with 3 columns: -log10 pvalue,
                log2 fold and single gene names or uniprotIDs",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    ),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE),
                    radioButtons(
                      "sep",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = ","
                    ),
                    actionButton("upload", "Upload")
                  ),
                  
                  mainPanel(tableOutput("contents"))
                ))

server <- function(input, output) {
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    df <- read.csv(in_file$datapath,
                   header = input$header,
                   sep = input$sep)
    output$contents <- renderTable({
      head(df)
    })
  })
}

shinyApp(ui = ui, server = server)