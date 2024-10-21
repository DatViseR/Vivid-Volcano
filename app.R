#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
titlePanel("Upload CSV File"),
sidebarLayout(
  sidebarPanel(
    fileInput("file1", "Upload a csv file with 3 columns: -log10 pvalue,
                log2 fold and single gene names or uniprotIDs",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    # add action button to upload the file
    actionButton("upload", "Upload File"),
      ),
  
  
  mainPanel(
    tableOutput("contents")
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
# create an observer for the file input
output$contents <- renderTable({
  req(input$file1)
 
  in_file <- input$file1
  df <- read.csv(in_file$datapath, header = input$header, sep = input$sep)
  head(df)
})


}

# Run the application 
shinyApp(ui = ui, server = server)
