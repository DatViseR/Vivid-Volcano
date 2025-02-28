# Simplified test script without problematic renderPrint outputs
library(shiny)
library(DBI)
source("telemetry_module.R")

# Test app with simplified UI
ui <- fluidPage(
  tags$head(
    tags$script(src = "telemetry.js"),
    tags$script(HTML('
      $(document).ready(function() {
        Shiny.addCustomMessageHandler("getLocalStorage", function(id) {
          var value = localStorage.getItem(id);
          Shiny.setInputValue("storage_value", {id: id, value: value});
        });
        
        Shiny.addCustomMessageHandler("clearLocalStorage", function(id) {
          localStorage.removeItem(id);
          Shiny.setInputValue("storage_cleared", new Date().toISOString());
        });
      });
    '))
  ),
  titlePanel("Telemetry Test (Simplified)"),
  
  # Debug controls
  fluidRow(
    column(6,
           h3("Local Storage Testing"),
           actionButton("show_storage", "Show localStorage", class = "btn-info"),
           actionButton("clear_storage", "Clear localStorage", class = "btn-danger"),
           br(), br(),
           textOutput("storage_value")
    ),
    column(6,
           h3("Telemetry Status"),
           textOutput("session_id"),
           textOutput("visit_count"),
           textOutput("browser_info")
    )
  ),
  
  hr(),
  
  # Test buttons
  fluidRow(
    column(12,
           h3("Test Buttons"),
           actionButton("upload", label = HTML('<i class="upload icon"></i> Upload'), 
                        class = "ui primary button"),
           
           actionButton("draw_volcano", "Draw Volcano Plot", 
                        class = "ui primary button", 
                        icon = icon("chart line")),
           
           actionButton(
             inputId = "run_gsea",
             label = HTML('<i class="play icon"></i> Run GSEA'),
             class = "ui primary button"
           )
    )
  ),
  
  hr(),
  
  # Counter display
  fluidRow(
    column(6,
           h3("Button Click Counters"),
           textOutput("upload_counter"),
           textOutput("gsea_counter"),
           textOutput("volcano_counter")
    ),
    column(6,
           h3("Database"),
           actionButton("verify_db", "Check Database", class = "btn-info"),
           actionButton("end_now", "End Session Now", class = "btn-warning"),
           br(), br(),
           textOutput("db_status")
    )
  )
)

server <- function(input, output, session) {
  # Initialize telemetry
  user_agent <- session$request$HTTP_USER_AGENT
  telemetry <- create_telemetry(user_agent)
  
  # Display basic telemetry info using textOutput instead of verbatimTextOutput
  output$session_id <- renderText({
    paste("Session ID:", telemetry$session_id)
  })
  
  output$browser_info <- renderText({
    paste("Browser:", telemetry$browser_info)
  })
  
  output$visit_count <- renderText({
    paste("Visit count:", telemetry$visit_count)
  })
  
  # LocalStorage functions
  observeEvent(input$show_storage, {
    session$sendCustomMessage("getLocalStorage", "vividVolcanoVisits")
  })
  
  observeEvent(input$clear_storage, {
    session$sendCustomMessage("clearLocalStorage", "vividVolcanoVisits")
  })
  
  output$storage_value <- renderText({
    if (!is.null(input$storage_value)) {
      paste("localStorage value:", input$storage_value$value)
    } else {
      "Click 'Show localStorage' to see value"
    }
  })
  
  # When visit information is received
  observeEvent(input$telemetry_visit_count, {
    req(input$telemetry_visit_count)
    message("Received visit count: ", input$telemetry_visit_count)
    
    if (!is.null(telemetry)) {
      result <- telemetry$update_visitor_info(
        visit_count = input$telemetry_visit_count
      )
      message("Update visitor result: ", result)
      
      # Update UI
      output$visit_count <- renderText({
        paste("Visit count:", telemetry$visit_count)
      })
    }
  })
  
  # Track button clicks
  observeEvent(input$telemetry_button_click, {
    req(input$telemetry_button_click)
    
    button_type <- input$telemetry_button_click$button
    message("Button click received: ", button_type)
    
    if (!is.null(telemetry)) {
      telemetry$increment_counter(button_type)
      
      # Update counters display
      output$upload_counter <- renderText({
        paste("Upload clicks:", telemetry$upload_count)
      })
      
      output$gsea_counter <- renderText({
        paste("GSEA clicks:", telemetry$gsea_count)
      })
      
      output$volcano_counter <- renderText({
        paste("Volcano clicks:", telemetry$volcano_count)
      })
    }
  })
  
  # Initialize counter displays
  output$upload_counter <- renderText({
    paste("Upload clicks:", ifelse(is.null(telemetry), 0, telemetry$upload_count))
  })
  
  output$gsea_counter <- renderText({
    paste("GSEA clicks:", ifelse(is.null(telemetry), 0, telemetry$gsea_count))
  })
  
  output$volcano_counter <- renderText({
    paste("Volcano clicks:", ifelse(is.null(telemetry), 0, telemetry$volcano_count))
  })
  
  # Database verification
  observeEvent(input$verify_db, {
    output$db_status <- renderText({
      if (is.null(telemetry)) {
        return("Telemetry not initialized")
      }
      
      tryCatch({
        con <- telemetry$get_postgres_connection()
        if (!is.null(con)) {
          # Get current session
          current <- DBI::dbGetQuery(
            con,
            "SELECT visit_count, upload_count, gsea_count, volcano_count FROM app_usage_stats WHERE session_id = $1",
            params = list(telemetry$session_id)
          )
          
          DBI::dbDisconnect(con)
          
          if (nrow(current) > 0) {
            paste("Database record found - Visit count:", current$visit_count, 
                  "| Upload:", current$upload_count,
                  "| GSEA:", current$gsea_count,
                  "| Volcano:", current$volcano_count)
          } else {
            "No record found yet - click buttons first"
          }
        } else {
          "Could not connect to database"
        }
      }, error = function(e) {
        paste("Database error:", e$message)
      })
    })
  })
  
  # Manual session end
  observeEvent(input$end_now, {
    if (!is.null(telemetry)) {
      # End the session
      result <- telemetry$end_session()
      
      # Show results after small delay
      Sys.sleep(1)
      output$db_status <- renderText({
        if (result) {
          "✅ Session ended successfully in database"
        } else {
          "❌ Failed to end session in database"
        }
      })
    }
  })
  
  # End session tracking when user leaves
  session$onSessionEnded(function() {
    if (!is.null(telemetry)) {
      telemetry$end_session()
    }
  })
}

# Run the test app
shinyApp(ui = ui, server = server)