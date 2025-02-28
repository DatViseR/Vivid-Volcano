# Comprehensive test script for telemetry
library(shiny)
library(DBI)
library(RPostgres)
source("telemetry_module.R")

# Test app with telemetry
ui <- fluidPage(
  tags$head(
    tags$script(src = "telemetry.js"),
    # Custom JavaScript for localStorage handling
    tags$script(HTML('
      // Custom message handlers for localStorage testing
      $(document).ready(function() {
        Shiny.addCustomMessageHandler("getLocalStorage", function(id) {
          var value = localStorage.getItem(id);
          Shiny.setInputValue("storage_value", {id: id, value: value});
        });
        
        Shiny.addCustomMessageHandler("clearLocalStorage", function(id) {
          localStorage.removeItem(id);
          Shiny.setInputValue("storage_value", {id: id, value: null, action: "cleared"});
        });
        
        Shiny.addCustomMessageHandler("testTelemetry", function(message) {
          if (window.testTelemetry) {
            window.testTelemetry();
          }
        });
        
        // Override console.log to capture debug messages
        var originalLog = console.log;
        var logs = [];
        
        console.log = function() {
          logs.push(Array.from(arguments).join(" "));
          if (logs.length > 15) logs.shift(); // Keep last 15 logs
          
          // Update Shiny
          if (window.Shiny) {
            Shiny.setInputValue("console_logs", logs.join("\\n"));
          }
          
          // Original behavior
          return originalLog.apply(console, arguments);
        };
      });
    '))
  ),
  titlePanel("Telemetry Test"),
  
  fluidRow(
    column(4,
           h3("Debug Controls"),
           actionButton("show_storage", "Show localStorage", class = "btn-info"),
           actionButton("clear_storage", "Clear localStorage", class = "btn-danger"),
           tags$hr(),
           textOutput("storage_value")
    ),
    column(8,
           h3("Telemetry Status"),
           verbatimTextOutput("telemetry_status"),
           verbatimTextOutput("js_messages")
    )
  ),
  
  tags$hr(),
  
  fluidRow(
    column(6,
           h3("Test Buttons"),
           # Using your exact button definitions
           actionButton("upload", label = HTML('<i class="upload icon"></i> Upload'), 
                        class = "ui primary button"),
           
           actionButton("draw_volcano", "Draw Volcano Plot", 
                        class = "ui primary button", 
                        icon = icon("chart line")),
           
           actionButton(
             inputId = "run_gsea",
             label = HTML('<i class="play icon"></i> Run GSEA'),
             class = "ui primary button"
           ),
           
           tags$hr(),
           
           h3("Click Counters"),
           verbatimTextOutput("click_counts")
    ),
    
    column(6,
           h3("Database Verification"),
           actionButton("verify_db", "Check Database", class = "btn-info"),
           actionButton("end_now", "End Session Now", class = "btn-warning"),
           actionButton("test_direct", "Send Test Event", class = "btn-primary"),
           tags$hr(),
           verbatimTextOutput("db_records")
    )
  )
)

server <- function(input, output, session) {
  # Debug output
  output$js_messages <- renderPrint({
    if (!is.null(input$console_logs)) {
      cat(input$console_logs)
    } else {
      cat("Waiting for JavaScript messages...")
    }
  })
  
  # Initialize telemetry
  user_agent <- session$request$HTTP_USER_AGENT
  telemetry <- create_telemetry(user_agent)
  
  # LocalStorage debug functions
  observeEvent(input$show_storage, {
    session$sendCustomMessage("getLocalStorage", "vividVolcanoVisits")
  })
  
  observeEvent(input$clear_storage, {
    session$sendCustomMessage("clearLocalStorage", "vividVolcanoVisits")
  })
  
  # Track direct test event
  observeEvent(input$test_direct, {
    session$sendCustomMessage("testTelemetry", "")
  })
  
  # When visit information is received
  observeEvent(input$telemetry_visit_count, {
    req(input$telemetry_visit_count)
    
    # Debug
    message("Received visit count: ", input$telemetry_visit_count)
    
    if (!is.null(telemetry)) {
      result <- telemetry$update_visitor_info(
        visit_count = input$telemetry_visit_count
      )
      message("Update visitor result: ", result)
    }
  })
  
  # Track button clicks
  observeEvent(input$telemetry_button_click, {
    req(input$telemetry_button_click)
    
    # Debug
    message("Button click received: ", input$telemetry_button_click$button)
    
    if (!is.null(telemetry)) {
      button_type <- input$telemetry_button_click$button
      telemetry$increment_counter(button_type)
      
      # Update UI immediately
      output$click_counts <- renderPrint({
        if (!is.null(telemetry)) {
          cat("Upload button: ", telemetry$upload_count, "\n")
          cat("GSEA button: ", telemetry$gsea_count, "\n")
          cat("Volcano button: ", telemetry$volcano_count, "\n")
        }
      })
    }
  })
  
  # Display telemetry status
  output$telemetry_status <- renderPrint({
    if (is.null(telemetry)) {
      return("Telemetry not initialized")
    }
    
    cat("Session ID: ", telemetry$session_id, "\n")
    cat("First time user: ", telemetry$visit_count == 1, "\n")
    cat("Visit count: ", telemetry$visit_count, "\n")
    cat("Browser: ", telemetry$browser_info, "\n")
  })
  
  # Display click counts
  output$click_counts <- renderPrint({
    if (is.null(telemetry)) {
      return("Telemetry not initialized")
    }
    
    cat("Upload button: ", telemetry$upload_count, "\n")
    cat("GSEA button: ", telemetry$gsea_count, "\n")
    cat("Volcano button: ", telemetry$volcano_count, "\n")
  })
  
  # Show localStorage value
  output$storage_value <- renderText({
    if (!is.null(input$storage_value)) {
      if (!is.null(input$storage_value$action) && 
          input$storage_value$action == "cleared") {
        return("localStorage cleared")
      } else {
        return(paste("localStorage value:", input$storage_value$value))
      }
    }
    return("Click 'Show localStorage' to see stored visit count")
  })
  
  # Database verification 
  observeEvent(input$verify_db, {
    output$db_records <- renderPrint({
      if (is.null(telemetry)) {
        return("Telemetry not initialized")
      }
      
      tryCatch({
        con <- telemetry$get_postgres_connection()
        if (!is.null(con)) {
          # Get current session
          current <- DBI::dbGetQuery(
            con,
            "SELECT * FROM app_usage_stats WHERE session_id = $1",
            params = list(telemetry$session_id)
          )
          
          # Get recent sessions
          recent <- DBI::dbGetQuery(
            con,
            "SELECT * FROM app_usage_stats ORDER BY session_start DESC LIMIT 5"
          )
          
          DBI::dbDisconnect(con)
          
          cat("==== CURRENT SESSION ====\n")
          if (nrow(current) > 0) {
            print(current)
          } else {
            cat("No record found for current session\n")
          }
          
          cat("\n==== RECENT SESSIONS ====\n")
          if (nrow(recent) > 0) {
            print(recent)
          } else {
            cat("No recent sessions found\n")
          }
        } else {
          cat("Could not connect to database\n")
        }
      }, error = function(e) {
        cat("Error querying database:", e$message, "\n")
      })
    })
  })
  
  # Manual session end
  observeEvent(input$end_now, {
    if (!is.null(telemetry)) {
      # Show final counters
      output$telemetry_status <- renderPrint({
        cat("ENDING SESSION\n")
        cat("Session ID: ", telemetry$session_id, "\n")
        cat("Final counters:\n")
        cat("- Upload: ", telemetry$upload_count, "\n")
        cat("- GSEA: ", telemetry$gsea_count, "\n")
        cat("- Volcano: ", telemetry$volcano_count, "\n")
      })
      
      # End the session
      result <- telemetry$end_session()
      
      # Show results after small delay
      Sys.sleep(1)
      output$db_records <- renderPrint({
        cat("Session end result:", if(result) "Success" else "Failed", "\n\n")
        
        tryCatch({
          con <- telemetry$get_postgres_connection()
          if (!is.null(con)) {
            # Get the session we just ended
            session_data <- DBI::dbGetQuery(
              con,
              "SELECT * FROM app_usage_stats WHERE session_id = $1",
              params = list(telemetry$session_id)
            )
            
            DBI::dbDisconnect(con)
            
            if (nrow(session_data) > 0) {
              cat("Session record after ending:\n")
              print(session_data)
              
              # Check if end time is different from start time
              if (!is.na(session_data$session_end[1]) && 
                  session_data$session_end[1] != session_data$session_start[1]) {
                cat("\n✅ SUCCESS: End time is different from start time\n")
              } else {
                cat("\n❌ FAILURE: End time is same as start time or NULL\n")
              }
              
              # Check counter values
              if (session_data$upload_count[1] == telemetry$upload_count &&
                  session_data$gsea_count[1] == telemetry$gsea_count &&
                  session_data$volcano_count[1] == telemetry$volcano_count) {
                cat("✅ SUCCESS: Counter values correctly saved\n")
              } else {
                cat("❌ FAILURE: Counter values don't match what's in memory\n")
                cat("  Memory: Upload=", telemetry$upload_count, 
                    ", GSEA=", telemetry$gsea_count, 
                    ", Volcano=", telemetry$volcano_count, "\n")
                cat("  DB: Upload=", session_data$upload_count[1], 
                    ", GSEA=", session_data$gsea_count[1], 
                    ", Volcano=", session_data$volcano_count[1], "\n")
              }
            } else {
              cat("No record found for session after ending\n")
            }
          } else {
            cat("Could not connect to database\n")
          }
        }, error = function(e) {
          cat("Error verifying session end:", e$message, "\n")
        })
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