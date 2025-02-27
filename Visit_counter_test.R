# Special test app focused on visit counter
library(shiny)
source("Telemetry_module.R")

ui <- fluidPage(
  tags$head(
    tags$script(src = "telemetry.js"),
    tags$script(HTML('
      // Additional test functions
      $(document).ready(function() {
        Shiny.addCustomMessageHandler("runStorageDiagnostics", function(message) {
          var results = window.telemetryDiagnostics.testStorage();
          Shiny.setInputValue("storage_test_results", results);
        });
        
        Shiny.addCustomMessageHandler("checkVisitCount", function(message) {
          var count = window.telemetryDiagnostics.checkVisitCount();
          Shiny.setInputValue("raw_visit_count", count);
        });
        
        Shiny.addCustomMessageHandler("resetVisitCount", function(message) {
          var success = window.telemetryDiagnostics.resetVisitCount();
          Shiny.setInputValue("reset_result", {success: success, timestamp: new Date().toISOString()});
        });
        
        // Capture console logs
        (function(){
          var oldLog = console.log;
          console.log = function (message) {
            oldLog.apply(console, arguments);
            if (message.toString().includes("[Telemetry]")) {
              Shiny.setInputValue("console_log", {
                message: message,
                time: new Date().toISOString()
              });
            }
          };
        })();
      });
    '))
  ),
  
  titlePanel("Visit Counter Test"),
  
  fluidRow(
    column(6,
           wellPanel(
             h3("Visit Counter Information"),
             verbatimTextOutput("visit_info"),
             hr(),
             actionButton("check_raw", "Check Raw localStorage Value", class = "btn-info"),
             actionButton("test_storage", "Run Storage Diagnostics", class = "btn-primary"), 
             actionButton("reset_counter", "Reset Visit Counter", class = "btn-danger"),
             hr(),
             strong("Test Instructions:"),
             tags$ol(
               tags$li("Note the current visit count"),
               tags$li("Close your browser completely"),
               tags$li("Reopen this app - count should increase"),
               tags$li("If it's not increasing, check browser privacy settings")
             )
           )
    ),
    column(6,
           wellPanel(
             h3("Telemetry Debug Output"),
             verbatimTextOutput("debug_output"),
             hr(),
             h4("Raw Storage Data"),
             verbatimTextOutput("raw_storage"),
             hr(),
             h4("Storage Test Results"),
             verbatimTextOutput("storage_results")
           )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           h3("Database Record"),
           actionButton("check_db", "Check Database Record", class = "btn-info"),
           actionButton("end_session", "End Session", class = "btn-warning"),
           verbatimTextOutput("db_info")
    )
  ),
  
  # Privacy policy explanation
  wellPanel(
    h4("About Browser Storage"),
    p("This app uses localStorage to count your visits. This data is stored only in your browser."),
    p("If localStorage is not working, possible reasons include:"),
    tags$ul(
      tags$li("Private/incognito browsing mode"),
      tags$li("Browser configured to block cookies/storage"),
      tags$li("Browser extension blocking localStorage"),
      tags$li("Browser using anti-tracking features")
    )
  )
)

server <- function(input, output, session) {
  # Initialize telemetry
  telemetry <- create_telemetry(session$request$HTTP_USER_AGENT)
  
  # Store all debug logs
  debug_logs <- reactiveVal(character(0))
  
  # Capture console logs
  observeEvent(input$console_log, {
    current_logs <- debug_logs()
    new_log <- input$console_log$message
    debug_logs(c(current_logs, new_log))
  })
  
  # Display debug logs
  output$debug_output <- renderPrint({
    logs <- debug_logs()
    if (length(logs) > 0) {
      cat(paste(logs, collapse = "\n"))
    } else {
      cat("No telemetry logs yet...")
    }
  })
  
  # When visit information is received
  observeEvent(input$telemetry_visit_count, {
    req(input$telemetry_visit_count)
    
    # Add to debug log
    current_logs <- debug_logs()
    new_log <- paste0("[R] Received visit count from JavaScript: ", input$telemetry_visit_count)
    debug_logs(c(current_logs, new_log))
    
    if (!is.null(telemetry)) {
      telemetry$update_visitor_info(
        visit_count = input$telemetry_visit_count
      )
      
      new_log <- paste0("[R] Updated visitor info in telemetry object: ", telemetry$visit_count)
      debug_logs(c(debug_logs(), new_log))
    }
  })
  
  # Display visit info
  output$visit_info <- renderPrint({
    if (!is.null(telemetry)) {
      cat("Session ID: ", telemetry$session_id, "\n")
      cat("Visit Count: ", telemetry$visit_count, "\n")
      cat("Browser: ", telemetry$browser_info, "\n")
      cat("\n")
      cat("Storage issues detected: ", !is.null(input$telemetry_storage_issue), "\n")
      cat("Last update time: ", format(Sys.time(), "%H:%M:%S"), "\n")
    } else {
      cat("Telemetry not initialized")
    }
  })
  
  # Check raw localStorage
  observeEvent(input$check_raw, {
    session$sendCustomMessage("checkVisitCount", list())
  })
  
  # Show raw localStorage value
  output$raw_storage <- renderPrint({
    if (!is.null(input$raw_visit_count)) {
      cat("Raw localStorage value: \"", input$raw_visit_count, "\"", sep = "")
    } else {
      cat("No raw value available. Click the button to check.")
    }
  })
  
  # Run storage diagnostics
  observeEvent(input$test_storage, {
    session$sendCustomMessage("runStorageDiagnostics", list())
  })
  
  # Show storage test results
  output$storage_results <- renderPrint({
    if (!is.null(input$storage_test_results)) {
      results <- input$storage_test_results
      cat("localStorage available: ", results$available, "\n")
      cat("Can write values: ", results$canWrite, "\n")
      cat("Can read values: ", results$canRead, "\n")
      cat("Can persist values: ", results$canPersist, "\n")
      
      if (!results$available || !results$canWrite || !results$canRead) {
        cat("\nYour browser appears to be blocking localStorage.\n")
        cat("Visit counting will not work properly.\n")
      } else {
        cat("\nYour browser supports localStorage correctly.\n")
        cat("Visit counting should work as expected.\n")
      }
    } else {
      cat("No test results yet. Click the button to test.")
    }
  })
  
  # Reset counter
  observeEvent(input$reset_counter, {
    session$sendCustomMessage("resetVisitCount", list())
  })
  
  # Check database
  observeEvent(input$check_db, {
    output$db_info <- renderPrint({
      if (!is.null(telemetry)) {
        tryCatch({
          con <- telemetry$get_postgres_connection()
          if (!is.null(con)) {
            # Get current session
            query_result <- DBI::dbGetQuery(
              con,
              "SELECT * FROM app_usage_stats WHERE session_id = $1",
              params = list(telemetry$session_id)
            )
            
            DBI::dbDisconnect(con)
            
            if (nrow(query_result) > 0) {
              cat("Database record found:\n\n")
              print(query_result)
            } else {
              cat("No database record found for this session yet.\n")
              cat("This is normal if you haven't ended the session.")
            }
          } else {
            cat("Could not connect to database")
          }
        }, error = function(e) {
          cat("Database error:", e$message)
        })
      } else {
        cat("Telemetry not initialized")
      }
    })
  })
  
  # End session
  observeEvent(input$end_session, {
    if (!is.null(telemetry)) {
      result <- telemetry$end_session()
      
      output$db_info <- renderPrint({
        cat("Session end result:", if(result) "Success" else "Failed", "\n\n")
        
        if (result) {
          tryCatch({
            con <- telemetry$get_postgres_connection()
            if (!is.null(con)) {
              # Get session info
              query_result <- DBI::dbGetQuery(
                con,
                "SELECT * FROM app_usage_stats WHERE session_id = $1",
                params = list(telemetry$session_id)
              )
              
              DBI::dbDisconnect(con)
              
              if (nrow(query_result) > 0) {
                cat("Final session record:\n\n")
                print(query_result)
                
                # Check visit count
                cat("\nVisit count in database:", query_result$visit_count[1], "\n")
                cat("Visit count in memory:", telemetry$visit_count, "\n")
                
                if (query_result$visit_count[1] == telemetry$visit_count) {
                  cat("✅ Visit count correctly saved to database\n")
                } else {
                  cat("❌ Visit count mismatch between memory and database\n")
                }
              } else {
                cat("No database record found (unusual error)")
              }
            }
          }, error = function(e) {
            cat("Database error:", e$message)
          })
        }
      })
    }
  })
  
  # End session when browser closes
  session$onSessionEnded(function() {
    if (!is.null(telemetry)) {
      telemetry$end_session()
    }
  })
}

# Run the test app
shinyApp(ui = ui, server = server)