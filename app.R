
# Set maximum file size limit (e.g., 10MB = 10*1024^2)
options(shiny.maxRequestSize = 40*1024^2)  # Set to 40MB

# LIBRARY SETUP ----

library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(colourpicker)
library(ggrepel)
library(arrow)
library(DT)
library(plotly)
library(Cairo)
library(gt)
library(shiny.semantic)
library(semantic.dashboard)
library(gridExtra)
library(webshot2)
library(shinyalert)  
library(tidyr)
library(data.table)

# TELEMETRY ----

# This telemetry module uses REST API to connect to supabase and send the data to Postgres database
# If you want to use this module, you need to set up the supabase (or similar database) account and get the API key and URL
# If you want to use the app locally this should be commented out

source("./Scripts_R/Telemetry_module_API.R")

message("------TELEMETRY INFO---------")
message("Telemetry module is not commented out in this branch - 
        data is being collected. Add API key to .Renviron file if you want to use your own telemetry credentials. The default variables used by telemetry module are SUPABASE_URL=[url for you database] and 
        SUPABASE_KEY=[API_key]. You can set them in .Renviron file. You can also use direct connection
        to database - check optional module in telemetry_modules folder. If you just want to use the app locally,clone the master branch where the telemetry code should stay commented out.")
        
message("----------------------------")

# Loading the GO data once globally
# The preparation of this file is described in https://github.com/DatViseR/Vivid-GO-data and in the script
# Parquet_GO_source_data_preparation_script.R
# The file is also available in the data folder of this repository
# This newfile contains around 8000 non-obsolete unique GO categories with at least 6 annotated genes in the category
GO <- arrow::read_parquet("GO.parquet2")

# This is the structure of the one main "source of truth" file for GO

# Classes ‘spec_tbl_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	693408 obs. of  4 variables:
#   $ id      : chr  "GO:0003723" "GO:0005515" "GO:0046872" "GO:0005829" ...
# $ name    : chr  "RNA binding" "protein binding" "metal ion binding" "cytosol" ...
# $ gene    : chr  "NUDT4B" "NUDT4B" "NUDT4B" "NUDT4B" ...
# $ ontology: chr  "F" "F" "F" "C" ...
# - attr(*, "spec")=List of 3
# ..$ cols   :List of 4
# .. ..$ id      : list()
# .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
# .. ..$ name    : list()
# .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
# .. ..$ gene    : list()
# .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
# .. ..$ ontology: list()
# .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
# ..$ default: list()
# .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
# ..$ delim  : chr "\t"
# ..- attr(*, "class")= chr "col_spec"

# SOURCING CUSTOM FUNCTIONS FOR VIVID VOLCANO APP----

# source file with custom functions for Vivid Volcano
source("./Scripts_R/Vivid_functions.R")



#------UI----------------------------------------------------------------------


ui <- semanticPage(
  useShinyjs(),
## full screen loader for GSEA ----
  div(
    id = "gsea-loader-overlay",
    style = "display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 200px; height: 200px; background: rgba(0,0,0,0.7); z-index: 9999;",
    div(
      class = "ui active big text loader",
      style = "color: white !important;",
      "Running GSEA analysis..."
    )
  ),
## full screen loader for draw volcano ----
  div(
    id = "volcano-loader-overlay",
    style = "display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 200px; height: 200px; background: rgba(0,0,0,0.7); z-index: 9999;",
    div(
      class = "ui active big text loader",
      style = "color: white !important;",
      "Creaing volcano plots and GO analysis tables..."
    )
  ),
  
## Includes custom CSS and JS files ----
  tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css", 
              href = paste0("custom.css?v=", Sys.time())),
    tags$link(rel = "stylesheet", 
              href = "https://cdn.jsdelivr.net/npm/fomantic-ui@2.9.3/dist/semantic.min.css"),
    tags$script(src = "telemetry.js"),
  tags$script(HTML(
    "
    // Send the current client width to the server
    $(document).on('shiny:connected', function() {
      Shiny.setInputValue('clientWidth', window.innerWidth);
    });
    $(window).on('resize', function() {
      Shiny.setInputValue('clientWidth', window.innerWidth);
    });
   // Toggle behavior for annotations - including click events on labels
    $(document).on('change', '#hide_annot', function() {
      toggleAnnotationState($(this).is(':checked'));
    });

    // Add click handlers for the text labels
    $(document).on('click', '#show-text, #hide-text', function() {
      var isHideText = $(this).attr('id') === 'hide-text';
      var checkbox = $('#hide_annot');
      
      // Update checkbox state
      checkbox.prop('checked', isHideText);
      
      // Trigger change event for Shiny
      checkbox.trigger('change');
      
      // Update visual state
      toggleAnnotationState(isHideText);
      
      // Send value to Shiny explicitly
      Shiny.setInputValue('hide_annot', isHideText);
    });

    // Function to handle state changes
    function toggleAnnotationState(isHidden) {
      if (isHidden) {
        $('#show-text').removeClass('green').addClass('basic');
        $('#hide-text').removeClass('basic').addClass('green');
      } else {
        $('#hide-text').removeClass('green').addClass('basic');
        $('#show-text').removeClass('basic').addClass('green');
      }
    }
  "
  ))
    
    
  ),
## navbar ----  
  segment(class = "navbar",
        img(src = "Vivid_volcano_logo.png", alt = "Logo", class = "logo"),
      div(class = "left-section",
          h1("Vivid Volcano", class = "title"),
          h4("Publication-ready volcano plots and GO analysis with ease", class = "subtitle")
      ),
      div(class = "social-buttons",
          h5("Source Code", class = "icon-header"),  
          a(href = "https://github.com/DatViseR",
            icon("github big"),
            class = "github",
            target = "_blank"
          ),
          h5("Developer", class = "icon-header"),
          a(href = "https://www.linkedin.com/in/tomasz-st%C4%99pkowski/",
            icon("linkedin big"),
            class = "linkedin",
            target = "_blank"
          ),
          h5("Buy me a coffee?", class = "icon-header"),
          a(href = "https://buymeacoffee.com/datviser",
            icon("fa-solid fa-mug-hot"),
            class = "coffe",
            target = "_blank"
          ),
          
      )
  ),

## sidebar and main layout ----  
  div(class = "ui fluid container",
      # Main layout using sidebar
      sidebar_layout(
        # Sidebar panel with controls
        sidebar_panel(
                      width = 3,
                      
### Data Upload Segment ----
                      div(class = "ui raised segment",
                          
                         
                          header(title = "Upload your data", description = "", icon = "upload"),
                          div(class = "ribbon-container",
                              div(class = "ui grey ribbon label", 
                                  "Upload a CSV or TSV file"),
                             ),
                          div(class = "tooltip-container",
                              tags$i(class = "info circle icon info-icon"),
                              div(class = "tooltip-text",
                                  HTML("
    <div style='line-height: 1.4; max-width: 400px;'>
        <strong style='color: #2185D0;'>Accepted file formats:</strong><br>
        - CSV (comma or semicolon separated values)<br>
        - TSV (tab-separated values)<br>
        <strong style='color: #2185D0;'>Upload an omics dataset in which a single gene/protein is an observation</strong><br>
        (must contain gene names!)<br>
        <strong style='color: #2185D0;'>Performs many data curations</strong><br>
        (always displays an info in case data is modified!)<br>
    </div>
")
                              )
                          ),
                          
                          
                          div(class = "ui file input", 
                              file_input("file1", 
                                         label = paste0("Maximum file size: 40MB"),
                                         accept = c(".csv", ".tsv"))
                          ),
                          
####  download link for demo data ----
                        
                          div(class = "download-demo",
                              style = "display: flex; justify-content: center; align-items: center;",  # Removed padding here as it's handled in CSS
                              tags$a(
                                href = "demo_data.csv",
                                download = NA,
                                class = "ui labeled icon button compact",
                                style = "box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: all 0.2s ease;",
                                tags$i(class = "download icon", 
                                       style = "margin-right: 0.5em !important;"),
                                span(
                                  "Download demo data",
                                  style = "margin-right: 0.5em;"
                                ),
                                span(
                                  "(separator:tab,decimal:comma)",
                                  style = "font-size: 0.9em; opacity: 0.8;"
                                )
                              )
                          ),
                        
#### layout for checkbox and radio buttons ----
                          div(class = "ui form",
                              style = "margin: 0.2rem 0;",  # Reduced vertical margin
                              div(class = "three fields",
                                  style = "margin: 0.2rem !important; gap: 0.5rem !important;",  # Reduced margin and gap between fields
                                  # Header Checkbox
                                  div(class = "field",
                                      style = "margin: 0 !important;",  # Remove default field margin
                                      div(style = "display: flex; flex-direction: column; gap: 10px;",  # Reduced gap
                                          div(style = "font-weight: bold; margin-bottom: 1px;", "Header"),  # Reduced margin
                                          shiny.semantic::toggle("header", "", is_marked = TRUE)
                                      )
                                  ),
                                  # Separator Radio Buttons
                                  div(class = "field",
                                      style = "margin: 0 !important;",  # Remove default field margin
                                      multiple_radio(class = "radio compact", 
                                                     "sep", 
                                                     "Separator", 
                                                     choices = list("Comma" , 
                                                                    "Semicolon", 
                                                                    "Tab"), 
                                                     choices_value = c(",", ";", "\t"),
                                                     selected = ",")
                                  ),
                                  # Decimal Point Radio Buttons
                                  div(class = "field",
                                      style = "margin: 0 !important;",  # Remove default field margin
                                      multiple_radio("dec", 
                                                     "Decimal Point", 
                                                     choices = list("Dot" , 
                                                                    "Comma"), 
                                                     choices_value = c(".", ","),
                                                     selected = ".")
                                  )
                              )
                          ),
                          
                          actionButton("upload", label = HTML('<i class="upload icon"></i> Upload'), 
                                       class = "ui primary button")
                      ),
                      
### Column Selection Segment ----
                      uiOutput("column_select_ui"),
                      
                      
                      
### Analysis Options ----
                      div(class = "ui raised segment",
                           # ribbon
                           header(title = "Analysis Options", description = "Customize GSEA and volcano plot options",icon = "cogs"),
                           div(class = "ui grey ribbon label", "Customize p value adjustment"),
                          div(class = "tooltip-container",
                              style = "display: inline-block;", 
                              tags$i(class = "info circle icon info-icon"),
                              div(class = "tooltip-text",
                                  style = "position: absolute; z-index: 100;", 
                                  HTML("
    <div style='line-height: 1.4; max-width: 400px;'> <!-- Add max-width to control tooltip size -->
        <strong style='color: #2185D0;'>Why Adjust P-values?</strong>
        <div style='margin: 8px 0; font-size: 0.9em;'>
            Multiple testing increases false positive risk - when testing many hypotheses, some will appear significant by chance alone.
        </div>

        <strong style='color: #2185D0;'>Available Methods:</strong>
        <ul style='margin: 8px 0; padding-left: 20px;'>
            <li><strong>Benjamini-Hochberg (BH)</strong>: Controls false discovery rate (FDR), balances power and false positives</li>
            <li><strong>Benjamini-Yekutieli (BY)</strong>: More conservative than BH, makes no assumptions about dependencies</li>
            <li><strong>Hochberg</strong>: Less conservative than Bonferroni, controls family-wise error rate</li>
            <li><strong>Bonferroni</strong>: Most conservative, strongly controls family-wise error rate</li>
            <li><strong>None</strong>: Unadjusted p-values, high false positive risk</li>
        </ul>
        <div style='font-size: 0.9em; color: #666; margin-top: 8px;'>
            <i>Recommendation:</i> BH is suitable for most analyses. BY provides more conservative control but at the cost of statistical power.Use BY if you have, for example, RNA‐sequencing data from heterogeneous tumor samples where unpredictable, complex gene co-expression patterns create unknown dependencies among tests, necessitating robust FDR control despite reduced power. </div>
    </div>
")
                              )
                          ),
#### p value adjustment controls ----                           
                           dropdown_input("adj",
                                          choices = c("None",
                                                      "Bonferroni",
                                                      "Hochberg",
                                                      "Benjamini-Hochberg",
                                                      "Benjamini-Yekutieli"),
                                          
                                          choices_value = c("none", "bonferroni", "hochberg", "BH", "BY"),
                                          value = "BH"),
                           numericInput("alpha", "Significance Threshold", value = 0.050, min = 0.0001, max = 1, step = 0.0001,
                                                                               ),
                           
                           div(class = "ui grey ribbon label", "GSEA analysis controls"),
                          div(class = "tooltip-container",
                              tags$i(class = "info circle icon info-icon"),
                              div(class = "tooltip-text",
                                  HTML("
    <div style='line-height: 1.4;'>
        <strong style='color: #2185D0;'>GO Term Selection Criteria for Gene Set Enrichment analysis (GSEA):</strong>
        <ul style='margin: 8px 0; padding-left: 20px;'>
            <li>Terms must contain 5-500 genes detected in your experiment</li>
            <li>At least 5% of genes in each term must be detected in your data</li>
            <li>Filtered by selected ontology category</li>
        </ul>
        <div style='font-size: 0.9em; color: #666;'>
            This ensures meaningful and statistically relevant GO terms for your analysis.
        </div>
    </div>
")

                              )
                          ),
                           div(class = "ui form",
                               toggle("GSEA_acvited", "I want to run GSEA", FALSE)
#### GSEA controls ----                              
                               ), uiOutput("gsea_controls_ui"),
                          
                           
#### Plot Options Card ----
                           div(class = "ui grey ribbon label", "Customize volcano plot annotations")  ,
                           toggle("color_highlight", "Color significantly regulated genes", FALSE),
                           uiOutput("color_highlight_ui"),
                           toggle("show_go_category", "Visualize GO Categories", FALSE),
                           uiOutput("go_category_ui"),
                           uiOutput("color_picker_ui"),
                           numericInput("num_labels", "Number of Gene Labels (0-100)", 
                                        value = 10, min = 0, max = 100),
                           toggle("trim_gene_names", "Trim Multiplied Gene Names to First Occurrence", TRUE),
                           toggle("select_custom_labels", "Label your choosen genes", FALSE),
                           uiOutput("custom_gene_labels_ui"),
                          div(class = "ui grey ribbon label", 
                              style = "margin-bottom: 0rem !important;",  # Reduced margin for first ribbon
                              "Customize plot title"),
                          textInput("plot_title", "", "Vivid Volcano"),
                          
                          div(class = "ui grey ribbon label", 
                              style = "margin-bottom: 0rem !important;",  # Reduced margin for second ribbon
                              "Customize X -axis label"),
                          textInput("x_axis_label", "", 
                                    "Log2 Fold Change (Condition X vs. Condition Y)"),
                          
                          actionButton("draw_volcano", "Draw Volcano Plot", 
                                       class = "ui primary button", 
                                       icon = icon("chart line icon")),
                                        
                     uiOutput("download_log_ui")
                      ),
        ),
## Main panel ----        
        main_panel(
          width = 12,
## Dataset preview ----
          segment(
            class = "raised",
            div(class = "ui grey ribbon label", "State of data source preview"),
            semantic_DTOutput("dataset_summary", height = "auto")
          ),
## Results ----
          segment(
            class = "placeholder",
            header(title = "Results", description = "", icon = "fa-solid fa-square-poll-vertical"),
            uiOutput("dynamic_tabset")
          )
        )
      )
  )
)



# SERVER----

server <- function(input, output, session) {
  
## Telemetry ----

# If you don not want to use telemetry clone the master branch or comment out the code below. To use telemetry create a local
#  .Renviron file with your API key and URL for your database  credentials for direct connection
# If you just want to use the app locally, the code below should stay commented out  
  
  # Initialize telemetry
  user_agent <- session$request$HTTP_USER_AGENT
  telemetry <- create_telemetry(user_agent)

  # When visit information is received from browser local storage
  observeEvent(input$telemetry_visit_count, {
    req(input$telemetry_visit_count)

    if (!is.null(telemetry)) {
      telemetry$update_visitor_info(
        visit_count = input$telemetry_visit_count
      )
    }
  })

  # Track button clicks
  observeEvent(input$telemetry_button_click, {
    req(input$telemetry_button_click)

    if (!is.null(telemetry)) {
      button_type <- input$telemetry_button_click$button
      telemetry$increment_counter(button_type)
    }
  })

  # End session tracking when user leaves
  session$onSessionEnded(function() {
    if (!is.null(telemetry)) {
      telemetry$end_session()
    }
  })


## Reactive values----  
  
  # Data source and plots
  uploaded_df <- reactiveVal()

  regulated_sets <- reactiveVal(NULL)
  
  volcano_plot_rv <- reactiveVal()
  
  volcano_plot_original <- reactiveVal()
  
  # logging system
  log_messages <- reactiveVal("")
  #display
  is_mobile <- reactiveVal(FALSE)
 
  #analysis
  gsea_results <- reactiveVal(NULL)
  
  gsea_filtered_results <- reactiveVal(NULL)
  
  # chosen ontology used for creation of non-reactive title after GSEA was run
  plotOntologyValue <- reactiveVal("P")
  # State management
  column_select_module_state <- reactiveVal("initial")
  # to prevent multiple triggers of cancel button
  reset_input <- debounce(reactive(input$reset_columns), 500)
  
#Creates session variables at server start
  session_id <- substr(digest::digest(session$token), 1, 6)
  session_start_time <- Sys.time()
  
  # Creates the logging functions with session context
  log_event <- create_logger(session)
  log_structure <- create_structure_logger(session)
  

## Screen width observer ----  
  # Immediate logging of initial display state
observeEvent(input$clientWidth, {
    req(input$clientWidth)  # Ensure the value is available
    current_is_mobile <- input$clientWidth <= 800
    is_mobile(current_is_mobile)
    log_event(log_messages, 
              sprintf("Browser window size: %dpx (%s view)", 
                      input$clientWidth,
                      if(current_is_mobile) "MOBILE" else "DESKTOP"), 
              "INFO display initialization")
  }, once = TRUE)
  
  # Monitor for changes in window size
  observeEvent(input$clientWidth, {
    req(input$clientWidth)
    current_is_mobile <- input$clientWidth <= 800
    previous_is_mobile <- isolate(is_mobile())
    if (current_is_mobile != previous_is_mobile) {
      is_mobile(current_is_mobile)
      log_event(log_messages, 
                sprintf("Display changed to %s view (width: %dpx)", 
                        if(current_is_mobile) "MOBILE" else "DESKTOP",
                        input$clientWidth), 
                "INFO display change")
    }
  })
  
# Upload observer ---- 
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    # Instaed of read_delim which was slow I introduced fread from data.table but to maintain consistency with the rest
    #of the code the df is saved as standard data frame not data.table. Also multithreading for upload was introduced.  
    df <- data.table::fread(
      in_file$datapath,
      sep = input$sep,
      header = input$header,
      dec = input$dec,
      data.table = FALSE,
      na.strings = c("NA", ""), #   I added this after testing as fread parsed some of the test trailing columns as empty strings...
      nThread = min(4, parallel::detectCores()-1)  # Use multiple cores, but not all
    )
    uploaded_df(df)
    # create log event for successful initialization of reactive values
    
    log_event(log_messages, "Reactive value uploaded_df initialized successfully", "INFO from upload observer")   
    
    # Log the structure of the uploaded dataset
    log_structure(log_messages, df, "The structure of the uploaded dataset is:", "INFO from upload observer")
    log_event(log_messages, "Dataset uploaded successfully", "SUCCESS from upload observer")
 
       

    # Run combined diagnostic and cleaning
    df_cleaned <- diagnose_and_clean_data(
      df = df,
      log_messages_rv = log_messages,
      log_event = log_event,
      log_structure = log_structure
    )
    
    # Update if cleaning returns result
    if (!is.null(df_cleaned)) {
      uploaded_df(df_cleaned)
    }
    
   log_structure(log_messages, df, "The structure of the uploaded dataset after 1st diagnostic preprocesing:", "INFO from upload observer")
   column_select_module_state("reseted")
   
## Reactive column select UI ----
    output$column_select_ui <- renderUI({
      if (is.null(df)) return(NULL)
      # Log event to indicate that the UI has been rendered
      log_event(log_messages, "Reactive UI for column selection rendered", "INFO from output$column_select_ui")
      div(class = "ui raised segment",
          div(class = "ui grey ribbon label", "Select Data"),
          div(class = "tooltip-container",
              tags$i(class = "info circle icon info-icon"),
              div(class = "tooltip-text",
                  HTML("
    <div style='line-height: 1.4; max-width: 400px;'>
        <h4 style='color: #2185D0; margin-bottom: 2px;'><strong>Upload crucial columns for all observations:</strong></h4>
        <ul style='margin: 8px 0; padding-left: 20px;'>
            <li style='margin-bottom: 12px;'>
                <i class='circle icon' style='color: #2185D0;'></i>
                <strong>Raw p-values</strong>
                <div style='margin-left: 20px; color: #2185D0; font-size: 0.9em;'>
                    Automatically detects and handles log-transformed values
                </div>
            </li>
            <li style='margin-bottom: 12px;'>
                <i class='circle icon' style='color: #2185D0;'></i>
                <strong>Log2 fold expression difference</strong>
            </li>
            <li style='margin-bottom: 12px;'>
                <i class='circle icon' style='color: #2185D0;'></i>
                <strong>Gene names</strong>
                <div style='margin-left: 20px; color: #2185D0; font-size: 0.9em;'>
                    Supports both human and mice nomenclature
                </div>
            </li>
        </ul>
        <div style='margin-top: 20px; padding: 10px; border-left: 3px solid #2185D0; border-radius: 3px;'>
            <strong>Additional Features:</strong>
            <div style='color: #2185D0; margin-top: 5px; font-size: 0.9em;'>
                Handles missing data and provides notifications for dataset modifications
            </div>
        </div>
    </div>
")

              )
          ),
          selectInput("pvalue_col", "Select p-value column", choices = names(df)),
          selectInput("fold_col", "Select regulation column - log2(fold)", choices = names(df)),
          selectInput("annotation_col", "Select gene symbols column", choices = names(df)),
          # Add buttons in a button group
          div(class = "ui two buttons",
              div(
                id = "upload_check",
                class = "ui animated fade button primary",
                type = "button",
                onclick = "Shiny.setInputValue('upload_check', Math.random(), {priority: 'event'})",  # Use random value to ensure new trigger
                div(class = "visible content", "Upload and Check Columns"),
                div(class = "hidden content", icon("check"))
              ),
              div(
                id = "reset_columns",
                class = "ui animated fade button negative",
                type = "button",
                onclick = "Shiny.setInputValue('reset_columns', true, {priority: 'event'})",
                div(class = "visible content", "Reset Selection"),
                div(class = "hidden content", icon("undo"))
              )
          )
      )
      
    })


   #test
   
## Column upload observer ----   
       
   # 2. COLUMN CHECK OBSERVER
   observeEvent(input$upload_check, {
     req(input$upload_check)
     req(input$pvalue_col, input$fold_col, input$annotation_col)
     
     # Only proceed if state allows validation
     if (column_select_module_state() %in% c("initial", "reseted")) {
       log_event(log_messages, 
                 sprintf("Starting column check at %s - State: %s", 
                         format(Sys.time(), "%H:%M:%S.%OS3"),
                         column_select_module_state()), 
                 "DEBUG")
       
       results <- isolate({
         diagnose_input_columns_and_remove_NA(
           df = uploaded_df(),
           pvalue_col = input$pvalue_col,
           fold_col = input$fold_col,
           annotation_col = input$annotation_col,
           log_messages_rv = log_messages,
           log_event = log_event
         )
       })
       
       # Update data and state
       uploaded_df(results$cleaned_data)
       column_select_module_state("validated")
       
       # Update UI
       runjs('
                document.getElementById("upload_check").classList.remove("primary");
                document.getElementById("upload_check").classList.add("positive");
                document.querySelector("#upload_check .visible.content").textContent = "Columns Checked ✓";
            ')
       
       # Log results
       log_event(log_messages, 
                 sprintf("Columns selected and checked. Removed %d rows with NAs", 
                         results$statistics$dropped_rows), 
                 "SUCCESS from upload_check")
       
     } else {
       log_event(log_messages, 
                 sprintf("Column check not needed - Columns already validated (current state: %s)", 
                         column_select_module_state()), 
                 "DEBUG")
     }
   }, ignoreInit = TRUE)
   
   # 3. IMPROVED RESET COLUMNS OBSERVER
   observeEvent(reset_input(), {
     req(reset_input())
     
     isolate({
       current_state <- column_select_module_state()
       
       # Log start of reset
       log_event(log_messages, 
                 sprintf("Column selection reset initiated from state: %s", 
                         current_state), 
                 "INFO from reset_columns")
       
       # Update state
       column_select_module_state("reseted")
       
       # Reset UI elements
       updateSelectInput(session, "pvalue_col", selected = character(0))
       updateSelectInput(session, "fold_col", selected = character(0))
       updateSelectInput(session, "annotation_col", selected = character(0))
       
       runjs('
                document.getElementById("upload_check").classList.remove("positive");
                document.getElementById("upload_check").classList.add("primary");
                document.querySelector("#upload_check .visible.content").textContent = "Upload and Check Columns";
            ')
       
       # Log completion
       log_event(log_messages, 
                 sprintf("Column selections reset completed (from %s to reseted)", 
                         current_state), 
                 "SUCCESS from reset_columns")
     })
   }, ignoreInit = TRUE)
   
   # 4. STATE MONITOR
   observeEvent(column_select_module_state(), {
     log_event(log_messages,
               sprintf("State transition: %s at %s", 
                       column_select_module_state(),
                       format(Sys.time(), "%H:%M:%S.%OS3")),
               "DEBUG state_monitor")
   }, ignoreInit = TRUE)
   
   # 5. NEW DATA UPLOAD HANDLER
   observeEvent(input$upload, {
     column_select_module_state("initial")
     reset_timer(NULL)
     log_event(log_messages, 
               "State reset to initial due to new data upload", 
               "INFO")
   }, ignoreInit = TRUE)

   

   
## Render DT data preview ----    
   
    output$dataset_summary <- renderDT({
      log_event(log_messages, "Rendering dataset summary table", "INFO from output$dataset_summary")
      
      table <- semantic_DT(
        data.frame(uploaded_df(), check.names = FALSE), # Convert to data.frame if not already
        options = list(
          responsive = TRUE,
          pageLength = 3,
          dom = 'lftp',
          lengthMenu = list(c(1, 3, 5, 10), c('1','3', '5', '10')),
          rownames = FALSE,
          scrollX = TRUE,
          columnDefs = list(list(
            targets = '_all',  # Apply to all columns
            className = 'dt-nowrap'  # Add nowrap class
          ))
        ),
        style = "semanticui",
        class = "ui small compact table",
        selection = 'none'  # Disable row selection if needed
      )
      
      if (!is.null(table)) {
        log_event(log_messages, "Dataset summary table created successfully", "INFO from output$dataset_summary")
        # check the structure of the table  
        log_structure(log_messages, table, "The structure of the dataset summary table is:\n")
      } else {
        log_event(log_messages, "Failed to create dataset summary table", "ERROR from output$dataset_summary")
      }
      
      table
    })
      
  })
  
# REACTIVE UI FOR OPTIONAL GSEA ANALYSIS ----
  
  output$gsea_controls_ui <- renderUI({
    if (input$GSEA_acvited) {
      div(class = "ui segment custom-segment",
          div(class = "ui stackable grid",
              div(class = "row",
                  # First column: Gene Set Selection with Multiple Radio Buttons
                  div(class = "eight wide column",
                      div(class = "field",
                          multiple_radio(
                            "gsea_ontology",
                            label = HTML("<strong>Ontology</strong>"),
                            choices = list(
                              "Cellular Component",
                              "Molecular Function",
                              "Biological Process"
                            ),
                            choices_value = c(
                              "C",  # Changed from "CC"
                              "F",  # Changed from "MF"
                              "P"   # Changed from "BP"
                            ),
                            selected = "P"
                          )
                      )
                  ),
                    
                  
                  # Second column: Action Button (Centered)
                  div(class = "eight wide column",
                      div(class = "ui container", style = "position: relative; min-height: 50px;",
                          # Button
                          actionButton(
                            inputId = "run_gsea",
                            label = HTML('<i class="play icon"></i> Run GSEA'),
                            class = "ui primary button"
                          ),
                          
                      )
                  )
              )
          )
      )
 
    }
  })
  
## Reactive 4 tabset appearing if GSEA is activated and 3 tabset if not ----
  
  # Add these to your server function
  tab_list <- reactive({
    # Define base tabs that are always present
    base_tabs <- list(
      list(
        menu = "Static Volcano Plot and GO enrichment table",
        id = "static_volcano",
        content = div(
          div(class = "ui two column grid",
              # First column (50%) - Plot and Downloads
              div(class = "column",
                  
                  segment(
                    class = "basic",
                    plotOutput("volcano_plot", width = "100%", height = "600px")
                  ),uiOutput("x_limits_ui"),
                  segment(
                    class = "basic",
                    h4(class = "ui header", "Download Plots"),
                    div(
                      class = "ui tiny fluid buttons",
                      downloadButton("download_plot1", "85x85mm (1 col)", class = "ui button"),
                      downloadButton("download_plot2", "114x114mm (1.5 col)", class = "ui button"),
                      downloadButton("download_plot3", "114x65mm (landscape)", class = "ui button")
                    ),
                    div(
                      style = "margin-top: 10px;",
                      class = "ui tiny fluid buttons",
                      downloadButton("download_plot4", "174x174mm (square)", class = "ui button"),
                      downloadButton("download_plot5", "174x98mm (landscape)", class = "ui button")
                    )
                  )
              ),
              # Second column (50%) - GO Table
              div(class = "column",
                  segment(
                    class = "basic",
                    h4(class = "ui header", "Download GO Enrichment Table"),
                    div(
                      class = "ui tiny fluid buttons",
                      downloadButton("download_go_enrichment", "Download GO enrichment table", class = "ui button")
                    ),
                    gt_output("go_enrichment_gt")
                  )
              )
          )
        )
      ),
      list(
        menu = "Interactive Volcano Plot",
        id = "interactive_volcano",
        content = div(
          plotlyOutput("volcano_plotly", width = "800px", height = "740px")
        )
      ),
      list(
        menu = "GO Category Details",
        id = "go_category",
        content = div(
          segment(
            class = "basic",
            h4(class = "ui header", "Download GO Gene List Table"),
            div(
              class = "ui tiny fluid buttons",
              downloadButton("download_go_gene_list", "Download GO gene lists", class = "ui button")
            ),
            gt_output("go_gene_list_gt")
          )
        )
      )
    )
    
    ## Add GSEA tab conditionally ----
    if (isTRUE(input$GSEA_acvited)) {
      gsea_tab <- list(
        menu = "GSEA Results",
        id = "gsea_results_tab",
        content = div(
          segment(
            class = "basic",
            h4(class = "ui header", "GSEA Analysis Results"),
            div(
              class = "ui tiny fluid buttons",
              downloadButton("reg_gene_list", "Download regulated genes lists", class = "ui button"),
              downloadButton("download_full_gsea", "Full GSEA Results" , class = "ui button"),
              downloadButton("download_top_gsea", "Top 10 significant GSEA results", class = "ui button"),
              downloadButton("download_top10_gsea", "Top 10 GSEA Results(inc. nonsig)", class = "ui button")
            ),
            
            
            div(class = "ui grid stackable mobile reversed",
                # First column (9/16)
                div(class = "nine wide computer wide tablet sixteen wide mobile column",
                    div(class = "ui segment basic",
                        div(class = "segment-header",
                            h4(class = "ui header", "GSEA Enrichment Plot"),
                            downloadButton("download_gsea_plot", "Download GSEA Plot", class = "ui tiny button")
                        ),
                        # Responsive controls aligned together in a single row using a semantic stackable grid.
                        div(
                          class = "ui stackable grid",
                          div(
                            class = "row",
                            # Column for radio buttons (Which category to show)
                            div(
                              class = "four wide computer four wide tablet sixteen wide mobile column",
                              multiple_radio(
                                input_id = "plot_category",
                                label = "Which category to show:",
                                choices = c(
                                  "bidirectional" = "bidirectional",
                                  "upregulated" = "up",
                                  "downregulated" = "down"
                                ),
                                selected = "up"
                              )
                            ),
                            # Column for toggle: Hide non-significant results
                            div(
                              class = "three wide computer three wide tablet sixteen wide mobile column",
                              div(style = "margin-top: 22px;",
                                  toggle("hide_nonsig", "Hide non-significant results", FALSE)
                              )
                            ),
                            # Column for text input and action button in vertical layout
                            div(
                              class = "five wide computer five wide tablet sixteen wide mobile column",
                              div(
                                # Text input on top
                                div(style = "margin-bottom: 5px;",
                                    textInput("gsea_filter_pattern", "Filter redundant GO terms", placeholder = "Input redundant text")
                                ),
                                # Action button below text input
                                div(
                                  actionButton("apply_filter", "Apply Filter", class = "ui blue button")
                                )
                              )
                            ),div(class = "tooltip-container",
                                  style = "display: inline-block;",  # Prevents layout breaking
                                  tags$i(class = "info circle icon info-icon"),
                                  div(class = "tooltip-text",
                                      style = "position: absolute; z-index: 100;",  # Ensures tooltip does not disrupt layout
                                      HTML("
<div style='line-height: 1.4; max-width: 400px;'>
    <strong style='color: #2185D0;'>Filtering Redundant Terms</strong>
    <div style='margin: 8px 0; font-size: 0.9em;'>
        This filtering may be aplied when there are many redundant terms in the top results.
        It preserves the top hit that matches the provided text pattern, removing other entries matching this pattern,
        and then rearranges the results accordingly by original adjusted p value.For example typing `ribosom` would 
        affect terms containing `ribosomal`, `ribosome`, `ribosomal subunit`, `ribosomal biogenesis` etc. This is purely semantic filter that 
        helps to achieve meaningful plots and table. It does not affect the full GSEA results that you can download separately.You can 
        come back to the original results by switching toogle on the right.
    </div>
</div>
")
                                  )
                            ),
                            # Column for conditional toggle: Show original results (appears after apply_filter is clicked)
                            div(
                              class = "three wide computer three wide tablet sixteen wide mobile column",
                              conditionalPanel(
                                condition = "input.apply_filter > 0",
                                div(style = "margin-top: 22px;",
                                    toggle("show_original", "Switch back to nonfiltered results", FALSE)
                                )
                              )
                            )
                          )
                        ),
                        div(class = "segment-content",
                            div(class = "mobile-shrink-plot",
                             
                                plotOutput("gsea_plot")
                            )
                        )
                    )
                ),
                # Second column (7/16)
                div(class = "seven wide computer wide tablet sixteen wide mobile column",
                    div(class = "ui segment basic",
                        div(class = "segment-header",
                            # Header and download button stacked vertically
                            h4(class = "ui header", "GSEA Results Table"),
                            downloadButton("download_gsea_results", "Download GSEA Table", 
                                           class = "ui tiny button")
                        ),
                        div(class = "segment-content",
                            gt_output("gsea_results_table")
                        )
                    )
                )
            )
          )
        )
      )
      # Return combined tabs with GSEA tab first
      c(list(gsea_tab), base_tabs)
    } else {
      # Return only base tabs
      base_tabs
    }
  })
  
  ### Render the dynamic tabset ----
  
  output$dynamic_tabset <- renderUI({
    tabset(
      tabs = tab_list(),
      id = "dynamic_tabset",
      active = "Static Volcano Plot and GO enrichment table"  # Use the menu text
    )
  })
  
  
  
## Run GSEA observer ---- 
  observeEvent(input$run_gsea, {
    # First validate if columns are checked
    if (is.null(input$upload_check)) {
      shinyalert(
        title = "Column Check Required",
        text = "Please check your column selections first",
        type = "warning"
      )
      return()
    }
    
    # Then validate all other requirements
    validate <- try({
      req(
        input$GSEA_acvited,
        input$gsea_ontology,
        uploaded_df(),
        input$annotation_col,
        input$fold_col,
        input$alpha
      )
    })
    
    if (inherits(validate, "try-error")) {
      shinyalert(
        title = "Missing Requirements",
        text = "Please ensure all required fields are filled",
        type = "warning"
      )
      return()
    }
    
    # If all validations pass, proceed with GSEA
    shinyjs::show("gsea-loader-overlay") 
    
    update_tabset(session, 
                  "dynamic_tabset", "gsea_results_tab") 
                 
    
    plotOntologyValue(input$gsea_ontology)
    
    # Log the GSEA analysis start
    log_event(log_messages, 
              sprintf("Starting GSEA analysis for all gene sets with ontology: %s", 
                      input$gsea_ontology),
              "INFO from GSEA observer")
    
    # Get the current data frame
    df <- uploaded_df()
    
    # Get all detected genes (all non-NA genes in the annotation column)
    detected_genes <- df[[input$annotation_col]] %>%
      clean_gene_names(., log_messages, log_event)
    
    # Log the number of detected genes
    log_event(log_messages,
              sprintf("Found %d total detected genes", length(detected_genes)),
              "INFO from GSEA observer")
    
    # Validate we have detected genes
    if (length(detected_genes) == 0) {
      shinyalert(
        title = "No Genes Found",
        text = "No valid genes found in the selected annotation column",
        type = "error"
      )
      
      shinyjs::hide("gsea-loader-overlay")
      return()
    }
    
    ### Check unlog and adjust p-values before GSEA ----
    df <- check_and_unlog_pvalues(df, input$pvalue_col, log_messages, log_event)
    uploaded_df(df)  # Update the reactive value with unlogged p-values
    log_structure(log_messages, df, "The structure of the uploaded dataset after unlogging p-values is:", "INFO pvalues module")
    
    # Adjust p-values
    pvalues <- df[[input$pvalue_col]]
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    log_event(log_messages, paste(input$adj, "method choosen for p-value adjustment", "INFO adjusting pvalues"))
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)  # Ensure reactive value is updated
    log_event(log_messages, "Unlogging and adjusting p-values completed", "SUCCESS")
    log_structure(log_messages, df, "The structure of the uploaded dataset after adjusting p-values is:", "INFO")
    
    ### Define gene sets based on regulation ----
    regulated_sets <- list(
      up = df %>%
        filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>%
        pull(!!sym(input$annotation_col)) %>%
        clean_gene_names(., log_messages, log_event),
      
      down = df %>%
        filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>%
        pull(!!sym(input$annotation_col)) %>%
        clean_gene_names(., log_messages, log_event),
      
      bidirectional = df %>%
        filter(adjusted_pvalues < input$alpha) %>%
        pull(!!sym(input$annotation_col)) %>%
        clean_gene_names(., log_messages, log_event)
    )
    
    regulated_sets(regulated_sets)  # stored in the reactive for future use 
    
    # Log gene set counts
    log_event(log_messages,
              sprintf("Gene set counts:\nUpregulated: %d\nDownregulated: %d\nTotal regulated: %d",
                      length(regulated_sets$up),
                      length(regulated_sets$down),
                      length(regulated_sets$bidirectional)),
              "INFO from GSEA observer")
    
    # Validate we have genes in any set
    if (all(sapply(regulated_sets, length) == 0)) {
      shinyalert(
        title = "Empty Gene Sets",
        text = sprintf("No regulated genes found with current threshold (alpha = %g)",
                       input$alpha),
        type = "warning"
      )
     
      shinyjs::hide("gsea-loader-overlay")
      return()
    }
    
  
    # Until this part the code was adjusted and simplified 
    # we have gene list with regulated and cleaned genes
    # now we need to filter GO data (number of detected genes having annotations
    # for a particular ontology) and run the enrichment analysis
    
    
    ## Filtering of the source GO data to make the computations more efficient ----
    
    # More efficient GO filtering - first filter by detected genes and ontology
    # Calculate coverage and filter GO terms
    go_filtered <- GO %>%
      # First get total counts per GO term before filtering
      group_by(name) %>%
      mutate(
        total_genes_in_term = n_distinct(gene)
      ) %>%
      ungroup() %>%
      # Then filter by ontology and detected genes
      filter(ontology == input$gsea_ontology) %>%
      # Calculate detected genes and coverage per term
      group_by(name) %>%
      mutate(
        detected_genes_in_term = n_distinct(intersect(gene, detected_genes)),
        category_coverage = detected_genes_in_term / total_genes_in_term
      ) %>%
      ungroup() %>%
      # Apply coverage and size filters
      filter(
        gene %in% detected_genes,
        total_genes_in_term >= 5,
        total_genes_in_term <= 500,
        category_coverage  >= 0.05   
        )
      
    
    # Add diagnostic logging if needed
    if (!is.null(log_messages) && !is.null(log_event)) {
      log_event(log_messages,
                sprintf("GO filtering results:\n- Original terms: %d\n- Terms after coverage filtering: %d\n- Coverage threshold: %.2f\n- Minimum detected genes: %d",
                        length(unique(GO$name)),
                        length(unique(go_filtered$name)),
                        0.05,    # Fixed coverage threshold
                        5),      # Fixed minimum genes
                "INFO")
    }
    
    # Get missing genes and prepare ontology name
    missing_genes <- setdiff(detected_genes, go_filtered$gene)
    ontology_name <- switch(input$gsea_ontology,
                            "C" = "Cellular Component",
                            "F" = "Molecular Function",
                            "P" = "Biological Process")
    
    # Create comprehensive single log message
    log_message <- sprintf(
      "GO Analysis Summary:

1. Data Overview:
- Ontology: %s
- Total Terms: %d
- Unique Genes: %d

2. Terms by Ontology Category:
- Cellular Component (C): %d terms
- Molecular Function (F): %d terms
- Biological Process (P): %d terms

3. Missing Annotations (%d genes):
%s",
      ontology_name,
      n_distinct(go_filtered$name),
      n_distinct(go_filtered$gene),
      sum(go_filtered$ontology == "C"),
      sum(go_filtered$ontology == "F"),
      sum(go_filtered$ontology == "P"),
      length(missing_genes),
      if(length(missing_genes) > 0) paste(missing_genes, collapse = ", ") else "None"
    )
    
    # Single log event for all GO data information
    log_event(log_messages, log_message, "INFO from GSEA observer")
    
    # Validate we have GO data for the selected ontology
    if (nrow(go_filtered) == 0) {
      shinyalert(
        title = "No GO Data",
        text = sprintf("No GO terms found for %s ontology with the detected genes", 
                       ontology_name),
        type = "error"
      )
      log_event(log_messages,
                sprintf("ANALYSIS STOPPED: No GO terms found for %s ontology", 
                        ontology_name),
                "ERROR from GSEA observer")
      
      shinyjs::hide("gsea-loader-overlay")
      return()
    }
## GSEA ----     
    
    
    
    
    # Run enrichment analysis using identify_top_go_enrichment
    enrichment_results_list <- identify_top_go_enrichment(
      detected_genes = detected_genes,
      regulated_sets = regulated_sets,  # Use the regulated_sets list we already created
      go_filtered = go_filtered,        # Use the filtered GO data
      ontology = input$gsea_ontology,   # Use the selected ontology from input
      p_adj_method = "BH",
      alpha = input$alpha,
      max_categories = 10,
      min_genes_in_term = 5,
      max_genes_in_term = round(length(detected_genes) * 0.10),  # dynamic calculation
      min_fold_enrichment = 1.3,
      log_messages_rv = log_messages,
      log_event = log_event
    )
    
    # Debug print the structure
    if (!is.null(log_event)) {
      log_event(log_messages,
                sprintf("GSEA Results Structure:\n- Has all_results: %s\n- Has top_results: %s\n- Number of missing genes: %d",
                        !is.null(enrichment_results_list$all_results),
                        !is.null(enrichment_results_list$top_results),
                        length(enrichment_results_list$missing_genes)),
                "DEBUG")
  
    
      # Create summary log for top_results
      if (!is.null(enrichment_results_list$top_results) && !is.null(log_event)) {
        # Header for the summary
        log_event(log_messages,
                  "Summary of significant GO terms from top results:",
                  "INFO")
        
        # Process each regulation group
        for (reg_group in names(enrichment_results_list$top_results)) {
          results_df <- enrichment_results_list$top_results[[reg_group]]
          
          if (!is.null(results_df) && nrow(results_df) > 0) {
            # Log header for regulation group
            log_event(log_messages,
                      sprintf("\n%s regulation:", toupper(reg_group)),
                      "INFO")
            
            # Process each term using apply instead of for loop
            apply(results_df, 1, function(row) {
              term_summary <- sprintf(
                "- %s: %.2f-fold enrichment (p-adj=%.2e), %d/%d genes",
                row["name"],
                as.numeric(row["fold_enrichment"]),
                as.numeric(row["p_adj"]),
                as.numeric(row["regulated_count"]),
                as.numeric(row["total_count"])
              )
              log_event(log_messages, term_summary, "INFO")
            })
            
          } else {
            # Log when no significant terms found
            log_event(log_messages,
                      sprintf("\n%s regulation: No significant terms found", toupper(reg_group)),
                      "INFO")
          }
        }
      }
                     
        
    }
    

    # Store results
gsea_results(enrichment_results_list)
# 



    
    # Log completion    
        
log_event(log_messages, "GSEA calculations completed successfully", "SUCCESS from GSEA observer")
log_structure(log_messages, enrichment_results_list, "The structure of the GSEA results is:", "INFO from GSEA observer")
log_structure(log_messages, enrichment_results_list$top_results, "The structure of the top (significant) GSEA results is:", "INFO from GSEA observer") 
log_structure(log_messages, enrichment_results_list$top10_results, "The structure of the top_10 (including nonsignificant) GSEA results is:", "INFO from GSEA observer") 
 
 
  # Hide loader and re-enable button
  
  shinyjs::hide("gsea-loader-overlay")
   
# Validate we got results
if (is.null(enrichment_results_list) || 
    is.null(enrichment_results_list$top_results) || 
    all(sapply(enrichment_results_list$top_results, function(x) nrow(x) == 0))) {
  shinyalert(
    title = "No Enrichment Found",
    text = "No significant GO term enrichment found with current parameters",
    type = "warning"
  )
  
  shinyjs::hide("gsea-loader-overlay")
  return()
}
    
## Build GSEA plots (observer and downloader for barplot) ----


## Use enrichment results to create the GSEA plot
## Build 3 barpolots with the same width and height
## The first plot will show the upregulated genes,
## the second the downregulated genes and the third the bidirectionaly regulaed
## genes. The plot will show top 20 enriched terms for each regulation group.
## The y-axis will show the GO terms.The x axis will show the fold enrichment.
## The - log10 adjusted pvalue will be used to color the bars with scale continous
## The non-significant terms will have no color fill. The plot should use ggplot2.
## Under each go term the number of genes in the term among detected and the number of genes among regulated set
## should be displayed. The plot should have a title and the x-axis should be labeled but not y.
## The plot should have a legend with the color scale and the title should be "GSEA Enrichment Plot for [ontology]"


# Reactive expression to determine which results to use for plotting.
selected_gsea_results <- reactive({
  # If the Apply Filter button was pressed, and "Switch back" toggle is NOT active,
  # then use the filtered results. Otherwise, use the original results.
  if (input$apply_filter > 0 && !isTRUE(input$show_original)) {
    req(gsea_filtered_results())
    gsea_filtered_results()
  } else {
    req(gsea_results())
    gsea_results()
  }
})
# GSEA Plot Output
# GSEA Plot Output
# Server
# GSEA Plot Output

# Download handler for GSEA plot
# GSEA Plot Renderer
output$gsea_plot <- renderPlot({
  req(selected_gsea_results(), 
      input$gsea_ontology, 
      input$plot_category, 
      plotOntologyValue()
     )
  
  log_event(
    log_messages,
    sprintf("Starting GSEA plot generation for ontology: %s, category: %s, hide_nonsig: %s",
            input$gsea_ontology, input$plot_category, input$hide_nonsig),
    "DEBUG from gsea_plot"
  )
  
  plots <- tryCatch({
    build_gsea_plots(
      enrichment_results_list = selected_gsea_results(),
      ontology = input$gsea_ontology,
      show_not_significant = !input$hide_nonsig,
      log_messages_rv = log_messages,
      log_event = log_event,
      plotOntologyValue = plotOntologyValue()
    )
  }, error = function(e) {
    log_event(
      log_messages,
      sprintf("Error in build_gsea_plots: %s", e$message),
      "ERROR from gsea_plot"
    )
    return(NULL)
  })
  
  if (is.null(plots) || is.null(plots[[input$plot_category]])) {
    log_event(
      log_messages, 
      "No valid plot available for selected category", 
      "DEBUG from gsea_plot"
    )
    
    shinyjs::hide("gsea-loader-overlay")
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No significant enrichment found") +
        theme_void()
    )
  }
  
  p <- plots[[input$plot_category]]
  
  if (is_mobile()) {
    p <- p + theme_classic() +
      theme(
        axis.text.x = element_markdown(size = 8),
        axis.text.y = element_markdown(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        plot.margin = margin(5, 5, 5, 5)
      )
  }
  
  p
})

# GSEA Results Table Renderer----
output$gsea_results_table <- render_gt({
  req(gsea_results())
  
  if (input$color_highlight) {
    req(input$down_color, input$up_color)
  }
  
  build_gsea_gt_table(
    enrichment_results_list = gsea_results(),
    color_highlight = if(input$color_highlight) {
      c(input$down_color, input$up_color)
    } else {
      c("#D3D3D3", "#D3D3D3")
    },
    log_messages_rv = log_messages,
    log_event = log_event
  )
})

# Hide loader and re-enable button


## GSEA result downloaders ----

output$download_gsea_plot <- downloadHandler(
  filename = function() {
    paste0("GSEA_plot_", input$plot_category, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
  },
  content = function(file) {
    req(gsea_results(), 
        input$gsea_ontology, 
        input$plot_category, 
        plotOntologyValue())
    
    plots <- build_gsea_plots(
      enrichment_results_list = gsea_results(),
      ontology = input$gsea_ontology,
      show_not_significant = !input$hide_nonsig,
      log_messages_rv = log_messages,
      log_event = log_event,
      plotOntologyValue = plotOntologyValue()
    )
    
    # Get the specific plot based on category
    p <- plots[[input$plot_category]]
    
    # Save the plot
    ggsave(file, p, width = 10, height = 8, device = cairo_pdf)
  }
)


output$reg_gene_list <- downloadHandler(
  filename = function() {
    current_datetime <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(
      "reg_genes_adj_pval_",
      input$alpha,
      "_method_",
      input$adj,
      "_",
      current_datetime,
      ".csv"
    )
  },
  content = function(file) {
    req(regulated_sets())
    
    tryCatch({
      # Get the regulated sets
      sets <- regulated_sets()
      
      # Log the start of download process
      log_event(log_messages, 
                "Starting regulated genes list download using GSEA-processed sets", 
                "INFO")
      
      # Create a data frame with two columns
      max_length <- max(length(sets$up), length(sets$down))
      reg_genes_df <- data.frame(
        Upregulated = c(sets$up, rep(NA, max_length - length(sets$up))),
        Downregulated = c(sets$down, rep(NA, max_length - length(sets$down)))
      )
      
      # Write to CSV
      write.csv(reg_genes_df, file, row.names = FALSE)
      
      # Log successful download
      log_event(log_messages, 
                sprintf("Download successful: %d up-regulated and %d down-regulated genes", 
                        length(sets$up), 
                        length(sets$down)), 
                "SUCCESS")
      
    }, error = function(e) {
      log_event(log_messages, 
                sprintf("Error during download: %s", e$message), 
                "ERROR")
      stop(paste("Error generating file:", e$message))
    })
  }
) 


output$download_full_gsea <- downloadHandler(
  filename = function() {
    paste0("GSEA_full_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    # Debug log the state
    if (!is.null(log_event)) {
      log_event(log_messages,
                sprintf("Download full results triggered:\n- Results exist: %s\n- Results structure valid: %s",
                        !is.null(gsea_results()),
                        !is.null(gsea_results()$all_results)),
                "DEBUG")
    }
    
    # Ensure results exist
    req(gsea_results())
    req(gsea_results()$all_results)
    
    tryCatch({
      # Debug the data before binding
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("All results content:\n- Number of sets: %d\n- Set names: %s",
                          length(gsea_results()$all_results),
                          paste(names(gsea_results()$all_results), collapse = ", ")),
                  "DEBUG")
      }
      
      # Combine results
      full_results_df <- bind_rows(
        gsea_results()$all_results,
        .id = "regulation_type"
      )
      
      # Debug the combined data
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Combined results:\n- Rows: %d\n- Columns: %d\n- Column names: %s",
                          nrow(full_results_df),
                          ncol(full_results_df),
                          paste(colnames(full_results_df), collapse = ", ")),
                  "DEBUG")
      }
      
      # Write to CSV
      write.csv(full_results_df, file, row.names = FALSE)
      
    }, error = function(e) {
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Error in full results download: %s", e$message),
                  "ERROR")
      }
      # Write empty file with structure
      empty_df <- data.frame(
        regulation_group = character(),
        name = character(),
        gene_set = character(),
        total_count = integer(),
        genes_in_term = character(),
        regulated_count = integer(),
        regulated_genes = character(),
        expected_count = numeric(),
        fold_enrichment = numeric(),
        p_value = numeric(),
        p_adj = numeric(),
        stringsAsFactors = FALSE
      )
      write.csv(empty_df, file, row.names = FALSE)
    })
  }
)

output$download_top_gsea <- downloadHandler(
  filename = function() {
    # Create filename with timestamp
    paste0("GSEA_top_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    # Verify results exist
    req(gsea_results())
    req(gsea_results()$top_results)
    
    tryCatch({
      # Log download attempt if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Download top results triggered at %s by %s",
                          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                          "DatViseR"),
                  "INFO")
      }
      
      # Combine top results from all regulation groups
      top_results_df <- bind_rows(
        gsea_results()$top_results,
        .id = "regulation_group"
      ) %>%
        select(
          regulation_group,    # Regulation direction (up/down/bidirectional)
          name,               # GO term name
          gene_set,           # Gene set identifier
          total_count,        # Total genes in GO term
          genes_in_term,      # All genes in the GO term
          regulated_count,    # Number of regulated genes
          regulated_genes,    # List of regulated genes
          expected_count,     # Expected number by chance
          fold_enrichment,    # Enrichment ratio
          p_value,           # Raw p-value
          p_adj              # Adjusted p-value
        )
      
      # Log data summary if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Top results summary:\n- Total rows: %d\n- Regulation groups: %s",
                          nrow(top_results_df),
                          paste(unique(top_results_df$regulation_group), collapse = ", ")),
                  "DEBUG")
      }
      
      # Write data or empty structure based on whether we have results
      if (nrow(top_results_df) > 0) {
        write.csv(top_results_df, file, row.names = FALSE)
        
        # Log successful write if logging is enabled
        if (!is.null(log_event)) {
          log_event(log_messages,
                    sprintf("Successfully wrote %d rows of top GSEA results to file",
                            nrow(top_results_df)),
                    "SUCCESS")
        }
      } else {
        # Create empty data frame with correct structure
        empty_df <- data.frame(
          regulation_group = character(),
          name = character(),
          gene_set = character(),
          total_count = integer(),
          genes_in_term = character(),
          regulated_count = integer(),
          regulated_genes = character(),
          expected_count = numeric(),
          fold_enrichment = numeric(),
          p_value = numeric(),
          p_adj = numeric(),
          stringsAsFactors = FALSE
        )
        
        # Write empty structure
        write.csv(empty_df, file, row.names = FALSE)
        
        # Log empty result if logging is enabled
        if (!is.null(log_event)) {
          log_event(log_messages,
                    "No significant results found, wrote empty structure",
                    "WARNING")
        }
      }
      
    }, error = function(e) {
      # Log error if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Error in top results download: %s", e$message),
                  "ERROR")
      }
      
      # Create and write empty structure on error
      empty_df <- data.frame(
        regulation_group = character(),
        name = character(),
        gene_set = character(),
        total_count = integer(),
        genes_in_term = character(),
        regulated_count = integer(),
        regulated_genes = character(),
        expected_count = numeric(),
        fold_enrichment = numeric(),
        p_value = numeric(),
        p_adj = numeric(),
        stringsAsFactors = FALSE
      )
      write.csv(empty_df, file, row.names = FALSE)
    })
  }
)


output$download_top10_gsea <- downloadHandler(
  filename = function() {
    # Create filename with timestamp
    paste0("GSEA_top10_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    # Verify results exist
    req(gsea_results())
    req(gsea_results()$top10_results)
    
    tryCatch({
      # Log download attempt if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Download top10 results triggered at %s by %s",
                          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                          "DatViseR"),
                  "INFO")
      }
      
      # Combine top results from all regulation groups
      top10_results_df <- bind_rows(
        gsea_results()$top10_results,
        .id = "regulation_group"
      ) %>%
        select(
          regulation_group,    # Regulation direction (up/down/bidirectional)
          name,               # GO term name
          gene_set,           # Gene set identifier
          total_count,        # Total genes in GO term
          genes_in_term,      # All genes in the GO term
          regulated_count,    # Number of regulated genes
          regulated_genes,    # List of regulated genes
          expected_count,     # Expected number by chance
          fold_enrichment,    # Enrichment ratio
          p_value,           # Raw p-value
          p_adj              # Adjusted p-value
        )
      
      # Log data summary if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Top results summary:\n- Total rows: %d\n- Regulation groups: %s",
                          nrow(top10_results_df),
                          paste(unique(top10_results_df$regulation_group), collapse = ", ")),
                  "DEBUG")
      }
      
      # Write data or empty structure based on whether we have results
      if (nrow(top10_results_df) > 0) {
        write.csv(top10_results_df, file, row.names = FALSE)
        
        # Log successful write if logging is enabled
        if (!is.null(log_event)) {
          log_event(log_messages,
                    sprintf("Successfully wrote %d rows of top GSEA results to file",
                            nrow(top10_results_df)),
                    "SUCCESS")
        }
      } else {
        # Create empty data frame with correct structure
        empty_df <- data.frame(
          regulation_group = character(),
          name = character(),
          gene_set = character(),
          total_count = integer(),
          genes_in_term = character(),
          regulated_count = integer(),
          regulated_genes = character(),
          expected_count = numeric(),
          fold_enrichment = numeric(),
          p_value = numeric(),
          p_adj = numeric(),
          stringsAsFactors = FALSE
        )
        
        # Write empty structure
        write.csv(empty_df, file, row.names = FALSE)
        
        # Log empty result if logging is enabled
        if (!is.null(log_event)) {
          log_event(log_messages,
                    "No significant results found, wrote empty structure",
                    "WARNING")
        }
      }
      
    }, error = function(e) {
      # Log error if logging is enabled
      if (!is.null(log_event)) {
        log_event(log_messages,
                  sprintf("Error in top results download: %s", e$message),
                  "ERROR")
      }
      
      # Create and write empty structure on error
      empty_df <- data.frame(
        regulation_group = character(),
        name = character(),
        gene_set = character(),
        total_count = integer(),
        genes_in_term = character(),
        regulated_count = integer(),
        regulated_genes = character(),
        expected_count = numeric(),
        fold_enrichment = numeric(),
        p_value = numeric(),
        p_adj = numeric(),
        stringsAsFactors = FALSE
      )
      write.csv(empty_df, file, row.names = FALSE)
    })
  }
)








output$download_gsea_results <- downloadHandler(
  filename = function() {
    paste0("GSEA_Results_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
  },
  content = function(file) {
    req(gsea_results())
    
    # Log download attempt
    log_event(log_messages,
              "GSEA results table download initiated",
              "INFO from download_gsea_results")
    
    tryCatch({
      # Create table with current color settings
      gt_table <- build_gsea_gt_table(
        enrichment_results_list = gsea_results(),
        color_highlight = if(input$color_highlight) {
          c(input$down_color, input$up_color)
        } else {
          c("#D3D3D3", "#D3D3D3")
        },
        log_messages_rv = log_messages,
        log_event = log_event
      )
      
      # Save as HTML with inline CSS
      log_event(log_messages,
                "GSEA results table created successfully and ready for saving as formatted html",
                "SUCCESS from download_gsea_results")
      
      gtsave(gt_table, file, inline_css = TRUE)
      
    }, error = function(e) {
      log_event(log_messages,
                sprintf("Error in GSEA results table download: %s", e$message),
                "ERROR from download_gsea_results")
      
      # Create and save empty table with error message
      empty_table <- gt(tibble(
        Message = "Error generating GSEA results table. Please try again."
      )) %>%
        tab_options(
          table.width = pct(40),
          container.width = pct(40),
          column_labels.visible = FALSE
        )
      gtsave(empty_table, file, inline_css = TRUE)
    })
  }
)  







shinyjs::hide("gsea-loader-overlay")


})

# Observer for GSEA filter pattern----
observeEvent(input$apply_filter, {
  req(input$apply_filter, input$gsea_filter_pattern)
  
  # Log the start of filtering process.
  log_event(log_messages, 
            sprintf("Applying filter pattern: %s", input$gsea_filter_pattern), 
            "INFO from GSEA filter observer")
  
  # Apply the filter using our function.
  filtered_results <- filter_gsea_results(
    enrichment_results_list = gsea_results(),
    filter_pattern = input$gsea_filter_pattern,
    log_messages_rv = log_messages,
    log_event = log_event
  )
  
  # Store the filtered results.
  gsea_filtered_results(filtered_results)
  
  # Log the completion of filtering.
  log_event(log_messages, 
            "Filter pattern applied successfully", 
            "SUCCESS from GSEA filter observer")
}, ignoreInit = TRUE)


    
# Color highlight observer and reactive UI color pickers----
    
  # Observe changes to the color_highlight input
  observeEvent(input$color_highlight, {
    if (input$color_highlight) {
      log_event(log_messages, "Color highlighting enabled", "INFO from input$color_highlight")
    } else {
      log_event(log_messages, "Color highlighting disabled", "INFO from input$color_highlight")
    }
  })
  
  # Dynamic UI for color highlight options
  output$color_highlight_ui <- renderUI({
    if (input$color_highlight) {
      tagList(
        colourInput("up_color", "Up-regulated color", value = "#FF7081"),
        colourInput("down_color", "Down-regulated color", value = "#7973FA")
      )
    }
  })

# GO category selection observer and reactive UI color pickers ----
  
  # Observe changes to the show_go_category input
  observeEvent(input$show_go_category, {
    if (input$show_go_category) {
      log_event(log_messages, "GO categories selection enabled", "INFO from input$show_go_category")
    } else {
      log_event(log_messages, "GO categories selection disabled", "INFO from input$show_go_category")
      # Clears the selectize input when disabling
      updateSelectizeInput(session, "go_category", selected = NULL)
    }
  })
  
  # Render the UI based on the toggle
  output$go_category_ui <- renderUI({
    if (input$show_go_category) {
      selectizeInput("go_category", "Select from ~8000 unique GO categories", choices = NULL, multiple = TRUE)
    }
  })
  
  observe({
    if (input$show_go_category) {
      log_event(log_messages, "GO categories updated", "INFO from input$show_go_category")
      updateSelectizeInput(session, "go_category", choices = unique(GO$name), server = TRUE)
      
    }
  })
  
  # Reactive expression to track chosen GO categories
  # chosen_go reactive returns NULL when GO categories are disabled
  chosen_go <- reactive({
    # Only return GO categories if the feature is enabled
    if (!input$show_go_category) {
      log_event(log_messages, "GO categories disabled - returning NULL", "INFO")
      return(NULL)
    }
    
    log_event(log_messages, "Returning selected GO categories", "INFO")
    input$go_category
  })
  
  
  
  
  
  color_palette <- c("#440154FF", "darkblue","gold","darkorange","darkcyan","deeppink","black") 
  
  
## Dynaic UI for additional color pickers ---- 
  
  output$color_picker_ui <- renderUI({
    if (!input$show_go_category) {
      log_event(log_messages, "Color pickers for GO categories are disabled", "INFO from output$color_picker_ui")
      return(NULL)
    }
    
    req(chosen_go())
    chosen <- chosen_go()
    cat("Chosen GO categories: ", paste(chosen, collapse = ", "), "\n")  # Debug statement
    log_event(log_messages, "Creating color inputs for GO categories", "INFO from output$color_picker_ui")
    
    # Iterate over indices, not values, to correctly access both `chosen[i]` and `color_palette[i]`
    color_inputs <- lapply(seq_along(chosen), function(i) {
      go <- chosen[i]
      sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go)  # Sanitize ID
      color_value <- color_palette[(i - 1) %% length(color_palette) + 1]  # Cycle through colors using index `i`
      cat("Creating color input for: ", go, " with ID: ", sanitized_id, " and color: ", color_value, "\n")  # Debug statement
      
      colourInput(paste0("color_", sanitized_id), paste("Color for", go), value = color_value)
    })
    
    do.call(tagList, color_inputs)
  })
  
  observe({
    print(str(chosen_go()))
    log_structure(log_messages, input$go_category, "Chosen GO categories modified:") 
  
  })
  
## Custom gene labels observer and UI ----  
  
  # Observe changes to the select_custom_labels input
  observeEvent(input$select_custom_labels, {
    state <- if (input$select_custom_labels) "ENABLED" else "DISABLED"
    log_event(log_messages, paste("Custom gene labels selection", state), "INFO input$select_custom_labels")
  })
  
  # First, create the UI with empty choices
output$custom_gene_labels_ui <- renderUI({
    if (input$select_custom_labels) {
      req(uploaded_df(), input$annotation_col)
      tagList(
        selectizeInput(
          "custom_gene_labels", 
          "Select gene names to label", 
          choices = NULL,  # Start with no choices
          multiple = TRUE
        ),
        colourInput(
          "custom_label_color",
          "Color for custom labeled genes",
          value = "#000000"  # Default black color
        )
      )
    }
})
  
  # Then, update it with server-side processing - this server side processing was suggested by R console warning message that
  # appeared in the previous version with client side processing and no updataSelectizeInput function
  observe({
    req(input$select_custom_labels, uploaded_df(), input$annotation_col)
    gene_names <- unique(uploaded_df()[[input$annotation_col]])
    updateSelectizeInput(
      session,
      "custom_gene_labels",
      choices = gene_names,
      server = TRUE  
    )
  })
  
  # Reactive expression to track chosen custom gene labels
  custom_genes <- reactive({
    log_event(log_messages, "Reactive to track choosen custom gene labels initialized or modified", "INFO")
    input$custom_gene_labels
  })
  
  

# Draw volcano observer ----  
  observeEvent(input$draw_volcano, {
    req(uploaded_df(), input$pvalue_col, input$fold_col, input$annotation_col, input$adj)
    volcano_plot_rv(NULL)  # Reset the reavtive value
    
    shinyjs::show("volcano-loader-overlay")
    

    update_tabset(session, 
                 "dynamic_tabset", 
                "static_volcano")  
                   
    df <- uploaded_df() 

    log_event(log_messages, "Starting volcano plot generation", "INFO input$draw_volcano")
    log_structure(log_messages, df, "The structure of the uploaded_df before creating volcano plot is:\n","INFO")
    
    # Checks if input$pvalue_col and input$fold_col are numeric and breaks the code with a warning allert to the user if not 
    # informs the user to double check the decimal point in the upload section - the wrong decimal point resulted in coercing
    # the vectors to character instead of numeric
    
    if (!is.numeric(df[[input$pvalue_col]]) || !is.numeric(df[[input$fold_col]])) {
      log_event(log_messages, "Error: p-value and fold change columns are not numeric", "ERROR - draw volcano observer checks")
      shinyalert(
        title = "Data Processing Error",
        text = "The p-value and fold change columns must be numeric. Please double-check the decimal point in the upload section",
        type = "error",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        timer = 0
      )
      shinyjs::hide("volcano-loader-overlay")
      return(NULL)
    }
    
    
    # Check and unlog p-values
    df <- check_and_unlog_pvalues(df, input$pvalue_col, log_messages, log_event)
    uploaded_df(df)  # Update the reactive value with unlogged p-values
    log_structure(log_messages, df, "The structure of the uploaded dataset after unlogging p-values is:", "INFO pvalues module")
    
    # Adjust p-values
    pvalues <- df[[input$pvalue_col]]
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    log_event(log_messages, paste(input$adj, "method choosen for p-value adjustment", "INFO adjusting pvalues"))
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)  # Ensure reactive value is updated
    log_event(log_messages, "Unlogging and adjusting p-values completed", "SUCCESS")
    log_structure(log_messages, df, "The structure of the uploaded dataset after adjusting p-values is:", "INFO")
    
   
    
    
    
    # Only perform GO enrichment calculations if the toggle is on
    # GO enrichment and gene lists section
    if (input$show_go_category && length(input$go_category) > 0) {
     
       # Calculate enrichment results
      enrichment_results_list <- calculate_go_enrichment_table(
        df = df,
        annotation_col = input$annotation_col,
        go_categories = input$go_category,
        go_data = GO,
        alpha = input$alpha,
        fold_col = input$fold_col,
        log_messages_rv = log_messages, 
        log_event = log_event,
        log_structure = log_structure
      )
      
      # Render GO gene list table
      output$go_gene_list_gt <- render_gt({
        # First check if color highlighting is enabled
   #     req(input$color_highlight)
        log_event(log_messages, "Rendering GO gene list table", "INFO from output$go_gene_list_gt")
        
        # Get the colors from the proper inputs when color highlighting is enabled
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        
        # Debug the actual colors being passed
        cat("\n==== Color Values Being Passed ====\n")
        cat("Down-regulated color:", colors_to_use[1], "\n")
        cat("Up-regulated color:", colors_to_use[2], "\n")
        cat("================================\n\n")
        
        build_gt_gene_lists(
          df = df,  # Use the actual df from the parent scope
          annotation_col = input$annotation_col,
          chosen_go = input$go_category,
          go_data = GO,
          alpha = input$alpha,
          fold_col = input$fold_col,
          color_highlight = colors_to_use,
          log_messages_rv = log_messages, 
          log_event = log_event
        )
      })
      
      # Render GO enrichment table
      output$go_enrichment_gt <- render_gt({
        req(enrichment_results_list)
        
        # Get the colors from the proper inputs when color highlighting is enabled
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#A0A0A0", "#A0A0A0")  # default black if highlighting is disabled
        }
        
        
        build_gt_table(
          enrichment_results_list,
          upregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
          downregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0)),
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
      })
    } else {
      # Handle both outputs in the else block
      log_event(log_messages, "GO categories not selected or no categories chosen", "INFO output$go_enrichment_gt")
      output$go_gene_list_gt <- render_gt({
        gt(data.frame(Message = if(!input$show_go_category) {
          "Enable GO category visualization to see results"
        } else {
          "Select at least one GO category to see results"
        }))
      })
      
      output$go_enrichment_gt <- render_gt({
        log_event(log_messages, "GO categories not selected or no categories chosen", "INFO output$go_enrichment_gt")
        gt(data.frame(Message = if(!input$show_go_category) {
          "Enable GO category visualization to see results"
        } else {
          "Select at least one GO category to see results"
        }))
      })
    }
 
    
    # Draw the volcano plot
    if (!"adjusted_pvalues" %in% names(df)) {
      print("Error: adjusted_pvalues column is missing.")
     
      shinyjs::hide("volcano-loader-overlay")
      return(NULL)
    }
    
    # default limits for the y axis
    max_y <- max(-log10(as.numeric(df[[input$pvalue_col]])))
    limits_y <- c(-0.01, max_y + 0.03 *max_y)
    
    abs_min <- min(abs(df[[input$fold_col]]), na.rm = TRUE)
    abs_max <- max(abs(df[[input$fold_col]]), na.rm = TRUE)
    limit_for_x_scale <- ifelse(abs_max > abs_min, abs_max, abs_min)
    cat(paste("Limits for x scale are:", limit_for_x_scale, "\n"))
   
    log_event(log_messages, "Creating volcano plot", "INFO input$draw_volcano")
    volcano_plot <- ggplot(df, aes(x = round(!!sym(input$fold_col), 4), 
                                   y = -log10(!!sym(input$pvalue_col)),
                                   text = paste("Gene:", !!sym(input$annotation_col),
                                                "\nP-value:", round(!!sym(input$pvalue_col), 4),
                                                "<br>log2 Fold Change:", round(!!sym(input$fold_col), 3),
                                                "<br>Adjusted P-value:", round(adjusted_pvalues, 4)))) +
                                                
      geom_point(size = 1.8, alpha = 0.5, color = "gray70") +
      theme_classic() +
      labs(title = input$plot_title, x = input$x_axis_label, y = "-Log10 P-Value") +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            aspect.ratio = 0.75,
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 16, color = "black", face = "bold"),
            axis.text = element_text(size = 14, color = "black", face = "bold"),
             ) +  
      geom_hline(yintercept = -log10(input$alpha), linetype = "dashed", color = "red") +
      scale_x_continuous(
        limits = c(-limit_for_x_scale, limit_for_x_scale)
      ) +
      scale_y_continuous(
        limits = limits_y
      )
    
    
    # Reset to base layer only - this helps to erase all the layers that 
    # might have persisted in memory (case that happened with GO annotations)
    volcano_plot$layers <- volcano_plot$layers[1] 
      
   
    # Generate subtitle based on the input settings
    subtitle <- NULL
    
    if (input$color_highlight) {
      # increase the limits_y to fitt the annotations 
      
      limits_y <- c(-0.01, max_y + 0.09*max_y)
      
      log_event(log_messages, "Color highlighting enabled", "INFO input$draw_volcano")
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      total_count <- df %>% nrow()
      volcano_plot <- volcano_plot +
      
      # increase the scale y to fit the annotations   
       scale_y_continuous(limits = limits_y) +  
          
        annotate("text", x = -Inf, y = Inf, label = paste0("Upregulated n= ", upregulated_count), color = input$up_color, hjust = -0.1 ,vjust = 2, size = 5.5 ) +
        annotate("text", x = -Inf, y = Inf, label = paste0("Downregulated n= ", downregulated_count), color = input$down_color, hjust = -0.1, vjust = 1, size = 5.5)+
        annotate("text", x = -Inf, y = Inf, label = paste0("Detected n= ", total_count), color = "#A0A0A0", hjust = -0.1, vjust = 3, size = 5.5)
      # Detailed debug analysis of input$color_highlight
      cat("\n==== Color Input Debug Analysis ====\n")
      cat("Current Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("\n1. Basic Information:\n")
      cat("- Is input$color_highlight NULL?:", is.null(input$color_highlight), "\n")
      cat("- Class of input$color_highlight:", class(input$color_highlight), "\n")
      cat("- Length of input$color_highlight:", length(input$color_highlight), "\n")
      
      cat("\n2. Value Details:\n")
      cat("- Raw value of input$color_highlight:\n")
      print(input$color_highlight)  # Using print() for structured output
      
      cat("\n3. Individual Elements:\n")
      cat("- First element:", input$color_highlight[1], 
          " (class:", class(input$color_highlight[1]), ")\n")
      cat("- Second element:", input$color_highlight[2],
          " (class:", class(input$color_highlight[2]), ")\n")
      
      cat("\n4. Structure Information:\n")
      str(input$color_highlight)
      
      cat("\n================================\n\n")
      
          }
    
    # Add annotations for chosen GO categories
    if (input$show_go_category) {
      chosen <- chosen_go()
      limits_y <- c(-0.01, max_y + ifelse(input$color_highlight, 0.09*max_y + 0.03*max_y*length(chosen),
                                      0.03*max_y*length(chosen) )
                    )                                                                  
      
      selected_GO <- GO %>% filter(name %in% chosen)
      
      if ("id" %in% colnames(selected_GO)) {
        go_details <- paste0(paste(chosen, unique(selected_GO$id), collapse = "\n"))
      } else {
        go_details <- paste0("GO: ", paste(chosen, collapse = ", "), "\nID: Not available")
        cat("Warning: 'id' column not found in selected_GO\n")  
        cat(go_details)  
      }
      
      for (i in seq_along(chosen)) {
        go <- chosen[i]
        color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
        go_detail <- paste0(go, ": ", unique(selected_GO$id[selected_GO$name == go]))
        volcano_plot <- volcano_plot +
          scale_y_continuous(limits = limits_y) +
          
          annotate("text", x = Inf, y = Inf, label = go_detail, color = color, hjust = 1.1, vjust = 1 + i*1.2, size = 5.5)
      }
    }  
     
    # Add subtitle to the plot
    if (!is.null(subtitle)) {
      volcano_plot <- volcano_plot + labs(subtitle = subtitle)
    }
  
    
    if (input$color_highlight) {
      log_event(log_messages, "Adding color point layers from color_highlight input", "INFO input$draw_volcano")
      volcano_plot <- volcano_plot +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0), aes(color = "Up"), size = 2, color = input$up_color, alpha = 0.5) +
        geom_point(data = df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0), aes(color = "Down"), size = 2, color = input$down_color, alpha = 0.5)
    }
    
    # Highlighting genes belonging to chosen GO categories
    if (!is.null(chosen_go())) {
      log_event(log_messages, "Coloring genes belonging to chosen GO categories", "INFO input$draw_volcano")
      selected_GO <- GO %>% filter(name %in% chosen_go())
      for (go in chosen_go()) {
        color <- input[[paste0("color_", gsub("[^a-zA-Z0-9]", "_", go))]]
        genes <- selected_GO %>% filter(name == go) %>% pull(gene)
        
        # Convert all gene names to uppercase in one step
        genes <- toupper(genes)
        
        # print a message if any genes were modified
        non_human_genes <- genes[genes != toupper(genes)]
        if (length(non_human_genes) > 0) {
          cat(sprintf("\n %d non-human genes detected - converted to uppercase\n", 
                      length(non_human_genes)))
          cat("\n the following genes were converted to uppercase: \n")
          print(non_human_genes)
        }
        
        volcano_plot <- volcano_plot +
          geom_point(
            data = df %>% filter(toupper(!!sym(input$annotation_col)) %in% genes),
            aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col))),
            size = 1.8, color = color, alpha = 0.7
          )
      }
    }
    
    # Create a new column for trimmed labels if input$trim_gene_names is TRUE
    if (input$trim_gene_names) {
      log_event(log_messages, "Trimming gene names to first occurrence", "INFO input$draw_volcano")
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
        geom_text_repel(data = top_hits, aes(label = trimmed_labels, color = label_color), size = 4, max.overlaps = Inf, nudge_y = 0.2) +
        scale_color_identity()  # Use identity scale to apply the colors directly
    }
    
    # Add custom gene labels if feature is enabled
    if (input$select_custom_labels && !is.null(custom_genes())) {
      log_event(log_messages, "Adding custom gene labels", "INFO input$draw_volcano")
      custom_label_data_nonsig <- df %>% filter(!!sym(input$annotation_col) %in% custom_genes()& adjusted_pvalues > input$alpha)
      custom_label_data_sig <- df %>% filter(!!sym(input$annotation_col) %in% custom_genes() & adjusted_pvalues < input$alpha)
      
      # Use trimmed labels if applicable
      if (input$trim_gene_names) {
        custom_label_data_nonsig$trimmed_labels <- sapply(custom_label_data_nonsig[[input$annotation_col]], function(x) {
          strsplit(as.character(x), "[,; :]+")[[1]][1]
        })
      } else {
        custom_label_data_nonsig$trimmed_labels <- custom_label_data_nonsig[[input$annotation_col]]
      }
      
      # Use trimmed labels if applicable
      if (input$trim_gene_names) {
        custom_label_data_sig$trimmed_labels <- sapply(custom_label_data_sig[[input$annotation_col]], function(x) {
          strsplit(as.character(x), "[,; :]+")[[1]][1]
        })
      } else {
        custom_label_data_sig$trimmed_labels <- custom_label_data_sig[[input$annotation_col]]
      }
      
      
      
      volcano_plot <- volcano_plot +
        geom_label_repel(
          data = custom_label_data_nonsig,
          aes(label = trimmed_labels),
          size = 4,
          color = "black",
          fill = "white",
          max.overlaps = Inf,
          nudge_y = 0.3,
          alpha = 0.7
        )+
        
        geom_label_repel(
          data = custom_label_data_sig,
          aes(label = trimmed_labels),
          size = 4,
          color = "white",
          fill = input$custom_label_color,
          max.overlaps = Inf,
          nudge_y = 0.3,
          alpha = 0.7,
          fontface = "bold"
        )+
        
        
        
        
      # added fill for custom genes points
      geom_point(data = custom_label_data_nonsig, 
                 aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col)))
                 , size = 1.8, color = input$custom_label_color, alpha = 0.7)+
        
      geom_point(data = custom_label_data_sig, 
                   aes(x = !!sym(input$fold_col), y = -log10(!!sym(input$pvalue_col)))
                   , size = 1.8, color = input$custom_label_color, alpha = 0.7)   
        
        
    }
    
    if (!is_mobile()) {
      volcano_plot_rv(volcano_plot)
    } else {
      # Calculate scaling factors based on screen width
      width <- input$clientWidth
      text_scale <- max(0.6, min(1, width / 768))
      point_scale <- max(0.8, min(1.2, width / 768))
      
      mobile_plot <- volcano_plot +
        # Adjust theme elements for mobile
        theme(
          # Reduce text sizes
          plot.title = element_text(size = 18 * text_scale),
          axis.title = element_text(size = 16 * text_scale),
          axis.text = element_text(size = 14 * text_scale),
          
          # Adjust margins for mobile
          plot.margin = margin(
            t = 5 * text_scale,
            r = 5 * text_scale,
            b = 5 * text_scale,
            l = 5 * text_scale
          ),
          
          # Modify aspect ratio for better mobile viewing
          aspect.ratio = 0.85,
          
          # Ensure plot uses full width
          panel.spacing = unit(1, "mm")
        )
      
      # Modify all existing layers for mobile optimization
      mobile_plot$layers <- lapply(mobile_plot$layers, function(l) {
        # Adjust point sizes
        if (inherits(l$geom, "GeomPoint")) {
          l$aes_params$size <- l$aes_params$size * point_scale
          l$aes_params$alpha <- min(l$aes_params$alpha + 0.1, 1) # Slightly increase visibility
        }
        # Adjust text labels
        else if (inherits(l$geom, "GeomTextRepel")) {
          l$aes_params$size <- 4 * text_scale
          l$aes_params$max.overlaps <- 10  # Reduce overlaps on mobile
          l$aes_params$box.padding <- 0.35
          l$aes_params$point.padding <- 0.25
          l$aes_params$force <- 2
        }
        else if (inherits(l$geom, "GeomLabelRepel")) {
          l$aes_params$size <- 4 * text_scale
          l$aes_params$max.overlaps <- 10  # Reduce overlaps on mobile
          l$aes_params$box.padding <- 0.35
          l$aes_params$point.padding <- 0.25
          l$aes_params$force <- 2
        }
        
        
        
        # Adjust annotations
        else if (inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel")) {
          l$aes_params$size <- l$aes_params$size * text_scale
        }
        return(l)
      })
      
      log_event(log_messages, 
                sprintf("Plot optimized for mobile view (width: %dpx, text scale: %.2f, point scale: %.2f)", 
                        width, text_scale, point_scale), 
                "INFO mobile optimization")
      # Store the plot in reactive value
      volcano_plot_rv(mobile_plot)
    }
    
      
    
    
    output$volcano_plot <- renderPlot({ 
      log_event(log_messages, "Rendering static volcano plot", "INFO output$volcano_plot")
      req(volcano_plot_rv())
      print(volcano_plot_rv()) 
    })
    cat("\n=== Debug Information for Volcano Plot ===\n")
    cat("1. Object Class:", class(volcano_plot), "\n")
    # cat("2. Structure:\n")
    # print(str(volcano_plot))
    cat("2. structure of the plot: output disabled \n")
    cat("3. Plot Layers:", length(volcano_plot$layers), "\n")
    cat("4. Plot Data Dimensions:", dim(volcano_plot$data), "\n")
    cat("================================\n")
    
    
    output$volcano_plotly <- renderPlotly({ 
      req(volcano_plot_rv())
      log_event(log_messages, "Rendering interactive volcano plot", "INFO output$volcano_plotly")
      # Convert to plotly with proper layout
      p <- ggplotly(volcano_plot_rv(), tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 50, r = 50, b = 50, t = 50),
          showlegend = TRUE
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          responsive = TRUE
        )
      
      # Adds try-catch for error handling
      tryCatch({
        return(p)
      }, error = function(e) {
        # Log the error and return a blank plot
        message("Error in plotly rendering: ", e$message)
        plot_ly() %>% 
          add_annotations(
            text = "Unable to render interactive plot. Please check your inputs.",
            showarrow = FALSE
          )
       
        shinyjs::hide("volcano-loader-overlay")
      })
      # Renders the download button
      output$download_log_button <- renderUI({
        downloadButton("download_log", "Download Analysis Log", class = "ui gray button")
      })
      
      # Renders the download button only after draw_volcano is clicked
      outputOptions(output, "download_log_button", suspendWhenHidden = FALSE)
      
      
    })
    
    
    # Download handler for GO enrichment table
    output$download_go_enrichment <- downloadHandler(
      filename = function() {
        paste0("GO_Enrichment_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        req(enrichment_results_list, input$color_highlight)
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        gt_table <- build_gt_table(
          enrichment_results_list,
          upregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
          downregulated_count = nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0)),
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
        log_event(log_messages, "GO enrichment table created successfully and ready for saving as formatted html", "SUCCESS from output$download_go_enrichment")
        gtsave(gt_table, file, inline_css = TRUE)
      }
    )
    
    output$download_go_gene_list <- downloadHandler(
      filename = function() {
        paste0("GO_Gene_List_Table_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        req(input$color_highlight, enrichment_results_list)
        colors_to_use <- if(input$color_highlight) {
          req(input$up_color, input$down_color)
          c(input$down_color, input$up_color)
        } else {
          c("#000000", "#000000")  # default black if highlighting is disabled
        }
        gt_table <- build_gt_gene_lists(
          df = uploaded_df(),  
          annotation_col = input$annotation_col,
          chosen_go = input$go_category,
          go_data = GO,
          alpha = input$alpha,
          fold_col = input$fold_col,
          color_highlight = colors_to_use,
          log_messages_rv = log_messages,
          log_event = log_event
        )
        
        # Save as HTML with inline CSS
        log_event(log_messages, "GO gene list table created successfully and ready for saving as formatted html", "SUCCESS from output$download_go_gene_list")
        gt::gtsave(gt_table, file, inline_css = TRUE)
      }
    )  
    
    
    
    
    ####   Downloaders for the volcano ----
    
    output$download_plot1 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_85x85_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 85, 85, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 85, height = 85, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot2 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_114x114_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 114, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 114, height = 114, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot3 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_114x65_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 65, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 114, height = 65, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot4 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_174x174_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 174,log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 174, height = 174, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    output$download_plot5 <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_174x98_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        req(volcano_plot_rv())
        publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 98, log_messages_rv = log_messages, log_event = log_event)
        ggsave(file, publication_plot, width = 174, height = 98, 
               units = "mm", device = cairo_pdf)
      }
    )
    
    
    
    
    
    
    
    
    
    volcano_plot_rv(volcano_plot)
    volcano_plot_original(volcano_plot)
    shinyjs::hide("volcano-loader-overlay")
  
  })

  
  
  
### This is were the draw volcano observer ends ----    
    

  
#### Reactive UI for the custom x-axis limits and hiding text annotations----

  # reactive expression for the x-axis limits calculation
  x_axis_limits <- reactive({
    req(input$draw_volcano, uploaded_df(), input$fold_col)  # Add proper requirements
    df <- uploaded_df()  # Get the data from your reactive value
    
    abs_min <- min(abs(df[[input$fold_col]]), na.rm = TRUE)
    abs_max <- max(abs(df[[input$fold_col]]), na.rm = TRUE)
    limit_for_x_scale <- ifelse(abs_max > abs_min, abs_max, abs_min)
    round(limit_for_x_scale, 0.1)
  })
  
  # Reactive UI for x limits using the reactive expression
 
  has_annotations <- reactive({
    req(input$draw_volcano)
    # Check if either color highlighting or GO categories are active and have annotations
    (input$color_highlight && 
        nrow(uploaded_df() %>% filter(adjusted_pvalues < input$alpha)) > 0) || 
      (input$show_go_category && length(input$go_category) > 0)
  })
  
  # Modify the renderUI
  output$x_limits_ui <- renderUI({
    req(input$draw_volcano)
    limit_for_x_scale <- x_axis_limits()
    
    tagList(
      # Main container
      div(class = "ui center aligned container volcano-limits-container",
          div(class = "ui action input volcano-limits-input-group",
              numericInput(
                inputId = "x_limits",
                label = NULL,
                value = limit_for_x_scale,
                min = 0.5,
                max = 10,
                step = 0.1
              ),
              actionButton(
                inputId = "redraw_volcano",
                label = "Add custom x-axis limits [+- log2 fold]",
                class = "ui blue button volcano-limits-button"
              )
          )),
      
      # Toggle section - only shown when annotations exist
      if (has_annotations()) {
        div(class = "ui center aligned container toggle-container",
            div(class = "ui grid middle aligned compact", # Added 'compact' class
                div(class = "seven wide column right aligned no-padding-right", # Changed from 6 to 7 and added custom class
                    tags$span(
                      id = "show-text",
                      class = "ui label green tiny",
                      "showing top annotations"
                    )
                ),
                div(class = "two wide column center aligned no-padding", # Changed from 4 to 2 and added custom class
                    div(class = "ui fitted toggle checkbox",
                        tags$input(
                          type = "checkbox",
                          id = "hide_annot"
                        ),
                        tags$label("")
                    )
                ),
                div(class = "seven wide column left aligned no-padding-left", # Changed from 6 to 7 and added custom class
                    tags$span(
                      id = "hide-text",
                      class = "ui label basic tiny",
                      "top annotations hidden"
                    )
                )
            )
        )
      }
    )
  })
    
  # Observer for custom x-axis limits
  observeEvent(input$redraw_volcano, {
    req(input$draw_volcano, input$x_limits, volcano_plot_rv())    
    log_event(log_messages, "Redrawing volcano plot with custom x-axis limits", "INFO input$redraw_volcano")
    
    # Store current state before modification
    current_plot <- volcano_plot_rv()
    modified_plot <- current_plot +
      scale_x_continuous(
        limits = c(-input$x_limits, input$x_limits)
      )
    
    # Update both reactive values
    volcano_plot_rv(modified_plot)
    volcano_plot_original(modified_plot)  # Update original to maintain x-axis changes
  })
  
  # Observer for toggling annotations
  observeEvent(input$hide_annot, {
    req(input$draw_volcano, uploaded_df(), input$pvalue_col, volcano_plot_rv())    
    log_event(log_messages, "Toggling text annotations", "INFO input$hide_annot")
    
    if (input$hide_annot) {
      # When toggle is ON - hide annotations and reset y-axis
      mod_plot <- volcano_plot_rv()
      df <- uploaded_df()  # Get current data frame from reactive value
      
      max_y <- max(-log10(as.numeric(df[[input$pvalue_col]])))
      limits_y <- c(-0.01, max_y + 0.03 * max_y)
      
      # Modify only mod_plot - keep all layers EXCEPT those created by annotate()
      mod_plot$layers <- mod_plot$layers[sapply(mod_plot$layers, function(x) {
        is_not_annotation <- TRUE
        if (inherits(x$geom, "GeomText") || inherits(x$geom, "GeomLabel")) {
          is_not_annotation <- !is.null(x$mapping$label) ||
            inherits(x$geom, "GeomTextRepel") ||
            inherits(x$geom, "GeomLabelRepel")
        }
        return(is_not_annotation)
      })]
      
      # Reset y-axis limits
      mod_plot <- mod_plot + 
        scale_y_continuous(limits = limits_y)
      
      volcano_plot_rv(mod_plot)
    } else {
      # When toggle is OFF - restore from original
      volcano_plot_rv(volcano_plot_original())
    }
  })
    
    
    
    
    # Add the download log UI
    output$download_log_ui <- renderUI({
      if (!is.null(uploaded_df())) {
        tags$div(
          style = "margin-top: 20px;",
          downloadButton("download_log", "Download Process Log")
        )
      }
    })
  

    output$download_log <- downloadHandler(
      filename = function() {
        paste0("vivid_volcano_log_", session_id, ".txt")
      },
      content = function(file) {
        # Add session information header
        session_info <- sprintf(
          "Log File for Vivid Volcano Analysis\nSession ID: %s\nSession Start: %s\nDownload Time: %s\n----------------------------------------\n\n",
          session_id,
          format(session_start_time),
          format(Sys.time())
        )
        
        # Combine session info with logs
        complete_log <- paste0(session_info, log_messages())
        writeLines(complete_log, file)
      }
    )
    
    # Clear logs when session ends
    session$onSessionEnded(function() {
      log_event(log_messages, "Session ended", "INFO")
      
      
    
       
      
      isolate(log_messages(""))  # Clear logs
    })
   
}
  
shinyApp(ui = ui, server = server)
