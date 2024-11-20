library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(ggrepel)
library(arrow)
library(DT)
library(plotly)
library(Cairo)
library(gt)

# Load the GO data once globally
GO <- arrow::read_parquet("GO.parquet")

##################---CRUCIAL FUNCTION DEFINITIONS---- ###########################

perform_hypergeometric_test <- function(population_size, success_population_size, sample_size, sample_success_size) {
  cat("Performing hypergeometric test with parameters:\n")
  cat("population_size:", population_size, "success_population_size:", success_population_size, "sample_size:", sample_size, "sample_success_size:", sample_success_size, "\n")
  phyper(sample_success_size - 1, success_population_size, population_size - success_population_size, sample_size, lower.tail = FALSE)
}

calculate_go_enrichment <- function(genes, go_categories, go_data) {
  # Remove duplicates from input genes
  genes <- unique(genes)
  cat("Calculating GO enrichment for unique genes:\n", paste(genes, collapse = ", "), "\n")
  
  enrichment_results <- lapply(go_categories, function(go_category) {
    # Get unique genes for this GO category
    go_genes <- go_data %>% 
      filter(name == go_category) %>% 
      pull(gene) %>%
      unique()  # Remove duplicates from GO genes
    
    cat("GO category:", go_category, "GO genes:", paste(go_genes, collapse = ", "), "\n")
    
    population_size <- 19689  # Number of human coding genes after pseudogene exclusion
    success_population_size <- length(go_genes)
    sample_size <- length(genes)
    
    # Check for exact matches with cleaned and unique genes
    sample_success_size <- sum(sapply(genes, function(gene) {
      # Remove special characters and split
      cleaned_gene <- gsub("[c\\(\\)\";]", "", gene)
      gene_parts <- unique(unlist(strsplit(cleaned_gene, "[ ,;:]+")))  # Make parts unique
      
      # Check if any of the cleaned parts match exactly with go_genes
      any(gene_parts %in% go_genes)
    }))
    
    cat("Computed values - population_size:", population_size, "success_population_size:", success_population_size,
        "sample_size:", sample_size, "sample_success_size:", sample_success_size, "\n")
    
    p_value <- perform_hypergeometric_test(population_size, success_population_size, sample_size, sample_success_size)
    cat("Calculated p_value for category", go_category, "is:", p_value, "\n")
    
    data.frame(
      GO_Category = go_category,
      P_Value = p_value,
      Population_Size = population_size,
      Success_Population_Size = success_population_size,
      Sample_Size = sample_size,
      Sample_Success_Size = sample_success_size
    )
  })
  
  enrichment_results <- bind_rows(enrichment_results)
  enrichment_results$Adjusted_P_Value <- p.adjust(enrichment_results$P_Value, method = "bonferroni", n = 1160)
  
  return(enrichment_results)
}

calculate_go_enrichment_table <- function(df, annotation_col, go_categories, go_data, alpha, fold_col) {
  cat("Calculating GO enrichment table with alpha:", alpha, "fold_col:", fold_col, "\n")
  
  # Get genes
  upregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) > 0) %>% 
    pull(!!sym(annotation_col))
  
  downregulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha & !!sym(fold_col) < 0) %>% 
    pull(!!sym(annotation_col))
  
  regulated_genes <- df %>% 
    filter(adjusted_pvalues < alpha) %>% 
    pull(!!sym(annotation_col))
  
  # Get GO IDs for categories
  go_ids <- go_data %>% 
    filter(name %in% go_categories) %>% 
    distinct(name, id)
  
  # Calculate enrichment with added GO IDs
  upregulated_enrichment <- calculate_go_enrichment(upregulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  downregulated_enrichment <- calculate_go_enrichment(downregulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  regulated_enrichment <- calculate_go_enrichment(regulated_genes, go_categories, go_data) %>%
    left_join(go_ids, by = c("GO_Category" = "name"))
  
  enrichment_results_list <- list(
    upregulated = list(
      data = upregulated_enrichment
    ),
    downregulated = list(
      data = downregulated_enrichment
    ),
    regulated = list(
      data = regulated_enrichment
    )
  )
  
  return(enrichment_results_list)
}

build_gt_table <- function(enrichment_results_list, upregulated_count, downregulated_count) {
  # Prepare data frames with rounded values and formatted p-values
  regulated_df <- enrichment_results_list$regulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.3f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.3f", Adjusted_P_Value))
    )
  
  upregulated_df <- enrichment_results_list$upregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value))
    )
  
  downregulated_df <- enrichment_results_list$downregulated$data %>%
    mutate(
      Population_Enrichment_Ratio = round(Success_Population_Size / Population_Size, 3),
      Subpopulation_Enrichment_Ratio = round(Sample_Success_Size / Sample_Size, 3),
      P_Value = ifelse(P_Value < 0.001, "<0.001", sprintf("%.2f", P_Value)),
      Adjusted_P_Value = ifelse(Adjusted_P_Value < 0.001, "<0.001", sprintf("%.2f", Adjusted_P_Value))
    )
  
  # Combine all data frames and remove "Population_Size" column
  combined_df <- bind_rows(
    regulated_df,
    upregulated_df,
    downregulated_df
  ) %>%
    select(-Population_Size) # 1. Remove "Number of human genes" column
  
  # Create the gt table with formatted numbers
  gt_table <- combined_df %>%
    gt() %>%
    tab_header(
      title = "Gene Ontology Enrichment Results",
      subtitle = paste(
        "three groups of regulated proteins and",
        nrow(regulated_df) + nrow(upregulated_df) + nrow(downregulated_df),
        "chosen GO terms"
      )
    ) %>%
    fmt_markdown(
      columns = c(
        "Population_Enrichment_Ratio",
        "Subpopulation_Enrichment_Ratio"
      )
    ) %>%
    cols_label(
      GO_Category = "GO name",
      id = "GO id",
      Population_Enrichment_Ratio = "Genomic enrichment",
      Subpopulation_Enrichment_Ratio = "Regulated genes enrichment",
      P_Value = "Hypergeometric test p-value",
      Adjusted_P_Value = "Bonferroni(n = 1160) adj-p value",
      Success_Population_Size = "Genes in GO category",
      Sample_Size = "Number of regulated genes",
      Sample_Success_Size = "Regulated genes in GO category"
    ) %>%
    # Add footnote for Adjusted_P_Value column
    tab_footnote(
      footnote = "Bonferroni correction based on the estimated number of level 4 hierarchy GO tags.",
      locations = cells_column_labels("Adjusted_P_Value")
    )
  
  # Add colored row groups
  if(nrow(regulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Bidirectionally regulated n = ", nrow(regulated_df)),
        rows = 1:nrow(regulated_df)
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#D3D3D3") # Light gray color
        ),
        locations = cells_row_groups(groups = paste0("Bidirectionally regulated n = ", nrow(regulated_df)))
      )
  }
  
  if(nrow(upregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Upregulated n = ", nrow(upregulated_df)),
        rows = (nrow(regulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#ADD8E6") # Light blue color
        ),
        locations = cells_row_groups(groups = paste0("Upregulated n = ", nrow(upregulated_df)))
      )
  }
  
  if(nrow(downregulated_df) > 0) {
    gt_table <- gt_table %>%
      tab_row_group(
        label = paste0("Downregulated n = ", nrow(downregulated_df)),
        rows = (nrow(regulated_df) + nrow(upregulated_df) + 1):(nrow(regulated_df) + nrow(upregulated_df) + nrow(downregulated_df))
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#FFC0CB") # Light pink color
        ),
        locations = cells_row_groups(groups = paste0("Downregulated n = ", nrow(downregulated_df)))
      )
  }
  
  return(gt_table)
}


create_publication_plot <- function(base_plot, width_mm, height_mm) {
  # Convert mm to inches for ggsave compatibility
  width_in <- width_mm * 0.0393701
  height_in <- height_mm * 0.0393701
  
  # Define fixed base sizes for different plot elements
  title_size <- 12
  text_size <- 8
  
  # Define fixed point sizes for different plot dimensions
  point_sizes <- list(
    "85x85" = list(base = 0.6, highlight = 0.9, annotation = 1.5),
    "114x114" = list(base = 0.8, highlight = 1.2, annotation = 2),
    "114x65" = list(base = 0.7, highlight = 1.05, annotation = 1.75),
    "174x174" = list(base = 1, highlight = 1.5,annotation = 2.5),
    "174x98" = list(base = 0.9, highlight = 1.35, annotation = 2.25)) 
  
  # Determine which size configuration to use
  plot_key <- paste0(width_mm, "x", height_mm)
  point_size <- point_sizes[[plot_key]] %||% point_sizes[["85x85"]]
  
  # Modify the plot with publication-ready settings
  publication_plot <- base_plot +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size),
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      plot.margin = margin(t = 30, r = 85, b = 10, l = 10, unit = "pt")
    )
  
  # Update all layers
  for(i in seq_along(publication_plot$layers)) {
    layer <- publication_plot$layers[[i]]
    
    # Handle point geometries
    if(inherits(layer$geom, "GeomPoint")) {
      is_highlight <- !is.null(layer$aes_params$color) &&
        layer$aes_params$color %in% c("darkgreen", "red")
      new_size <- if(is_highlight) point_size$highlight else point_size$base
      layer$aes_params$size <- new_size
    }
    
    # This part will need to be modified when the annotations by annotate() will be deleted
    # Handle text annotations
    if(inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomTextRepel")) {
      # Check if this is an annotation text (positioned at Inf,Inf)
      is_annotation <- all(
        !is.null(layer$data),
        "x" %in% names(layer$data),
        "y" %in% names(layer$data),
        all(is.infinite(layer$data$x) & is.infinite(layer$data$y))
      )
      
      if(is_annotation) {
        # This is an annotation text (upregulated/downregulated counts or GO details)
        layer$aes_params$size <- point_size$annotation
      } else {
        # This is a regular text label (gene names)
        layer$aes_params$size <- text_size * 0.25
      }
    }
  }
  
  return(publication_plot)
}

################################### ----UI---#################################

ui <- fluidPage(
  titlePanel("Vivid Volcano Controls"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload a CSV or TSV file", accept = c(".csv", ".tsv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      radioButtons("dec", "Decimal Point", choices = c(Dot = ".", Comma = ","), selected = "."),
      actionButton("upload", "Upload"),
      h4("Select columns for analysis"),
      uiOutput("column_select_ui"),
      radioButtons("adj", "pvalue adjustment", choices = c(None = "none", Bonferroni = "bonferroni", Hochberg = "hochberg", Benjamini_Hochberg = "BH", Benjamini_Yekutieli = "BY"), selected = "BH"),
      numericInput("alpha", "Significance threshold", value = 0.05),
      h4("Volcano Plot Options"),
      checkboxInput("color_highlight", "Highlight significant hits", FALSE),
      uiOutput("color_highlight_ui"),
      checkboxInput("show_go_category", "I want to visualise GO categories", FALSE),
      uiOutput("go_category_ui"),  # Placeholder for dynamic UI
      uiOutput("color_picker_ui"),  # Placeholder for dynamic color pickers
      numericInput("num_labels", "Number of labels (0-100)", value = 10, min = 0, max = 100),
      checkboxInput("trim_gene_names", "Trim multiple gene names to first occurrence", FALSE),
      textInput("plot_title", "Plot Title", "Vivid Volcano"),
      textInput("x_axis_label", "X Axis Label", "log2 fold condition X vs. condition Y"),
      actionButton("draw_volcano", "Draw Volcano Plot")
    ),
    mainPanel(
      uiOutput("uploaded_dataset_ui"),
      DT::dataTableOutput("dataset_summary"),
      cat("column_structure"),
      cat("pvalue_distribution"),
      cat("significant_genes"),
      cat("df_structure"),
      tabsetPanel(
        tabPanel("Static", 
                 fluidRow(
                   column(6, plotOutput("volcano_plot", width = "auto", height = "720px")),
                   column(6, gt_output("go_enrichment_gt"))
                 )
        ),
        tabPanel("Interactive", plotlyOutput("volcano_plotly", width = "auto", height = "720px"))
      ),
      # Add after the tabsetPanel in mainPanel
      hr(),  # Add a horizontal line for visual separation
      h3("Download Publication-Ready Plots"),
      fluidRow(
        column(12,
               downloadButton("download_plot1", "Download 85x85mm (1 column)"),
               downloadButton("download_plot2", "Download 114x114mm (1.5 column)"),
               downloadButton("download_plot3", "Download 114x65mm (1.5 column landscape)"),
               downloadButton("download_plot4", "Download 174x174mm (square)"),
               downloadButton("download_plot5", "Download 174x98mm (landscape)"),
               style = "margin-bottom: 20px"
        )
      ),
      htmlOutput("go_enrichment_regulated"),
      htmlOutput("go_enrichment_upregulated"),
      htmlOutput("go_enrichment_downregulated")
    )
  )
)

##########################-----SERVER----######################################


server <- function(input, output, session) {
  
  # Reactive values
  uploaded_df <- reactiveVal()
  volcano_plot_rv <- reactiveVal()
  
  
  check_and_unlog_pvalues <- function(df, pvalue_col) {
    pvalues <- df[[pvalue_col]]
    if (all(pvalues >= 0 & pvalues <= 1) == FALSE) {
      cat("P-values appear to be -log10 transformed. Unlogging...\n")
      pvalues <- 10^(-abs(pvalues))
      df[[pvalue_col]] <- pvalues
    }
    return(df)
  }
  
  observeEvent(input$upload, {
    req(input$file1)
    in_file <- input$file1
    df <- read_delim(in_file$datapath, delim = input$sep, col_names = input$header, locale = locale(decimal_mark = input$dec))
    uploaded_df(df)
    
    output$column_select_ui <- renderUI({
      if (is.null(df)) return(NULL)
      colnames <- names(df)
      tagList(
        selectInput("pvalue_col", "Select p-value column", choices = colnames),
        selectInput("fold_col", "Select regulation column - log2(fold)", choices = colnames),
        selectInput("annotation_col", "Select human gene symbols column", choices = colnames)
      )
    })
    
    output$dataset_summary <- DT::renderDataTable({ 
      datatable(df, rownames = FALSE, options = list(pageLength = 3))
    })
    
    output$uploaded_dataset_ui <- renderUI({
      tagList(
        h3("Uploaded Dataset Preview"),
        DT::dataTableOutput("dataset_summary")
      )
    })
  })
  
  output$color_highlight_ui <- renderUI({
    if (input$color_highlight) {
      tagList(
        colourInput("up_color", "Up-regulated color", value = "darkgreen"),
        colourInput("down_color", "Down-regulated color", value = "red")
      )
    }
  })
  
  color_palette <- c("darkviolet", "#8E44AD", "#F39C12", "#D35400", "#2C3E50", "#D4AC0D")
  
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
  
  chosen_go <- reactive({
    input$go_category
  })
  
  
  
  output$color_picker_ui <- renderUI({
    if (!input$show_go_category) {
      return(NULL)
    }
    
    req(chosen_go())
    chosen <- chosen_go()
    color_inputs <- lapply(seq_along(chosen), function(i) {
      go <- chosen[i]
      sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go)
      color_value <- color_palette[(i - 1) %% length(color_palette) + 1]
      colourInput(paste0("color_", sanitized_id), paste("Color for", go), value = color_value)
    })
    
    do.call(tagList, color_inputs)
  })
  
  
#################------------DRAW VOLCANO OBSERVER-----------------#################
    
  observeEvent(input$draw_volcano, {
    req(uploaded_df(), input$pvalue_col, input$fold_col, input$annotation_col, input$adj)
    df <- uploaded_df()
    
    df <- check_and_unlog_pvalues(df, input$pvalue_col)
    uploaded_df(df)
    
    
    pvalues <- df[[input$pvalue_col]]
    adjusted_pvalues <- p.adjust(pvalues, method = input$adj)
    df$adjusted_pvalues <- adjusted_pvalues
    uploaded_df(df)
    
    output$pvalue_distribution <- renderPrint({ 
      req(uploaded_df())
      cat("Summary of the adjusted p-values: \n\n")
      summary(adjusted_pvalues)
    })
    
    output$significant_genes <- renderPrint({
      req(uploaded_df())
      significant_genes <- df %>% filter(adjusted_pvalues < input$alpha)
      non_significant_genes <- df %>% filter(adjusted_pvalues >= input$alpha)
      cat("Number of significant genes/proteins: ", nrow(significant_genes), "\n")
      cat("Number of non-significant genes/proteins: ", nrow(non_significant_genes), "\n")
    })
    
    output$df_structure <- renderPrint({
      req(uploaded_df())
      updated_df <- uploaded_df()
      cat("Column structure of the updated data frame:\n")
      print(updated_df)
    })
    
    enrichment_results_list <- calculate_go_enrichment_table(df, input$annotation_col, input$go_category, GO, input$alpha, input$fold_col)
    
    
    output$go_enrichment_gt <- render_gt({
      req(enrichment_results_list)
      build_gt_table(
        enrichment_results_list,
        upregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0)),
        downregulated_count = nrow(df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0))
      )
    })
  
    volcano_plot <- ggplot(data = df, aes(x = round(!!sym(input$fold_col), 4), 
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
            axis.title = element_text(size = 16, color = "navy", face = "bold"),
            axis.text = element_text(size = 14, color = "navy", face = "bold")) +  
      geom_hline(yintercept = -log10(input$alpha), linetype = "dashed", color = "red") +
      scale_x_continuous(limits = c(-max(abs(df[[input$fold_col]])), max(abs(df[[input$fold_col]]))))
    
    if (input$color_highlight) {
      upregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) > 0) %>% nrow()
      downregulated_count <- df %>% filter(adjusted_pvalues < input$alpha & !!sym(input$fold_col) < 0) %>% nrow()
      volcano_plot <- volcano_plot +
        annotate("text", x = Inf, y = Inf, label = paste0("Upregulated n= ", upregulated_count), color = input$up_color, hjust = 1.1, vjust = 2, size = 6, fontface = "italic") +
        annotate("text", x = Inf, y = Inf, label = paste0("Downregulated n= ", downregulated_count), color = input$down_color, hjust = 1.1, vjust = 3, size = 6, fontface = "italic")
    }
    
    if (input$show_go_category) {
      chosen <- chosen_go()
      selected_GO <- GO %>% filter(name %in% chosen)
      
      if ("id" %in% colnames(selected_GO)) {
        go_details <- paste0(paste(chosen, unique(selected_GO$id), collapse = "\n"))
      } else {
        go_details <- paste0("GO: ", paste(chosen, collapse = ", "), "\nID: Not available")
      }
      
      volcano_plot <- volcano_plot +
        annotate("text", x = Inf, y = Inf, label = go_details, hjust = 1.1, vjust = 1, size = 5, fontface = "italic")
    }
    
    # create a reactive value to store the plot
    volcano_plot_rv(volcano_plot)
    
    output$volcano_plot <- renderPlot({
      req(volcano_plot_rv())
      print(volcano_plot_rv())
    })
    
    output$volcano_plotly <- renderPlotly({
      req(volcano_plot_rv())
      ggplotly(volcano_plot_rv(), tooltip = "text")
    })
    
  })  

#################------------DOWNLOAD HANDLERS-----------------#################


output$download_plot1 <- downloadHandler(
  filename = function() {
    paste0("volcano_plot_85x85_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
  },
  content = function(file) {
    req(volcano_plot_rv())
    publication_plot <- create_publication_plot(volcano_plot_rv(), 85, 85)
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
    publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 114)
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
    publication_plot <- create_publication_plot(volcano_plot_rv(), 114, 65)
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
    publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 174)
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
    publication_plot <- create_publication_plot(volcano_plot_rv(), 174, 98)
    ggsave(file, publication_plot, width = 174, height = 98, 
           units = "mm", device = cairo_pdf)
  }
)

}
    


shinyApp(ui = ui, server = server)
