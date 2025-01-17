diagnose_dataframe_v4 <- function(df, log_messages_rv, log_event) {
  # Define keywords for identifying numeric-like columns
  keywords <- c("log", "fold", "pvalue", "padj", "mean", "std", "variance", 
                "count", "value", "diff", "change", "ratio", "score", "rank")
  
  # Find potential numeric columns based on keywords
  potential_numeric_cols <- grep(
    paste(keywords, collapse = "|"), 
    colnames(df), 
    value = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(potential_numeric_cols) == 0) {
    log_event(log_messages_rv, "No numeric-like columns detected.", "INFO from diagnose_dataframe_v4")
    return(df)
  }
  
  # Check for non-numeric values in numeric-like columns
  invalid_rows_list <- lapply(potential_numeric_cols, function(col) {
    suspicious_rows <- which(!is.na(df[[col]]) & is.na(suppressWarnings(as.numeric(df[[col]]))))
    if (length(suspicious_rows) > 0) {
      log_event(
        log_messages_rv,
        paste0("Column '", col, "' has ", length(suspicious_rows), " suspicious (non-numeric) entries."),
        "WARN from diagnose_dataframe_v4"
      )
      list(
        rows = suspicious_rows,
        count = length(suspicious_rows),
        column = col
      )
    } else {
      NULL
    }
  })
  
  # Remove NULL entries and get unique rows
  invalid_rows_list <- Filter(function(x) !is.null(x), invalid_rows_list)
  all_suspicious_rows <- unique(unlist(lapply(invalid_rows_list, function(x) x$rows)))
  
  if (length(all_suspicious_rows) == 0) {
    log_event(log_messages_rv, "No suspicious non-numeric entries found.", "INFO from diagnose_dataframe_v4")
    return(df)
  }
  
  # Create detailed message for each problematic column
  column_messages <- sapply(invalid_rows_list, function(x) {
    sprintf("• %d suspicious non-numeric and not NA values found in column '%s' that is supposed to be numeric",
            x$count,
            x$column)
  })
  
  # Build the complete alert text
  alert_text <- paste0(
    "<div style='text-align: left;'>",
    "<strong>Data Processing Warning:</strong><br/><br/>",
    paste(column_messages, collapse = "<br/>"),
    "<br/><br/>",
    "Total rows to be removed: ", length(all_suspicious_rows),
    "<br/><br/>",
    "Click 'Proceed' to remove these rows and convert columns to numeric format.",
    "</div>"
  )
  
  # Show alert and handle cleaning
  shinyalert::shinyalert(
    title = "Data Warning",
    text = alert_text,
    type = "warning",
    html = TRUE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "Proceed",
    callbackR = function(user_proceeded) {
      if (isTRUE(user_proceeded)) {
        log_event(
          log_messages_rv,
          sprintf("User proceeded. Removing %d suspicious rows.", length(all_suspicious_rows)),
          "INFO from diagnose_dataframe_v4"
        )
        
        # Create cleaned version of the dataframe
        cleaned_df <- df[-all_suspicious_rows, ]
        
        # Coerce problematic columns to numeric
        for (col_name in potential_numeric_cols) {
          cleaned_df[[col_name]] <- suppressWarnings(as.numeric(cleaned_df[[col_name]]))
        }
        
        log_event(log_messages_rv, "Data cleaning completed successfully", "SUCCESS from diagnose_dataframe_v4")
        return(cleaned_df)
      } else {
        log_event(log_messages_rv, "User cancelled data cleaning", "INFO from diagnose_dataframe_v4")
        return(df)
      }
    }
  )
}

diagnose_expression_column <- function(df, log_messages_rv, log_event, uploaded_df) {
  # Define keywords for identifying log2 fold change columns
  keywords <- c("log2.*fold", "log2.*fc", "l2fc", "fold.*change", "expression.*ratio")
  
  # Find potential expression columns based on keywords
  potential_expr_cols <- grep(
    paste(keywords, collapse = "|"), 
    colnames(df), 
    value = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(potential_expr_cols) == 0) {
    log_event(log_messages_rv, 
              "No log2 fold change columns detected.", 
              "INFO from diagnose_expression_column")
    return(df)
  }
  
  # Analyze each expression column for extreme values
  suspicious_data <- lapply(potential_expr_cols, function(col) {
    # Try to ensure column is numeric
    if (!is.numeric(df[[col]])) {
      log_event(log_messages_rv,
                sprintf("Column '%s' is not numeric - attempting to coerce", col),
                "WARN from diagnose_expression_column")
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
    
    if (is.numeric(df[[col]])) {
      # Find values with absolute log2 fold change > 10
      extreme_values <- which(abs(df[[col]]) > 10)
      n_extreme <- length(extreme_values)
      
      if (n_extreme > 0) {
        log_event(log_messages_rv,
                  sprintf("Column '%s' has %d values with |log2 fold change| > 10",
                          col, n_extreme),
                  "WARN from diagnose_expression_column")
        list(
          column = col,
          rows = extreme_values,
          count = n_extreme,
          values = df[[col]][extreme_values]
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  # Remove NULL entries
  suspicious_data <- Filter(function(x) !is.null(x), suspicious_data)
  
  if (length(suspicious_data) == 0) {
    log_event(log_messages_rv,
              "No suspicious expression values detected.",
              "INFO from diagnose_expression_column")
    return(df)
  }
  
  # Create detailed message for each problematic column
  column_messages <- sapply(suspicious_data, function(x) {
    sprintf("• Column '%s' contains %d extreme values (|log2 fold change| > 10)<br/>  Values: %s",
            x$column,
            x$count,
            paste(round(x$values, 2), collapse = ", "))
  })
  
  # Get all suspicious rows
  all_suspicious_rows <- unique(unlist(lapply(suspicious_data, function(x) x$rows)))
  
  # Build alert text with HTML formatting
  alert_text <- paste0(
    "<div style='text-align: left;'>",
    "<strong>Suspicious Log2 Fold Change Values Detected:</strong><br/><br/>",
    paste(column_messages, collapse = "<br/><br/>"),
    "<br/><br/>",
    "Total rows to be removed: ", length(all_suspicious_rows),
    "<br/><br/>",
    "Click 'Proceed' to remove rows with extreme fold changes.",
    "</div>"
  )
  
  # Create a promise to handle the asynchronous alert
  result_df <- df  # Default to original df
  
  # Show alert
  shinyalert::shinyalert(
    title = "Expression Data Warning",
    text = alert_text,
    type = "warning",
    html = TRUE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "Proceed",
    callbackR = function(user_proceeded) {
      if (isTRUE(user_proceeded)) {
        log_event(
          log_messages_rv,
          sprintf("User proceeded. Removing %d rows with extreme fold changes.",
                  length(all_suspicious_rows)),
          "INFO from diagnose_expression_column"
        )
        
        # Remove rows with extreme values
        result_df <- df[-all_suspicious_rows, ]
        
        log_event(
          log_messages_rv,
          "Extreme fold change values removed successfully",
          "SUCCESS from diagnose_expression_column"
        )
        return(result_df)
      } else {
        log_event(log_messages_rv, 
                  "User cancelled extreme value removal", 
                  "INFO from diagnose_expression_column")
        return(df)
      }
    }
  )
}