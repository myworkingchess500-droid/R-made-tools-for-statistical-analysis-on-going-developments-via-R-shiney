# ==============================================================================
# Data Coding, Text/Numeric Recoding, 0-5 Inverter & Balanced Sampling Shiny App
# Single-file R Shiny application (app.R)
# ==============================================================================

# 1. Package Management & Setup ------------------------------------------------
required_packages <- c(
  "shiny", "DT", "tidyverse", "data.table", "janitor", "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(openxlsx)
})

options(shiny.maxRequestSize = 100 * 1024^2)

# Helper: Re-apply all codebook rules from df_raw() ----------------------------
apply_codebook_rules <- function(raw_df, rules_df) {
  if (is.null(raw_df)) return(NULL)
  proc_df <- as.data.frame(raw_df, stringsAsFactors = FALSE)
  
  if (is.null(rules_df) || nrow(rules_df) == 0) {
    return(proc_df)
  }
  
  for (i in seq_len(nrow(rules_df))) {
    var_name <- rules_df$Variable[i]
    rule_type <- rules_df$Rule_Type[i]
    orig_spec <- rules_df$Original_Value_or_Range[i]
    new_code <- as.character(rules_df$Assigned_Code[i])
    
    if (!var_name %in% names(proc_df)) next
    
    if (rule_type == "exact") {
      target_vals <- strsplit(orig_spec, " \\| ")[[1]]
      col_chr <- as.character(proc_df[[var_name]])
      matched <- !is.na(col_chr) & (col_chr %in% target_vals)
      col_chr[matched] <- new_code
      proc_df[[var_name]] <- col_chr
    } else if (rule_type == "range") {
      bounds <- suppressWarnings(as.numeric(strsplit(orig_spec, " to ")[[1]]))
      if (length(bounds) == 2 && !any(is.na(bounds))) {
        col_num <- suppressWarnings(as.numeric(as.character(proc_df[[var_name]])))
        col_chr <- as.character(proc_df[[var_name]])
        matched <- !is.na(col_num) & (col_num >= bounds[1]) & (col_num <= bounds[2])
        col_chr[matched] <- new_code
        proc_df[[var_name]] <- col_chr
      }
    } else if (rule_type == "reverse") {
      clean_spec <- gsub("Reverse: ", "", orig_spec)
      bounds <- suppressWarnings(as.numeric(strsplit(clean_spec, " to ")[[1]]))
      if (length(bounds) == 2 && !any(is.na(bounds))) {
        min_val <- bounds[1]
        max_val <- bounds[2]
        col_num <- suppressWarnings(as.numeric(as.character(proc_df[[var_name]])))
        matched <- !is.na(col_num) & (col_num >= min_val) & (col_num <= max_val)
        col_num[matched] <- (min_val + max_val) - col_num[matched]
        proc_df[[var_name]] <- col_num
      }
    }
  }
  
  # Convert columns to numeric only if ALL non-NA values are valid numbers
  for (col in names(proc_df)) {
    vals <- proc_df[[col]]
    non_na <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
    if (length(non_na) > 0) {
      converted <- suppressWarnings(as.numeric(as.character(non_na)))
      if (!any(is.na(converted))) {
        proc_df[[col]] <- suppressWarnings(as.numeric(as.character(vals)))
      }
    }
  }
  
  proc_df
}

# Helper: Balanced Sampling with Deficit Redistribution ------------------------
balanced_sample_group <- function(df_sub, balance_col, total_limit) {
  if (nrow(df_sub) <= total_limit) {
    return(df_sub)
  }
  
  if (is.null(balance_col) || balance_col == "None" || !balance_col %in% names(df_sub)) {
    return(df_sub[sample(seq_len(nrow(df_sub)), total_limit), , drop = FALSE])
  }
  
  split_vals <- as.character(df_sub[[balance_col]])
  split_vals[is.na(split_vals) | trimws(split_vals) == ""] <- "(Missing)"
  
  counts <- sort(table(split_vals))
  subgroups <- names(counts)
  m <- length(subgroups)
  remaining_needed <- total_limit
  
  sampled_indices <- integer(0)
  
  for (i in seq_len(m)) {
    sg <- subgroups[i]
    avail_idx <- which(split_vals == sg)
    avail_n <- length(avail_idx)
    
    target_n <- floor(remaining_needed / (m - i + 1))
    take_n <- min(avail_n, target_n)
    
    if (take_n > 0) {
      picked <- sample(avail_idx, take_n)
      sampled_indices <- c(sampled_indices, picked)
      remaining_needed <- remaining_needed - take_n
    }
  }
  
  df_sub[sort(sampled_indices), , drop = FALSE]
}

# 2. UI ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Data Coding, Recoding, 0-5 Inverter & Sampling App"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput(
        inputId = "file_upload",
        label = "Upload Dataset",
        accept = c(".csv", ".tsv", ".xlsx", ".rds"),
        placeholder = "CSV, TSV, XLSX, or RDS"
      ),
      checkboxInput(
        inputId = "clean_col_names",
        label = "Clean column names (janitor)",
        value = FALSE
      ),
      hr(),
      actionButton(
        inputId = "reset_data",
        label = "Reset to Original Data",
        icon = icon("rotate-left"),
        class = "btn-outline-secondary w-100 mb-2"
      ),
      downloadButton(
        outputId = "download_excel",
        label = "Download Excel (.xlsx)",
        class = "btn-primary w-100"
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Row 1: Recoding Engine & Ordinal Inverter (Side-by-Side)
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("Recoding & Code Sheet Engine"),
            selectInput("selected_col", "Select Column to Recode", choices = NULL),
            radioButtons(
              inputId = "recode_mode",
              label = "Recoding Mode",
              choices = c("Exact Value(s)" = "exact", "Numeric Range" = "range"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.recode_mode == 'exact'",
              selectizeInput("selected_values", "Select Value(s)", choices = NULL, multiple = TRUE)
            ),
            conditionalPanel(
              condition = "input.recode_mode == 'range'",
              fluidRow(
                column(6, numericInput("range_min", "Min Value", value = NA)),
                column(6, numericInput("range_max", "Max Value", value = NA))
              )
            ),
            textInput("assigned_code", "Assign Code (Text or Number)", value = "", placeholder = "e.g., High, 1"),
            actionButton("apply_code", "Apply Code", icon = icon("check"), class = "btn-success w-100")
          ),
          wellPanel(
            h4("Ordinal Inverter (0-5 Scale)"),
            selectInput("rev_col", "Select Column to Invert", choices = NULL),
            fluidRow(
              column(6, numericInput("rev_min", "Min Scale", value = 0)),
              column(6, numericInput("rev_max", "Max Scale", value = 5))
            ),
            actionButton("apply_reverse", "Apply Inverter (0-5 Reverse)", icon = icon("exchange-alt"), class = "btn-warning w-100")
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Live Codebook / Code Sheet"),
            DTOutput("codebook_table"),
            br(),
            actionButton("delete_rule", "Undo / Delete Selected Rule", icon = icon("trash"), class = "btn-danger btn-sm")
          )
        )
      ),
      
      # Row 2: Balanced Distribution / Sampling Controls
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Balanced Distribution / Sampling"),
            fluidRow(
              column(3, selectInput("sample_group_col", "Group Column", choices = c("None" = "None"))),
              column(3, selectInput("sample_group_val", "Target Group", choices = c("All Groups" = "ALL"))),
              column(3, selectInput("sample_balance_col", "Balance / Split Column", choices = c("None" = "None"))),
              column(2, numericInput("sample_limit", "Limit per Group", value = 12, min = 1)),
              column(1, br(), actionButton("run_sampling", "Sample", icon = icon("shuffle"), class = "btn-info"))
            ),
            uiOutput("sampling_summary_ui")
          )
        )
      ),
      
      # Row 3: Tabbed Data Previews
      fluidRow(
        column(
          width = 12,
          tabsetPanel(
            tabPanel("Live Coded Data", br(), DTOutput("data_preview")),
            tabPanel("Sampled / Distributed Data", br(), DTOutput("sampled_preview"))
          )
        )
      )
    )
  )
)

# 3. Server --------------------------------------------------------------------
server <- function(input, output, session) {
  df_raw <- reactiveVal(NULL)
  df_proc <- reactiveVal(NULL)
  df_sampled <- reactiveVal(NULL)
  
  codebook <- reactiveVal(
    tibble(
      Variable = character(),
      Original_Value_or_Range = character(),
      Assigned_Code = character(),
      Rule_Type = character()
    )
  )
  
  # Load uploaded file ---------------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    path <- input$file_upload$datapath
    ext <- tolower(tools::file_ext(input$file_upload$name))
    
    tryCatch({
      loaded <- switch(
        ext,
        "csv" = data.table::fread(path, data.table = FALSE, colClasses = "character", na.strings = c("", "NA", "N/A")),
        "tsv" = data.table::fread(path, sep = "\t", data.table = FALSE, colClasses = "character", na.strings = c("", "NA", "N/A")),
        "xlsx" = openxlsx::read.xlsx(path, detectDates = FALSE),
        "rds" = as.data.frame(readRDS(path), stringsAsFactors = FALSE),
        stop("Unsupported file format.")
      )
      
      if (isTRUE(input$clean_col_names)) {
        loaded <- janitor::clean_names(loaded)
      }
      
      loaded[] <- lapply(loaded, function(x) {
        if (is.factor(x)) as.character(x) else x
      })
      
      df_raw(loaded)
      df_proc(loaded)
      df_sampled(NULL)
      codebook(
        tibble(
          Variable = character(),
          Original_Value_or_Range = character(),
          Assigned_Code = character(),
          Rule_Type = character()
        )
      )
      
      col_names <- names(loaded)
      updateSelectInput(session, "selected_col", choices = col_names)
      updateSelectInput(session, "rev_col", choices = col_names)
      updateSelectInput(session, "sample_group_col", choices = c("None" = "None", col_names))
      updateSelectInput(session, "sample_balance_col", choices = c("None" = "None", col_names))
      
      showNotification("Dataset loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Update unique values for recoding ------------------------------------------
  observe({
    req(df_proc(), input$selected_col)
    req(input$selected_col %in% names(df_proc()))
    
    col_vals <- as.character(df_proc()[[input$selected_col]])
    unique_vals <- sort(unique(col_vals[!is.na(col_vals) & trimws(col_vals) != ""]))
    
    updateSelectizeInput(
      session,
      "selected_values",
      choices = unique_vals,
      selected = character(0),
      server = TRUE
    )
  })
  
  # Update target group values for sampling ------------------------------------
  observe({
    req(df_proc(), input$sample_group_col)
    if (input$sample_group_col == "None" || !input$sample_group_col %in% names(df_proc())) {
      updateSelectInput(session, "sample_group_val", choices = c("All Rows" = "ALL"))
    } else {
      vals <- as.character(df_proc()[[input$sample_group_col]])
      u_vals <- sort(unique(vals[!is.na(vals) & trimws(vals) != ""]))
      updateSelectInput(
        session,
        "sample_group_val",
        choices = c("All Groups (Limit Each)" = "ALL", u_vals)
      )
    }
  })
  
  # Apply coding rule ----------------------------------------------------------
  observeEvent(input$apply_code, {
    req(df_raw(), df_proc(), input$selected_col)
    
    new_code <- trimws(as.character(input$assigned_code))
    if (!nzchar(new_code)) {
      showNotification("Please enter a valid code (text or number).", type = "warning")
      return()
    }
    
    var_name <- input$selected_col
    
    if (input$recode_mode == "exact") {
      req(length(input$selected_values) > 0)
      orig_spec <- paste(input$selected_values, collapse = " | ")
      new_rule <- tibble(
        Variable = var_name,
        Original_Value_or_Range = orig_spec,
        Assigned_Code = new_code,
        Rule_Type = "exact"
      )
    } else {
      if (is.na(input$range_min) || is.na(input$range_max) || input$range_min > input$range_max) {
        showNotification("Please provide a valid Min <= Max numeric range.", type = "warning")
        return()
      }
      orig_spec <- paste(input$range_min, "to", input$range_max)
      new_rule <- tibble(
        Variable = var_name,
        Original_Value_or_Range = orig_spec,
        Assigned_Code = new_code,
        Rule_Type = "range"
      )
    }
    
    updated_cb <- bind_rows(codebook(), new_rule)
    codebook(updated_cb)
    df_proc(apply_codebook_rules(df_raw(), updated_cb))
    
    showNotification(paste0("Applied code '", new_code, "' to ", var_name, "."), type = "message")
  })
  
  # Apply Ordinal Inverter (Reverse Coding 0-5) --------------------------------
  observeEvent(input$apply_reverse, {
    req(df_raw(), df_proc(), input$rev_col)
    
    var_name <- input$rev_col
    min_val <- input$rev_min
    max_val <- input$rev_max
    
    if (is.na(min_val) || is.na(max_val) || min_val >= max_val) {
      showNotification("Please provide valid Min and Max scale bounds (Min < Max).", type = "warning")
      return()
    }
    
    orig_spec <- paste(min_val, "to", max_val)
    new_rule <- tibble(
      Variable = var_name,
      Original_Value_or_Range = paste("Reverse:", orig_spec),
      Assigned_Code = paste0("Inverted (", min_val, "-", max_val, ")"),
      Rule_Type = "reverse"
    )
    
    updated_cb <- bind_rows(codebook(), new_rule)
    codebook(updated_cb)
    df_proc(apply_codebook_rules(df_raw(), updated_cb))
    
    showNotification(paste0("Applied ordinal inverter to ", var_name, " range [", min_val, ", ", max_val, "]."), type = "message")
  })
  
  # Undo / Delete selected rule ------------------------------------------------
  observeEvent(input$delete_rule, {
    req(df_raw())
    sel <- input$codebook_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select a rule row in the Codebook table to delete.", type = "warning")
      return()
    }
    
    updated_cb <- codebook()[-sel, , drop = FALSE]
    codebook(updated_cb)
    df_proc(apply_codebook_rules(df_raw(), updated_cb))
    showNotification("Selected rule removed and data updated.", type = "message")
  })
  
  # Reset to original data -----------------------------------------------------
  observeEvent(input$reset_data, {
    req(df_raw())
    df_proc(df_raw())
    df_sampled(NULL)
    codebook(
      tibble(
        Variable = character(),
        Original_Value_or_Range = character(),
        Assigned_Code = character(),
        Rule_Type = character()
      )
    )
    showNotification("Data reset to original uploaded state.", type = "message")
  })
  
  # Run Balanced Sampling ------------------------------------------------------
  observeEvent(input$run_sampling, {
    req(df_proc())
    limit_n <- as.integer(input$sample_limit)
    if (is.na(limit_n) || limit_n < 1) {
      showNotification("Please enter a valid Limit >= 1.", type = "warning")
      return()
    }
    
    dat <- df_proc()
    grp_col <- input$sample_group_col
    grp_val <- input$sample_group_val
    bal_col <- input$sample_balance_col
    
    if (grp_col == "None" || !grp_col %in% names(dat)) {
      res <- balanced_sample_group(dat, bal_col, limit_n)
    } else if (grp_val != "ALL") {
      sub_dat <- dat[!is.na(dat[[grp_col]]) & as.character(dat[[grp_col]]) == grp_val, , drop = FALSE]
      res <- balanced_sample_group(sub_dat, bal_col, limit_n)
    } else {
      groups <- unique(as.character(dat[[grp_col]]))
      groups <- groups[!is.na(groups)]
      res_list <- lapply(groups, function(g) {
        sub_dat <- dat[!is.na(dat[[grp_col]]) & as.character(dat[[grp_col]]) == g, , drop = FALSE]
        balanced_sample_group(sub_dat, bal_col, limit_n)
      })
      res <- bind_rows(res_list)
    }
    
    df_sampled(res)
    showNotification(paste0("Sampled ", nrow(res), " rows."), type = "message")
  })
  
  output$sampling_summary_ui <- renderUI({
    req(df_sampled())
    res <- df_sampled()
    bal_col <- input$sample_balance_col
    
    msg <- paste0("Total Sampled Rows: ", nrow(res))
    if (!is.null(bal_col) && bal_col != "None" && bal_col %in% names(res)) {
      counts <- table(as.character(res[[bal_col]]))
      breakdown <- paste(names(counts), counts, sep = ": ", collapse = " | ")
      msg <- paste0(msg, "  —  Breakdown (", bal_col, "): ", breakdown)
    }
    div(class = "alert alert-info py-2 mb-0 mt-2", msg)
  })
  
  # Render Live Codebook -------------------------------------------------------
  output$codebook_table <- renderDT({
    cb_display <- codebook() %>%
      select(Variable, Original_Value_or_Range, Assigned_Code)
    
    datatable(
      cb_display,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 6, dom = "tip", language = list(emptyTable = "No coding rules applied yet."))
    )
  })
  
  # Render Live Data Previews --------------------------------------------------
  output$data_preview <- renderDT({
    req(df_proc())
    datatable(df_proc(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$sampled_preview <- renderDT({
    req(df_sampled())
    datatable(df_sampled(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Excel Export (.xlsx) -------------------------------------------------------
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("coded_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      req(df_proc())
      
      wb <- openxlsx::createWorkbook()
      
      # Sheet 1: Coded_Data
      openxlsx::addWorksheet(wb, "Coded_Data")
      openxlsx::writeData(wb, sheet = "Coded_Data", x = df_proc())
      
      # Sheet 2: Code_Sheet
      openxlsx::addWorksheet(wb, "Code_Sheet")
      cb_export <- codebook() %>%
        select(Variable, Original_Value_or_Range, Assigned_Code)
      
      openxlsx::writeData(wb, sheet = "Code_Sheet", x = cb_export)
      
      header_style <- openxlsx::createStyle(
        fontName = "Calibri",
        fontSize = 11,
        fontColour = "#FFFFFF",
        fgFill = "#2C3E50",
        halign = "LEFT",
        textDecoration = "bold"
      )
      openxlsx::addStyle(wb, sheet = "Code_Sheet", style = header_style, rows = 1, cols = seq_len(ncol(cb_export)), gridExpand = TRUE)
      openxlsx::setColWidths(wb, sheet = "Code_Sheet", cols = 1:3, widths = "auto")
      
      # Sheet 3: Sampled_Data (if sampling was run)
      if (!is.null(df_sampled()) && nrow(df_sampled()) > 0) {
        openxlsx::addWorksheet(wb, "Sampled_Data")
        openxlsx::writeData(wb, sheet = "Sampled_Data", x = df_sampled())
      }
      
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)