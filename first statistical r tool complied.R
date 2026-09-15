#Arpon,Elijah P.
#Edoc,Sebastien Alexandre B.
#Important note this is a two part code the first is a one shot run the other is an app
#for producing similar results remmeber to run them by parts for part one lines
# ==============================================================================
# UNIFIED ENTERPRISE MULTIVARIATE ANALYTICS & HITL IMPUTATION PLATFORM
# ==============================================================================

# ==============================================================================
# 1. PACKAGE MANAGEMENT & SYSTEM INITIALIZATION
# ==============================================================================
required_packages <- c(
  "shiny", "bslib", "data.table", "tidyverse", "plotly", "DT", 
  "jsonlite", "janitor", "tools", "openxlsx", "ggplot2", 
  "car", "corrplot", "ggridges", "hexbin", "pROC", "mice", "naniar", "scales"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  install.packages(missing_packages, dependencies = TRUE, Ncpus = max(1, parallel::detectCores() - 1))
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(data.table)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(jsonlite)
  library(janitor)
  library(tools)
  library(openxlsx)
  library(ggplot2)
  library(car)
  library(corrplot)
  library(ggridges)
  library(hexbin)
  library(pROC)
  library(mice)
  library(naniar)
  library(scales)
})

options(shiny.maxRequestSize = 100 * 1024^2)

# ==============================================================================
# 2. STATISTICAL & AUXILIARY ESTIMATION HELPERS
# ==============================================================================

get_mode <- function(v) {
  v <- na.omit(v)
  if (length(v) == 0) return(NA)
  tbl <- table(v)
  mod_val <- names(tbl)[which.max(tbl)]
  if (is.numeric(v)) return(as.numeric(mod_val))
  mod_val
}

mice.impute.mode <- function(y, ry, x, ...) {
  observed_vals <- y[ry]
  tbl <- table(observed_vals)
  mod_val <- names(tbl)[which.max(tbl)]
  if (is.numeric(y)) {
    mod_val <- as.numeric(mod_val)
  } else if (is.factor(y)) {
    mod_val <- factor(mod_val, levels = levels(y))
  }
  rep(mod_val, sum(!ry))
}

mice.impute.polyreg_num <- function(y, ry, x, ...) {
  y_fac <- as.factor(y)
  imp_fac <- mice::mice.impute.polyreg(y_fac, ry, x, ...)
  if (is.numeric(y)) {
    res_num <- suppressWarnings(as.numeric(as.character(imp_fac)))
    if (any(is.na(res_num) & !is.na(imp_fac))) {
      return(as.numeric(imp_fac))
    }
    return(res_num)
  } else {
    return(imp_fac)
  }
}

run_littles_mcar_test <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      Interpretation = "No data uploaded."
    ))
  }
  
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) < 2) {
    return(data.frame(
      Statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      Interpretation = "Insufficient numeric variables (minimum 2 required)."
    ))
  }
  
  miss_counts <- vapply(df[, num_cols, drop = FALSE], function(x) sum(is.na(x)), integer(1))
  cols_with_miss <- names(miss_counts[miss_counts > 0])
  
  selected_cols <- unique(c(cols_with_miss[1:min(6, length(cols_with_miss))], num_cols))
  selected_cols <- selected_cols[1:min(8, length(selected_cols))]
  test_data <- df[, selected_cols, drop = FALSE]
  
  if (nrow(test_data) > 1000) {
    set.seed(42)
    test_data <- test_data[sample(nrow(test_data), 1000), , drop = FALSE]
  }
  
  tryCatch({
    mcar_res <- naniar::mcar_test(test_data)
    p_val <- mcar_res$p.value
    interp <- if (is.na(p_val)) {
      "Undetermined"
    } else if (p_val > 0.05) {
      "Fail to reject MCAR (Data consistent with MCAR, p > 0.05)"
    } else {
      "Reject MCAR (Data suggests MAR or MNAR, p <= 0.05)"
    }
    
    data.frame(
      Statistic = round(mcar_res$statistic, 4),
      df = mcar_res$df,
      p_value = round(p_val, 5),
      Interpretation = interp
    )
  }, error = function(e) {
    data.frame(
      Statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      Interpretation = paste("MCAR skipped:", e$message)
    )
  })
}

recommend_imputation_methods <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list())
  
  cols <- names(data)
  n_rows <- nrow(data)
  recommendations <- vector("list", length(cols))
  names(recommendations) <- cols
  
  for (col in cols) {
    vec <- data[[col]]
    n_miss <- sum(is.na(vec))
    prop_miss <- n_miss / n_rows
    
    if (n_miss == 0) {
      rec <- ""
      rationale <- "No missing values present."
    } else if (is.numeric(vec)) {
      v_clean <- na.omit(vec)
      if (length(v_clean) > 2000) v_clean <- sample(v_clean, 2000)
      
      n <- length(v_clean)
      m3 <- sum((v_clean - mean(v_clean))^3) / n
      s3 <- (sqrt(sum((v_clean - mean(v_clean))^2) / (n - 1)))^3
      skewness <- if (s3 > 0) m3 / s3 else 0
      
      if (abs(skewness) > 1.0) {
        rec <- "pmm"
        rationale <- paste0("Skewed numeric (skewness = ", round(skewness, 2), "). Predictive Mean Matching suggested.")
      } else {
        rec <- "norm"
        rationale <- paste0("Symmetric numeric (skewness = ", round(skewness, 2), "). Bayesian Linear Regression suggested.")
      }
    } else if (is.factor(vec) || is.character(vec)) {
      vec_fac <- as.factor(vec)
      n_levels <- nlevels(vec_fac)
      
      if (n_levels == 2) {
        rec <- "logreg"
        rationale <- "Binary factor vector. Logistic Regression suggested."
      } else if (is.ordered(vec_fac)) {
        rec <- "polr"
        rationale <- paste0("Ordered factor with ", n_levels, " levels. Proportional Odds suggested.")
      } else {
        rec <- "polyreg"
        rationale <- paste0("Unordered factor with ", n_levels, " levels. Polytomous Regression suggested.")
      }
    } else {
      rec <- "pmm"
      rationale <- "Default fallback: Predictive Mean Matching."
    }
    
    recommendations[[col]] <- list(
      n_miss = n_miss,
      pct_miss = round(prop_miss * 100, 2),
      recommended_method = rec,
      rationale = rationale
    )
  }
  return(recommendations)
}

compute_distribution_profiles <- function(df_orig, df_imputed) {
  if (is.null(df_orig) || is.null(df_imputed)) {
    return(data.frame(Status = "Data unavailable for profiling."))
  }
  
  num_cols <- names(df_orig)[vapply(df_orig, is.numeric, logical(1))]
  if (length(num_cols) == 0) {
    return(data.frame(Status = "No numeric variables available for distribution profiling."))
  }
  
  profiles <- lapply(num_cols, function(col) {
    v_orig <- na.omit(df_orig[[col]])
    v_imp  <- df_imputed[[col]]
    
    if (length(v_orig) == 0 || is.null(v_imp) || any(is.na(v_imp))) return(NULL)
    
    data.frame(
      Variable    = col,
      Orig_Mean   = round(mean(v_orig), 3),
      Imp_Mean    = round(mean(v_imp), 3),
      Orig_SD     = round(sd(v_orig), 3),
      Imp_SD      = round(sd(v_imp), 3),
      Orig_Median = round(median(v_orig), 3),
      Imp_Median  = round(median(v_imp), 3),
      Orig_IQR    = round(IQR(v_orig), 3),
      Imp_IQR     = round(IQR(v_imp), 3),
      Orig_Min    = round(min(v_orig), 3),
      Imp_Min     = round(min(v_imp), 3),
      Orig_Max    = round(max(v_orig), 3),
      Imp_Max     = round(max(v_imp), 3),
      stringsAsFactors = FALSE
    )
  })
  
  res <- do.call(rbind, profiles)
  if (is.null(res)) data.frame(Status = "No complete numeric comparisons available.") else res
}

parse_uploaded_file <- function(file_path, file_ext) {
  tryCatch({
    ext <- tolower(file_ext)
    if (ext %in% c("csv", "tsv")) {
      df <- data.table::fread(file_path, data.table = FALSE)
    } else if (ext == "json") {
      df <- jsonlite::fromJSON(file_path)
      if (!is.data.frame(df)) df <- as.data.frame(df)
    } else if (ext == "rds") {
      df <- readRDS(file_path)
    } else if (ext == "xlsx") {
      df <- openxlsx::read.xlsx(file_path)
      if (!is.data.frame(df)) df <- as.data.frame(df)
    } else {
      stop("Unsupported format. Please upload .csv, .tsv, .json, .rds, or .xlsx")
    }
    return(janitor::clean_names(as.data.frame(df)))
  }, error = function(e) {
    stop(paste("File Parsing Error:", e$message))
  })
}

# ==============================================================================
# 3. UNIFIED USER INTERFACE
# ==============================================================================
ui <- page_navbar(
  title = "Unified Multivariate Analytics & HITL Imputation Engine",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    success = "#18BC9C",
    base_font = font_google("Inter")
  ),
  collapsible = TRUE,
  
  sidebar = sidebar(
    width = 330,
    title = "Ingestion & Master Controls",
    fileInput("file_input", "Upload Dataset (.csv, .tsv, .json, .rds, .xlsx)", 
              accept = c(".csv", ".tsv", ".json", ".rds", ".xlsx")),
    actionButton("btn_reset_pipeline", "Reset Pipeline & Overrides", class = "btn-danger w-100 mb-3", icon = icon("rotate-left")),
    hr(),
    downloadButton("btn_export_excel", "Export Master Multi-Sheet Excel Package", class = "btn-success w-100")
  ),
  
  nav_panel(
    title = "Data & Feature Types",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        title = "Feature Management",
        h6("Feature Scale Classification"),
        selectInput("class_col", "Select Column:", choices = NULL),
        selectInput("class_type", "Set Measurement Scale:", 
                    choices = c("Continuous / Metric", "Nominal (Categorical)", "Ordinal (Ordered)")),
        actionButton("btn_apply_class", "Update Scale Setting", class = "btn-info w-100 mb-3"),
        hr(),
        h6("Quick Univariate Imputation"),
        selectInput("imp_col", "Target Column:", choices = NULL),
        selectInput("imp_method", "Method:", choices = c("Mean", "Median", "Mode", "Custom Value")),
        conditionalPanel(
          condition = "input.imp_method == 'Custom Value'",
          textInput("imp_custom_val", "Custom Value:", value = "0")
        ),
        actionButton("btn_apply_imp", "Apply Univariate Imputation", class = "btn-primary w-100 mb-3"),
        hr(),
        h6("Mathematical Feature Transformations"),
        selectInput("trans_col", "Target Numeric Column:", choices = NULL),
        selectInput("trans_method", "Transformation:", choices = c("Log (ln)", "Log1p (ln(x+1))", "Standardize (Z-score)", "Min-Max Scale")),
        actionButton("btn_apply_trans", "Apply Transformation", class = "btn-primary w-100")
      ),
      card(
        card_header("Variable Measurement Scale Metadata"),
        DTOutput("tbl_feature_types")
      ),
      card(
        card_header("Active Dataset View"),
        DTOutput("tbl_data_proc")
      )
    )
  ),
  
  nav_panel(
    title = "Missingness & Patterns",
    icon = icon("microscope"),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Active Raw Data (df_raw)"),
        DTOutput("tbl_raw_data")
      ),
      card(
        card_header("Little's MCAR Test & Missingness Proportions"),
        h6("Little's MCAR Test Diagnostic:"),
        tableOutput("tbl_mcar_test"),
        hr(),
        h6("Variable Missingness Rates:"),
        DTOutput("tbl_missing_prop")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Missingness Combination Pattern Map"),
        plotlyOutput("plot_missing_pattern", height = "360px")
      )
    )
  ),
  
  nav_panel(
    title = "HITL Imputation & Pooling",
    icon = icon("wand-magic-sparkles"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Machine Recommendations vs. Human Overrides"),
        uiOutput("ui_machine_recommendation_summary"),
        hr(),
        h6("Global Imputation Strategy:"),
        selectInput("sel_global_strategy", "Imputation Paradigm:", choices = c(
          "MICE (Multiple Imputation)",
          "Single Imputation (PMM)",
          "Listwise Deletion (Complete Cases)"
        )),
        conditionalPanel(
          condition = "input.sel_global_strategy == 'MICE (Multiple Imputation)'",
          numericInput("num_m", "Number of Imputations (m):", value = 5, min = 2, max = 20),
          numericInput("num_maxit", "MICE Iterations (maxit):", value = 5, min = 2, max = 30)
        ),
        hr(),
        h6("Variable Method Overrides:"),
        selectInput("sel_target_var", "Select Variable to Override:", choices = NULL),
        selectInput("sel_override_method", "Method Override:", choices = c(
          "Automatic (Use Machine Proposal)" = "Automatic (Use Machine Proposal)",
          "pmm (Predictive Mean Matching)" = "pmm",
          "norm (Linear Regression)" = "norm",
          "logreg (Logistic Regression)" = "logreg",
          "polr (Proportional Odds)" = "polr",
          "polyreg (Multinomial)" = "polyreg",
          "rf (Random Forest)" = "rf",
          "sample (Random Sample)" = "sample",
          "mode (Mode Imputation)" = "mode"
        )),
        selectInput("sel_override_mechanism", "Assumed Mechanism Override:", choices = c(
          "Automatic (Use Little's Test)",
          "MCAR (Missing Completely at Random)",
          "MAR (Missing at Random)",
          "MNAR (Missing Not at Random)"
        )),
        textAreaInput("txt_override_justification", "Human Decision Justification / Rationale:", 
                      placeholder = "Enter domain or statistical rationale for overriding proposal..."),
        actionButton("btn_apply_override", "Apply Method Override to Variable", class = "btn-primary w-100 mb-2"),
        actionButton("btn_execute_imputation", "Execute Imputation Engine", class = "btn-success btn-lg w-100")
      ),
      card(
        card_header("Configured Variable Method Allocation Matrix"),
        DTOutput("tbl_method_matrix")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Density Overlay: Observed vs. Synthetic Imputed Distributions"),
        selectInput("sel_density_var", "Select Numeric Variable to Plot:", choices = NULL),
        plotlyOutput("plot_density_overlay", height = "380px")
      ),
      card(
        card_header("Downstream Model Pooling (Rubin's Rules)"),
        p("Fit a downstream linear regression model across imputed datasets:"),
        uiOutput("ui_pooling_controls"),
        actionButton("btn_run_pooling", "Fit & Pool Regression Model", class = "btn-info text-white mb-2"),
        verbatimTextOutput("txt_pooling_summary")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Pre vs. Post Imputation Distribution Statistics Comparison"),
        DTOutput("tbl_dist_profile")
      )
    )
  ),
  
  nav_panel(
    title = "Correlation & Outliers",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        title = "Multivariate Controls",
        selectInput("corr_method", "Correlation Metric:", choices = c("pearson", "spearman", "kendall")),
        numericInput("plot_height", "Heatmap Height (px):", value = 650, min = 400, max = 2000, step = 50),
        numericInput("font_size", "Label Font Size:", value = 10, min = 6, max = 20, step = 1),
        hr(),
        h6("Mahalanobis Distance Diagnostics"),
        checkboxGroupInput("mahal_cols", "Select Covariance Columns:", choices = NULL),
        numericInput("mahal_cutoff", "Outlier Cutoff (p-value threshold):", value = 0.001, min = 0.0001, max = 0.05, step = 0.001),
        actionButton("btn_run_mahal", "Compute Mahalanobis Distance", class = "btn-info w-100")
      ),
      card(
        card_header("Correlation Heatmap Matrix"),
        uiOutput("ui_plot_corr")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Pairwise Correlation Matrix Table"), DTOutput("tbl_corr")),
        card(card_header("Multivariate Outliers (Mahalanobis Summary)"), DTOutput("tbl_mahal_outliers"))
      )
    )
  ),
  
  nav_panel(
    title = "Visual Diagnostics",
    icon = icon("chart-pie"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        title = "Plot Scenarios",
        selectInput("diag_type", "Select Diagnostic Scenario:", 
                    choices = c("1 Continuous: Box Plot / Outliers",
                                "2 Continuous: Scatter Plot",
                                "2 Continuous: Hexbin Plot",
                                "1 Cat + 1 Cont: Grouped Ridge Plot",
                                "2 Categorical: Stacked Bar Plot")),
        uiOutput("ui_diag_controls")
      ),
      card(
        card_header("Diagnostic Visual Canvas"),
        plotlyOutput("plot_diagnostic_main", height = "580px")
      )
    )
  ),
  
  nav_panel(
    title = "Regression Models",
    icon = icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        title = "Model Specification",
        selectizeInput("reg_y", "Dependent Variables (Y):", choices = NULL, multiple = TRUE),
        selectizeInput("reg_x", "Independent Predictors (X):", choices = NULL, multiple = TRUE),
        helpText("Binary response variables automatically route to Binomial Logistic GLM with ROC diagnostics. Continuous variables route to OLS Linear Regression."),
        actionButton("btn_fit_reg", "Fit Regression Models", class = "btn-primary w-100")
      ),
      uiOutput("ui_regression_outputs")
    )
  ),
  
  nav_panel(
    title = "Audit History & Export",
    icon = icon("file-excel"),
    layout_column_wrap(
      width = 1,
      card(card_header("Missing Data Diagnostics Breakdown"), DTOutput("tbl_missing_breakdown")),
      card(card_header("Human-in-the-Loop & Pipeline Audit Trail Log"), DTOutput("tbl_audit_log")),
      card(card_header("Univariate Statistical Profiles"), DTOutput("tbl_stat_profiles")),
      card(
        card_header("Master Multi-Sheet Excel Package Export"),
        card_body(
          p("Generate and export the complete analytical, statistical, and imputation audit workbook with embedded graphs:"),
          tags$ul(
            tags$li(tags$b("Sheet 1: Cleaned Dataset"), " - Active processed dataset & variable scale metadata"),
            tags$li(tags$b("Sheet 2: Audit Dashboard"), " - Little's MCAR test, method allocation matrix, transaction history, & Mahalanobis outliers"),
            tags$li(tags$b("Sheet 3: Statistical Profiles"), " - Pre vs. post distribution comparisons & univariate descriptors"),
            tags$li(tags$b("Sheet 4: Correlation Analysis"), " - Pairwise correlation matrix & embedded high-resolution heatmap"),
            tags$li(tags$b("Sheet 5: Regression Diagnostics"), " - Model specs, coefficients, VIF, metrics, & embedded diagnostic plots (ROC/Residuals)"),
            tags$li(tags$b("Sheet 6: Imputation Visuals"), " - Embedded observed vs. imputed density diagnostic graphic")
          ),
          downloadButton("btn_export_excel_tab7", "Download Master Audit Package (.xlsx)", class = "btn-success btn-lg")
        )
      )
    )
  )
)

# ==============================================================================
# 4. SERVER LOGIC & REACTIVE STATE ARCHITECTURE
# ==============================================================================
server <- function(input, output, session) {
  
  df_raw   <- reactiveVal(NULL)
  df_proc  <- reactiveVal(NULL)
  mids_obj <- reactiveVal(NULL)
  
  feature_types     <- reactiveVal(NULL)
  custom_methods    <- reactiveVal(list())
  custom_mechanisms <- reactiveVal(list())
  mahal_results     <- reactiveVal(NULL)
  reg_fit_state     <- reactiveVal(NULL)
  
  audit_log <- reactiveVal(
    tibble(
      Timestamp        = character(0),
      Variable         = character(0),
      Target           = character(0),
      Method           = character(0),
      `Replaced Count` = integer(0),
      Justification    = character(0)
    )
  )
  
  log_action <- function(variable, target, method, count, justification = "Automated / Pipeline") {
    new_entry <- tibble(
      Timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Variable         = as.character(variable),
      Target           = as.character(target),
      Method           = as.character(method),
      `Replaced Count` = as.integer(count),
      Justification    = as.character(justification)
    )
    audit_log(bind_rows(audit_log(), new_entry))
  }
  
  observeEvent(input$btn_reset_pipeline, {
    req(df_raw())
    df_proc(df_raw())
    mids_obj(NULL)
    custom_methods(list())
    custom_mechanisms(list())
    mahal_results(NULL)
    reg_fit_state(NULL)
    
    type_df <- tibble(
      Variable = names(df_raw()),
      `Inferred Scale` = sapply(df_raw(), function(col) {
        if (is.numeric(col)) {
          if (length(unique(col[!is.na(col)])) <= 5) "Ordinal (Ordered)" else "Continuous / Metric"
        } else {
          "Nominal (Categorical)"
        }
      })
    )
    feature_types(type_df)
    
    audit_log(tibble(
      Timestamp = character(0), Variable = character(0),
      Target = character(0), Method = character(0), 
      `Replaced Count` = integer(0), Justification = character(0)
    ))
    
    showNotification("Pipeline state and overrides reset to original raw data.", type = "warning")
  })
  
  observeEvent(input$file_input, {
    file <- input$file_input
    req(file)
    ext <- tools::file_ext(file$name)
    
    loaded_df <- tryCatch({
      parse_uploaded_file(file$datapath, ext)
    }, error = function(e) {
      showNotification(paste("Ingestion Error:", e$message), type = "error")
      return(NULL)
    })
    
    req(loaded_df)
    cleaned_data <- janitor::clean_names(as_tibble(loaded_df)) %>%
      mutate(across(where(is.character), as.character))
    
    df_raw(cleaned_data)
    df_proc(cleaned_data)
    mids_obj(NULL)
    custom_methods(list())
    custom_mechanisms(list())
    mahal_results(NULL)
    reg_fit_state(NULL)
    
    type_df <- tibble(
      Variable = names(cleaned_data),
      `Inferred Scale` = sapply(cleaned_data, function(col) {
        if (is.numeric(col)) {
          if (length(unique(col[!is.na(col)])) <= 5) "Ordinal (Ordered)" else "Continuous / Metric"
        } else {
          "Nominal (Categorical)"
        }
      })
    )
    feature_types(type_df)
    
    audit_log(tibble(
      Timestamp = character(0), Variable = character(0),
      Target = character(0), Method = character(0), 
      `Replaced Count` = integer(0), Justification = character(0)
    ))
    
    log_action("ALL_VARIABLES", "Primary Ingestion", paste0("File: ", file$name), nrow(cleaned_data), "Initial Ingestion")
    showNotification("Dataset successfully ingested and initialized.", type = "message")
  })
  
  observe({
    df <- df_proc()
    if (is.null(df)) return()
    
    all_cols <- names(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    
    updateSelectInput(session, "class_col", choices = all_cols)
    updateSelectInput(session, "imp_col", choices = all_cols)
    updateSelectInput(session, "trans_col", choices = num_cols)
    updateSelectInput(session, "sel_target_var", choices = all_cols)
    updateSelectInput(session, "sel_density_var", choices = num_cols)
    updateCheckboxGroupInput(session, "mahal_cols", choices = num_cols, selected = num_cols)
    updateSelectizeInput(session, "reg_y", choices = all_cols)
    updateSelectizeInput(session, "reg_x", choices = all_cols)
  })
  
  machine_recs <- reactive({
    req(df_raw())
    recommend_imputation_methods(df_raw())
  })
  
  mcar_res <- reactive({
    req(df_proc())
    run_littles_mcar_test(df_proc())
  })
  
  observeEvent(input$btn_apply_class, {
    req(feature_types(), input$class_col, input$class_type)
    ft <- feature_types() %>%
      mutate(`Inferred Scale` = if_else(Variable == input$class_col, input$class_type, `Inferred Scale`))
    feature_types(ft)
    
    updated_df <- df_proc()
    if (input$class_type == "Continuous / Metric") {
      updated_df[[input$class_col]] <- as.numeric(updated_df[[input$class_col]])
    } else if (input$class_type == "Ordinal (Ordered)") {
      updated_df[[input$class_col]] <- factor(updated_df[[input$class_col]], ordered = TRUE)
    } else {
      updated_df[[input$class_col]] <- factor(updated_df[[input$class_col]], ordered = FALSE)
    }
    df_proc(updated_df)
    
    log_action(input$class_col, "Scale Assigned", input$class_type, nrow(updated_df), "User Scale Assignment")
    showNotification(sprintf("Updated %s scale to %s.", input$class_col, input$class_type), type = "message")
  })
  
  output$tbl_feature_types <- renderDT({
    req(feature_types())
    datatable(feature_types(), options = list(pageLength = 8, scrollX = TRUE))
  })
  
  observeEvent(input$btn_apply_imp, {
    req(df_proc(), input$imp_col, input$imp_method)
    df <- df_proc()
    col <- input$imp_col
    na_idx <- is.na(df[[col]])
    na_cnt <- sum(na_idx)
    
    if (na_cnt == 0) {
      showNotification("No missing values found in target column.", type = "warning")
      return()
    }
    
    tryCatch({
      if (input$imp_method == "Mean") {
        if (!is.numeric(df[[col]])) stop("Mean imputation requires a numeric column.")
        imp_val <- mean(df[[col]], na.rm = TRUE)
      } else if (input$imp_method == "Median") {
        if (!is.numeric(df[[col]])) stop("Median imputation requires a numeric column.")
        imp_val <- median(df[[col]], na.rm = TRUE)
      } else if (input$imp_method == "Mode") {
        imp_val <- get_mode(df[[col]])
      } else if (input$imp_method == "Custom Value") {
        imp_val <- type.convert(input$imp_custom_val, as.is = TRUE)
      }
      
      df[[col]][na_idx] <- imp_val
      df_proc(df)
      log_action(col, "Quick Univariate Imputation", input$imp_method, na_cnt, "Tab 1 Quick Impute")
      showNotification(sprintf("Imputed %d missing values in %s.", na_cnt, col), type = "message")
    }, error = function(e) {
      showNotification(paste("Imputation Failed:", e$message), type = "error")
    })
  })
  
  observeEvent(input$btn_apply_trans, {
    req(df_proc(), input$trans_col, input$trans_method)
    df <- df_proc()
    col <- input$trans_col
    vec <- df[[col]]
    
    tryCatch({
      if (input$trans_method == "Log (ln)") {
        if (any(vec <= 0, na.rm = TRUE)) stop("Log transform requires strictly positive values.")
        df[[col]] <- log(vec)
      } else if (input$trans_method == "Log1p (ln(x+1))") {
        if (any(vec < -1, na.rm = TRUE)) stop("Log1p requires values > -1.")
        df[[col]] <- log1p(vec)
      } else if (input$trans_method == "Standardize (Z-score)") {
        df[[col]] <- as.vector(scale(vec))
      } else if (input$trans_method == "Min-Max Scale") {
        min_v <- min(vec, na.rm = TRUE); max_v <- max(vec, na.rm = TRUE)
        if (max_v == min_v) stop("Cannot scale constant vector.")
        df[[col]] <- (vec - min_v) / (max_v - min_v)
      }
      
      df_proc(df)
      log_action(col, "Feature Transformation", input$trans_method, nrow(df), "Tab 1 Feature Transform")
      showNotification(sprintf("Applied %s to %s.", input$trans_method, col), type = "message")
    }, error = function(e) {
      showNotification(paste("Transformation Failed:", e$message), type = "error")
    })
  })
  
  output$tbl_data_proc <- renderDT({
    req(df_proc())
    datatable(df_proc(), options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$tbl_raw_data <- renderDT({
    req(df_raw())
    datatable(df_raw(), options = list(pageLength = 8, scrollX = TRUE))
  }, server = TRUE)
  
  output$tbl_mcar_test <- renderTable({
    mcar_res()
  }, digits = 4)
  
  output$tbl_missing_prop <- renderDT({
    req(df_proc())
    df <- df_proc()
    counts <- vapply(df, function(x) sum(is.na(x)), integer(1))
    
    missing_summary <- tibble(
      Variable = names(df),
      Missing_Count = counts,
      `Missing (%)` = round((counts / nrow(df)) * 100, 2)
    ) %>% arrange(desc(Missing_Count))
    
    datatable(missing_summary, options = list(pageLength = 6, dom = "tp"))
  }, server = TRUE)
  
  output$plot_missing_pattern <- renderPlotly({
    req(df_proc())
    df <- df_proc()
    
    if (sum(is.na(df)) == 0) {
      return(plotly_empty() %>% layout(title = "No missing values present in dataset."))
    }
    
    if (nrow(df) > 1000) {
      set.seed(42)
      df <- df[sample(nrow(df), 1000), ]
    }
    
    miss_mat <- t(apply(is.na(df), 2, as.numeric))
    
    plot_ly(
      x = seq_len(ncol(miss_mat)),
      y = colnames(df),
      z = miss_mat,
      type = "heatmap",
      colorscale = list(c(0, "#2C3E50"), c(1, "#E74C3C")),
      showscale = FALSE
    ) %>% layout(
      title = "Missingness Pattern Matrix (Dark = Observed, Red = Missing)",
      xaxis = list(title = "Observation Index (Sampled)", showgrid = FALSE),
      yaxis = list(title = "", showgrid = FALSE)
    )
  })
  
  output$ui_machine_recommendation_summary <- renderUI({
    if (is.null(df_proc())) {
      return(div(class = "alert alert-warning", "Ingest dataset to view algorithm proposals."))
    }
    
    t_var <- input$sel_target_var
    req(t_var, machine_recs(), mcar_res())
    
    rec_info <- machine_recs()[[t_var]]
    mcar_info <- mcar_res()
    
    tagList(
      div(
        class = "alert alert-info py-2",
        h6(class = "alert-heading mb-1", paste("Machine Proposal for:", t_var)),
        tags$small(
          tags$b("Missing: "), rec_info$n_miss, " (", rec_info$pct_miss, "%) | ",
          tags$b("Suggested Method: "), rec_info$recommended_method, tags$br(),
          tags$b("Rationale: "), rec_info$rationale, tags$br(),
          tags$b("MCAR Diagnostic: "), mcar_info$Interpretation[1]
        )
      )
    )
  })
  
  configured_method_matrix <- reactive({
    req(df_proc(), machine_recs())
    df <- df_proc()
    cols <- names(df)
    
    over_meth <- custom_methods()
    over_mech <- custom_mechanisms()
    
    tibble(
      Variable = cols,
      Missing_Count = vapply(df, function(x) sum(is.na(x)), integer(1)),
      Machine_Method = vapply(cols, function(c) machine_recs()[[c]]$recommended_method, character(1)),
      Configured_Method = vapply(cols, function(c) {
        if (!is.null(over_meth[[c]]) && over_meth[[c]] != "Automatic (Use Machine Proposal)") {
          over_meth[[c]]
        } else {
          machine_recs()[[c]]$recommended_method
        }
      }, character(1)),
      Configured_Mechanism = vapply(cols, function(c) {
        if (!is.null(over_mech[[c]]) && over_mech[[c]] != "Automatic (Use Little's Test)") {
          over_mech[[c]]
        } else {
          "Auto (Little's Test)"
        }
      }, character(1)),
      Status = vapply(cols, function(c) {
        is_m <- !is.null(over_meth[[c]]) && over_meth[[c]] != "Automatic (Use Machine Proposal)"
        is_k <- !is.null(over_mech[[c]]) && over_mech[[c]] != "Automatic (Use Little's Test)"
        if (is_m || is_k) "OVERRIDDEN" else "MACHINE DEFAULT"
      }, character(1))
    )
  })
  
  output$tbl_method_matrix <- renderDT({
    req(configured_method_matrix())
    datatable(configured_method_matrix(), options = list(pageLength = 8, scrollX = TRUE))
  }, server = TRUE)
  
  observeEvent(input$btn_apply_override, {
    req(df_proc(), input$sel_target_var, machine_recs())
    t_var <- input$sel_target_var
    
    meths <- custom_methods()
    mechs <- custom_mechanisms()
    
    meths[[t_var]] <- input$sel_override_method
    mechs[[t_var]] <- input$sel_override_mechanism
    
    custom_methods(meths)
    custom_mechanisms(mechs)
    
    m_rec <- machine_recs()[[t_var]]$recommended_method
    h_dec <- if (input$sel_override_method != "Automatic (Use Machine Proposal)") "OVERRIDE" else "ACCEPT"
    applied_m <- if (h_dec == "OVERRIDE") input$sel_override_method else m_rec
    
    just_txt <- if (trimws(input$txt_override_justification) == "") "Standard Human Decision" else input$txt_override_justification
    log_action(t_var, paste("HITL Override:", h_dec), applied_m, sum(is.na(df_proc()[[t_var]])), just_txt)
    showNotification(paste("Override stored for:", t_var), type = "message")
  })
  
  observeEvent(input$btn_execute_imputation, {
    req(df_proc(), configured_method_matrix())
    df <- df_proc()
    matrix_cfg <- configured_method_matrix()
    strat <- input$sel_global_strategy
    total_nas <- sum(is.na(df))
    
    if (total_nas == 0) {
      showNotification("No missing values detected. Imputation skipped.", type = "warning")
      return()
    }
    
    withProgress(message = "Executing Imputation Pipeline...", value = 0.25, {
      tryCatch({
        if (strat == "Listwise Deletion (Complete Cases)") {
          df_completed <- na.omit(df)
          df_proc(df_completed)
          mids_obj(NULL)
          log_action("ALL_VARIABLES", "Listwise Deletion", "Complete Cases Drop", total_nas, "Dropped incomplete rows")
          showNotification("Listwise deletion completed.", type = "warning")
        } else if (strat == "Single Imputation (PMM)") {
          pred_mat <- mice::quickpred(df, mincor = 0.1)
          imp <- mice::mice(df, m = 1, maxit = 5, predictorMatrix = pred_mat, printFlag = FALSE)
          df_completed <- complete(imp, 1)
          
          for (col in names(df_completed)) {
            if (is.numeric(df_completed[[col]])) {
              v_orig <- na.omit(df[[col]])
              if (grepl("pct|percent|rate|share|proportion", tolower(col)) || 
                  (length(v_orig) > 0 && all(v_orig >= 0 & v_orig <= 100) && max(v_orig) > 1)) {
                df_completed[[col]] <- pmin(pmax(df_completed[[col]], 0), 100)
              }
            }
          }
          
          df_proc(df_completed)
          mids_obj(imp)
          log_action("ALL_VARIABLES", "Single PMM Imputation", "PMM Single Chain", total_nas, "Single Imputation PMM")
          showNotification("Single PMM imputation completed successfully.", type = "message")
        } else {
          incProgress(0.3, detail = "Optimizing predictor matrices...")
          method_vec <- setNames(matrix_cfg$Configured_Method, matrix_cfg$Variable)
          for (v in names(method_vec)) {
            if (sum(is.na(df[[v]])) == 0) {
              method_vec[v] <- ""
            } else if (method_vec[v] == "polyreg" && is.numeric(df[[v]])) {
              method_vec[v] <- "polyreg_num"
            }
          }
          
          pred_mat <- mice::quickpred(df, mincor = 0.1)
          
          incProgress(0.3, detail = "Running MICE iterations...")
          imp_obj <- mice::mice(
            df, 
            m = input$num_m, 
            maxit = input$num_maxit, 
            method = method_vec, 
            predictorMatrix = pred_mat,
            printFlag = FALSE
          )
          
          df_completed <- complete(imp_obj, 1)
          
          for (col in names(df_completed)) {
            if (is.numeric(df_completed[[col]])) {
              v_orig <- na.omit(df[[col]])
              if (grepl("pct|percent|rate|share|proportion", tolower(col)) || 
                  (length(v_orig) > 0 && all(v_orig >= 0 & v_orig <= 100) && max(v_orig) > 1)) {
                df_completed[[col]] <- pmin(pmax(df_completed[[col]], 0), 100)
              }
            }
          }
          
          mids_obj(imp_obj)
          df_proc(df_completed)
          log_action("MICE Execution", "Multiple Imputation", paste0("m=", input$num_m, ", maxit=", input$num_maxit), total_nas, "Full MICE execution")
          showNotification(paste("MICE completed successfully (m =", input$num_m, ")"), type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Imputation Engine Error:", e$message), type = "error")
      })
    })
  })
  
  output$plot_density_overlay <- renderPlotly({
    req(df_proc())
    v <- input$sel_density_var
    req(v, df_raw())
    
    v_orig <- na.omit(df_raw()[[v]])
    v_imp  <- df_proc()[[v]]
    
    if (length(v_orig) == 0 || length(v_imp) == 0 || !is.numeric(v_imp)) {
      return(plotly_empty() %>% layout(title = "Select a numeric column to render overlay."))
    }
    
    df_combo <- tibble(
      Value = c(v_orig, v_imp),
      Source = c(rep("Observed", length(v_orig)), rep("Imputed", length(v_imp)))
    )
    
    p <- ggplot(df_combo, aes(x = Value, fill = Source, color = Source)) +
      geom_density(alpha = 0.35) +
      scale_fill_manual(values = c("Observed" = "#2ECC71", "Imputed" = "#3498DB")) +
      scale_color_manual(values = c("Observed" = "#2ECC71", "Imputed" = "#3498DB")) +
      theme_minimal() +
      labs(title = paste("Density Profile Overlay:", v), x = v, y = "Density")
    
    ggplotly(p)
  })
  
  output$tbl_dist_profile <- renderDT({
    req(df_raw(), df_proc())
    prof <- compute_distribution_profiles(df_raw(), df_proc())
    datatable(prof, options = list(pageLength = 6, scrollX = TRUE))
  }, server = TRUE)
  
  output$ui_pooling_controls <- renderUI({
    req(df_proc())
    num_cols <- names(df_proc())[vapply(df_proc(), is.numeric, logical(1))]
    if (length(num_cols) < 2) {
      return(p("Minimum 2 numeric variables required for regression pooling."))
    }
    
    tagList(
      selectInput("sel_pool_dv", "Dependent Target (Y):", choices = num_cols, selected = num_cols[1]),
      selectizeInput("sel_pool_ivs", "Independent Predictors (X):", choices = setdiff(num_cols, num_cols[1]), 
                     selected = setdiff(num_cols, num_cols[1])[1], multiple = TRUE)
    )
  })
  
  output$txt_pooling_summary <- renderPrint({
    req(input$btn_run_pooling)
    isolate({
      req(input$sel_pool_dv, input$sel_pool_ivs, df_proc())
      dv <- input$sel_pool_dv
      ivs <- input$sel_pool_ivs
      
      f <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
      
      if (!is.null(mids_obj())) {
        fit <- with(mids_obj(), lm(f))
        pooled <- pool(fit)
        summary(pooled)
      } else {
        fit <- lm(f, data = df_proc())
        summary(fit)
      }
    })
  })
  
  corr_mat_calc <- reactive({
    req(df_proc())
    num_df <- df_proc() %>% select(where(is.numeric)) %>% drop_na()
    if (ncol(num_df) < 2) return(NULL)
    cor(num_df, method = input$corr_method)
  })
  
  output$ui_plot_corr <- renderUI({
    plotlyOutput("plot_corr", height = paste0(input$plot_height, "px"))
  })
  
  output$plot_corr <- renderPlotly({
    cmat <- corr_mat_calc()
    req(cmat)
    
    truncated_names <- make.unique(stringr::str_trunc(colnames(cmat), width = 12))
    colnames(cmat) <- truncated_names
    rownames(cmat) <- truncated_names
    
    c_df <- cmat %>%
      as.data.frame() %>%
      rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
      mutate(
        Var1 = factor(Var1, levels = truncated_names),
        Var2 = factor(Var2, levels = rev(truncated_names))
      )
    
    p <- ggplot(c_df, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(low = "#3498DB", mid = "#FFFFFF", high = "#E74C3C", limits = c(-1, 1), na.value = "grey90") +
      geom_text(aes(label = sprintf("%.2f", Correlation)), size = max(1.8, input$font_size / 3.2)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$font_size),
        axis.text.y = element_text(size = input$font_size),
        panel.grid = element_blank()
      ) +
      labs(x = "", y = "")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$tbl_corr <- renderDT({
    cmat <- corr_mat_calc()
    req(cmat)
    datatable(round(cmat, 4), options = list(scrollX = TRUE, pageLength = 8))
  })
  
  observeEvent(input$btn_run_mahal, {
    req(df_proc(), input$mahal_cols)
    if (length(input$mahal_cols) < 2) {
      showNotification("Mahalanobis estimation requires at least 2 numeric columns.", type = "warning")
      return()
    }
    
    tryCatch({
      sub_df <- df_proc() %>% select(all_of(input$mahal_cols)) %>% drop_na()
      if (nrow(sub_df) <= ncol(sub_df)) stop("Sample size too small relative to dimensions for covariance estimation.")
      
      center <- colMeans(sub_df)
      cov_matrix <- cov(sub_df)
      md <- mahalanobis(sub_df, center, cov_matrix)
      p_vals <- pchisq(md, df = ncol(sub_df), lower.tail = FALSE)
      
      result_df <- sub_df %>%
        mutate(
          Mahalanobis_Distance = md,
          p_value = p_vals,
          Is_Outlier = p_vals < input$mahal_cutoff
        )
      
      mahal_results(result_df)
      log_action(paste(input$mahal_cols, collapse = ", "), "Mahalanobis Outliers", paste0("Cutoff p < ", input$mahal_cutoff), sum(result_df$Is_Outlier), "Mahalanobis Outlier Detection")
      showNotification(sprintf("Identified %d multivariate outliers.", sum(result_df$Is_Outlier)), type = "message")
    }, error = function(e) {
      showNotification(paste("Mahalanobis Error:", e$message), type = "error")
    })
  })
  
  output$tbl_mahal_outliers <- renderDT({
    req(mahal_results())
    datatable(mahal_results() %>% filter(Is_Outlier == TRUE), options = list(scrollX = TRUE, pageLength = 5))
  })
  
  output$ui_diag_controls <- renderUI({
    req(df_proc())
    df <- df_proc()
    all_cols <- names(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x) || length(unique(x)) <= 10, logical(1))]
    
    switch(input$diag_type,
           "1 Continuous: Box Plot / Outliers" = tagList(
             selectInput("diag_var_cont1", "Select Continuous Variable:", choices = num_cols)
           ),
           "2 Continuous: Scatter Plot" = tagList(
             selectInput("diag_var_x", "X Variable:", choices = num_cols),
             selectInput("diag_var_y", "Y Variable:", choices = num_cols)
           ),
           "2 Continuous: Hexbin Plot" = tagList(
             selectInput("diag_var_hex_x", "X Variable:", choices = num_cols),
             selectInput("diag_var_hex_y", "Y Variable:", choices = num_cols)
           ),
           "1 Cat + 1 Cont: Grouped Ridge Plot" = tagList(
             selectInput("diag_var_cat", "Categorical Grouping:", choices = cat_cols),
             selectInput("diag_var_cont2", "Continuous Variable:", choices = num_cols)
           ),
           "2 Categorical: Stacked Bar Plot" = tagList(
             selectInput("diag_var_cat1", "Category X:", choices = cat_cols),
             selectInput("diag_var_cat2", "Fill Grouping Y:", choices = cat_cols)
           )
    )
  })
  
  output$plot_diagnostic_main <- renderPlotly({
    req(df_proc(), input$diag_type)
    df <- df_proc()
    
    if (input$diag_type == "1 Continuous: Box Plot / Outliers") {
      req(input$diag_var_cont1)
      p <- ggplot(df, aes(y = .data[[input$diag_var_cont1]])) +
        geom_boxplot(fill = "#3498DB", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Box Plot & Outlier Detection:", input$diag_var_cont1))
      ggplotly(p)
    } else if (input$diag_type == "2 Continuous: Scatter Plot") {
      req(input$diag_var_x, input$diag_var_y)
      p <- ggplot(df, aes(x = .data[[input$diag_var_x]], y = .data[[input$diag_var_y]])) +
        geom_point(color = "#2980B9", alpha = 0.6) +
        geom_smooth(method = "lm", color = "#E74C3C", se = TRUE) +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", input$diag_var_x, "vs", input$diag_var_y))
      ggplotly(p)
    } else if (input$diag_type == "2 Continuous: Hexbin Plot") {
      req(input$diag_var_hex_x, input$diag_var_hex_y)
      p <- ggplot(df, aes(x = .data[[input$diag_var_hex_x]], y = .data[[input$diag_var_hex_y]])) +
        geom_hex(bins = 30) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(title = paste("Hexbin Density Plot:", input$diag_var_hex_x, "vs", input$diag_var_hex_y))
      ggplotly(p)
    } else if (input$diag_type == "1 Cat + 1 Cont: Grouped Ridge Plot") {
      req(input$diag_var_cat, input$diag_var_cont2)
      p <- ggplot(df, aes(x = .data[[input$diag_var_cont2]], y = as.factor(.data[[input$diag_var_cat]]), fill = as.factor(.data[[input$diag_var_cat]]))) +
        geom_density_ridges(alpha = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = paste("Grouped Ridge Plot:", input$diag_var_cont2, "by", input$diag_var_cat), y = input$diag_var_cat)
      ggplotly(p)
    } else if (input$diag_type == "2 Categorical: Stacked Bar Plot") {
      req(input$diag_var_cat1, input$diag_var_cat2)
      p <- ggplot(df, aes(x = as.factor(.data[[input$diag_var_cat1]]), fill = as.factor(.data[[input$diag_var_cat2]]))) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        labs(title = paste("Proportional Stacked Bar Plot:", input$diag_var_cat1, "vs", input$diag_var_cat2), 
             x = input$diag_var_cat1, y = "Percentage", fill = input$diag_var_cat2)
      ggplotly(p)
    }
  })
  
  observeEvent(input$btn_fit_reg, {
    req(df_proc(), input$reg_y, input$reg_x)
    
    if (any(input$reg_y %in% input$reg_x)) {
      showNotification("Dependent variables (Y) cannot overlap with independent predictors (X).", type = "error")
      return()
    }
    
    tryCatch({
      models_list <- list()
      df_clean <- df_proc() %>% drop_na(all_of(c(input$reg_y, input$reg_x)))
      
      if (nrow(df_clean) == 0) {
        showNotification("No complete cases available across selected regression variables.", type = "error")
        return()
      }
      
      for (y_var in input$reg_y) {
        y_vals <- df_clean[[y_var]]
        unique_vals <- unique(na.omit(y_vals))
        is_binary <- length(unique_vals) == 2
        fml_str <- paste(y_var, "~", paste(input$reg_x, collapse = " + "))
        fml <- as.formula(fml_str)
        
        if (is_binary) {
          df_clean_mod <- df_clean
          if (!is.numeric(df_clean_mod[[y_var]]) || !all(unique_vals %in% c(0, 1))) {
            df_clean_mod[[y_var]] <- as.numeric(as.factor(df_clean_mod[[y_var]])) - 1
          }
          
          model <- glm(fml, data = df_clean_mod, family = binomial(link = "logit"))
          sm <- summary(model)
          
          coef_summary <- as.data.frame(sm$coefficients) %>% rownames_to_column(var = "Term")
          colnames(coef_summary) <- c("Term", "Estimate", "Std_Error", "z_value", "p_value")
          coef_summary <- coef_summary %>% mutate(Odds_Ratio = exp(Estimate))
          
          if (length(input$reg_x) > 1) {
            vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
            if (!is.null(vif_vals)) {
              if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]
              vif_df <- data.frame(Term = names(vif_vals), VIF = as.numeric(vif_vals))
              coef_summary <- left_join(coef_summary, vif_df, by = "Term")
            } else { coef_summary$VIF <- NA_real_ }
          } else { coef_summary$VIF <- NA_real_ }
          
          null_dev <- sm$null.deviance; res_dev <- sm$deviance
          mcfadden_r2 <- 1 - (res_dev / null_dev)
          metrics_df <- tibble(
            Metric = c("Model Architecture", "AIC", "Null Deviance", "Residual Deviance", "McFadden Pseudo R-squared"),
            Value = c("Logistic Regression (GLM Logit)", round(sm$aic, 4), round(null_dev, 4), round(res_dev, 4), round(mcfadden_r2, 4))
          )
          
          models_list[[y_var]] <- list(
            model = model, is_binary = TRUE, coef_tbl = coef_summary,
            metrics_tbl = metrics_df, formula = fml_str, data_used = df_clean_mod
          )
        } else {
          model <- lm(fml, data = df_clean)
          sm <- summary(model)
          
          coef_summary <- as.data.frame(sm$coefficients) %>% rownames_to_column(var = "Term")
          colnames(coef_summary) <- c("Term", "Estimate", "Std_Error", "t_value", "p_value")
          coef_summary$Odds_Ratio <- NA_real_
          
          if (length(input$reg_x) > 1) {
            vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
            if (!is.null(vif_vals)) {
              if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]
              vif_df <- data.frame(Term = names(vif_vals), VIF = as.numeric(vif_vals))
              coef_summary <- left_join(coef_summary, vif_df, by = "Term")
            } else { coef_summary$VIF <- NA_real_ }
          } else { coef_summary$VIF <- NA_real_ }
          
          f_stat <- sm$fstatistic
          f_p_val <- if (!is.null(f_stat)) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else NA_real_
          metrics_df <- tibble(
            Metric = c("Model Architecture", "R-squared", "Adjusted R-squared", "Residual Std Error", "F-Statistic", "Model p-value"),
            Value = c("OLS Linear Regression (lm)", round(sm$r.squared, 4), round(sm$adj.r.squared, 4), 
                      round(sm$sigma, 4), round(f_stat[1], 4), format.pval(f_p_val))
          )
          
          models_list[[y_var]] <- list(
            model = model, is_binary = FALSE, coef_tbl = coef_summary,
            metrics_tbl = metrics_df, formula = fml_str, data_used = df_clean
          )
        }
        log_action(y_var, "Regression Model Fit", fml_str, nrow(df_clean), "Regression Fit")
      }
      
      reg_fit_state(models_list)
      showNotification(sprintf("Successfully fitted models for %d response variable(s).", length(input$reg_y)), type = "message")
    }, error = function(e) {
      showNotification(paste("Regression Error:", e$message), type = "error")
    })
  })
  
  output$ui_regression_outputs <- renderUI({
    req(reg_fit_state())
    fits <- reg_fit_state()
    
    tabs <- lapply(names(fits), function(y_var) {
      fit_obj <- fits[[y_var]]
      diag_title_1 <- if (fit_obj$is_binary) "ROC Curve Diagnostic" else "Residuals vs Fitted Values"
      diag_title_2 <- if (fit_obj$is_binary) "Predicted Probabilities by Actual Class" else "Normal Q-Q Plot"
      
      nav_panel(
        title = paste("Y:", y_var),
        layout_column_wrap(
          width = 1/2,
          card(card_header(paste("Coefficients & Multicollinearity (VIF):", y_var)), DTOutput(paste0("tbl_coef_", y_var))),
          card(card_header(paste("Goodness-of-Fit Performance Metrics:", y_var)), tableOutput(paste0("tbl_metrics_", y_var)))
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header(diag_title_1), plotlyOutput(paste0("plot_diag1_", y_var))),
          card(card_header(diag_title_2), plotlyOutput(paste0("plot_diag2_", y_var)))
        )
      )
    })
    do.call(navset_card_tab, tabs)
  })
  
  observe({
    req(reg_fit_state())
    fits <- reg_fit_state()
    
    lapply(names(fits), function(y_var) {
      fit_obj <- fits[[y_var]]
      
      output[[paste0("tbl_coef_", y_var)]] <- renderDT({
        datatable(fit_obj$coef_tbl, options = list(scrollX = TRUE, pageLength = 5))
      })
      
      output[[paste0("tbl_metrics_", y_var)]] <- renderTable({
        fit_obj$metrics_tbl
      })
      
      output[[paste0("plot_diag1_", y_var)]] <- renderPlotly({
        fit <- fit_obj$model
        if (fit_obj$is_binary) {
          prob <- predict(fit, type = "response")
          roc_obj <- pROC::roc(fit_obj$data_used[[y_var]], prob, quiet = TRUE)
          roc_df <- data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities)
          p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
            geom_line(color = "#2980B9", linewidth = 1) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            theme_minimal() +
            labs(title = paste0("ROC Curve (AUC = ", round(pROC::auc(roc_obj), 4), ")"), x = "1 - Specificity", y = "Sensitivity")
          ggplotly(p)
        } else {
          res_df <- data.frame(Fitted = fitted(fit), Residuals = residuals(fit))
          p <- ggplot(res_df, aes(x = Fitted, y = Residuals)) +
            geom_point(alpha = 0.6, color = "steelblue") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            theme_minimal() +
            labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
          ggplotly(p)
        }
      })
      
      output[[paste0("plot_diag2_", y_var)]] <- renderPlotly({
        fit <- fit_obj$model
        if (fit_obj$is_binary) {
          prob_df <- data.frame(Prob = predict(fit, type = "response"), Actual = as.factor(fit_obj$data_used[[y_var]]))
          p <- ggplot(prob_df, aes(x = Prob, fill = Actual)) +
            geom_density(alpha = 0.5) +
            theme_minimal() +
            labs(title = "Predicted Probabilities Distribution", x = "Predicted Probability", fill = "Actual Class")
          ggplotly(p)
        } else {
          res_df <- data.frame(Residuals = scale(residuals(fit)))
          p <- ggplot(res_df, aes(sample = Residuals)) +
            stat_qq() + stat_qq_line(color = "red") +
            theme_minimal() +
            labs(title = "Standardized Residuals Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Residuals")
          ggplotly(p)
        }
      })
    })
  })
  
  output$tbl_missing_breakdown <- renderDT({
    req(df_proc())
    df <- df_proc()
    miss <- df %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing Count") %>%
      mutate(`Missing Percentage (%)` = round((`Missing Count` / nrow(df)) * 100, 2))
    datatable(miss, options = list(scrollX = TRUE, pageLength = 8))
  })
  
  output$tbl_audit_log <- renderDT({
    datatable(audit_log(), options = list(scrollX = TRUE, pageLength = 8))
  })
  
  output$tbl_stat_profiles <- renderDT({
    req(df_proc())
    num_df <- df_proc() %>% select(where(is.numeric))
    if (ncol(num_df) == 0) return(NULL)
    
    prof <- num_df %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Val") %>%
      group_by(Variable) %>%
      summarise(
        Count = sum(!is.na(Val)),
        Mean = round(mean(Val, na.rm = TRUE), 4),
        StdDev = round(sd(Val, na.rm = TRUE), 4),
        Min = round(min(Val, na.rm = TRUE), 4),
        Q25 = round(quantile(Val, 0.25, na.rm = TRUE), 4),
        Median = round(median(Val, na.rm = TRUE), 4),
        Q75 = round(quantile(Val, 0.75, na.rm = TRUE), 4),
        Max = round(max(Val, na.rm = TRUE), 4),
        .groups = "drop"
      )
    datatable(prof, options = list(scrollX = TRUE, pageLength = 8))
  })
  
  # ============================================================================
  # 5. FAIL-SAFE MASTER EXCEL EXPORT ENGINE (GUARANTEED .XLSX WITH EMBEDDED PLOTS)
  # ============================================================================
  generate_master_excel <- function(file) {
    wb <- createWorkbook()
    
    header_style <- createStyle(
      fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
      fgFill = "#2C3E50", halign = "center", valign = "center", textDecoration = "bold",
      border = "TopBottomLeftRight", borderColour = "#000000"
    )
    section_style <- createStyle(
      fontName = "Arial", fontSize = 12, fontColour = "#2C3E50", textDecoration = "bold"
    )
    
    active_df <- df_proc()
    raw_df    <- df_raw()
    
    # Handle export before data ingestion safely
    if (is.null(active_df) || nrow(active_df) == 0) {
      addWorksheet(wb, "Status")
      writeData(wb, "Status", "No dataset has been uploaded yet. Please ingest data in Tab 1 before exporting.", startRow = 1, startCol = 1)
      saveWorkbook(wb, file, overwrite = TRUE)
      return()
    }
    
    # Sheet 1: Cleaned Dataset & Scale Metadata
    addWorksheet(wb, "Cleaned Dataset")
    writeData(wb, "Cleaned Dataset", "Variable Scale Metadata", startRow = 1, startCol = 1)
    addStyle(wb, "Cleaned Dataset", section_style, rows = 1, cols = 1)
    
    f_types <- feature_types()
    if (!is.null(f_types) && nrow(f_types) > 0) {
      writeData(wb, "Cleaned Dataset", f_types, startRow = 2, startCol = 1, headerStyle = header_style)
      row_offset <- nrow(f_types) + 4
    } else {
      row_offset <- 3
    }
    
    writeData(wb, "Cleaned Dataset", "Active Processed Dataset", startRow = row_offset, startCol = 1)
    addStyle(wb, "Cleaned Dataset", section_style, rows = row_offset, cols = 1)
    writeData(wb, "Cleaned Dataset", active_df, startRow = row_offset + 1, startCol = 1, headerStyle = header_style)
    setColWidths(wb, "Cleaned Dataset", cols = 1:max(ncol(active_df), 5), widths = "auto")
    
    # Sheet 2: Audit Dashboard
    addWorksheet(wb, "Audit Dashboard")
    writeData(wb, "Audit Dashboard", "Table A: Little's MCAR Test Diagnostic Summary", startRow = 1, startCol = 1)
    addStyle(wb, "Audit Dashboard", section_style, rows = 1, cols = 1)
    
    mcar_df <- tryCatch(mcar_res(), error = function(e) data.frame(Status = "MCAR diagnostic unavailable."))
    writeData(wb, "Audit Dashboard", mcar_df, startRow = 2, startCol = 1, headerStyle = header_style)
    
    row_offset_b <- nrow(mcar_df) + 4
    writeData(wb, "Audit Dashboard", "Table B: Method Allocation Matrix", startRow = row_offset_b, startCol = 1)
    addStyle(wb, "Audit Dashboard", section_style, rows = row_offset_b, cols = 1)
    
    cfg_df <- tryCatch(configured_method_matrix(), error = function(e) tibble(Status = "No method allocation configured."))
    writeData(wb, "Audit Dashboard", cfg_df, startRow = row_offset_b + 1, startCol = 1, headerStyle = header_style)
    
    row_offset_c <- row_offset_b + nrow(cfg_df) + 3
    writeData(wb, "Audit Dashboard", "Table C: Mahalanobis Distance Outliers", startRow = row_offset_c, startCol = 1)
    addStyle(wb, "Audit Dashboard", section_style, rows = row_offset_c, cols = 1)
    
    m_data <- if (!is.null(mahal_results())) mahal_results() %>% filter(Is_Outlier == TRUE) else tibble(Status = "No Mahalanobis analysis executed.")
    writeData(wb, "Audit Dashboard", m_data, startRow = row_offset_c + 1, startCol = 1, headerStyle = header_style)
    
    row_offset_d <- row_offset_c + max(nrow(m_data) + 3, 4)
    writeData(wb, "Audit Dashboard", "Table D: Transaction History Audit Log", startRow = row_offset_d, startCol = 1)
    addStyle(wb, "Audit Dashboard", section_style, rows = row_offset_d, cols = 1)
    
    log_data <- audit_log()
    if (nrow(log_data) == 0) log_data <- tibble(Status = "No pipeline transactions recorded.")
    writeData(wb, "Audit Dashboard", log_data, startRow = row_offset_d + 1, startCol = 1, headerStyle = header_style)
    setColWidths(wb, "Audit Dashboard", cols = 1:8, widths = 22)
    
    # Sheet 3: Statistical Profiles
    addWorksheet(wb, "Statistical Profiles")
    writeData(wb, "Statistical Profiles", "Distribution Profiles (Pre vs. Post Imputation)", startRow = 1, startCol = 1)
    addStyle(wb, "Statistical Profiles", section_style, rows = 1, cols = 1)
    
    prof_df <- tryCatch(compute_distribution_profiles(raw_df, active_df), error = function(e) data.frame(Status = "Profiling unavailable."))
    writeData(wb, "Statistical Profiles", prof_df, startRow = 2, startCol = 1, headerStyle = header_style)
    setColWidths(wb, "Statistical Profiles", cols = 1:13, widths = 16)
    
    # Sheet 4: Correlation Analysis & Embedded Heatmap
    addWorksheet(wb, "Correlation Analysis")
    cmat <- corr_mat_calc()
    if (!is.null(cmat) && nrow(cmat) >= 2) {
      writeData(wb, "Correlation Analysis", "Pairwise Correlation Matrix", startRow = 1, startCol = 1)
      addStyle(wb, "Correlation Analysis", section_style, rows = 1, cols = 1)
      writeData(wb, "Correlation Analysis", as.data.frame(cmat), startRow = 2, startCol = 1, rowNames = TRUE, headerStyle = header_style)
      
      tryCatch({
        c_df <- cmat %>%
          as.data.frame() %>%
          rownames_to_column("Var1") %>%
          pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")
        
        p_c <- ggplot(c_df, aes(x = Var1, y = Var2, fill = Correlation)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(low = "#3498DB", mid = "#FFFFFF", high = "#E74C3C", limits = c(-1, 1)) +
          geom_text(aes(label = round(Correlation, 2)), size = 3) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Correlation Heatmap Matrix", x = "", y = "")
        
        # Use safe temporary filename to prevent OS file path errors
        img_corr_path <- tempfile(pattern = "corr_heatmap_", fileext = ".png")
        ggsave(img_corr_path, plot = p_c, width = 8, height = 6, dpi = 150)
        insertImage(wb, "Correlation Analysis", img_corr_path, startRow = nrow(cmat) + 5, startCol = 1, width = 8, height = 6, units = "in")
      }, error = function(e) {
        writeData(wb, "Correlation Analysis", paste("Heatmap graphic omitted:", e$message), startRow = nrow(cmat) + 5, startCol = 1)
      })
    } else {
      writeData(wb, "Correlation Analysis", "Fewer than 2 numeric columns available for correlation.", startRow = 1, startCol = 1)
    }
    
    # Sheet 5: Regression Diagnostics & Embedded Diagnostic Plots
    addWorksheet(wb, "Regression Diagnostics")
    fits <- reg_fit_state()
    if (!is.null(fits) && length(fits) > 0) {
      curr_row <- 1
      for (y_var in names(fits)) {
        fit_data <- fits[[y_var]]
        writeData(wb, "Regression Diagnostics", paste("Model Specification:", fit_data$formula), startRow = curr_row, startCol = 1)
        addStyle(wb, "Regression Diagnostics", section_style, rows = curr_row, cols = 1)
        
        writeData(wb, "Regression Diagnostics", fit_data$metrics_tbl, startRow = curr_row + 2, startCol = 1, headerStyle = header_style)
        writeData(wb, "Regression Diagnostics", fit_data$coef_tbl, startRow = curr_row + nrow(fit_data$metrics_tbl) + 4, startCol = 1, headerStyle = header_style)
        
        plot_row <- curr_row + nrow(fit_data$metrics_tbl) + nrow(fit_data$coef_tbl) + 6
        
        tryCatch({
          if (fit_data$is_binary) {
            prob <- predict(fit_data$model, type = "response")
            roc_obj <- pROC::roc(fit_data$data_used[[y_var]], prob, quiet = TRUE)
            roc_df <- data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities)
            p_diag <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
              geom_line(color = "#2980B9", linewidth = 1) +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
              theme_minimal() +
              labs(title = paste0("ROC Curve (Y: ", y_var, " | AUC = ", round(pROC::auc(roc_obj), 3), ")"), x = "1 - Specificity", y = "Sensitivity")
          } else {
            res_df <- data.frame(Fitted = fitted(fit_data$model), Residuals = residuals(fit_data$model))
            p_diag <- ggplot(res_df, aes(x = Fitted, y = Residuals)) +
              geom_point(alpha = 0.6, color = "steelblue") +
              geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
              theme_minimal() +
              labs(title = paste0("Residuals vs Fitted (Y: ", y_var, ")"), x = "Fitted Values", y = "Residuals")
          }
          
          img_reg_path <- tempfile(pattern = "reg_diag_", fileext = ".png")
          ggsave(img_reg_path, plot = p_diag, width = 7, height = 4.5, dpi = 150)
          insertImage(wb, "Regression Diagnostics", img_reg_path, startRow = plot_row, startCol = 1, width = 7, height = 4.5, units = "in")
          curr_row <- plot_row + 24
        }, error = function(e) {
          writeData(wb, "Regression Diagnostics", paste("Diagnostic plot omitted:", e$message), startRow = plot_row, startCol = 1)
          curr_row <<- plot_row + 4
        })
      }
    } else {
      writeData(wb, "Regression Diagnostics", "No regression models fitted yet. Specify and fit models in Tab 6.", startRow = 1, startCol = 1)
    }
    
    # Sheet 6: Imputation Visual Diagnostics & Embedded Density Plot
    addWorksheet(wb, "Imputation Visuals")
    target_v <- input$sel_density_var
    if (is.null(target_v) || target_v == "") {
      num_cols <- names(active_df)[vapply(active_df, is.numeric, logical(1))]
      if (length(num_cols) > 0) target_v <- num_cols[1]
    }
    
    if (!is.null(raw_df) && !is.null(target_v) && target_v %in% names(raw_df)) {
      tryCatch({
        v_orig <- na.omit(raw_df[[target_v]])
        v_imp  <- active_df[[target_v]]
        
        if (length(v_orig) > 1 && length(v_imp) > 1 && is.numeric(v_orig)) {
          df_combo <- data.frame(
            Value = c(v_orig, v_imp),
            Source = c(rep("Observed", length(v_orig)), rep("Imputed", length(v_imp)))
          )
          
          p_static <- ggplot(df_combo, aes(x = Value, fill = Source, color = Source)) +
            geom_density(alpha = 0.4) +
            scale_fill_manual(values = c("Observed" = "#2ECC71", "Imputed" = "#3498DB")) +
            scale_color_manual(values = c("Observed" = "#2ECC71", "Imputed" = "#3498DB")) +
            theme_minimal() +
            labs(title = paste("Density Diagnostic Profile (Observed vs. Imputed):", target_v), x = target_v, y = "Density")
          
          img_density_path <- tempfile(pattern = "density_diag_", fileext = ".png")
          ggsave(img_density_path, plot = p_static, width = 7.5, height = 4.8, dpi = 150)
          insertImage(wb, "Imputation Visuals", img_density_path, startRow = 2, startCol = 2, width = 7.5, height = 4.8, units = "in")
        } else {
          writeData(wb, "Imputation Visuals", "Selected target variable contains insufficient numeric points to compute density.", startRow = 2, startCol = 2)
        }
      }, error = function(e) {
        writeData(wb, "Imputation Visuals", paste("Density diagnostic omitted:", e$message), startRow = 2, startCol = 2)
      })
    } else {
      writeData(wb, "Imputation Visuals", "No completed imputation available to generate density plots.", startRow = 1, startCol = 1)
    }
    
    saveWorkbook(wb, file, overwrite = TRUE)
  }
  
  # Set explicit MIME content-type to ensure browser downloads as .xlsx
  output$btn_export_excel <- downloadHandler(
    filename = function() {
      paste0("Master_Multivariate_Analytics_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = generate_master_excel,
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$btn_export_excel_tab7 <- downloadHandler(
    filename = function() {
      paste0("Master_Multivariate_Analytics_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = generate_master_excel,
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

# ==============================================================================
# 6. APPLICATION RUNNER
# ==============================================================================
shinyApp(ui = ui, server = server)