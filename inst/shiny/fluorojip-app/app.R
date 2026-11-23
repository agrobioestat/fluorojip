library(shiny)

load_example_fluorojip <- function() {
  x <- new.env(parent = emptyenv())
  data("example_fluorojip", package = "fluorojip", envir = x)
  x$example_fluorojip
}

read_summary_input <- function(path, ext, sep = ",", sheet = 1) {
  ext <- tolower(ext)
  if (ext %in% c("csv", "txt")) {
    return(utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("xls", "xlsx")) {
    return(as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE, check.names = FALSE))
  }
  stop("Unsupported summary-table file type: ", ext, call. = FALSE)
}

dq <- function(x) paste0('"', gsub("\\\\", "/", x), '"')

ui <- navbarPage(
  title = "fluorojip",
  id = "main_nav",
  tabPanel(
    "Home",
    fluidPage(
      h2("fluorojip"),
      p("Interactive interface for OJIP / JIP-test calculation, trace import, quality checks, visualization, validation, and export."),
      tags$ol(
        tags$li("Data & Calculation: load a summary table, a FluorPen workbook, or a Biolyzer export, choose the extraction preset, and calculate."),
        tags$li("OJIP Curves: inspect the traces and the points used for O, K, J, I, and P."),
        tags$li("Parameters, Normalized 2D Plot, Heatmap, 3D Plot: explore the results."),
        tags$li("Validation: compare fluorojip with the values exported by Biolyzer or the FluorPen software."),
        tags$li("Export: download results, normalized tables, and an analysis log (an R script that records the settings and reproduces the analysis).")
      ),
      p(tags$b("Interpretation note:"), " fluorojip computes parameters from the values supplied. It cannot verify dark adaptation, saturating excitation, or the absence of measurement artefacts. Inspect the curves and the quality flags before interpreting parameters physiologically.")
    )
  ),
  tabPanel(
    "Data & Calculation",
    sidebarLayout(
      sidebarPanel(
        actionButton("load_example", "Load example summary data"),
        actionButton("load_example_fluorpen", "Load example FluorPen workbook"),
        actionButton("load_example_biolyzer", "Load example Biolyzer workbook"),
        tags$hr(),
        radioButtons("data_mode", "Input type",
                     choices = c("Summary table" = "summary", "FluorPen workbook" = "fluorpen",
                                 "Biolyzer export (CSV or Excel)" = "biolyzer")),
        fileInput("data_file", "Upload data file"),
        conditionalPanel(
          "input.data_mode == 'summary'",
          selectInput("summary_sep", "CSV separator", choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"), selected = ","),
          numericInput("summary_sheet", "Excel sheet index", value = 1, min = 1, step = 1)
        ),
        conditionalPanel(
          "input.data_mode != 'summary'",
          selectInput("preset", "Extraction preset",
                      choices = c("FluorPen (background subtracted; F50us, F300us, 2 ms, 30 ms)" = "fluorpen",
                                  "Biolyzer / Handy-PEA (F20us, F270us, 2 ms, 30 ms)" = "biolyzer")),
          radioButtons("step_method", "J and I positions",
                       choices = c("Fixed times (recommended)" = "fixed", "Derivative minima (experimental)" = "derivative"))
        ),
        actionButton("run_calc", "Calculate parameters", class = "btn-primary")
      ),
      mainPanel(
        h4(textOutput("data_status")),
        h5("Extraction preset"),
        verbatimTextOutput("preset_note"),
        h5("Quality flags"),
        tableOutput("qc_table"),
        h5("Input summary (first rows)"),
        tableOutput("summary_preview"),
        h5("Calculated results (first rows)"),
        tableOutput("results_preview")
      )
    )
  ),
  tabPanel(
    "OJIP Curves",
    sidebarLayout(
      sidebarPanel(
        helpText("Available after loading a FluorPen workbook or a Biolyzer export."),
        uiOutput("curve_sample_ui"),
        radioButtons("curve_norm", "Display", choices = c("Signal" = "none", "F / Fm" = "fm", "Vt" = "vt"), selected = "vt")
      ),
      mainPanel(
        h4(textOutput("curve_status")),
        plotOutput("curve_plot", height = "520px")
      )
    )
  ),
  tabPanel(
    "Parameters",
    fluidPage(
      fluidRow(
        column(4,
               actionButton("select_core_params", "Select core parameters"),
               actionButton("select_all_params", "Select all"),
               actionButton("clear_all_params", "Clear all")),
        column(8, h4(textOutput("param_selection_status")))
      ),
      fluidRow(
        column(6, h4("Input and basic parameters"), uiOutput("param_basic_ui")),
        column(6, h4("Calculated JIP-test parameters"), uiOutput("param_calculated_ui"))
      )
    )
  ),
  tabPanel(
    "Normalized 2D Plot",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("norm_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("norm_method", "Normalization", choices = c("none", "zscore", "minmax", "control_ratio", "control_then_zscore"), selected = "zscore"),
        textInput("control_level", "Control level (control-based normalization)", value = "control"),
        helpText("Normalized values are for exploratory comparison only; they are not physiological quantities.")
      ),
      mainPanel(plotOutput("normalized_plot", height = "520px"))
    )
  ),
  tabPanel(
    "Heatmap",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("heatmap_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("heatmap_scale", "Scale", choices = c("zscore", "none", "control_then_zscore"), selected = "zscore"),
        checkboxInput("heatmap_cluster", "Cluster rows and columns", value = TRUE)
      ),
      mainPanel(plotOutput("heatmap_plot", height = "640px"))
    )
  ),
  tabPanel(
    "3D Plot",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("plot3d_params", "Three parameters", choices = NULL, multiple = TRUE),
        checkboxInput("plot3d_normalize", "Normalize axes (z-scores)", value = TRUE)
      ),
      mainPanel(plotOutput("plot3d_output", height = "520px"))
    )
  ),
  tabPanel(
    "Validation",
    sidebarLayout(
      sidebarPanel(
        radioButtons("val_source", "Reference software", choices = c("Biolyzer" = "biolyzer", "FluorPen" = "fluorpen")),
        radioButtons("val_mode", "What is tested",
                     choices = c("Equations only (reference fluorescence values as input)" = "engine",
                                 "Import + equations (values extracted from traces)" = "importer")),
        fileInput("val_file", "Workbook (leave empty to use the bundled example)"),
        actionButton("run_validation", "Run validation", class = "btn-primary")
      ),
      mainPanel(
        h5("Agreement metrics"),
        tableOutput("val_metrics"),
        h5("Comparison (first rows)"),
        tableOutput("val_preview")
      )
    )
  ),
  tabPanel(
    "Export",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("export_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("export_norm_method", "Normalization", choices = c("none", "zscore", "minmax", "control_ratio", "control_then_zscore"), selected = "none"),
        textInput("export_control_level", "Control level", value = "control"),
        downloadButton("download_results", "Results (CSV)"),
        downloadButton("download_normalized", "Normalized table (CSV, ';')"),
        tags$hr(),
        downloadButton("download_log", "Analysis log (R script)"),
        helpText("The analysis log records the date, R and package versions, input file name and MD5 checksum, all settings, quality-flag counts, and the R commands that reproduce the calculation outside the app.")
      ),
      mainPanel(tableOutput("export_preview"))
    )
  ),
  tabPanel(
    "Help",
    fluidPage(
      h3("Supported inputs"),
      tags$ul(
        tags$li("Summary tables (CSV, TXT, XLS, XLSX) with columns fo, f300us (or k), j, i, fm and optionally area."),
        tags$li("FluorPen OJIP workbooks exported by the FluorPen software (Photon Systems Instruments)."),
        tags$li("Biolyzer exports: the 'Record No' CSV trace table or the Excel workbook with 'Trace data' and 'JIP Parameters' sheets.")
      ),
      h3("Quality flags"),
      p("Flags such as fm_at_last_point, fm_before_i, *_time_out_of_tolerance, mo_interval_deviation, and non_monotonic indicate that a value could not be extracted at the documented time, or that the curve does not have the usual OJIP shape. Values outside tolerance are returned as NA, never replaced by distant points."),
      h3("Reproducibility"),
      p("Interactive sessions are reproducible only if the input file, the exported results, and the analysis log are archived together. The log can be run in R to repeat the calculation.")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    source_label = "No data loaded",
    file_name = NA_character_, file_path = NA_character_, input_type = NA_character_,
    summary_df = NULL, results = NULL, raw = NULL, val = NULL, val_settings = NULL
  )

  run_traces <- function(path, type, name) {
    if (type == "fluorpen") {
      rv$raw <- fluorojip::read_fluorpen_xlsx(path)
      rv$raw$times_plot <- rv$raw$times_us
      rv$raw$unit <- "us"
      rv$summary_df <- fluorojip::fluorpen_to_ojip(rv$raw, preset = input$preset, step_method = input$step_method)
    } else {
      ext <- tolower(tools::file_ext(name))
      rv$raw <- if (ext %in% c("xls", "xlsx")) fluorojip::read_biolyzer_xls(path) else fluorojip::read_biolyzer_csv(path)
      rv$raw$times_plot <- if (!is.null(rv$raw$times_ms)) rv$raw$times_ms else rv$raw$times_s
      rv$raw$unit <- if (!is.null(rv$raw$times_ms)) "ms" else "auto"
      rv$summary_df <- fluorojip::biolyzer_to_ojip(rv$raw, preset = input$preset, step_method = input$step_method)
    }
    rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
    rv$file_name <- name
    rv$file_path <- path
    rv$code_path <- dq(name)
    rv$input_type <- type
    rv$source_label <- paste0("Loaded ", type, " traces: ", name, " (", nrow(rv$results), " measurements)")
  }

  observeEvent(input$load_example, {
    rv$summary_df <- load_example_fluorojip()
    rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
    rv$raw <- NULL
    rv$file_name <- "example_fluorojip (bundled dataset)"
    rv$file_path <- NA_character_
    rv$input_type <- "example"
    rv$source_label <- "Loaded bundled example summary dataset"
  })
  observeEvent(input$load_example_fluorpen, {
    updateRadioButtons(session, "data_mode", selected = "fluorpen")
    updateSelectInput(session, "preset", selected = "fluorpen")
    p <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
    withCallingHandlers(run_traces(p, "fluorpen", "FluorPen_test.xlsx (bundled)"), warning = function(w) invokeRestart("muffleWarning"))
    rv$code_path <- 'system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")'
  })
  observeEvent(input$load_example_biolyzer, {
    updateRadioButtons(session, "data_mode", selected = "biolyzer")
    updateSelectInput(session, "preset", selected = "biolyzer")
    p <- fluorojip::fluorojip_example_biolyzer_file()
    run_traces(p, "biolyzer", basename(p))
    rv$code_path <- "fluorojip_example_biolyzer_file()"
  })

  observeEvent(input$run_calc, {
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))
    if (identical(input$data_mode, "summary")) {
      rv$summary_df <- read_summary_input(input$data_file$datapath, ext, sep = input$summary_sep, sheet = input$summary_sheet)
      rv$raw <- NULL
      rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
      rv$file_name <- input$data_file$name
      rv$file_path <- input$data_file$datapath
      rv$code_path <- dq(input$data_file$name)
      rv$input_type <- "summary"
      rv$source_label <- paste("Loaded summary table:", input$data_file$name)
    } else {
      run_traces(input$data_file$datapath, input$data_mode, input$data_file$name)
    }
  })

  numeric_params <- reactive({
    req(rv$results)
    names(rv$results)[vapply(rv$results, is.numeric, logical(1))]
  })
  groups <- reactive({
    np <- numeric_params()
    basic <- intersect(c("fo", "f300us", "k", "j", "i", "fm", "area", "t_fm_ms", "Fv", "Fv_Fm", "Vj", "Vi"), np)
    list(basic = basic, calculated = setdiff(np, c(basic, "p", "t_fm", "background", grep("^t_", np, value = TRUE))))
  })
  core <- c("fo", "f300us", "j", "i", "fm", "area", "Fv_Fm", "Vj", "Vi", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "phi_Eo", "psi_Eo", "PI_abs")
  selected_parameters <- reactive({
    req(rv$results)
    sel <- unique(c(input$param_basic, input$param_calculated))
    if (length(sel) == 0) sel <- intersect(core, numeric_params())
    sel
  })

  output$param_basic_ui <- renderUI({
    g <- groups()
    checkboxGroupInput("param_basic", NULL, choices = g$basic, selected = intersect(core, g$basic))
  })
  output$param_calculated_ui <- renderUI({
    g <- groups()
    checkboxGroupInput("param_calculated", NULL, choices = g$calculated, selected = intersect(core, g$calculated))
  })
  observeEvent(input$select_core_params, {
    g <- groups()
    updateCheckboxGroupInput(session, "param_basic", selected = intersect(core, g$basic))
    updateCheckboxGroupInput(session, "param_calculated", selected = intersect(core, g$calculated))
  })
  observeEvent(input$select_all_params, {
    g <- groups()
    updateCheckboxGroupInput(session, "param_basic", selected = g$basic)
    updateCheckboxGroupInput(session, "param_calculated", selected = g$calculated)
  })
  observeEvent(input$clear_all_params, {
    updateCheckboxGroupInput(session, "param_basic", selected = character(0))
    updateCheckboxGroupInput(session, "param_calculated", selected = character(0))
  })
  output$param_selection_status <- renderText({
    paste(length(selected_parameters()), "parameters available to plots and exports")
  })

  observeEvent(selected_parameters(), {
    p <- selected_parameters()
    pick <- function(def, n) { s <- intersect(def, p); if (length(s) == 0) head(p, n) else s }
    updateSelectizeInput(session, "norm_params", choices = p, selected = pick(c("Fv_Fm", "PI_abs", "ABS_RC", "DIo_RC"), 4), server = TRUE)
    updateSelectizeInput(session, "heatmap_params", choices = p, selected = pick(c("Fv_Fm", "Vj", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs"), 6), server = TRUE)
    updateSelectizeInput(session, "plot3d_params", choices = p, selected = head(pick(c("Fv_Fm", "PI_abs", "ABS_RC"), 3), 3), server = TRUE)
    updateSelectizeInput(session, "export_params", choices = p, selected = pick(c("Fv_Fm", "Vj", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs"), 6), server = TRUE)
  })

  output$data_status <- renderText(rv$source_label)
  output$preset_note <- renderText({
    if (is.null(rv$raw)) return("Not applicable (summary-table input).")
    ext <- attr(rv$summary_df, "extraction")
    paste0(fluorojip::ojip_preset(ext$preset)$note, "\nTime unit: ", ext$time_unit,
           "; background: ", ext$background, "; J/I method: ", ext$step_method,
           "; discarded time points: ", ext$n_discarded_time_points)
  })
  output$qc_table <- renderTable({
    req(rv$results)
    tab <- as.data.frame(table(qc_flags = rv$results$qc_flags), stringsAsFactors = FALSE)
    names(tab)[2] <- "measurements"
    tab
  })
  output$summary_preview <- renderTable(head(rv$summary_df, 8), digits = 4)
  output$results_preview <- renderTable({
    req(rv$results)
    keep <- intersect(c("sample_id", "treatment", "Fv_Fm", "Vj", "Mo", "ABS_RC", "ETo_RC", "DIo_RC", "PI_abs", "qc_flags"), names(rv$results))
    head(rv$results[, keep, drop = FALSE], 8)
  }, digits = 4)

  output$curve_sample_ui <- renderUI({
    if (is.null(rv$raw)) return(tags$em("No traces loaded."))
    ids <- rv$summary_df$sample_id
    selectInput("curve_samples", "Measurements", choices = ids, selected = head(ids, 4), multiple = TRUE)
  })
  output$curve_status <- renderText({
    if (is.null(rv$raw)) "No traces available." else paste(nrow(rv$raw$mat), "traces loaded.")
  })
  output$curve_plot <- renderPlot({
    validate(need(!is.null(rv$raw), "Load a FluorPen workbook or a Biolyzer export to see the curves."))
    req(input$curve_samples)
    fluorojip::plot_ojip_trace(rv$raw$times_plot, rv$raw$mat, ojip = rv$summary_df,
                               samples = input$curve_samples, normalize = input$curve_norm)
  })

  norm_table <- function(params, method, ctrl) {
    fluorojip::normalized_jiptable(rv$results, params = params, normalize = method,
                                   control_level = if (nzchar(ctrl)) ctrl else NULL, output = "wide")
  }
  output$normalized_plot <- renderPlot({
    req(rv$results, input$norm_params)
    tab <- norm_table(input$norm_params, input$norm_method, input$control_level)
    mat <- as.matrix(tab[, input$norm_params, drop = FALSE])
    cols <- grDevices::hcl.colors(ncol(mat), "Dark 3")
    graphics::matplot(seq_len(nrow(mat)), mat, type = "b", lty = 1, pch = 16, col = cols,
                      xaxt = "n", xlab = "", ylab = paste("Normalized value (", input$norm_method, ")"))
    graphics::axis(1, at = seq_len(nrow(mat)), labels = tab$sample_id, las = 2, cex.axis = 0.8)
    graphics::legend("topright", legend = input$norm_params, col = cols, lty = 1, pch = 16, cex = 0.8, bty = "n")
  })
  output$heatmap_plot <- renderPlot({
    req(rv$results, input$heatmap_params)
    fluorojip::plot_heatmap_fluorojip(rv$results, params = input$heatmap_params,
                                      scale = input$heatmap_scale, cluster = isTRUE(input$heatmap_cluster),
                                      control_level = input$control_level)
  })
  output$plot3d_output <- renderPlot({
    req(rv$results, input$plot3d_params)
    validate(need(length(input$plot3d_params) == 3, "Please select exactly 3 parameters."))
    fluorojip::plot_3d_fluorojip(rv$results, params = input$plot3d_params, normalize = isTRUE(input$plot3d_normalize))
  })

  observeEvent(input$run_validation, {
    path <- if (!is.null(input$val_file)) input$val_file$datapath else NULL
    rv$val <- suppressWarnings(if (input$val_source == "biolyzer") {
      if (is.null(path)) fluorojip::validate_biolyzer(mode = input$val_mode) else fluorojip::validate_biolyzer(path, mode = input$val_mode)
    } else {
      if (is.null(path)) fluorojip::validate_fluorpen(mode = input$val_mode) else fluorojip::validate_fluorpen(path, mode = input$val_mode)
    })
    rv$val_settings <- list(source = input$val_source, mode = input$val_mode,
                            file = if (is.null(path)) "bundled example" else input$val_file$name)
  })
  output$val_metrics <- renderTable({
    req(rv$val)
    rv$val$metrics[, c("parameter", "n", "bias", "mae", "max_abs_diff", "lin_ccc")]
  }, digits = 6)
  output$val_preview <- renderTable(head(req(rv$val)$comparison, 15), digits = 6)

  normalized_export <- reactive({
    req(rv$results)
    p <- input$export_params
    if (length(p) == 0) p <- selected_parameters()
    norm_table(p, input$export_norm_method, input$export_control_level)
  })
  output$export_preview <- renderTable(head(normalized_export(), 12), digits = 6)
  output$download_results <- downloadHandler(
    filename = function() "fluorojip_results.csv",
    content = function(file) utils::write.csv(rv$results, file, row.names = FALSE)
  )
  output$download_normalized <- downloadHandler(
    filename = function() "fluorojip_normalized.csv",
    content = function(file) fluorojip::write_normalized_jiptable(normalized_export(), file)
  )

  output$download_log <- downloadHandler(
    filename = function() paste0("fluorojip_analysis_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content = function(file) {
      req(rv$results)
      md5 <- if (!is.na(rv$file_path) && file.exists(rv$file_path)) unname(tools::md5sum(rv$file_path)) else NA
      ext <- attr(rv$summary_df, "extraction")
      qc <- table(rv$results$qc_flags)
      p <- input$export_params
      if (length(p) == 0) p <- selected_parameters()
      hdr <- c(
        "# fluorojip analysis log (generated by the Shiny app)",
        paste0("# date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        paste0("# R: ", R.version.string),
        paste0("# fluorojip: ", as.character(utils::packageVersion("fluorojip")),
               "; shiny: ", as.character(utils::packageVersion("shiny")),
               "; readxl: ", as.character(utils::packageVersion("readxl"))),
        paste0("# input_type: ", rv$input_type),
        paste0("# input_file: ", rv$file_name),
        paste0("# input_md5: ", md5),
        paste0("# n_measurements: ", nrow(rv$results)),
        if (!is.null(ext)) c(
          paste0("# preset: ", ext$preset, "; time_unit: ", ext$time_unit, "; background: ", ext$background,
                 "; step_method: ", ext$step_method, "; area_time_unit: ", ext$area_time_unit),
          paste0("# targets_ms: ", paste(names(ext$targets_ms), ext$targets_ms, sep = "=", collapse = ", ")),
          paste0("# tolerance_ms: ", paste(names(ext$tolerance_ms), ext$tolerance_ms, sep = "=", collapse = ", ")),
          paste0("# discarded_time_points: ", ext$n_discarded_time_points)
        ),
        paste0("# qc_flags: ", paste(names(qc), as.integer(qc), sep = " = ", collapse = "; ")),
        paste0("# export_normalization: ", input$export_norm_method, "; control_level: ", input$export_control_level),
        paste0("# export_parameters: ", paste(p, collapse = ", ")),
        if (!is.null(rv$val_settings)) paste0("# last_validation: ", rv$val_settings$source, " / ", rv$val_settings$mode, " / ", rv$val_settings$file),
        "",
        "library(fluorojip)",
        "# Replace the path below by the location of the archived input file."
      )
      fname <- if (is.na(rv$file_name)) "input_file" else rv$file_name
      cpath <- if (is.null(rv$code_path)) dq(fname) else rv$code_path
      code <- switch(
        rv$input_type,
        example = "res <- calc_fluorojip(example_fluorojip)",
        summary = c(paste0("dat <- utils::read.csv(", cpath, ", sep = ", dq(input$summary_sep), ")"),
                    "res <- calc_fluorojip(dat)"),
        fluorpen = c(paste0("raw <- read_fluorpen_xlsx(", cpath, ")"),
                     paste0("ojip <- fluorpen_to_ojip(raw, preset = ", dq(ext$preset), ", step_method = ", dq(ext$step_method), ")"),
                     "res <- calc_fluorojip(ojip)"),
        biolyzer = c(paste0("raw <- ", if (tolower(tools::file_ext(fname)) %in% c("xls", "xlsx")) "read_biolyzer_xls(" else "read_biolyzer_csv(", cpath, ")"),
                     paste0("ojip <- biolyzer_to_ojip(raw, preset = ", dq(ext$preset), ", step_method = ", dq(ext$step_method), ")"),
                     "res <- calc_fluorojip(ojip)")
      )
      tail_code <- c(
        "table(res$qc_flags)",
        paste0("tab <- normalized_jiptable(res, params = c(", paste(dq(p), collapse = ", "), "), normalize = ", dq(input$export_norm_method),
               if (grepl("control", input$export_norm_method)) paste0(", control_level = ", dq(input$export_control_level)) else "", ")"),
        "utils::write.csv(res, \"fluorojip_results.csv\", row.names = FALSE)"
      )
      writeLines(c(hdr, code, tail_code), file)
    }
  )
}

shinyApp(ui, server)
