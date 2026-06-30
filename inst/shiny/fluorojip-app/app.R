library(shiny)

load_example_fluorojip <- function() {
  x <- new.env(parent = emptyenv())
  data("example_fluorojip", package = "fluorojip", envir = x)
  x$example_fluorojip
}

find_project_fluorpen_dir <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    candidate <- file.path(current, "fluorpen")
    if (dir.exists(candidate)) {
      return(candidate)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    current <- parent
  }
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

summarize_abs_diffs <- function(df, cols) {
  data.frame(
    metric = names(cols),
    mean_abs_diff = vapply(cols, function(col) mean(abs(df[[col]]), na.rm = TRUE), numeric(1)),
    max_abs_diff = vapply(cols, function(col) max(abs(df[[col]]), na.rm = TRUE), numeric(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

parameter_groups_from_results <- function(res) {
  numeric_cols <- names(res)[vapply(res, is.numeric, logical(1))]

  basic_candidates <- c(
    "fo", "o", "k", "fk", "f300", "f300us", "f_300us", "f_300_us",
    "j", "fj", "i", "fi", "fm", "p", "t_fm", "area",
    "Fv", "phi_Po", "Fv_Fm", "Vj", "Vi"
  )

  basic <- intersect(basic_candidates, numeric_cols)
  calculated <- setdiff(numeric_cols, basic)

  list(
    basic = basic,
    calculated = calculated
  )
}

core_parameter_defaults <- function(groups) {
  unique(c(
    intersect(groups$basic, c("fo", "k", "j", "i", "fm", "area", "Fv_Fm", "Vj", "Vi")),
    intersect(groups$calculated, c("Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "phi_Eo", "psi_Eo", "PI_abs", "Sm", "N"))
  ))
}

compare_biolyzer_workbook <- function(path) {
  jp <- readxl::read_excel(path, sheet = "JIP Parameters", col_names = FALSE)
  hdr <- as.character(unlist(jp[1, ]))
  dat <- as.data.frame(jp[-1, ], stringsAsFactors = FALSE, check.names = FALSE)
  names(dat) <- hdr
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))

  inp <- data.frame(
    sample_id = as.character(dat[["0 Trace Name"]]),
    t_fm = num(dat[["18 T(Fm)"]]),
    fo = num(dat[["22 Fo"]]),
    k = num(dat[["13 F(K)"]]),
    j = num(dat[["15 F(J)"]]),
    i = num(dat[["17 F(I)"]]),
    fm = num(dat[["23 Fm"]]),
    stringsAsFactors = FALSE
  )

  res <- fluorojip::calc_fluorojip(inp)
  comp <- data.frame(
    sample_id = inp$sample_id,
    fluorojip_FvFm = res$Fv_Fm,
    biolyzer_FvFm = num(dat[["39 Fv/Fm"]]),
    fluorojip_Mo = res$Mo,
    biolyzer_Mo = num(dat[["51 Mo"]]),
    fluorojip_ABS_RC = res$ABS_RC,
    biolyzer_ABS_RC = num(dat[["68 ABS/RC"]]),
    fluorojip_phiPo = res$phi_Po,
    biolyzer_TRo_ABS = num(dat[["89 TRo/ABS"]]),
    fluorojip_phiEo = res$phi_Eo,
    biolyzer_ETo_ABS = num(dat[["90 ETo/ABS"]]),
    fluorojip_PI_abs = res$PI_abs,
    biolyzer_PI_abs = num(dat[["101 PI(abs)1"]]),
    stringsAsFactors = FALSE
  )

  comp$diff_FvFm <- comp$fluorojip_FvFm - comp$biolyzer_FvFm
  comp$diff_Mo <- comp$fluorojip_Mo - comp$biolyzer_Mo
  comp$diff_ABS_RC <- comp$fluorojip_ABS_RC - comp$biolyzer_ABS_RC
  comp$diff_TRo_ABS <- comp$fluorojip_phiPo - comp$biolyzer_TRo_ABS
  comp$diff_ETo_ABS <- comp$fluorojip_phiEo - comp$biolyzer_ETo_ABS
  comp$diff_PI_abs <- comp$fluorojip_PI_abs - comp$biolyzer_PI_abs

  metrics <- summarize_abs_diffs(comp, c(
    Fv_Fm = "diff_FvFm",
    Mo = "diff_Mo",
    ABS_RC = "diff_ABS_RC",
    TRo_ABS = "diff_TRo_ABS",
    ETo_ABS = "diff_ETo_ABS",
    PI_abs = "diff_PI_abs"
  ))

  list(comp = comp, metrics = metrics)
}

compare_fluorpen_files <- function(paths) {
  all_comp <- lapply(paths, function(path) {
    raw <- fluorojip::read_fluorpen_xlsx(path)
    ojip <- fluorojip::fluorpen_to_ojip(raw)
    res <- fluorojip::calc_fluorojip(ojip)
    vendor <- raw$summary_numeric

    comp <- data.frame(
      source_file = basename(path),
      sample_id = ojip$sample_id,
      fluorojip_FvFm = res$Fv_Fm,
      vendor_FvFm = vendor[["Fv/Fm"]],
      fluorojip_Mo = res$Mo,
      vendor_Mo = vendor[["Mo"]],
      fluorojip_ABS_RC = res$ABS_RC,
      vendor_ABS_RC = vendor[["ABS/RC"]],
      fluorojip_TRo_RC = res$TRo_RC,
      vendor_TRo_RC = vendor[["TRo/RC"]],
      fluorojip_ETo_RC = res$ETo_RC,
      vendor_ETo_RC = vendor[["ETo/RC"]],
      fluorojip_DIo_RC = res$DIo_RC,
      vendor_DIo_RC = vendor[["DIo/RC"]],
      fluorojip_psi_Eo = res$psi_Eo,
      vendor_Psi_o = vendor[["Psi_o"]],
      fluorojip_phi_Eo = res$phi_Eo,
      vendor_Phi_Eo = vendor[["Phi_Eo"]],
      fluorojip_PI_abs = res$PI_abs,
      vendor_PI_abs = vendor[["Pi_Abs"]],
      stringsAsFactors = FALSE
    )

    comp$diff_FvFm <- comp$fluorojip_FvFm - comp$vendor_FvFm
    comp$diff_Mo <- comp$fluorojip_Mo - comp$vendor_Mo
    comp$diff_ABS_RC <- comp$fluorojip_ABS_RC - comp$vendor_ABS_RC
    comp$diff_TRo_RC <- comp$fluorojip_TRo_RC - comp$vendor_TRo_RC
    comp$diff_ETo_RC <- comp$fluorojip_ETo_RC - comp$vendor_ETo_RC
    comp$diff_DIo_RC <- comp$fluorojip_DIo_RC - comp$vendor_DIo_RC
    comp$diff_Psi_o <- comp$fluorojip_psi_Eo - comp$vendor_Psi_o
    comp$diff_Phi_Eo <- comp$fluorojip_phi_Eo - comp$vendor_Phi_Eo
    comp$diff_PI_abs <- comp$fluorojip_PI_abs - comp$vendor_PI_abs
    comp
  })

  comp_all <- do.call(rbind, all_comp)
  metrics <- summarize_abs_diffs(comp_all, c(
    Fv_Fm = "diff_FvFm",
    Mo = "diff_Mo",
    ABS_RC = "diff_ABS_RC",
    TRo_RC = "diff_TRo_RC",
    ETo_RC = "diff_ETo_RC",
    DIo_RC = "diff_DIo_RC",
    Psi_o = "diff_Psi_o",
    Phi_Eo = "diff_Phi_Eo",
    PI_abs = "diff_PI_abs"
  ))

  list(comp = comp_all, metrics = metrics)
}

ui <- navbarPage(
  title = "fluorojip",
  id = "main_nav",
  tabPanel(
    "Home",
    fluidPage(
      h2("fluorojip Shiny App"),
      p("Interactive interface for FluorOJIP / JIP-test calculation, visualization, validation, and export."),
      p("The app supports summary-table workflows and FluorPen workbook workflows, and includes dedicated tabs for OJIP curves, parameter selection, normalized plots, heatmaps, 3D exploration, validation, export, and help."),
      tags$ul(
        tags$li("Load the bundled example dataset, a summary table, or a FluorPen workbook"),
        tags$li("Calculate JIP-test parameters from supported inputs"),
        tags$li("Inspect OJIP curves and selected parameter plots"),
        tags$li("Run Biolyzer and FluorPen validation workflows"),
        tags$li("Export raw results and normalized parameter tables")
      )
    )
  ),
  tabPanel(
    "Data & Calculation",
    sidebarLayout(
      sidebarPanel(
        actionButton("load_example", "Load Example Data"),
        tags$hr(),
        radioButtons("data_mode", "Input mode", choices = c("Summary table" = "summary", "FluorPen workbook" = "fluorpen")),
        fileInput("data_file", "Upload data file"),
        conditionalPanel(
          "input.data_mode == 'summary'",
          selectInput("summary_sep", "CSV separator", choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"), selected = ","),
          numericInput("summary_sheet", "Excel sheet index", value = 1, min = 1, step = 1)
        ),
        actionButton("run_calc", "Calculate Parameters", class = "btn-primary")
      ),
      mainPanel(
        h4(textOutput("data_status")),
        h5("Input summary"),
        tableOutput("summary_preview"),
        h5("Calculated results"),
        tableOutput("results_preview")
      )
    )
  ),
  tabPanel(
    "OJIP Curves",
    sidebarLayout(
      sidebarPanel(
        helpText("Load a FluorPen workbook here, or first load one in the Data & Calculation tab."),
        fileInput("curve_file", "Upload FluorPen workbook (.xlsx) for OJIP curves"),
        actionButton("use_current_raw_trace", "Use Current Raw Trace"),
        uiOutput("curve_sample_ui"),
        checkboxInput("curve_points", "Show points", value = FALSE),
        checkboxInput("curve_log_x", "Log10 time axis", value = TRUE)
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
        column(
          width = 4,
          actionButton("select_core_params", "Select Core Parameters"),
          actionButton("select_all_params", "Select All"),
          actionButton("clear_all_params", "Clear All")
        ),
        column(
          width = 8,
          h4(textOutput("param_selection_status"))
        )
      ),
      fluidRow(
        column(
          width = 6,
          h4("Basic / Direct Parameters"),
          uiOutput("param_basic_ui")
        ),
        column(
          width = 6,
          h4("Calculated JIP Parameters"),
          uiOutput("param_calculated_ui")
        )
      )
    )
  ),
  tabPanel(
    "Normalized 2D Plot",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("norm_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("norm_method", "Normalization", choices = c("none", "zscore", "minmax", "control_ratio", "control_then_zscore"), selected = "zscore"),
        textInput("control_level", "Control level (for control-based normalization)", value = "control")
      ),
      mainPanel(
        plotOutput("normalized_plot", height = "520px")
      )
    )
  ),
  tabPanel(
    "Heatmap",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("heatmap_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("heatmap_scale", "Scale", choices = c("zscore", "none"), selected = "zscore")
      ),
      mainPanel(
        plotOutput("heatmap_plot", height = "640px")
      )
    )
  ),
  tabPanel(
    "3D Plot",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("plot3d_params", "Three parameters", choices = NULL, multiple = TRUE),
        checkboxInput("plot3d_normalize", "Normalize axes", value = TRUE)
      ),
      mainPanel(
        plotOutput("plot3d_output", height = "520px")
      )
    )
  ),
  tabPanel(
    "Validation",
    tabsetPanel(
      tabPanel(
        "Biolyzer",
        sidebarLayout(
          sidebarPanel(
            actionButton("use_biolyzer_example", "Use Bundled Example"),
            fileInput("biolyzer_file", "Upload Biolyzer .xls/.xlsx"),
            actionButton("run_biolyzer_validation", "Run Biolyzer Validation", class = "btn-primary")
          ),
          mainPanel(
            h5("Validation metrics"),
            tableOutput("biolyzer_metrics"),
            h5("Comparison preview"),
            tableOutput("biolyzer_preview")
          )
        )
      ),
      tabPanel(
        "FluorPen",
        sidebarLayout(
          sidebarPanel(
            actionButton("use_project_fluorpen", "Use Project FluorPen Files"),
            fileInput("fluorpen_validation_files", "Upload FluorPen .xlsx files", multiple = TRUE),
            actionButton("run_fluorpen_validation", "Run FluorPen Validation", class = "btn-primary")
          ),
          mainPanel(
            h5("Validation metrics"),
            tableOutput("fluorpen_metrics"),
            h5("Comparison preview"),
            tableOutput("fluorpen_preview")
          )
        )
      )
    )
  ),
  tabPanel(
    "Export",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("export_params", "Parameters", choices = NULL, multiple = TRUE),
        selectInput("export_norm_method", "Normalization", choices = c("none", "zscore", "minmax", "control_ratio", "control_then_zscore"), selected = "zscore"),
        textInput("export_control_level", "Control level", value = "control"),
        helpText("The normalized export uses the package helper for normalized JIP tables, which writes a semicolon-separated file with decimal point."),
        downloadButton("download_results", "Download Results CSV"),
        downloadButton("download_normalized", "Download Normalized CSV")
      ),
      mainPanel(
        tableOutput("export_preview")
      )
    )
  ),
  tabPanel(
    "Help",
    fluidPage(
      h3("fluorojip Help"),
      h4("Quick Start"),
      tags$ol(
        tags$li("Open the Data & Calculation tab."),
        tags$li("Load the bundled example dataset, a summary table, or a FluorPen workbook."),
        tags$li("Click Calculate Parameters."),
        tags$li("Open the Parameters tab and choose which parameters you want to use."),
        tags$li("Explore the plot tabs, validation tabs, and export tab as needed.")
      ),
      h4("Main Tabs"),
      tags$ul(
        tags$li(tags$b("Data & Calculation:"), " Load data and calculate JIP-test parameters from summary tables or FluorPen workbooks."),
        tags$li(tags$b("OJIP Curves:"), " Inspect raw FluorPen traces with time shown on a linear or log-scale X-axis."),
        tags$li(tags$b("Parameters:"), " Choose the basic and calculated parameters made available to the plotting and export tabs."),
        tags$li(tags$b("Normalized 2D Plot:"), " Plot selected normalized parameters across samples."),
        tags$li(tags$b("Heatmap:"), " Visualize selected parameters as a heatmap."),
        tags$li(tags$b("3D Plot:"), " Explore exactly three selected parameters in a 3D scatter plot."),
        tags$li(tags$b("Validation:"), " Compare fluorojip outputs against Biolyzer or FluorPen vendor-oriented reference values."),
        tags$li(tags$b("Export:"), " Download results and normalized parameter tables."),
        tags$li(tags$b("Help:"), " Review supported inputs, tab purposes, and troubleshooting notes.")
      ),
      h4("Supported Inputs"),
      tags$ul(
        tags$li("Summary tables in CSV, TXT, XLS, or XLSX format."),
        tags$li("FluorPen .xlsx exports for raw-trace import and validation."),
        tags$li("Bundled Biolyzer example workbook for validation workflows.")
      ),
      h4("Parameter Selection"),
      p("The ", tags$b("Parameters"), " tab controls which parameters are available in the ", tags$b("Normalized 2D Plot"), ", ", tags$b("Heatmap"), ", ", tags$b("3D Plot"), ", and ", tags$b("Export"), " tabs."),
      tags$ul(
        tags$li("Use ", tags$b("Select Core Parameters"), " for a practical default subset."),
        tags$li("Use ", tags$b("Select All"), " to expose every numeric parameter."),
        tags$li("Use ", tags$b("Clear All"), " to reset the list and then choose a custom subset.")
      ),
      h4("Validation"),
      tags$ul(
        tags$li("Biolyzer validation compares fluorojip outputs against vendor-oriented reference values from the bundled example workbook or an uploaded workbook."),
        tags$li("FluorPen validation compares fluorojip calculations from raw traces against the vendor-calculated footer parameters available in FluorPen exports."),
        tags$li("Biolyzer remains the primary external validation reference. FluorPen is a useful secondary validation source.")
      ),
      h4("Export"),
      tags$ul(
        tags$li("Results export writes a standard comma-separated CSV file."),
        tags$li("Normalized export uses the package helper for normalized JIP parameter tables and writes a semicolon-separated file with decimal point."),
        tags$li("The export preview shows the same normalized table that will be written by the normalized export button.")
      ),
      h4("Troubleshooting"),
      tags$ul(
        tags$li("If the ", tags$b("OJIP Curves"), " tab is empty, load a FluorPen workbook there or in the ", tags$b("Data & Calculation"), " tab."),
        tags$li("If a plot tab shows no parameter choices, first calculate results and then select parameters in the ", tags$b("Parameters"), " tab."),
        tags$li("The ", tags$b("3D Plot"), " tab requires exactly three parameters."),
        tags$li("Control-based normalization requires a valid control label, usually ", tags$code("control"), ".")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    source_label = "No data loaded",
    summary_df = NULL,
    results = NULL,
    raw_trace = NULL,
    biolyzer = NULL,
    fluorpen_validation = NULL
  )

  all_numeric_params <- reactive({
    req(rv$results)
    names(rv$results)[vapply(rv$results, is.numeric, logical(1))]
  })

  parameter_groups <- reactive({
    req(rv$results)
    parameter_groups_from_results(rv$results)
  })

  selected_parameters <- reactive({
    req(rv$results)
    if (is.null(input$param_basic) && is.null(input$param_calculated)) {
      return(core_parameter_defaults(parameter_groups()))
    }

    sel <- unique(c(input$param_basic, input$param_calculated))
    sel[sel %in% all_numeric_params()]
  })

  current_params <- reactive({
    req(rv$results)
    selected_parameters()
  })

  normalized_export <- reactive({
    req(rv$results)
    params <- input$export_params
    if (is.null(params) || length(params) == 0) {
      params <- current_params()
    }
    req(length(params) > 0)
    fluorojip::normalized_jiptable(
      rv$results,
      params = params,
      normalize = input$export_norm_method,
      control_level = if (nzchar(input$export_control_level)) input$export_control_level else NULL,
      output = "wide"
    )
  })

  observeEvent(input$load_example, {
    rv$summary_df <- load_example_fluorojip()
    rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
    rv$raw_trace <- NULL
    rv$source_label <- "Loaded bundled example dataset"
  })

  observeEvent(input$run_calc, {
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))

    if (identical(input$data_mode, "summary")) {
      rv$summary_df <- read_summary_input(
        path = input$data_file$datapath,
        ext = ext,
        sep = input$summary_sep,
        sheet = input$summary_sheet
      )
      rv$raw_trace <- NULL
      rv$source_label <- paste("Loaded summary table:", input$data_file$name)
    } else {
      rv$raw_trace <- fluorojip::read_fluorpen_xlsx(input$data_file$datapath, sheet = 1)
      rv$summary_df <- fluorojip::fluorpen_to_ojip(rv$raw_trace)
      rv$source_label <- paste("Loaded FluorPen workbook:", input$data_file$name)
    }

    rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
  })

  observeEvent(input$use_current_raw_trace, {
    req(rv$raw_trace)
  })

  observeEvent(input$curve_file, {
    req(input$curve_file)
    rv$raw_trace <- fluorojip::read_fluorpen_xlsx(input$curve_file$datapath, sheet = 1)
    if (is.null(rv$summary_df)) {
      rv$summary_df <- fluorojip::fluorpen_to_ojip(rv$raw_trace)
      rv$results <- fluorojip::calc_fluorojip(rv$summary_df)
    }
    rv$source_label <- paste("Loaded FluorPen workbook for OJIP curves:", input$curve_file$name)
  })

  observeEvent(rv$results, {
    groups <- parameter_groups()
    core <- core_parameter_defaults(groups)

    basic_selected <- isolate(input$param_basic)
    calculated_selected <- isolate(input$param_calculated)

    if (is.null(basic_selected) && is.null(calculated_selected)) {
      basic_selected <- intersect(core, groups$basic)
      calculated_selected <- intersect(core, groups$calculated)
    } else {
      basic_selected <- intersect(basic_selected, groups$basic)
      calculated_selected <- intersect(calculated_selected, groups$calculated)
    }

    updateCheckboxGroupInput(session, "param_basic", choices = groups$basic, selected = basic_selected)
    updateCheckboxGroupInput(session, "param_calculated", choices = groups$calculated, selected = calculated_selected)
  })

  observeEvent(input$select_core_params, {
    req(rv$results)
    groups <- parameter_groups()
    core <- core_parameter_defaults(groups)
    updateCheckboxGroupInput(session, "param_basic", choices = groups$basic, selected = intersect(core, groups$basic))
    updateCheckboxGroupInput(session, "param_calculated", choices = groups$calculated, selected = intersect(core, groups$calculated))
  })

  observeEvent(input$select_all_params, {
    req(rv$results)
    groups <- parameter_groups()
    updateCheckboxGroupInput(session, "param_basic", choices = groups$basic, selected = groups$basic)
    updateCheckboxGroupInput(session, "param_calculated", choices = groups$calculated, selected = groups$calculated)
  })

  observeEvent(input$clear_all_params, {
    req(rv$results)
    groups <- parameter_groups()
    updateCheckboxGroupInput(session, "param_basic", choices = groups$basic, selected = character(0))
    updateCheckboxGroupInput(session, "param_calculated", choices = groups$calculated, selected = character(0))
  })

  observeEvent(current_params(), {
    params <- current_params()

    updateSelectizeInput(
      session,
      "norm_params",
      choices = params,
      selected = head(params, min(5, length(params))),
      server = TRUE
    )

    heat_defaults <- intersect(c("DIo_RC", "ABS_RC", "PI_abs", "ETo_RC", "Fv_Fm"), params)
    if (length(heat_defaults) == 0) heat_defaults <- head(params, min(5, length(params)))
    updateSelectizeInput(
      session,
      "heatmap_params",
      choices = params,
      selected = heat_defaults,
      server = TRUE
    )

    plot3d_defaults <- intersect(c("Fv_Fm", "PI_abs", "area"), params)
    if (length(plot3d_defaults) < 3) plot3d_defaults <- head(params, min(3, length(params)))
    updateSelectizeInput(
      session,
      "plot3d_params",
      choices = params,
      selected = plot3d_defaults,
      server = TRUE
    )

    export_defaults <- intersect(c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"), params)
    if (length(export_defaults) == 0) export_defaults <- head(params, min(6, length(params)))
    updateSelectizeInput(
      session,
      "export_params",
      choices = params,
      selected = export_defaults,
      server = TRUE
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$use_biolyzer_example, {
    path <- fluorojip::fluorojip_example_biolyzer_file()
    rv$biolyzer <- compare_biolyzer_workbook(path)
  })

  observeEvent(input$run_biolyzer_validation, {
    path <- if (!is.null(input$biolyzer_file)) input$biolyzer_file$datapath else fluorojip::fluorojip_example_biolyzer_file()
    rv$biolyzer <- compare_biolyzer_workbook(path)
  })

  observeEvent(input$use_project_fluorpen, {
    fluorpen_dir <- find_project_fluorpen_dir()
    req(!is.null(fluorpen_dir))
    paths <- list.files(fluorpen_dir, pattern = "\\.xlsx$", full.names = TRUE)
    paths <- paths[!grepl("(^~\\$)|(^\\.~lock\\.)", basename(paths))]
    req(length(paths) > 0)
    rv$fluorpen_validation <- compare_fluorpen_files(paths)
  })

  observeEvent(input$run_fluorpen_validation, {
    req(input$fluorpen_validation_files)
    rv$fluorpen_validation <- compare_fluorpen_files(input$fluorpen_validation_files$datapath)
  })

  output$data_status <- renderText({
    rv$source_label
  })

  output$summary_preview <- renderTable({
    req(rv$summary_df)
    head(rv$summary_df, 10)
  }, rownames = TRUE)

  output$results_preview <- renderTable({
    req(rv$results)
    head(rv$results, 10)
  }, rownames = TRUE)

  output$param_selection_status <- renderText({
    req(rv$results)
    paste(length(selected_parameters()), "parameters selected for plots and exports")
  })

  output$param_basic_ui <- renderUI({
    req(rv$results)
    groups <- parameter_groups()
    selected <- isolate(input$param_basic)
    if (is.null(selected)) {
      selected <- intersect(core_parameter_defaults(groups), groups$basic)
    } else {
      selected <- intersect(selected, groups$basic)
    }
    checkboxGroupInput("param_basic", "Choose basic parameters", choices = groups$basic, selected = selected)
  })

  output$param_calculated_ui <- renderUI({
    req(rv$results)
    groups <- parameter_groups()
    selected <- isolate(input$param_calculated)
    if (is.null(selected)) {
      selected <- intersect(core_parameter_defaults(groups), groups$calculated)
    } else {
      selected <- intersect(selected, groups$calculated)
    }
    checkboxGroupInput("param_calculated", "Choose calculated parameters", choices = groups$calculated, selected = selected)
  })

  output$curve_sample_ui <- renderUI({
    if (is.null(rv$raw_trace)) {
      return(tags$em("No raw trace loaded yet."))
    }
    selectInput("curve_samples", "Samples", choices = rv$raw_trace$sample_id, selected = rv$raw_trace$sample_id[1:min(5, length(rv$raw_trace$sample_id))], multiple = TRUE)
  })

  output$curve_status <- renderText({
    if (is.null(rv$raw_trace)) {
      return("No raw trace available. Upload a FluorPen workbook here or load one in Data & Calculation.")
    }
    paste("Raw trace loaded with", length(rv$raw_trace$sample_id), "samples.")
  })

  output$curve_plot <- renderPlot({
    validate(need(!is.null(rv$raw_trace), "No raw trace available. Upload a FluorPen workbook to visualize OJIP curves."))
    req(input$curve_samples)
    sel <- match(input$curve_samples, rv$raw_trace$sample_id)
    sel <- sel[!is.na(sel)]
    req(length(sel) > 0)
    xvals <- rv$raw_trace$times_ms
    keep <- is.finite(xvals) & xvals > 0
    req(any(keep))
    xplot <- xvals[keep]
    y <- t(rv$raw_trace$mat[sel, keep, drop = FALSE])
    matplot(
      xplot,
      y,
      type = if (isTRUE(input$curve_points)) "b" else "l",
      lty = 1,
      pch = 16,
      log = if (isTRUE(input$curve_log_x)) "x" else "",
      xlab = if (isTRUE(input$curve_log_x)) "Time (ms, log10 scale)" else "Time (ms)",
      ylab = "Fluorescence",
      main = "OJIP Curves"
    )
    legend("bottomright", legend = rv$raw_trace$sample_id[sel], col = seq_along(sel), lty = 1, pch = if (isTRUE(input$curve_points)) 16 else NA, cex = 0.8)
  })

  output$normalized_plot <- renderPlot({
    req(rv$results, input$norm_params)
    tab <- fluorojip::normalized_jiptable(
      rv$results,
      params = input$norm_params,
      normalize = input$norm_method,
      control_level = if (nzchar(input$control_level)) input$control_level else NULL,
      output = "wide"
    )
    mat <- as.matrix(tab[, input$norm_params, drop = FALSE])
    matplot(seq_len(nrow(mat)), mat, type = "b", lty = 1, pch = 16,
            xaxt = "n", xlab = "Samples", ylab = "Normalized value",
            main = "Normalized Parameters by Sample")
    axis(1, at = seq_len(nrow(mat)), labels = tab$sample_id, las = 2, cex.axis = 0.8)
    legend("topright", legend = input$norm_params, col = seq_along(input$norm_params), lty = 1, pch = 16, cex = 0.8)
  })

  output$heatmap_plot <- renderPlot({
    req(rv$results, input$heatmap_params)
    fluorojip::plot_heatmap_fluorojip(
      rv$results,
      params = input$heatmap_params,
      scale = input$heatmap_scale,
      main = "fluorojip heatmap"
    )
  })

  output$plot3d_output <- renderPlot({
    req(rv$results, input$plot3d_params)
    validate(need(length(input$plot3d_params) == 3, "Please select exactly 3 parameters."))
    fluorojip::plot_3d_fluorojip(
      rv$results,
      params = input$plot3d_params,
      normalize = isTRUE(input$plot3d_normalize)
    )
  })

  output$biolyzer_metrics <- renderTable({
    req(rv$biolyzer)
    rv$biolyzer$metrics
  }, digits = 6)

  output$biolyzer_preview <- renderTable({
    req(rv$biolyzer)
    head(rv$biolyzer$comp, 12)
  }, digits = 6)

  output$fluorpen_metrics <- renderTable({
    req(rv$fluorpen_validation)
    rv$fluorpen_validation$metrics
  }, digits = 6)

  output$fluorpen_preview <- renderTable({
    req(rv$fluorpen_validation)
    head(rv$fluorpen_validation$comp, 12)
  }, digits = 6)

  output$export_preview <- renderTable({
    req(normalized_export())
    head(normalized_export(), 12)
  }, digits = 6)

  output$download_results <- downloadHandler(
    filename = function() "fluorojip_results.csv",
    content = function(file) {
      utils::write.csv(rv$results, file, row.names = FALSE)
    }
  )

  output$download_normalized <- downloadHandler(
    filename = function() "fluorojip_normalized.csv",
    content = function(file) {
      fluorojip::write_normalized_jiptable(normalized_export(), file)
    }
  )
}

shinyApp(ui, server)
