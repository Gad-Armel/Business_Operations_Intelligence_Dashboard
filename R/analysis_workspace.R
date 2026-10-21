aw_card <- function(title, ..., class = "") {
  div(class = paste("aw-panel", class), div(class = "aw-panel-heading", tags$h3(title)),
      div(class = "aw-panel-body", ...))
}
aw_kpi <- function(label, value, detail = NULL) {
  div(class = "aw-stat", tags$span(label), tags$strong(value), tags$small(detail))
}
aw_workspace_ui <- function(id) {
  ns <- NS(id)
  div(class = "aw-workspace",
    div(class = "aw-page-heading",
      div(tags$span(class = "aw-eyebrow", "WORKSPACE / INTELLIGENCE"),
          tags$h1("Analysis Lab", tags$span(class = "aw-badge", "R engine")),
          tags$p("From source records to a decision. Explore, test and model in one place.")),
      div(class = "aw-heading-actions", actionButton(ns("refresh"), "Refresh files", icon = icon("sync-alt")),
          downloadButton(ns("export"), "Export CSV", class = "btn-primary"))),
    div(class = "aw-source-bar",
      div(class = "aw-source-main", selectizeInput(ns("dataset"), "Source table", choices = NULL)),
      uiOutput(ns("sheet_ui")),
      div(class = "aw-source-search", textInput(ns("search"), "Search all fields", placeholder = "Find a customer, order, country...")),
      div(class = "aw-source-state", uiOutput(ns("source_state")))),
    tags$details(class = "aw-import",
      tags$summary(icon("folder-open"), " Open another file / import options"),
      fluidRow(column(5, fileInput(ns("upload"), "Open a local table (session only, up to 100 MB)",
                                   accept = c(".csv", ".tsv", ".xlsx", ".xls", ".rds", ".json"))),
               column(3, selectInput(ns("delimiter"), "CSV separator", c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"))),
               column(4, actionButton(ns("use_catalog"), "Return to database files", icon = icon("database")),
                      tags$p(class = "aw-caption", "Tables in data/ are discovered automatically, including subfolders. Imports never overwrite source files.")))),
    div(class = "aw-filter-bar",
      selectizeInput(ns("facet_fields"), "Filter by any columns", choices = NULL, multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Add dates, regions, clients, numeric ranges...")),
      checkboxInput(ns("keep_missing"), "Keep missing filter values", FALSE),
      checkboxInput(ns("deduplicate"), "Remove duplicate rows", FALSE),
      actionButton(ns("reset"), "Reset", icon = icon("undo"))),
    uiOutput(ns("facets")),
    uiOutput(ns("scope")),
    div(class = "aw-analysis-controls aw-shared-controls", uiOutput(ns("measure_ui")),
      selectInput(ns("outliers"), "Outlier policy (analysis)", c("Keep all" = "keep", "Exclude IQR outliers" = "iqr", "Exclude |z| > 3" = "z")),
      sliderInput(ns("confidence"), "Confidence level", .80, .99, .95, .01)),
    tabsetPanel(id = ns("section"), type = "tabs",
      tabPanel("Data Explorer", value = "explorer",
        div(class = "aw-explorer-toolbar",
          selectizeInput(ns("columns"), "Visible columns", choices = NULL, multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "All columns")),
          tags$span(class = "aw-caption", "Column search  |  multi-sort with Shift  |  select a row to inspect")),
        aw_card("Source records", DTOutput(ns("table"))),
        uiOutput(ns("record")),
        tags$details(class = "aw-details", tags$summary("Schema & data quality"), DTOutput(ns("quality")))),
      tabPanel("Profile", value = "profile",
        uiOutput(ns("insights")),
        fluidRow(column(7, aw_card("Distribution", plotlyOutput(ns("distribution"), height = "310px"))),
                 column(5, aw_card("Descriptive statistics", DTOutput(ns("statistics"))))),
        fluidRow(column(6, aw_card("Box plot", plotlyOutput(ns("box"), height = "250px"))),
                 column(6, aw_card("Normal Q-Q diagnostic", plotlyOutput(ns("qq"), height = "250px"))))),
      tabPanel("Hypothesis tests", value = "tests",
        div(class = "aw-analysis-controls",
          selectInput(ns("test_method"), "Statistical test", c("One-sample t-test" = "one", "Welch two-sample t-test" = "welch",
            "Mann-Whitney rank test" = "rank", "Welch one-way ANOVA" = "anova", "Kruskal-Wallis" = "kruskal", "Shapiro-Wilk normality" = "normality")),
          numericInput(ns("benchmark"), "Null mean / benchmark", 0),
          selectInput(ns("alternative"), "Alternative (t / rank tests)", c("Two-sided" = "two.sided", "First > second / target" = "greater", "First < second / target" = "less")),
          numericInput(ns("alpha"), "Significance alpha", .05, min = .001, max = .2, step = .01)),
        uiOutput(ns("test_groups_ui")),
        uiOutput(ns("test_result")),
        aw_card("Group comparison", DTOutput(ns("group_summary"))),
        div(class = "aw-note", "Independent-sample tests assume independent observations; repeated invoice or order lines may violate this assumption. A small p-value does not establish business importance or causation.")),
      tabPanel("Trends & forecast", value = "trends",
        div(class = "aw-analysis-controls", uiOutput(ns("date_ui")),
          selectInput(ns("granularity"), "Period", c("Week" = "week", "Month" = "month", "Quarter" = "quarter", "Year" = "year"), selected = "month"),
          selectInput(ns("aggregation"), "Aggregate", c("Total" = "sum", "Average" = "mean")),
          sliderInput(ns("horizon"), "Forecast periods", 1, 12, 6, 1)),
        uiOutput(ns("trend_note")),
        aw_card("Observed performance & baseline forecast", plotlyOutput(ns("trend"), height = "370px")),
        aw_card("Period detail", DTOutput(ns("periods")))),
      tabPanel("Drivers", value = "drivers",
        div(class = "aw-analysis-controls", uiOutput(ns("predictors_ui")),
          selectInput(ns("correlation_method"), "Correlation", c("Pearson" = "pearson", "Spearman" = "spearman"))),
        fluidRow(column(6, aw_card("Correlation matrix", plotlyOutput(ns("correlation"), height = "350px"))),
                 column(6, aw_card("Linear model coefficients", uiOutput(ns("regression_note")), DTOutput(ns("coefficients"))))),
        fluidRow(column(6,aw_card("Residuals vs fitted",plotlyOutput(ns("residuals"),height="260px"))),
                 column(6,aw_card("Residual Q-Q",plotlyOutput(ns("residual_qq"),height="260px")))),
        div(class = "aw-note", "Outcome = analysis measure above. Predictors are associational, not causal. Avoid revenue, cost and profit identities in the same model; their arithmetic relationship can create a misleading fit.")),
      tabPanel("Scenarios", value = "scenarios",
        div(class = "aw-analysis-controls",
          sliderInput(ns("shift"), "Expected mean change (%)", -80, 100, 0, 1),
          sliderInput(ns("volatility"), "Historical dispersion x", .1, 3, 1, .1),
          selectInput(ns("iterations"), "Simulations", c(1000, 5000, 10000), selected = 5000),
          numericInput(ns("seed"), "Reproducible seed", 42, min = 1, max = 2147483646)),
        uiOutput(ns("scenario_summary")),
        aw_card("Empirical scenario distribution", plotlyOutput(ns("simulation"), height = "310px")),
        aw_card("Base / best / worst assumptions", DTOutput(ns("scenario_cases"))),
        div(class = "aw-note", "Resamples individual observations, centered on your assumed mean. This is a sensitivity model, not a cash-flow forecast or portfolio VaR. Lower-tail thresholds retain the units of the selected metric. No source values are changed.")),
      tabPanel("Financial metrics", value = "metrics",
        uiOutput(ns("financial_metrics")),
        aw_card("Investment sensitivity",
          div(class="aw-analysis-controls",
            numericInput(ns("investment"),"Initial investment",1000000,min=0),
            numericInput(ns("annual_cash"),"Annual free cash flow",250000),
            numericInput(ns("discount"),"Discount rate (%)",10,min=0,max=100),
            sliderInput(ns("investment_years"),"Annual periods",1,20,5,1)),
          uiOutput(ns("investment_result")),
          tags$p(class="aw-caption","User-entered constant annual cash flow, received at each year end. No terminal value, growth, taxes or financing assumptions are added. These inputs are independent of source records."))),
      tabPanel("Chart builder", value = "canvas",
        div(class = "aw-analysis-controls", uiOutput(ns("canvas_fields")),
          selectInput(ns("chart_type"), "Chart", c("Bar" = "bar", "Line" = "line", "Scatter" = "scatter", "Box" = "box")),
          selectInput(ns("transform"), "Y transform", c("Raw values" = "raw", "Z-score" = "z", "Log (positive only)" = "log"))),
        aw_card("Custom view", plotlyOutput(ns("canvas"), height = "390px")),
        aw_card("Calculated measure",
          textInput(ns("formula"), "Arithmetic expression", placeholder = "(Revenue - Cost) / Revenue * 100"),
          actionButton(ns("apply_formula"), "Create calculated measure", icon = icon("calculator")),
          textOutput(ns("formula_status")),
          tags$p(class = "aw-caption", "Use exact column names; wrap names containing spaces in backticks. Allowed: + - * / ^, abs, sqrt, log, log10, round, pmin, pmax. The new Calculated_Metric is available across the Lab."))),
      tabPanel("Probability studio", value = "probability",
        aw_card("Fitted models & probability calculator",
          tags$p("Open the existing probability studio for Normal, Student t, F, log-normal, exponential, gamma, Poisson and empirical models, quantiles and tail probabilities."),
          actionButton(ns("open_probability"), "Open probability studio", icon = icon("arrow-right")),
          tags$p(class = "aw-caption", "The probability studio uses its own dataset controls and the header year / month filters."))))
  )
}

aw_plot <- function(p, x = NULL, y = NULL) {
  if (exists("AURELIS_THEME_V2") && isTRUE(AURELIS_THEME_V2)) return(aurelis_plotly_theme(p, x, y))
  plotly::config(plotly::layout(p, paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    font = list(family = "Segoe UI, sans-serif", color = "#a8b3c2", size = 11),
    margin = list(l = 65, r = 24, b = 55, t = 20),
    xaxis = list(title = x, gridcolor = "#232a33", zerolinecolor = "#303945"),
    yaxis = list(title = y, gridcolor = "#232a33", zerolinecolor = "#303945"),
    colorway = c("#7dd3b0", "#8cb4ec", "#d8bb81", "#b4a2e8")), displaylogo = FALSE,
    modeBarButtonsToRemove = c("select2d", "lasso2d"))
}

aw_workspace_server <- function(id, data_dir, version, navigate, can_access = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0L)
    imported <- reactiveVal(NULL)
    formula <- reactiveVal("")
    formula_status <- reactiveVal("")
    catalog <- reactive({ version(); refresh(); aw_catalog(data_dir) })
    observe({
      files <- catalog()
      selected <- isolate(input$dataset)
      if (is.null(selected) || !selected %in% files$id) selected <- if ("Aurelis_Revenue.csv" %in% files$id) "Aurelis_Revenue.csv" else files$id[1]
      updateSelectizeInput(session, "dataset", choices = setNames(files$id, files$name), selected = selected, server = TRUE)
    })
    observeEvent(input$refresh, { refresh(refresh()+1L) })
    observeEvent(input$upload, {
      req(can_access())
      file <- input$upload
      if (file$size > 100*1024^2) { showNotification("Select a file below 100 MB.", type = "error"); return() }
      imported(list(path = file$datapath, name = file$name, ext = tools::file_ext(file$name)))
    })
    observeEvent(input$use_catalog, { imported(NULL) })
    observeEvent(input$dataset, { imported(NULL) }, ignoreInit = TRUE)
    source <- reactive({
      req(can_access())
      upload <- imported()
      if (!is.null(upload)) return(upload)
      files <- catalog(); req(input$dataset)
      at <- match(input$dataset, files$id)
      validate(need(!is.na(at), "Select a source table from the catalog."))
      list(path = files$path[at], name = files$id[at], ext = tools::file_ext(files$path[at]))
    })
    sheets <- reactive({
      info <- source()
      if (!tolower(info$ext) %in% c("xlsx", "xls")) return(character())
      tryCatch(readxl::excel_sheets(info$path), error = function(e) character())
    })
    output$sheet_ui <- renderUI({
      choices <- sheets(); if (!length(choices)) return(NULL)
      selectInput(ns("sheet"), "Workbook sheet", choices)
    })
    raw <- reactive({
      info <- source(); version(); refresh()
      sheet <- input$sheet
      if (length(sheets()) && (is.null(sheet) || !sheet %in% sheets())) sheet <- sheets()[1]
      tryCatch(aw_read(info$path, sheet, info$ext, delimiter = input$delimiter %||% ","),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    # A different table / sheet resets expressions and selections.
    observeEvent(raw(), {
      df <- raw(); fields <- names(df)
      formula(""); formula_status("")
      updateSelectizeInput(session, "facet_fields", choices = fields, selected = character(), server = TRUE)
      updateSelectizeInput(session, "columns", choices = fields, selected = character(), server = TRUE)
      updateTextInput(session, "search", value = "")
    })
    output$source_state <- renderUI({
      info <- source()
      tagList(tags$span(class = "aw-status-dot"), tags$span(if (is.null(imported())) "Database file" else "Session import"),
        tags$small(paste("File modified", format(file.info(info$path)$mtime, "%d %b %Y  |  %H:%M"))),
        tags$small("Lab filters are independent of the header filters"))
    })
    output$facets <- renderUI({
      df <- raw(); fields <- intersect(input$facet_fields, names(df))
      if (!length(fields)) return(NULL)
      div(class = "aw-facets", lapply(fields, function(field) {
        idx <- match(field, names(df)); key <- paste0("facet_", idx); x <- df[[field]]
        if (inherits(x, "Date")) {
          v <- x[!is.na(x)]; if (!length(v)) return(tags$p(paste(field, "has no dates")))
          dateRangeInput(ns(key), aw_label(field), start = min(v), end = max(v), min = min(v), max = max(v))
        } else if (aw_numeric(x)) {
          v <- aw_finite(x); if (!length(v)) return(tags$p(paste(field, "has no finite values")))
          div(class = "aw-range", tags$label(aw_label(field)),
              numericInput(ns(paste0(key,"_min")), "Minimum", min(v)),
              numericInput(ns(paste0(key,"_max")), "Maximum", max(v)))
        } else {
          choices <- sort(unique(as.character(x[!is.na(x)])))
          if (length(choices) > 500) textInput(ns(key), paste(aw_label(field), "contains"), placeholder = "Any value")
          else selectizeInput(ns(key), aw_label(field), choices, multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "All values"))
        }
      }))
    })
    observeEvent(input$reset, {
      updateSelectizeInput(session, "facet_fields", selected = character())
      updateSelectizeInput(session, "columns", selected = character())
      updateTextInput(session, "search", value = "")
      updateCheckboxInput(session, "deduplicate", value = FALSE)
      updateCheckboxInput(session, "keep_missing", value = FALSE)
      updateSelectInput(session, "outliers", selected = "keep")
    })
    filtered <- reactive({
      df <- raw(); filters <- list()
      for (field in intersect(input$facet_fields, names(df))) {
        key <- paste0("facet_", match(field, names(df))); x <- df[[field]]
        if (aw_numeric(x)) {
          a <- input[[paste0(key,"_min")]]; b <- input[[paste0(key,"_max")]]
          if (length(a) && length(b)) filters[[field]] <- c(a,b)
        } else if (inherits(x, "Date")) filters[[field]] <- input[[key]]
        else {
          choice <- input[[key]]
          filters[[field]] <- if (length(unique(x[!is.na(x)])) > 500 && length(choice)) paste0("text:",choice) else choice
        }
      }
      aw_filter(df, filters, input$search %||% "", isTRUE(input$keep_missing), isTRUE(input$deduplicate))
    })
    data <- reactive({
      df <- filtered()
      if (nzchar(formula())) df$Calculated_Metric <- aw_formula(formula(), df)
      df
    })
    observeEvent(input$apply_formula, {
      tryCatch({
        aw_formula(input$formula, filtered())
        if (!nzchar(trimws(input$formula))) stop("Enter an expression first.")
        formula(input$formula); formula_status("Calculated_Metric created. Select it as a measure on Profile.")
      }, error = function(e) formula_status(conditionMessage(e)))
    })
    output$formula_status <- renderText(formula_status())
    output$scope <- renderUI({
      df <- data(); original <- raw()
      div(class = "aw-scope",
        tags$span(class = "aw-badge", paste(format(nrow(df), big.mark = ","), "matching rows")),
        if (exists("AURELIS_THEME_V2") && isTRUE(AURELIS_THEME_V2)) tags$span(class = "v2-quality-badge",
          title = "Populated cells / all cells after structured filters. Completeness does not imply accuracy or validity.",
          if (nrow(df) && ncol(df)) paste0(round(100 * sum(!is.na(df)) / (nrow(df) * ncol(df)), 1), "% populated cells") else "No rows to assess"),
        tags$span(paste("of", format(nrow(original), big.mark = ","), " | ", ncol(df), "fields")),
        tags$span(class = "aw-caption", "Structured filters apply to all Lab analyses. Table column searches apply to the table and CSV export."))
    })
    table_frame <- reactive({
      df <- data(); columns <- intersect(input$columns, names(df))
      if (length(columns)) df <- df[, columns, drop = FALSE]
      df
    })
    output$table <- renderDT({
      DT::datatable(table_frame(), filter = "top", rownames = FALSE, selection = "single",
        class = "compact stripe hover", escape = TRUE,
        options = list(pageLength = 15, lengthMenu = c(15,30,50,100), scrollX = TRUE, autoWidth = TRUE,
          search = list(regex = FALSE), language = list(search = "Search table:"), dom = "lftip"))
    }, server = TRUE)
    output$record <- renderUI({
      df <- data(); selected <- input$table_rows_selected
      if (length(selected) != 1 || selected < 1 || selected > nrow(df)) return(NULL)
      aw_card(paste("Record", selected), div(class = "aw-record", lapply(names(df), function(field) {
        div(tags$span(aw_label(field)), tags$strong(if (is.na(df[[field]][selected])) "Missing" else as.character(df[[field]][selected])))
      })))
    })
    export_data <- reactive({
      df <- table_frame(); rows <- input$table_rows_all
      if (!is.null(rows)) df <- df[intersect(rows, seq_len(nrow(df))), , drop = FALSE]
      df
    })
    output$export <- downloadHandler(
      filename = function() paste0("aurelis-filtered-", Sys.Date(), ".csv"),
      content = function(file) {
        req(can_access()); df <- export_data()
        for (name in names(df)) if (is.factor(df[[name]])) df[[name]]<-as.character(df[[name]])
        # CSVs opened in Excel must not interpret source text as formulas.
        for (name in names(df)) if (is.character(df[[name]])) {
          risky <- !is.na(df[[name]]) & grepl("^[=+@\\t\\r-]", df[[name]])
          df[[name]][risky] <- paste0("'", df[[name]][risky])
        }
        utils::write.csv(df, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      })
    output$quality <- renderDT(DT::datatable(aw_profile(data()), rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")))
    output$measure_ui <- renderUI({
      fields <- aw_measures(data()); selected <- isolate(input$measure)
      if (!length(selected) || !selected %in% fields) selected <- if ("Revenue" %in% fields) "Revenue" else fields[1]
      selectInput(ns("measure"), "Analysis measure", fields, selected = selected)
    })
    analysis <- reactive({
      df <- data(); req(input$measure)
      validate(need(input$measure %in% aw_measures(df), "Choose a numeric analysis measure. This table may contain reference data only."))
      aw_outliers(df, input$measure, input$outliers %||% "keep")
    })
    values <- reactive({
      x <- aw_finite(analysis()[[input$measure]])
      validate(need(length(x) > 0, "No finite observations match these filters.")); x
    })
    output$insights <- renderUI({
      df <- analysis(); x <- values(); profile <- aw_profile(data()); n <- nrow(data())
      removed <- n - nrow(df)
      div(class = "aw-insight-surface",
        div(class = "aw-insight-title", icon("bolt"), tags$strong("Analyst brief"), tags$span(class = "aw-badge", "Computed from your selection")),
        tags$p(paste0(aw_label(input$measure), " averages ", aw_number(mean(x)), " across ", format(length(x), big.mark=","),
          " finite observations. The middle 50% lies between ", aw_number(stats::quantile(x,.25)), " and ", aw_number(stats::quantile(x,.75)), ".")),
        div(class = "aw-insight-grid",
          aw_kpi("Missing cells", sum(profile$Missing), "Review Schema & data quality"),
          aw_kpi("Duplicate rows", sum(duplicated(filtered())), "Exact full-row matches"),
          aw_kpi("Excluded outliers", removed, "Applied to Lab analyses only")),
        tags$p(class = "aw-caption", "Suggested next step: compare a meaningful segment with Welch's test, then inspect time trends before changing a business assumption. These are rule-based explanations, not model-generated advice."))
    })
    output$statistics <- renderDT({
      result <- aw_describe(values(), input$confidence %||% .95)
      DT::formatRound(DT::datatable(result, rownames=FALSE, options=list(dom="t", pageLength=25, scrollY="310px")), "Value", 3)
    })
    output$distribution <- renderPlotly({
      aw_plot(plotly::plot_ly(x=values(), type="histogram", nbinsx=35,
        marker=list(color="#7dd3b0", line=list(color="#11171e",width=1))), aw_label(input$measure), "Frequency")
    })
    output$box <- renderPlotly(aw_plot(plotly::plot_ly(y=values(), type="box", name=input$measure,
      marker=list(color="#8cb4ec"), boxpoints="outliers"), y=aw_label(input$measure)))
    output$qq <- renderPlotly({
      x <- values(); validate(need(length(x)>2 && stats::sd(x)>0, "At least three non-constant observations are required."))
      q <- stats::qqnorm(x, plot.it=FALSE)
      aw_plot(plotly::plot_ly(x=q$x, y=q$y, type="scatter", mode="markers", marker=list(color="#d8bb81", size=4)),
        "Theoretical normal quantile", "Observed quantile")
    })
    output$test_groups_ui <- renderUI({
      df <- analysis(); fields <- names(df)[vapply(df, function(x) (is.character(x)||is.factor(x)) && length(unique(x))<=100, logical(1))]
      div(class="aw-analysis-controls", selectInput(ns("group"), "Compare by", c("Select field"="",fields),
        selected=isolate(input$group) %||% ""), uiOutput(ns("group_values_ui")))
    })
    output$group_values_ui <- renderUI({
      df <- analysis(); req(input$group %in% names(df))
      choices <- sort(unique(as.character(df[[input$group]][!is.na(df[[input$group]])])))
      selected <- intersect(isolate(input$groups), choices)
      if (!length(selected)) selected <- head(choices,2)
      selectizeInput(ns("groups"), "Groups (first selection defines direction)", choices, selected=selected, multiple=TRUE)
    })
    output$test_result <- renderUI({
      result <- tryCatch(aw_test(analysis(),input$measure,input$test_method %||% "one", input$group %||% "",input$groups,
        input$benchmark %||% 0,input$alpha %||% .05,input$alternative %||% "two.sided"),error=function(e)e)
      if (inherits(result,"error")) return(div(class="aw-note",conditionMessage(result)))
      t <- result$test
      div(class="aw-insight-surface", tags$span(class="aw-eyebrow",t$method), tags$h3(result$conclusion),
        div(class="aw-insight-grid",aw_kpi("Statistic",aw_number(unname(t$statistic),4)),
          aw_kpi("p-value",format.pval(t$p.value,digits=4,eps=.0001)),
          aw_kpi("Cohen's d",aw_number(result$effect,3),"Reported for t-tests only")),
        tags$p(if (!is.null(t$conf.int)) paste0(round(100*(1-result$alpha)),"% confidence interval: ",paste(aw_number(t$conf.int),collapse=" to "))),
        tags$p(class="aw-caption",paste("Degrees of freedom:",if (is.null(t$parameter)) "not applicable" else paste(aw_number(t$parameter),collapse=", "),
          " |  alpha =",result$alpha," |  No multiple-testing correction. Failure to reject is not proof of equality.")))
    })
    output$group_summary <- renderDT({
      df <- analysis(); req(input$group %in% names(df), input$groups)
      out <- do.call(rbind,lapply(input$groups,function(g) {
        x <- aw_finite(df[[input$measure]][!is.na(df[[input$group]]) & as.character(df[[input$group]])==g])
        data.frame(Group=g,N=length(x),Mean=if(length(x)) mean(x) else NA_real_,Median=if(length(x)) median(x) else NA_real_,SD=if(length(x)>1) sd(x) else NA_real_)
      }))
      DT::formatRound(DT::datatable(out,rownames=FALSE,options=list(dom="t")),c("Mean","Median","SD"),2)
    })
    output$date_ui <- renderUI(selectInput(ns("date"),"Date field",aw_dates(data())))
    series <- reactive({
      tryCatch(aw_series(analysis(),input$date,input$measure,input$granularity %||% "month",input$aggregation %||% "sum"),
        error=function(e) validate(need(FALSE,conditionMessage(e))))
    })
    output$trend_note <- renderUI({
      s <- series()
      div(class="aw-note", paste(nrow(s),"observed periods. Missing calendar periods are left as gaps; no zero values are invented. Forecast uses a linear baseline and prediction intervals; seasonality and autocorrelation are not modeled. Incomplete final periods can bias the fit."))
    })
    forecast <- reactive({
      s <- series()
      # Do not fit an equally spaced model to missing calendar periods.
      expected <- seq(min(s$period),max(s$period),by=switch(input$granularity %||% "month",week="week",month="month",quarter="3 months",year="year"))
      validate(need(length(expected)==nrow(s),"Forecast requires consecutive calendar periods. Adjust filters or granularity; observed data remains in Period detail."))
      tryCatch(aw_forecast(s, input$horizon %||% 6,input$confidence %||% .95),error=function(e)validate(need(FALSE,conditionMessage(e))))
    })
    output$trend <- renderPlotly({
      s <- series(); fit <- tryCatch(forecast(), error=function(e) NULL)
      p <- plotly::plot_ly()
      p <- plotly::add_lines(p,x=s$period,y=s$value,name="Observed",line=list(color="#7dd3b0",width=2))
      if(is.null(fit)) return(aw_plot(p,y=aw_label(input$measure)))
      h <- nrow(fit$prediction)
      by <- switch(input$granularity %||% "month",week="week",month="month",quarter="3 months",year="year")
      future <- seq(max(s$period),by=by,length.out=h+1)[-1]
      p <- plotly::add_ribbons(p,x=future,ymin=fit$prediction[,"lwr"],ymax=fit$prediction[,"upr"],name="Prediction interval",fillcolor="rgba(140,180,236,.12)",line=list(color="transparent"))
      p <- plotly::add_lines(p,x=future,y=fit$prediction[,"fit"],name="Linear baseline",line=list(color="#8cb4ec",dash="dot"))
      aw_plot(p,y=aw_label(input$measure))
    })
    output$periods <- renderDT(DT::datatable(series(),rownames=FALSE,options=list(dom="tip",pageLength=12)))
    output$predictors_ui <- renderUI({
      fields <- setdiff(aw_measures(data()),input$measure)
      selectizeInput(ns("predictors"),"Predictors (up to 8)",fields,multiple=TRUE,options=list(maxItems=8))
    })
    output$correlation <- renderPlotly({
      df <- analysis(); fields <- unique(c(input$measure,head(input$predictors,8)))
      fields <- intersect(fields,aw_measures(df)); validate(need(length(fields)>1,"Select at least one predictor."))
      x <- df[,fields,drop=FALSE]; x[] <- lapply(x,function(v) {v[!is.finite(v)]<-NA_real_;v})
      validate(need(nrow(x)>2,"At least three rows are required."))
      matrix <- suppressWarnings(stats::cor(x,use="pairwise.complete.obs",method=input$correlation_method %||% "pearson"))
      aw_plot(plotly::plot_ly(x=fields,y=fields,z=matrix,type="heatmap",zmin=-1,zmax=1,
        colors=c("#b87676","#151c25","#7dd3b0")))
    })
    regression <- reactive({
      df <- analysis(); fields <- unique(c(input$measure,head(input$predictors,8)))
      validate(need(length(fields)>1,"Select at least one predictor."))
      df <- df[,fields,drop=FALSE]; names(df) <- c("outcome",paste0("x",seq_len(ncol(df)-1)))
      complete <- Reduce(`&`,lapply(df,is.finite)); df <- df[complete,,drop=FALSE]
      validate(need(nrow(df)>ncol(df)+2,"Not enough complete observations for this model."))
      model <- stats::lm(outcome ~ .,df)
      list(model=model,summary=summary(model),fields=fields,n=nrow(df))
    })
    output$regression_note <- renderUI({
      fit <- regression(); s <- fit$summary
      div(class="aw-note",paste0("Complete rows: ",fit$n,"  |  R^2 ",aw_number(s$r.squared,3),"  |  Adjusted R^2 ",aw_number(s$adj.r.squared,3),
        if(anyNA(coef(fit$model))) "  |  Collinearity detected: model has aliased coefficients." else ""))
    })
    output$coefficients <- renderDT({
      fit <- regression(); coefficients <- as.data.frame(fit$summary$coefficients)
      terms <- rownames(coefficients)
      for(i in seq_along(fit$fields[-1])) terms[terms==paste0("x",i)]<-fit$fields[-1][i]
      coefficients <- cbind(Term=terms,coefficients)
      DT::formatSignif(DT::datatable(coefficients,rownames=FALSE,options=list(dom="t",scrollX=TRUE)),2:ncol(coefficients),4)
    })
    output$residuals <- renderPlotly({
      model<-regression()$model
      aw_plot(plotly::plot_ly(x=fitted(model),y=residuals(model),type="scatter",mode="markers",
        marker=list(color="#8cb4ec",size=4,opacity=.6)),"Fitted outcome","Residual")
    })
    output$residual_qq <- renderPlotly({
      residual<-residuals(regression()$model)
      validate(need(length(residual)>2&&sd(residual)>0,"Residuals are constant or too few for this diagnostic."))
      q<-stats::qqnorm(residual,plot.it=FALSE)
      aw_plot(plotly::plot_ly(x=q$x,y=q$y,type="scatter",mode="markers",marker=list(color="#d8bb81",size=4)),
        "Theoretical normal quantile","Residual quantile")
    })
    simulation <- reactive({
      req(input$seed,input$iterations)
      validate(need(is.finite(input$seed) && input$seed>=1 && input$seed<2147483647,"Enter a valid positive integer seed."))
      aw_simulate(values(),input$shift %||% 0,input$volatility %||% 1,as.integer(input$iterations),as.integer(input$seed),input$confidence %||% .95)
    })
    output$scenario_summary <- renderUI({
      s <- simulation()$summary
      div(class="aw-insight-grid",aw_kpi("Expected observation",aw_number(s$Mean)),
        aw_kpi("Lower-tail threshold",aw_number(s[["Lower tail threshold"]])),
        aw_kpi("Probability below zero",scales::percent(s[["Probability below zero"]],accuracy=.1)))
    })
    output$simulation <- renderPlotly(aw_plot(plotly::plot_ly(x=simulation()$draws,type="histogram",nbinsx=45,
      marker=list(color="#8cb4ec")),aw_label(input$measure),"Simulated observations"))
    output$scenario_cases <- renderDT({
      x <- values(); shift <- input$shift %||% 0
      cases <- data.frame(Scenario=c("Worst assumption","Base assumption","Best assumption"),
        `Mean change %`=c(shift-10,shift,shift+10),check.names=FALSE)
      cases$`Expected mean` <- mean(x)*(1+cases[[2]]/100)
      DT::formatRound(DT::datatable(cases,rownames=FALSE,options=list(dom="t")),3,2)
    })
    output$financial_metrics <- renderUI({
      df<-data(); metrics<-list()
      add<-function(label,value,definition) metrics[[length(metrics)+1L]]<<-aw_kpi(label,value,definition)
      if(all(c("Revenue","Gross_Profit","Cost") %in% aw_measures(df))) {
        revenue<-sum(df$Revenue,na.rm=TRUE);profit<-sum(df$Gross_Profit,na.rm=TRUE);cost<-sum(df$Cost,na.rm=TRUE)
        add("Recognized revenue",scales::dollar(revenue,accuracy=1),"Sum of Revenue")
        add("Weighted gross margin",if(revenue!=0)scales::percent(profit/revenue,accuracy=.1)else"Undefined","Total gross profit / total revenue")
        add("Cost / revenue",if(revenue!=0)scales::percent(cost/revenue,accuracy=.1)else"Undefined","Total Cost / total Revenue")
      }
      if(all(c("Invoice_Amount","Balance_Remaining") %in% aw_measures(df))) {
        invoices<-sum(df$Invoice_Amount,na.rm=TRUE);balance<-sum(df$Balance_Remaining,na.rm=TRUE)
        add("Outstanding receivables",scales::dollar(balance,accuracy=1),"Sum of Balance_Remaining")
        add("Outstanding / invoiced",if(invoices!=0)scales::percent(balance/invoices,accuracy=.1)else"Undefined","Selected balance / selected invoice amount")
        if("Days_Past_Due" %in% names(df)) add("Past-due exposure",scales::dollar(sum(df$Balance_Remaining[!is.na(df$Days_Past_Due)&df$Days_Past_Due>0],na.rm=TRUE),accuracy=1),"Balance where Days_Past_Due > 0 in source snapshot")
      }
      if(all(c("Quantity","Received_Qty","Backordered_Qty") %in% aw_measures(df))) {
        lines<-if("Line_Type" %in% names(df))df[is.na(df$Line_Type)|df$Line_Type!="Information",,drop=FALSE]else df
        ordered<-sum(lines$Quantity,na.rm=TRUE)
        add("Ordered units",aw_number(ordered,0),"Excludes Information lines when present")
        add("Quantity fulfillment",if(ordered>0)scales::percent(sum(lines$Received_Qty,na.rm=TRUE)/ordered,accuracy=.1)else"Undefined","Received quantity / ordered quantity")
        add("Backorder rate",if(ordered>0)scales::percent(sum(lines$Backordered_Qty,na.rm=TRUE)/ordered,accuracy=.1)else"Undefined","Backordered quantity / ordered quantity")
      }
      if(!length(metrics))return(div(class="aw-note","Select Revenue, Accounts Receivable or Inventory to calculate financial and operating ratios from their mapped fields."))
      tagList(div(class="aw-insight-grid",metrics),div(class="aw-note","Ratios use all structurally filtered records, before analysis outlier removal. Missing values are omitted from sums. Source snapshot aging is not recomputed against today's date. Liquidity, ROE and runway require balance-sheet or cash data that is not present here."))
    })
    output$investment_result <- renderUI({
      req(input$investment,input$annual_cash,input$discount,input$investment_years)
      rate<-input$discount/100;years<-input$investment_years;initial<-input$investment;cash<-input$annual_cash
      validate(need(all(is.finite(c(rate,years,initial,cash)))&&rate>=0&&years>=1&&years<=20&&initial>=0,"Enter valid investment assumptions."))
      npv<-sum(cash/(1+rate)^seq_len(years))-initial
      div(class="aw-insight-grid",aw_kpi("Net present value",scales::dollar(npv,accuracy=1)),
        aw_kpi("Undiscounted net cash",scales::dollar(years*cash-initial,accuracy=1)),
        aw_kpi("Simple payback",if(cash>0)paste(aw_number(initial/cash,1),"years")else"No positive cash flow","Does not discount cash flows"))
    })
    output$canvas_fields <- renderUI({
      df <- data()
      tagList(selectInput(ns("canvas_x"),"X / category",names(df)),
        selectInput(ns("canvas_y"),"Y measure",aw_measures(df),selected=if("Revenue" %in% names(df)) "Revenue" else aw_measures(df)[1]))
    })
    output$canvas <- renderPlotly({
      df <- data(); req(input$canvas_x %in% names(df),input$canvas_y %in% aw_measures(df))
      x <- df[[input$canvas_x]]; y <- df[[input$canvas_y]]; transform <- input$transform %||% "raw"
      if(transform=="z") {validate(need(sd(y,na.rm=TRUE)>0,"Cannot standardize a constant measure.")); y<-(y-mean(y,na.rm=TRUE))/sd(y,na.rm=TRUE)}
      if(transform=="log") { y[y<=0]<-NA_real_; y<-log(y) }
      good <- !is.na(x)&is.finite(y); x<-x[good]; y<-y[good]
      validate(need(length(y)>0,"No usable chart values."))
      type <- input$chart_type %||% "bar"
      if(type %in% c("bar","line")) {
        chart <- aggregate(y,list(x=x),mean); names(chart)<-c("x","y")
        if(type=="bar") chart<-head(chart[order(chart$y,decreasing=TRUE),],30)
        else chart<-chart[order(chart$x),]
        p<-if(type=="bar") plotly::plot_ly(chart,x=~x,y=~y,type="bar",marker=list(color="#7dd3b0")) else plotly::plot_ly(chart,x=~x,y=~y,type="scatter",mode="lines",line=list(color="#8cb4ec"))
      } else if(type=="box") {
        validate(need(length(unique(x))<=30,"Use a category with 30 or fewer values for box plots."))
        p<-plotly::plot_ly(x=as.character(x),y=y,type="box",marker=list(color="#7dd3b0"))
      } else {
        validate(need(length(y)<=30000,"Filter to 30,000 rows or fewer for a scatter plot."))
        p<-plotly::plot_ly(x=x,y=y,type="scatter",mode="markers",marker=list(color="#8cb4ec",size=5,opacity=.5))
      }
      aw_plot(p,aw_label(input$canvas_x),paste(aw_label(input$canvas_y),if(type %in% c("bar","line")) "(mean; bar shows top 30)" else ""))
    })
    observeEvent(input$open_probability,navigate("financial_statistics"))
    list(data=data,raw=raw,filtered=filtered,analysis=analysis,export=export_data)
  })
}

# Local null coalescing helper, deliberately separate from dplyr's vector coalesce.
`%||%` <- function(x,y) if(is.null(x)||length(x)==0) y else x

aw_overview_ui <- function() {
  if (exists("AURELIS_THEME_V2") && isTRUE(AURELIS_THEME_V2)) return(aurelis_overview_v2_ui())
  div(class="aw-workspace aw-overview",
    div(class="aw-page-heading",div(tags$span(class="aw-eyebrow","AURELIS / BUSINESS OPERATIONS"),
      tags$h1("Operating overview"),tags$p("Performance, exposure and the work that needs your attention.")),
      div(class="aw-heading-actions",tags$span(class="aw-badge","Synthetic demo data"),
          actionButton("aw_open_lab","Explore data",icon=icon("arrow-right"),class="btn-primary"))),
    div(class="aw-overview-kpis",
      aw_kpi("Revenue",textOutput("kpi_total_revenue"),"Recognized revenue  |  selected period"),
      aw_kpi("Gross profit",textOutput("kpi_total_profit"),"Revenue less cost"),
      aw_kpi("Outstanding receivables",textOutput("kpi_total_ar"),"Uncollected invoice balance"),
      aw_kpi("Purchase orders",textOutput("kpi_total_po"),"Distinct tracked orders")),
    fluidRow(column(8,aw_card("Revenue & gross profit",plotlyOutput("exec_revenue_trend",height="285px"))),
      column(4,aw_card("Decision brief",uiOutput("aw_executive_brief"),class="aw-brief-panel"))),
    aw_card("Customer performance",div(class="aw-table-caption","Revenue concentration, margin and exposure | Select a customer to open their profile"),DTOutput("aw_customer_pulse")),
    tags$details(class="aw-details",tags$summary("More performance views"),
      fluidRow(column(7,aw_card("Top customers",plotlyOutput("exec_top_clients",height="310px"))),
        column(5,aw_card("Profit by representative",plotlyOutput("exec_profit_donut",height="310px")))),
      aw_card("Revenue by country",plotlyOutput("exec_country_treemap",height="320px")),
      aw_card("Year-over-year comparison",plotlyOutput("exec_yoy_comparison",height="300px"),
        report_action_bar("report_exec","Build an executive report from the current global filters."))))
}
