# Presentation-only v2 layer. Set AURELIS_THEME_V2=false before launching to opt out.
AURELIS_THEME_V2 <- tolower(Sys.getenv("AURELIS_THEME_V2", "true")) %in% c("1", "true", "yes", "on")

aurelis_theme_assets <- function() {
  if (!isTRUE(AURELIS_THEME_V2)) return(NULL)
  shiny::tagList(
    shiny::tags$meta(name = "aurelis-theme-v2", content = "enabled"),
    shiny::tags$link(rel = "stylesheet", href = "aurelis-theme.css"),
    shiny::tags$link(rel = "stylesheet", href = "aurelis-luxe.css?v=about-guide-1"),
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    shiny::tags$script(src = "aurelis-ui.js?v=about-guide-1"),
    shiny::tags$script(src = "aurelis-help.js?v=about-guide-1")
  )
}

# Shared chart defaults; the client applies theme tokens to legacy charts as well.
# Semantic series/trace colors and event sources are deliberately preserved.
aurelis_plotly_theme <- function(p, x = NULL, y = NULL) {
  plotly::config(plotly::layout(p,
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
    margin = list(l = 65, r = 24, b = 55, t = 20),
    xaxis = list(title = x, zeroline = FALSE),
    yaxis = list(title = y, zeroline = FALSE),
    transition = list(duration = 0)), displaylogo = FALSE,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud"), responsive = TRUE)
}

kpi_card <- function(key, label, output_id, unit, definition, page) {
  shiny::tags$article(class = "aw-stat v2-kpi", `data-destination` = page,
    shiny::tags$div(class = "v2-kpi-top", shiny::tags$span(label),
      shiny::tags$button(type = "button", class = "v2-kpi-open", `data-v2-page` = page,
        `aria-label` = paste("Open details for", label), shiny::icon("arrow-up-right-from-square"))),
    shiny::tags$strong(shiny::textOutput(output_id)),
    shiny::tags$small(class = "v2-kpi-unit", unit),
    shiny::uiOutput(paste0("v2_kpi_", key)),
    shiny::tags$small(class = "v2-kpi-definition", definition)
  )
}

aurelis_overview_v2_ui <- function() {
  shiny::div(class = "aw-workspace aw-overview v2-overview",
    shiny::div(class = "aw-page-heading",
      shiny::div(class = "aw-overview-brand",
        shiny::tags$img(class = "aw-overview-mark", src = aurelis_logo_sidebar_source, alt = ""),
        shiny::div(shiny::tags$span(class = "aw-eyebrow", "AURELIS / EXECUTIVE COMMAND"),
          shiny::tags$h1("Operating overview"),
          shiny::tags$p("The performance. The exposure. Your next move."))),
      shiny::div(class = "aw-heading-actions",
        shiny::tags$span(class = "aw-badge", "Synthetic demo data"),
        shiny::actionButton("aw_open_lab", "Explore data", icon = shiny::icon("arrow-right"), class = "btn-primary"))),
    shiny::div(class = "aw-overview-kpis",
      kpi_card("revenue", "Recognized revenue", "kpi_total_revenue", "USD / selected invoice period", "Sum of recognized revenue", "sale_performance"),
      kpi_card("profit", "Gross profit", "kpi_total_profit", "USD / selected invoice period", "Revenue less cost", "sale_performance"),
      kpi_card("ar", "Outstanding receivables", "kpi_total_ar", "USD / selected invoice cohort", "Remaining invoice balance at source snapshot", "ar_tab"),
      kpi_card("po", "Purchase orders", "kpi_total_po", "Orders / selected order period", "Tracked order rows, as in the existing KPI", "monthly_sales")),
    shiny::fluidRow(
      shiny::column(8, aw_card("Revenue & gross profit",
        shiny::tags$p(class = "v2-chart-subtitle", "SUM / USD / MONTHLY / GLOBAL PERIOD"),
        plotly::plotlyOutput("exec_revenue_trend", height = "320px"))),
      shiny::column(4, aw_card("Decision brief", shiny::uiOutput("aw_executive_brief"), class = "aw-brief-panel"))),
    shiny::fluidRow(shiny::column(12, aw_card("Client portfolio map",
      shiny::tags$p(class = "v2-chart-subtitle", "MARGIN / REVENUE SHARE / OUTSTANDING BALANCE · SELECT A CLIENT TO OPEN ITS PROFILE"),
      plotly::plotlyOutput("aw_client_portfolio", height = "380px")))),
    aw_card("Customer performance",
      shiny::div(class = "aw-table-caption", "Revenue concentration, margin and exposure. Select a client to open their profile."),
      DT::DTOutput("aw_customer_pulse"),
      shiny::uiOutput("v2_customer_cards")),
    shiny::tags$details(class = "aw-details v2-more",
      shiny::tags$summary("More performance views", shiny::tags$span("Clients, representatives, geography and annual comparison")),
      shiny::fluidRow(
        shiny::column(6, aw_card("Top customers", plotly::plotlyOutput("exec_top_clients", height = "310px"))),
        shiny::column(6, aw_card("Profit by representative", plotly::plotlyOutput("exec_profit_donut", height = "310px")))),
      shiny::fluidRow(
        shiny::column(6, aw_card("Revenue by country", plotly::plotlyOutput("exec_country_treemap", height = "320px"))),
        shiny::column(6, aw_card("Year-over-year comparison", plotly::plotlyOutput("exec_yoy_comparison", height = "300px")))),
      report_action_bar("report_exec", "Build an executive report from the current global filters.")))
}

# A comparison is only defined for an explicitly selected calendar period.
# All-years totals must never be compared to a fabricated preceding period.
aurelis_comparison_period <- function(year = "all", month = "all") {
  yr <- suppressWarnings(as.integer(year)); mo <- suppressWarnings(as.integer(month))
  if (length(yr) != 1 || !is.finite(yr)) return(NULL)
  if (identical(as.character(month), "all")) {
    return(list(year = yr - 1L, month = NULL, label = paste("vs", yr - 1L)))
  }
  if (length(mo) != 1 || !is.finite(mo) || !mo %in% 1:12) return(NULL)
  previous <- as.Date(sprintf("%04d-%02d-01", yr, mo)) - 1
  list(year = as.integer(format(previous, "%Y")), month = as.integer(format(previous, "%m")),
       label = paste("vs", format(previous, "%b %Y")))
}

aurelis_delta <- function(current, previous) {
  if (length(previous) != 1 || !is.finite(previous) || previous == 0 ||
      length(current) != 1 || !is.finite(current)) return(NA_real_)
  (current - previous) / abs(previous)
}

aurelis_sparkline <- function(values, label) {
  values <- as.numeric(values); values <- values[is.finite(values)]
  if (length(values) < 2) return(shiny::tags$span(class = "v2-spark-empty", "Insufficient periods for a trend"))
  span <- diff(range(values)); scaled <- if (span == 0) rep(16, length(values)) else 29 - (values - min(values)) / span * 26
  points <- paste(sprintf("%.2f,%.2f", seq(1, 199, length.out = length(values)), scaled), collapse = " ")
  shiny::tags$svg(class = "v2-spark", viewBox = "0 0 200 32", role = "img", `aria-label` = label,
    shiny::tags$title(label), shiny::tags$polyline(points = points, fill = "none", `vector-effect` = "non-scaling-stroke"))
}

aurelis_kpi_context <- function(values, delta = NA_real_, comparison = "Choose a year for a comparison", spark_label = "Monthly totals") {
  change <- if (is.finite(delta)) paste0(if (delta > 0) "+" else "", scales::percent(delta, accuracy = .1)) else "No comparison"
  # Direction is descriptive: increasing revenue/order count is not inherently good.
  shiny::tagList(shiny::div(class = "v2-kpi-change",
    shiny::tags$span(class = "v2-delta", if (is.finite(delta)) shiny::icon(if (delta >= 0) "arrow-trend-up" else "arrow-trend-down"), change),
    shiny::tags$span(comparison)), aurelis_sparkline(values, spark_label))
}

# Preserve category keys and source IDs while replacing an overcrowded ring.
aurelis_category_chart <- function(df, label, value, source, unit = "USD") {
  shiny::validate(shiny::need(nrow(df) > 0, "No positive category totals match the period."))
  labels <- as.character(df[[label]]); values <- as.numeric(df[[value]])
  if (nrow(df) > 5) {
    order <- order(values, decreasing = FALSE)
    p <- plotly::plot_ly(source = source, x = values[order], y = factor(labels[order], levels = labels[order]),
      key = labels[order], customdata = labels[order], type = "bar", orientation = "h",
      hovertemplate = paste0("<b>%{y}</b><br>", unit, " %{x:,.2f}<br>Click to explore<extra></extra>"))
    p <- plotly::layout(p, margin = list(l = 135, r = 25, t = 15, b = 45),
      xaxis = list(title = paste("Gross profit", unit), tickformat = ",.0f"), yaxis = list(title = "", automargin = TRUE))
  } else {
    p <- plotly::plot_ly(source = source, labels = labels, values = values, key = labels, customdata = labels,
      type = "pie", hole = .58, textinfo = "label+percent", textposition = "outside",
      hovertemplate = paste0("<b>%{label}</b><br>", unit, " %{value:,.2f}<br>%{percent}<extra></extra>"))
  }
  plotly::config(plotly::layout(p, showlegend = FALSE, paper_bgcolor = "transparent", plot_bgcolor = "transparent"), displaylogo = FALSE, responsive = TRUE)
}

# Server-side commands use an explicit allowlist and existing navigation checks.
# No data or privileges are inferred from browser-stored preferences.
aurelis_theme_server <- function(input, output, session, revenue, receivables, orders,
                                 inventory, raw_revenue, raw_orders, version,
                                 navigate, allowed, lab = NULL, updated_at = function() Sys.time(), warehouse = NULL,
                                 role = function() { value <- input$multi_user_role; if (is.null(value)) "Analyst" else value }) {
  if (!isTRUE(AURELIS_THEME_V2)) return(invisible(NULL))
  selected_period <- reactive({
    req(input$global_year, input$global_month)
    aurelis_comparison_period(input$global_year, input$global_month)
  })
  monthly_values <- function(df, date_field, value_field = NULL) {
    if (!nrow(df) || !date_field %in% names(df)) return(numeric())
    dates <- as.Date(df[[date_field]]); good <- !is.na(dates)
    if (!any(good)) return(numeric())
    period <- format(dates[good], "%Y-%m")
    values <- if (is.null(value_field)) rep(1, sum(good)) else df[[value_field]][good]
    totals <- tapply(values, period, sum, na.rm = TRUE)
    as.numeric(totals[order(names(totals))])
  }
  comparison_frame <- function(df, period) {
    if (is.null(period) || !all(c("Year", "Month") %in% names(df))) return(df[FALSE, , drop = FALSE])
    keep <- !is.na(df$Year) & df$Year == period$year
    if (!is.null(period$month)) keep <- keep & !is.na(df$Month) & df$Month == period$month
    df[keep, , drop = FALSE]
  }
  for (key in c("revenue", "profit", "po")) local({
    metric <- key
    output[[paste0("v2_kpi_", metric)]] <- renderUI({
      period <- selected_period()
      df <- if (metric == "po") orders() else revenue()
      raw <- if (metric == "po") raw_orders() else raw_revenue()
      previous <- comparison_frame(raw, period)
      field <- if (metric == "revenue") "Revenue" else "Gross_Profit"
      current_value <- if (metric == "po") nrow(df) else sum(df[[field]], na.rm = TRUE)
      previous_value <- if (!nrow(previous)) NA_real_ else if (metric == "po") nrow(previous) else sum(previous[[field]], na.rm = TRUE)
      label <- if (is.null(period)) "Choose a year to compare" else if (!nrow(previous)) paste(period$label, "(no data)") else period$label
      aurelis_kpi_context(monthly_values(df, "Date", if (metric == "po") NULL else field),
        aurelis_delta(current_value, previous_value), label,
        if (metric == "po") "Observed monthly order counts" else paste("Observed monthly sum of", field))
    })
  })
  output$v2_kpi_ar <- renderUI({
    df <- receivables()
    aurelis_kpi_context(monthly_values(df, "Txn_Date", "Balance_Remaining"),
      comparison = "Snapshot; no historical balance delta", spark_label = "Outstanding balance by invoice month, not a balance history")
  })

  output$v2_scope <- renderUI({
    version()
    tab <- input$sidebar_tabs
    if (is.null(tab)) tab <- "exec_overview"
    is_lab <- tab %in% c("analysis_lab", "presentation_studio")
    year <- input$global_year; month <- input$global_month
    year_label <- if (is.null(year) || year == "all") "All years" else year
    month_label <- if (is.null(month) || month == "all") "All months" else month.name[as.integer(month)]
    shiny::div(class = "v2-scope-bar", role = "region", `aria-label` = "Active data scope",
      shiny::div(class = "v2-scope-main",
        shiny::tags$span(class = "v2-scope-label", shiny::icon(if (is_lab) "flask" else "layer-group"), if (is_lab) "LAB SCOPE" else "GLOBAL PERIOD"),
        if (is_lab) shiny::tags$span(class = "v2-scope-note", "Source + filters below; header period does not apply") else shiny::tagList(
          shiny::actionButton("v2_clear_year", paste(year_label, if (!identical(year_label, "All years")) "\u00d7"), class = "v2-scope-chip", title = "Clear year filter"),
          shiny::actionButton("v2_clear_month", paste(month_label, if (!identical(month_label, "All months")) "\u00d7"), class = "v2-scope-chip", title = "Clear month filter"),
          shiny::tags$span(class = "v2-scope-note", "Page-specific filters may also apply"))),
      shiny::div(class = "v2-toolbar",
        shiny::tags$button(type = "button", class = "v2-tool v2-mobile-filter-trigger", `data-v2-action` = "filters", "Filters"),
        shiny::tags$span(class = "v2-freshness", `data-updated-at` = as.numeric(updated_at()) * 1000,
          shiny::tags$span(class = "v2-sync-dot"), "Source loaded"),
        shiny::tags$button(type = "button", class = "v2-tool", `data-v2-action` = "density", title = "Toggle comfortable / compact density", "Compact"),
        shiny::tags$button(type = "button", class = "v2-tool", `data-v2-action` = "focus", `aria-pressed` = "false", title = "Presentation mode (Escape to exit)", "Focus"),
        shiny::tags$button(type = "button", class = "v2-tool", `data-v2-action` = "alerts", `aria-expanded` = "false", "Priorities")))
  })
  observeEvent(input$v2_clear_year, updateSelectInput(session, "global_year", selected = "all"), ignoreInit = TRUE)
  observeEvent(input$v2_clear_month, updateSelectInput(session, "global_month", selected = "all"), ignoreInit = TRUE)

  output$v2_priorities <- renderUI({
    if (!allowed("ar_tab") && !allowed("order_tracker") && !allowed("inventory_tab")) return(shiny::tags$p("No operational priorities available for this profile."))
    ar <- receivables(); inv <- inventory()
    due <- if (all(c("Days_Past_Due", "Balance_Remaining") %in% names(ar))) sum(ar$Balance_Remaining[!is.na(ar$Days_Past_Due) & ar$Days_Past_Due > 0], na.rm = TRUE) else NA_real_
    late <- if ("Order_Status" %in% names(inv)) sum(as.character(inv$Order_Status) == "Overdue", na.rm = TRUE) else NA_integer_
    stock <- if (!is.null(warehouse) && allowed("inventory_tab")) warehouse() else data.frame()
    low <- if (all(c("Available", "Reorder_Point") %in% names(stock))) sum(stock$Available < stock$Reorder_Point, na.rm = TRUE) else NA_integer_
    shiny::tagList(shiny::tags$div(class = "v2-priority-title", "Operational priorities", shiny::tags$small("Receivables and delivery: global period. Stock: current warehouse filters. Independent of Lab filters.")),
      if (allowed("ar_tab")) shiny::tags$button(type = "button", class = "v2-priority", `data-v2-page` = "ar_tab",
        shiny::tags$span("Past-due invoice balance"), shiny::tags$strong(if (is.finite(due)) scales::dollar(due, accuracy = 1) else "Unavailable"), shiny::tags$small("Review receivables \u2192")),
      if (allowed("order_tracker")) shiny::tags$button(type = "button", class = "v2-priority", `data-v2-page` = "order_tracker",
        shiny::tags$span("Overdue delivery lines"), shiny::tags$strong(if (is.finite(late)) format(late, big.mark = ",") else "Unavailable"), shiny::tags$small("Review delivery schedule \u2192")),
      if (allowed("inventory_tab")) shiny::tags$button(type = "button", class = "v2-priority", `data-v2-page` = "inventory_tab",
        shiny::tags$span("Stock lines below reorder point"), shiny::tags$strong(if (is.finite(low)) format(low, big.mark = ",") else "Unavailable"), shiny::tags$small("Available quantity < reorder point \u2192")))
  })
  # The entity index stays server-side; only a bounded, role-checked result set
  # is sent to the browser in response to an explicit search.
  command_index <- reactive({
    version()
    rev <- raw_revenue(); po <- raw_orders()
    rows <- list()
    add <- function(values, type, page) {
      values <- sort(unique(as.character(values[!is.na(values)])))
      values <- values[nzchar(values)]
      if (length(values) && allowed(page)) rows[[length(rows) + 1L]] <<- data.frame(label = values, type = type, page = page, stringsAsFactors = FALSE)
    }
    add(rev$Customer, "Client", "customer_performance")
    add(rev$Invoice_Ref, "Invoice", "client_statement")
    add(po$PO_Number, "Order", "monthly_sales")
    inv <- inventory()
    add(inv$Supplier, "Supplier", "buyer_tool")
    if (!length(rows)) return(data.frame(label = character(), type = character(), page = character()))
    do.call(rbind, rows)
  })
  query <- shiny::debounce(reactive(input$v2_entity_query), 180)
  observeEvent(query(), {
    value <- query(); term <- if (is.list(value)) value$term else ""
    if (is.null(term) || nchar(term) < 2) return()
    term <- substr(as.character(term), 1, 120)
    index <- command_index()
    matches <- index[grepl(tolower(term), tolower(index$label), fixed = TRUE), , drop = FALSE]
    matches <- head(matches, 12)
    session$sendCustomMessage("aurelis-search-results", list(query = term, items = lapply(seq_len(nrow(matches)), function(i) as.list(matches[i, ]))))
  }, ignoreInit = TRUE)
  observeEvent(input$v2_command, {
    command <- input$v2_command
    req(is.list(command), command$page)
    page <- as.character(command$page)[1]
    valid_pages <- c("exec_overview", "kpi_board", "analysis_lab", "presentation_studio", "global_network", "executive_observatory",
      "sale_performance", "customer_performance", "client_statement", "monthly_sales", "buyer_activity",
      "order_tracker", "inventory_tab", "buyer_tool", "ar_tab", "multi_user_workspace", "quotation_studio", "product_intelligence", "contacts_tab", "misc_tab", "financial_statistics")
    if (!page %in% valid_pages || !allowed(page)) return()
    if (!is.null(command$type) && !is.null(command$label)) {
      index <- command_index()
      hit <- index[index$type == command$type & index$label == command$label & index$page == page, , drop = FALSE]
      if (!nrow(hit)) return()
      label <- as.character(hit$label[1])
      if (command$type == "Client") updateSelectizeInput(session, "cust_perf_client", selected = label)
      if (command$type == "Supplier") updateSelectizeInput(session, "buyer_search_supplier", selected = label)
      if (command$type == "Invoice") {
        rev <- raw_revenue(); customer <- rev$Customer[match(label, rev$Invoice_Ref)]
        if (length(customer) && !is.na(customer)) updateSelectizeInput(session, "stmt_client", selected = customer)
        session$sendCustomMessage("aurelis-table-search", list(id = "stmt_detail_table", value = label))
      }
      if (command$type == "Order") session$sendCustomMessage("aurelis-table-search", list(id = "sales_po_detail_table", value = label))
    }
    navigate(page)
  }, ignoreInit = TRUE)
  observe({
    pages <- c("exec_overview", "kpi_board", "analysis_lab", "presentation_studio", "global_network", "executive_observatory",
      "sale_performance", "customer_performance", "client_statement", "monthly_sales", "buyer_activity",
      "order_tracker", "inventory_tab", "buyer_tool", "ar_tab", "multi_user_workspace", "quotation_studio", "product_intelligence", "contacts_tab", "misc_tab", "financial_statistics")
    available <- pages[vapply(pages, allowed, logical(1))]
    current_role <- as.character(role())[1]
    homes <- c(CEO = "exec_overview", Manager = "kpi_board", Analyst = "analysis_lab", Buyer = "quotation_studio")
    home <- unname(homes[current_role])
    if (is.na(home) || !home %in% available) home <- "exec_overview"
    session$sendCustomMessage("aurelis-capabilities", list(pages = available, role = current_role, home = home))
  })
  invisible(NULL)
}
