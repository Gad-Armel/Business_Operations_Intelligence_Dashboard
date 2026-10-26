# These surfaces use the existing dashboard's global year / month filters.
aurelis_portfolio_summary <- function(revenue, receivables) {
  revenue <- as.data.frame(revenue)
  receivables <- as.data.frame(receivables)
  required_revenue <- c("Customer", "Revenue", "Gross_Profit")
  required_receivables <- c("Customer", "Balance_Remaining")
  if (!all(required_revenue %in% names(revenue))) {
    stop("Revenue data must contain Customer, Revenue and Gross_Profit.", call. = FALSE)
  }
  if (!all(required_receivables %in% names(receivables))) {
    stop("Receivables data must contain Customer and Balance_Remaining.", call. = FALSE)
  }
  if (!is.numeric(revenue$Revenue) || !is.numeric(revenue$Gross_Profit) ||
      !is.numeric(receivables$Balance_Remaining)) {
    stop("Portfolio revenue, profit and balance fields must be numeric.", call. = FALSE)
  }

  revenue$Customer <- trimws(as.character(revenue$Customer))
  revenue <- revenue[!is.na(revenue$Customer) & nzchar(revenue$Customer), , drop = FALSE]
  summary <- revenue %>%
    group_by(Customer) %>%
    summarise(
      Revenue = sum(Revenue, na.rm = TRUE),
      Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
      Invoices = dplyr::n(),
      .groups = "drop"
    )
  receivables$Customer <- trimws(as.character(receivables$Customer))
  receivables <- receivables[!is.na(receivables$Customer) & nzchar(receivables$Customer), , drop = FALSE]
  balances <- receivables %>%
    group_by(Customer) %>%
    summarise(Outstanding = sum(Balance_Remaining, na.rm = TRUE), .groups = "drop")
  summary <- left_join(summary, balances, by = "Customer")
  summary$Outstanding[is.na(summary$Outstanding)] <- 0
  summary <- summary[
    is.finite(summary$Revenue) & is.finite(summary$Gross_Profit) &
      is.finite(summary$Outstanding),
    ,
    drop = FALSE
  ]
  summary$Margin <- ifelse(summary$Revenue != 0, 100 * summary$Gross_Profit / summary$Revenue, NA_real_)
  total <- sum(summary$Revenue)
  summary$Share <- if (is.finite(total) && total != 0) 100 * summary$Revenue / total else NA_real_
  summary <- summary[order(summary$Revenue, decreasing = TRUE), , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

aurelis_portfolio_chart <- function(summary, source = "aw_client_portfolio") {
  required <- c("Customer", "Revenue", "Gross_Profit", "Outstanding", "Margin", "Share")
  if (!all(required %in% names(summary))) {
    stop("Portfolio chart data is missing required customer metrics.", call. = FALSE)
  }
  summary <- summary[order(summary$Revenue, decreasing = TRUE), , drop = FALSE]
  summary <- summary[
    is.finite(summary$Margin) & is.finite(summary$Share) &
      is.finite(summary$Revenue) & is.finite(summary$Gross_Profit) &
      is.finite(summary$Outstanding),
    ,
    drop = FALSE
  ]
  shiny::validate(shiny::need(nrow(summary) > 0, "No customers have finite revenue and margin values in this period."))

  summary$Tier <- ifelse(
    seq_len(nrow(summary)) <= 5,
    "Top five by revenue",
    "Other customers"
  )
  portfolio_revenue <- sum(summary$Revenue)
  weighted_margin <- if (is.finite(portfolio_revenue) && portfolio_revenue != 0) {
    100 * sum(summary$Gross_Profit) / portfolio_revenue
  } else {
    NA_real_
  }
  median_share <- stats::median(summary$Share)
  colors <- c("Top five by revenue" = "#D8B875", "Other customers" = "#79D8BB")
  max_balance <- max(abs(summary$Outstanding), na.rm = TRUE)
  size_ref <- if (is.finite(max_balance) && max_balance > 0) 2 * max_balance / 30^2 else 1

  chart <- plotly::plot_ly(source = source)
  for (tier in names(colors)) {
    points <- summary[summary$Tier == tier, , drop = FALSE]
    if (!nrow(points)) next
    points$Hover <- paste0(
      "<b>", htmltools::htmlEscape(points$Customer), "</b>",
      "<br>", points$Tier,
      "<br>Revenue: ", scales::dollar(points$Revenue, accuracy = 1),
      "<br>Gross margin: ", scales::percent(points$Margin / 100, accuracy = 0.1),
      "<br>Portfolio share: ", scales::percent(points$Share / 100, accuracy = 0.1),
      "<br>Outstanding balance: ", scales::dollar(points$Outstanding, accuracy = 1),
      "<br>Invoice rows: ", format(points$Invoices, big.mark = ",")
    )
    chart <- plotly::add_markers(
      chart,
      data = points,
      x = ~Margin,
      y = ~Share,
      key = ~Customer,
      customdata = ~Customer,
      text = ~Hover,
      name = tier,
      marker = list(
        color = unname(colors[[tier]]),
        size = ~abs(Outstanding),
        sizemode = "area",
        sizeref = size_ref,
        sizemin = 7,
        line = list(color = "#F4EFE4", width = 1)
      ),
      hovertemplate = "%{text}<extra></extra>"
    )
  }

  shapes <- list()
  if (is.finite(weighted_margin)) {
    shapes[[length(shapes) + 1L]] <- list(
      type = "line", x0 = weighted_margin, x1 = weighted_margin,
      y0 = min(summary$Share), y1 = max(summary$Share),
      line = list(color = "rgba(216,184,117,.68)", dash = "dot", width = 1)
    )
  }
  if (nrow(summary) > 1) {
    shapes[[length(shapes) + 1L]] <- list(
      type = "line", x0 = min(summary$Margin), x1 = max(summary$Margin),
      y0 = median_share, y1 = median_share,
      line = list(color = "rgba(121,216,187,.52)", dash = "dot", width = 1)
    )
  }

  plotly::config(
    plotly::layout(
      chart,
      xaxis = list(title = "Gross margin (%)", ticksuffix = "%", zeroline = FALSE, gridcolor = "rgba(177,187,201,.12)"),
      yaxis = list(title = "Share of portfolio revenue (%)", ticksuffix = "%", zeroline = FALSE, gridcolor = "rgba(177,187,201,.12)"),
      legend = list(orientation = "h", x = 0, y = 1.12),
      shapes = shapes,
      margin = list(l = 72, r = 24, t = 32, b = 62),
      hovermode = "closest",
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12)
    ),
    displaylogo = FALSE,
    displayModeBar = "hover",
    responsive = TRUE
  ) |>
    plotly::event_register("plotly_click")
}

aw_overview_server <- function(input, output, session, revenue, receivables, navigate) {
  pulse <- reactive({
    aurelis_portfolio_summary(revenue(), receivables())
  })
  output$aw_client_portfolio <- plotly::renderPlotly({
    shiny::validate(shiny::need(nrow(pulse()) > 0, "No revenue records match the current header filters."))
    aurelis_portfolio_chart(pulse())
  })
  observeEvent(plotly::event_data("plotly_click", source = "aw_client_portfolio", priority = "event"), {
    selected <- plotly::event_data("plotly_click", source = "aw_client_portfolio", priority = "event")
    if (is.null(selected) || !nrow(selected)) return()
    customer <- if ("key" %in% names(selected)) {
      as.character(selected$key[[1]])
    } else if ("customdata" %in% names(selected)) {
      as.character(selected$customdata[[1]])
    } else {
      showNotification("This chart selection did not include a customer identifier.", type = "error")
      return()
    }
    if (is.na(customer) || !nzchar(customer)) {
      showNotification("This chart selection did not include a customer identifier.", type = "error")
      return()
    }
    updateSelectizeInput(session, "cust_perf_client", selected = customer)
    navigate("customer_performance")
  }, ignoreInit = TRUE)
  output$aw_customer_pulse <- DT::renderDT({
    df<-pulse()
    validate(need(nrow(df)>0,"No revenue records match the current header filters."))
    names(df)<-c("Customer","Revenue","Gross profit","Margin %","Revenue share %","Invoice rows","Outstanding")
    widget<-DT::datatable(df,rownames=FALSE,selection="single",escape=TRUE,
      class="compact hover",options=list(pageLength=8,lengthChange=FALSE,scrollX=TRUE,dom="ftip",order=list(list(1,"desc"))))
    widget<-DT::formatCurrency(widget,c("Revenue","Gross profit","Outstanding"),currency="$",digits=0)
    DT::formatRound(widget,c("Margin %","Revenue share %"),1)
  },server=TRUE)
  output$aw_executive_brief <- renderUI({
    df<-pulse(); ar<-receivables()
    if(!nrow(df)) return(div(class="aw-note","Change the header filters to inspect an active period."))
    revenue<-sum(df$Revenue); margin<-if(revenue!=0) sum(df$Gross_Profit)/revenue else NA_real_
    top_share<-if(revenue!=0) sum(head(df$Revenue,5))/revenue else NA_real_
    overdue<-if("Days_Past_Due" %in% names(ar)) sum(ar$Balance_Remaining[!is.na(ar$Days_Past_Due)&ar$Days_Past_Due>0],na.rm=TRUE) else NA_real_
    tagList(
      div(class="aw-brief-item",tags$span(class="aw-brief-label","01 / REVENUE QUALITY"),
        tags$strong(paste0(scales::percent(margin,accuracy=.1)," weighted gross margin")),
        tags$p("Calculated from total gross profit / revenue. Compare client margins before increasing volume."),
        if (exists("AURELIS_THEME_V2") && isTRUE(AURELIS_THEME_V2)) tags$button(type = "button", class = "v2-insight-action", `data-v2-page` = "sale_performance", "Review profitability \u2192")),
      div(class="aw-brief-item",tags$span(class="aw-brief-label","02 / CONCENTRATION"),
        tags$strong(paste0("Top five clients account for ",scales::percent(top_share,accuracy=.1))),
        tags$p(paste0("Largest client: ",df$Customer[1],". Inspect exposure alongside growth.")),
        if (exists("AURELIS_THEME_V2") && isTRUE(AURELIS_THEME_V2)) tags$button(type = "button", class = "v2-insight-action", `data-v2-client` = df$Customer[1], "Open largest client \u2192")),
      div(class="aw-brief-item",tags$span(class="aw-brief-label","03 / COLLECTION PRIORITY"),
        tags$strong(paste0(scales::dollar(overdue,accuracy=1)," past-due balance")),
        tags$p("Based on the aging fields in the selected source snapshot; not a live bank balance.")),
      tags$p(class="aw-caption","Computed briefing  |  synthetic data  |  follows header filters"),
      actionButton("aw_open_receivables","Review receivables",icon=icon("arrow-right"),class="btn-default btn-sm"))
  })
  # A small mobile counterpart keeps the same selection/navigation semantics.
  # Existing desktop DT, IDs and export behavior remain intact.
  output$v2_customer_cards <- renderUI({
    if (!exists("AURELIS_THEME_V2") || !isTRUE(AURELIS_THEME_V2)) return(NULL)
    df <- head(pulse(), 8)
    if (!nrow(df)) return(div(class = "v2-empty", tags$strong("No clients in this period"), tags$p("Clear the year or month filter to explore another period.")))
    div(class = "v2-client-cards", lapply(seq_len(nrow(df)), function(i) {
      tags$button(type = "button", class = "v2-client-card", `data-v2-client` = df$Customer[i],
        tags$strong(df$Customer[i]),
        tags$span(tags$span("Revenue"), tags$b(scales::dollar(df$Revenue[i], accuracy = 1))),
        tags$span(tags$span("Gross margin"), tags$b(if (is.finite(df$Margin[i])) paste0(round(df$Margin[i], 1), "%") else "Unavailable")),
        tags$span(tags$span("Outstanding"), tags$b(scales::dollar(df$Outstanding[i], accuracy = 1))),
        tags$small("Open client profile \u2192"))
    }), tags$p(class = "aw-caption", "Top eight clients by revenue. Use a larger screen for the full searchable table."))
  })
  observeEvent(input$aw_open_lab,navigate("analysis_lab"))
  observeEvent(input$aw_open_receivables,navigate("ar_tab"))
  observeEvent(input$aw_customer_pulse_rows_selected,{
    selected<-input$aw_customer_pulse_rows_selected; df<-pulse()
    if(length(selected)!=1||selected<1||selected>nrow(df))return()
    updateSelectizeInput(session,"cust_perf_client",selected=df$Customer[selected])
    navigate("customer_performance")
  })
}
