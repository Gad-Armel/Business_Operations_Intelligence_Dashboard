# These surfaces use the existing dashboard's global year / month filters.
aw_overview_server <- function(input, output, session, revenue, receivables, navigate) {
  pulse <- reactive({
    rev <- revenue(); ar <- receivables()
    if (!nrow(rev)) return(data.frame())
    summary <- rev %>% group_by(Customer) %>% summarise(
      Revenue=sum(Revenue,na.rm=TRUE), Gross_Profit=sum(Gross_Profit,na.rm=TRUE),
      Invoices=dplyr::n(), .groups="drop")
    balances <- ar %>% group_by(Customer) %>% summarise(
      Outstanding=sum(Balance_Remaining,na.rm=TRUE), .groups="drop")
    summary <- left_join(summary,balances,by="Customer")
    summary$Outstanding[is.na(summary$Outstanding)]<-0
    summary$Margin<-ifelse(summary$Revenue!=0,100*summary$Gross_Profit/summary$Revenue,NA_real_)
    total<-sum(summary$Revenue)
    summary$Share<-if(total!=0) 100*summary$Revenue/total else NA_real_
    summary<-summary[order(summary$Revenue,decreasing=TRUE),]
    summary[,c("Customer","Revenue","Gross_Profit","Margin","Share","Invoices","Outstanding")]
  })
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
