({
  ready: document.readyState,
  active: document.querySelector('.tab-pane.active')?.id,
  inputPage: Shiny.shinyapp.$inputValues.sidebar_tabs,
  names: Object.keys(Shiny.shinyapp.$values || {}).filter(x => /kpi_total|v2_|aw_/.test(x)),
  values: Object.fromEntries(Object.entries(Shiny.shinyapp.$values || {}).filter(([k]) => /kpi_total|v2_scope/.test(k))),
  bindings: ['kpi_total_revenue','v2_scope','exec_revenue_trend','aw_executive_brief'].map(id => ({id,html:document.getElementById(id)?.outerHTML.slice(0,400),bound:!!jQuery(document.getElementById(id)).data('shiny-output-binding')})),
  htmlBusy: document.documentElement.className,
  body: document.body.className
})
