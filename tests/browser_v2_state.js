({
  title: document.title,
  v2: !!window.AurelisV2,
  classes: document.body.className,
  connected: !!window.Shiny?.shinyapp?.isConnected(),
  errors: [...document.querySelectorAll('.shiny-output-error')].filter(x => x.offsetParent !== null).map(x => x.textContent),
  kpis: [...document.querySelectorAll('.v2-kpi strong')].map(x => ({value: x.textContent, exact: x.querySelector('[title]')?.title})),
  scope: document.getElementById('v2_scope')?.innerText,
  plots: [...document.querySelectorAll('.js-plotly-plot')].filter(x => x.offsetParent !== null).map(x => ({id: x.id, width: x.clientWidth})),
  overflow: document.body.scrollWidth > innerWidth,
  viewport: innerWidth,
  dialogs: [...document.querySelectorAll('dialog')].map(x => x.id)
})
