({
  page:Shiny.shinyapp.$inputValues.sidebar_tabs,
  plots:[...document.querySelectorAll('.js-plotly-plot')].filter(e=>e.offsetParent!==null&&!e.closest('details:not([open])')).map(p=>({id:p.id,font:p._fullLayout?.font?.color,note:p.previousElementSibling?.classList.contains('v2-chart-meta'),ready:!!p._fullLayout,after:!!p._v2AfterPlot})),
  tables:[...document.querySelectorAll('.dataTables_scroll')].filter(e=>e.offsetParent!==null).slice(0,4).map(e=>({id:e.closest('.shiny-bound-output')?.id,head:e.querySelector('.dataTables_scrollHead table')?.getBoundingClientRect().toJSON(),body:e.querySelector('.dataTables_scrollBody table')?.getBoundingClientRect().toJSON(),headMargin:e.querySelector('.dataTables_scrollHead table')&&getComputedStyle(e.querySelector('.dataTables_scrollHead table')).margin,bodyMargin:e.querySelector('.dataTables_scrollBody table')&&getComputedStyle(e.querySelector('.dataTables_scrollBody table')).margin})),
  busy:document.documentElement.className,
  errors:[...document.querySelectorAll('.shiny-output-error')].filter(e=>e.offsetParent!==null).map(e=>e.textContent)
})
