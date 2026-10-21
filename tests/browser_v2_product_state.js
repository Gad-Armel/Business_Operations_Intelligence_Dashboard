({
  inputs:Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k])=>/pi_.*selected|sidebar_tabs|cust_perf_client/.test(k))),
  table:document.getElementById('pi_rank_table')?.innerText.slice(0,1500),
  rows:[...document.querySelectorAll('#pi_rank_table tbody tr')].slice(0,2).map(e=>({classes:e.className,text:e.innerText})),
  charts:[...document.querySelectorAll('.js-plotly-plot')].filter(e=>e.offsetParent!==null).map(p=>({id:p.id,font:p._fullLayout?.font?.color,note:p.previousElementSibling?.outerHTML.slice(0,180)})),
  active:Shiny.shinyapp.$inputValues.sidebar_tabs,
  scripts:[...document.scripts].map(s=>s.src).filter(s=>s.includes('datatables')),
  handlers:Object.keys(jQuery._data(document.querySelector('#pi_rank_table .dataTables_scrollBody table'),'events')||{}),
  bodyHandlers:Object.keys(jQuery._data(document.querySelector('#pi_rank_table .dataTables_scrollBody tbody'),'events')||{})
})
