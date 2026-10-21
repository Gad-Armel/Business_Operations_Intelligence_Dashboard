(async () => {
  const pause=ms=>new Promise(r=>setTimeout(r,ms));
  const pages=['exec_overview','kpi_board','global_network','executive_observatory','sale_performance','customer_performance','client_statement','monthly_sales','buyer_activity','order_tracker','inventory_tab','buyer_tool','multi_user_workspace','quotation_studio','product_intelligence','contacts_tab','misc_tab','analysis_lab','financial_statistics'];
  const start=window.__v2ReviewCursor||0;
  const report=[];
  const set=(id,value)=>{const e=document.getElementById(id);if(e?.selectize)e.selectize.setValue(value);else if(e){e.value=value;e.dispatchEvent(new Event('change',{bubbles:true}));}};
  set('cust_perf_client','Cobalt Controls');set('stmt_client','Cobalt Controls');
  for(const page of pages.slice(start,start+4)) {
    document.querySelector('.main-sidebar a[data-value="'+page+'"]').click();
    await pause(1400);
    let waited=0;while(document.documentElement.classList.contains('shiny-busy')&&waited<1800){await pause(150);waited+=150;}
    const visible=e=>e.offsetParent!==null&&!e.closest('details:not([open])');
    const plots=[...document.querySelectorAll('.js-plotly-plot')].filter(visible);
    const overflow=document.documentElement.scrollWidth>innerWidth;
    report.push({page,width:innerWidth,overflow,errors:[...document.querySelectorAll('.shiny-output-error')].filter(visible).map(e=>e.textContent),plots:plots.map(p=>({id:p.id,theme:p._fullLayout?.font?.color,note:p.previousElementSibling?.classList.contains('v2-chart-meta')})),overflowElements:overflow?[...document.querySelectorAll('.content-wrapper *')].filter(e=>visible(e)&&e.getBoundingClientRect().right>innerWidth+2&&!e.closest('.dataTables_scrollBody,.dataTables_scrollHead,.selectize-dropdown')).slice(0,8).map(e=>({tag:e.tagName,id:e.id,classes:String(e.className).slice(0,90),right:e.getBoundingClientRect().right})):[]});
  }
  window.__v2ReviewCursor=start+4;
  return {next:window.__v2ReviewCursor,report};
})()
