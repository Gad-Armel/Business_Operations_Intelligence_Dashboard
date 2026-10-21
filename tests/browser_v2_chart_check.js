(async () => {
  const pause=ms=>new Promise(r=>setTimeout(r,ms));
  const wait=async(fn,label)=>{const start=Date.now();while(!fn()){if(Date.now()-start>5000)throw Error('Timeout: '+label);await pause(80);}};
  await wait(()=>Shiny.shinyapp.isConnected()&&Shiny.shinyapp.$values.kpi_total_revenue,'initial server response');await pause(1000);
  const report=[];
  for(const page of ['kpi_board','sale_performance','order_tracker']) {
    document.querySelector('.main-sidebar a[data-value="'+page+'"]').click();
    await wait(()=>[...document.querySelectorAll('#shiny-tab-'+page+' .js-plotly-plot')].some(p=>p.offsetParent!==null&&p._fullLayout),'chart render '+page);await pause(600);
    report.push({page,plots:[...document.querySelectorAll('.js-plotly-plot')].filter(e=>e.offsetParent!==null&&!e.closest('details:not([open])')).map(p=>({id:p.id,font:p._fullLayout?.font?.color,note:p.previousElementSibling?.classList.contains('v2-chart-meta'),after:!!p._v2AfterPlot})),errors:[...document.querySelectorAll('.shiny-output-error')].filter(e=>e.offsetParent!==null).map(e=>e.textContent)});
  }
  return report;
})()
