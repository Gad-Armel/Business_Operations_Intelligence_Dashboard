(async () => {
  const pause=ms=>new Promise(r=>setTimeout(r,ms));
  const wait=async(fn,label,max=6000)=>{const start=Date.now();while(!fn()){if(Date.now()-start>max)throw Error('Timeout: '+label);await pause(80);}};
  const page=id=>document.querySelector('.main-sidebar a[data-value="'+id+'"]').click();
  page('exec_overview');await pause(350);
  const client=document.querySelector('.aw-brief-item [data-v2-client]');const clientName=client.dataset.v2Client;client.click();
  await wait(()=>Shiny.shinyapp.$inputValues.sidebar_tabs==='customer_performance','client navigation');
  const result={client:clientName,selectedClient:Shiny.shinyapp.$inputValues.cust_perf_client};
  page('product_intelligence');
  await wait(()=>document.querySelector('#pi_rank_table .dataTables_scrollBody tbody tr td')?.textContent.startsWith('AUR-'),'recommendations');
  await pause(200);
  const row=document.querySelector('#pi_rank_table .dataTables_scrollBody tbody tr');
  if(!row.classList.contains('selected')) {
    jQuery(row).trigger(jQuery.Event('mousedown',{which:1,button:0}));
    jQuery(row).trigger(jQuery.Event('mouseup',{which:1,button:0}));
    jQuery(row).trigger(jQuery.Event('click',{which:1,button:0}));
  }
  await wait(()=>Shiny.shinyapp.$inputValues.pi_rank_table_rows_selected?.length===1,'product selection');
  document.getElementById('pi_add_recommendation_to_quote').click();
  await wait(()=>Shiny.shinyapp.$inputValues.sidebar_tabs==='quotation_studio','quote navigation');
  await wait(()=>document.getElementById('quotation_kpi_total')?.textContent.trim(),'calculated quote total');
  await wait(()=>document.querySelector('#quotation_line_table .dataTables_scrollBody tbody tr td'),'quotation lines');
  await pause(250);
  result.quoteStep=document.querySelector('[data-quote-step][aria-current="step"]')?.dataset.quoteStep;
  result.quoteTotal=document.querySelector('.v2-quote-total')?.textContent;
  result.originalTotal=document.getElementById('quotation_kpi_total')?.textContent;
  result.sections=document.querySelectorAll('[data-quote-step]').length;
  result.lines=[...document.querySelectorAll('#quotation_line_table tbody tr')].length;
  result.errors=[...document.querySelectorAll('.shiny-output-error')].filter(e=>e.offsetParent!==null).map(e=>e.textContent);
  result.exports=['quotation_download_pdf','quotation_download_excel'].map(id=>({id,url:!!document.getElementById(id)?.href}));
  result.overflow=document.documentElement.scrollWidth>innerWidth;
  return result;
})()
