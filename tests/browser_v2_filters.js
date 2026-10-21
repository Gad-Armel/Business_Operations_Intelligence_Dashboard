(async () => {
  const pause=ms=>new Promise(r=>setTimeout(r,ms));
  const wait=async(fn,label)=>{const start=Date.now();while(!fn()){if(Date.now()-start>7000)throw Error('Timeout: '+label);await pause(80);}};
  const set=(id,value)=>{const e=document.getElementById(id);if(e.selectize)e.selectize.setValue(value);else{e.value=value;e.dispatchEvent(new Event('change',{bubbles:true}));}};
  document.querySelector('.main-sidebar a[data-value="exec_overview"]').click();
  set('global_year','2026');set('global_month','1');await wait(()=>Shiny.shinyapp.$values.kpi_total_revenue!=='$1,263,138,910','period revenue');
  const chosen={revenue:Shiny.shinyapp.$values.kpi_total_revenue,context:document.getElementById('v2_kpi_revenue').innerText,scope:document.getElementById('v2_scope').innerText};
  document.getElementById('v2_clear_month').click();await wait(()=>Shiny.shinyapp.$inputValues.global_month==='all','clear month');await pause(100);
  const clearedMonth=Shiny.shinyapp.$inputValues.global_month;
  document.getElementById('v2_clear_year').click();await wait(()=>Shiny.shinyapp.$inputValues.global_year==='all','clear year');await wait(()=>Shiny.shinyapp.$values.kpi_total_revenue==='$1,263,138,910','restore revenue');
  return {chosen,clearedMonth,clearedYear:Shiny.shinyapp.$inputValues.global_year,restored:Shiny.shinyapp.$values.kpi_total_revenue,errors:[...document.querySelectorAll('.shiny-output-error')].filter(e=>e.offsetParent!==null).map(e=>e.textContent)};
})()
