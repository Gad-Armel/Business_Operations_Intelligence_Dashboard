(async function () {
  const pause = ms => new Promise(r => setTimeout(r, ms));
  const errors = () => Array.from(document.querySelectorAll('.shiny-output-error')).filter(e => e.offsetParent !== null).map(e => e.textContent);
  document.getElementById('tab-exec_overview').click();
  await pause(1600);
  const overview = {kpis: Array.from(document.querySelectorAll('.aw-overview-kpis .aw-stat strong')).map(e=>e.innerText), errors: errors()};
  document.getElementById('tab-analysis_lab').click();
  await pause(1000);
  const results = [];
  for (const name of ['Profile','Hypothesis tests','Trends & forecast','Drivers','Scenarios','Financial metrics','Chart builder']) {
    const link = Array.from(document.querySelectorAll('#shiny-tab-analysis_lab .nav-tabs a')).find(e=>e.textContent === name);
    link.click(); await pause(1200);
    results.push({tab:name,errors:errors()});
  }
  document.dispatchEvent(new KeyboardEvent('keydown',{key:'k',ctrlKey:true,bubbles:true}));
  const commandOpen = document.getElementById('aw-command').open;
  document.getElementById('aw-command').close();
  const set = (id,value) => { const el=document.getElementById(id); if(el.selectize) el.selectize.setValue(value); else {el.value=value;el.dispatchEvent(new Event('change',{bubbles:true}));} };
  set('lab-predictors',['Cost']);
  const drivers=Array.from(document.querySelectorAll('#shiny-tab-analysis_lab .nav-tabs a')).find(e=>e.textContent==='Drivers');
  drivers.click(); await pause(2200);
  const driverErrors=errors();
  set('lab-test_method','welch');set('lab-group','Country'); await pause(500);
  const tests=Array.from(document.querySelectorAll('#shiny-tab-analysis_lab .nav-tabs a')).find(e=>e.textContent==='Hypothesis tests');
  tests.click(); await pause(1600);
  const testErrors=errors();
  return {overview,results,commandOpen,driverErrors,testErrors,overflow:document.body.scrollWidth>innerWidth};
})();
