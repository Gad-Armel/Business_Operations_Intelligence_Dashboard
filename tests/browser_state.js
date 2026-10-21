JSON.stringify({connected:Shiny.shinyapp.isConnected(),busy:document.documentElement.classList.contains('shiny-busy'),
  inputs:Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k,v])=>['lab-predictors','lab-measure','lab-group','lab-groups','lab-test_method','sidebar_tabs'].includes(k))),
  selects:['lab-predictors','lab-group','lab-groups','lab-measure'].map(id=>{const e=document.getElementById(id);return {id,value:e?.selectize?.getValue(),exists:!!e}}),
  result:document.getElementById('lab-test_result')?.innerText,
  errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(e=>e.offsetParent!==null).map(e=>e.textContent)})
