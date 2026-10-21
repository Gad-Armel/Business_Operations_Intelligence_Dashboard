(async () => {
  const files = ['aurelis-ui.js','aurelis-theme.css','workspace.js','InterVariable.woff2'];
  const resources = [];
  for (const file of files) {
    const response = await fetch(file);
    const text = file.endsWith('.js') ? await response.text() : '';
    let syntax = '';
    if (text) { try { new Function(text); syntax = 'OK'; } catch (error) { syntax = error.stack; } }
    resources.push({file,status:response.status,length:text.length,syntax});
  }
  const binding = await (await fetch('bs4Dash-2.3.5/bs4Dash.min.js')).text();
  return {resources,binding:binding.slice(0,4500),inputs:[...document.querySelectorAll('.shiny-bound-input')].map(x=>({tag:x.tagName,id:x.id,classes:x.className})).slice(-10),scripts:[...document.scripts].map(x=>x.src).filter(x=>x.includes('aurelis')||x.includes('workspace')),meta:!!document.querySelector('meta[name="aurelis-theme-v2"]')};
})()
