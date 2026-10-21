(async function () {
  const pause = ms => new Promise(r => setTimeout(r, ms));
  const wait = async (fn, label, max = 6000) => { const start = Date.now(); while (!fn()) { if (Date.now() - start > max) throw Error('Timeout: ' + label); await pause(80); } };
  const errors = () => [...document.querySelectorAll('.shiny-output-error')].filter(e => e.offsetParent !== null).map(e => e.textContent);
  await wait(() => Shiny.shinyapp.isConnected() && document.getElementById('kpi_total_revenue')?.textContent.includes('$'), 'overview');
  const report = {width: innerWidth, baseline: Shiny.shinyapp.$values.kpi_total_revenue, initialErrors: errors()};
  const all = [...document.querySelectorAll('[id]')].map(e => e.id);
  report.duplicateIds = all.filter((id,i)=>all.indexOf(id)!==i);
  report.quoteCardBindings = [...document.querySelectorAll('#shiny-tab-quotation_studio .card.bs4Dash')].filter(e=>e.id).every(e => e.parentElement.querySelector('script[data-for="' + CSS.escape(e.id) + '"]'));
  document.querySelector('[data-v2-action="density"]').click();
  report.density = document.body.dataset.density;
  document.querySelector('[data-v2-action="focus"]').click();
  report.focusOn = document.body.classList.contains('v2-focus');
  document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));
  report.focusOff = !document.body.classList.contains('v2-focus');
  document.querySelector('[data-v2-action="alerts"]').click();
  await pause(150);
  report.prioritiesOpen = !document.getElementById('v2-priorities-panel').hidden;
  document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));
  document.dispatchEvent(new KeyboardEvent('keydown',{key:'k',ctrlKey:true,bubbles:true}));
  const dialog = document.getElementById('aw-command');
  report.paletteOpen = dialog.open;
  const search = document.getElementById('aw-command-search'); search.value = 'Cobalt'; search.dispatchEvent(new Event('input',{bubbles:true}));
  await wait(() => search.dataset.resolved === 'Cobalt', 'entity search');
  report.entityResults = [...document.querySelectorAll('#aw-command-results button')].map(e=>e.textContent).filter(t=>t.includes('Cobalt'));
  dialog.close();
  if(innerWidth < 768) {
    document.querySelector('[data-v2-action="filters"]').click();
    report.mobileFiltersOpen = document.querySelector('.v2-filter-sheet').open;
    document.querySelector('.v2-filter-sheet').close();
    report.mobileCards = [...document.querySelectorAll('.v2-client-card')].filter(e=>e.offsetParent!==null).length;
    report.mobileNav = getComputedStyle(document.querySelector('.v2-mobile-nav')).display;
  }
  report.overflow = document.documentElement.scrollWidth > innerWidth;
  report.errors = errors();
  // Restore comfortable density for the screenshot series.
  if(document.body.dataset.density==='compact')document.querySelector('[data-v2-action="density"]').click();
  return report;
})();
