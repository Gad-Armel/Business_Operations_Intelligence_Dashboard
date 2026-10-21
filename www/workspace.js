(function () {
  'use strict';
  function setup() {
    if (window.AurelisV2) { window.AurelisV2.setup(); return; }
    document.title = 'Aurelis | Operations Intelligence';
    if (document.getElementById('aw-command')) return;
    const dialog = document.createElement('dialog');
    dialog.id = 'aw-command';
    dialog.setAttribute('aria-label', 'Go to workspace');
    dialog.innerHTML = '<div class="aw-command-top"><input id="aw-command-search" aria-label="Search workspaces" placeholder="Go to a workspace…" autocomplete="off"><button type="button" aria-label="Close command palette">Esc</button></div><div id="aw-command-results" role="list"></div><div class="aw-command-hint">↑ ↓ navigate &nbsp; Enter open &nbsp; Esc close</div>';
    document.body.appendChild(dialog);
    const search = dialog.querySelector('input');
    const results = dialog.querySelector('#aw-command-results');
    let active = 0;
    function updateActive() {
      results.querySelectorAll('button').forEach((b, i) => { b.classList.toggle('aw-command-active', i === active); });
      results.querySelector('.aw-command-active')?.scrollIntoView({block: 'nearest'});
    }
    function populate() {
      results.replaceChildren(); active = 0;
      const needle = search.value.toLowerCase();
      document.querySelectorAll('.main-sidebar a[data-value]').forEach(link => {
        const label = link.textContent.trim().replace(/\s+/g, ' ');
        if (!label.toLowerCase().includes(needle)) return;
        const button = document.createElement('button');
        button.type = 'button'; button.textContent = label;
        button.addEventListener('click', () => { dialog.close(); link.click(); window.scrollTo({top:0}); });
        results.appendChild(button);
      });
      if (!results.children.length) results.textContent = 'No matching workspaces.';
      updateActive();
    }
    function open() { search.value = ''; populate(); dialog.showModal(); search.focus(); }
    dialog.querySelector('.aw-command-top button').addEventListener('click', () => dialog.close());
    dialog.addEventListener('click', e => { if (e.target === dialog) dialog.close(); });
    search.addEventListener('input', populate);
    search.addEventListener('keydown', e => {
      const buttons = results.querySelectorAll('button');
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault(); active = (active + (e.key === 'ArrowDown' ? 1 : -1) + buttons.length) % (buttons.length || 1); updateActive();
      } else if (e.key === 'Enter') { e.preventDefault(); buttons[active]?.click(); }
    });
    document.addEventListener('keydown', e => {
      if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'k') { e.preventDefault(); if (dialog.open) dialog.close(); else open(); }
    }, true);
    const nav = document.querySelector('.main-header .navbar-nav.ml-auto');
    let light = false;
    try { light = localStorage.getItem('aurelis-theme') === 'light'; } catch (_) {}
    function styleCharts() {
      if (!window.Plotly) return;
      const color = document.body.classList.contains('aw-light') ? '#526174' : '#a8b3c2';
      const grid = document.body.classList.contains('aw-light') ? '#e2e7ed' : '#29303a';
      document.querySelectorAll('.aw-workspace .js-plotly-plot').forEach(plot => {
        if (!plot._fullLayout || !plot.offsetWidth) return;
        Plotly.relayout(plot, {'font.color': color, 'legend.font.color':color, 'xaxis.tickfont.color':color, 'yaxis.tickfont.color':color, 'xaxis.title.font.color':color, 'yaxis.title.font.color':color, 'xaxis.gridcolor': grid, 'yaxis.gridcolor': grid, 'paper_bgcolor': 'transparent', 'plot_bgcolor': 'transparent'});
      });
    }
    document.body.classList.toggle('aw-light', light);
    if (nav) {
      const li = document.createElement('li'); li.className = 'nav-item aw-command-nav';
      const button = document.createElement('button'); button.type = 'button'; button.className = 'aw-command-trigger';
      button.textContent = 'Go to…'; button.setAttribute('aria-label', 'Search workspaces, Control K');
      const key = document.createElement('kbd'); key.textContent = 'Ctrl K'; button.appendChild(key);
      button.addEventListener('click', open); li.appendChild(button); nav.prepend(li);
      const themeLi = document.createElement('li'); themeLi.className = 'nav-item aw-command-nav';
      const themeButton = document.createElement('button'); themeButton.type = 'button'; themeButton.className = 'aw-theme-toggle';
      function labelTheme() { themeButton.textContent = document.body.classList.contains('aw-light') ? 'Dark mode' : 'Light mode'; }
      labelTheme(); themeButton.setAttribute('aria-label', 'Switch color theme');
      themeButton.addEventListener('click', () => {
        document.body.classList.toggle('aw-light'); labelTheme();
        try { localStorage.setItem('aurelis-theme', document.body.classList.contains('aw-light') ? 'light' : 'dark'); } catch (_) {}
        styleCharts();
      });
      themeLi.appendChild(themeButton); nav.appendChild(themeLi);
    }
    if (window.jQuery) jQuery(document).on('shiny:value shown.bs.tab', () => setTimeout(styleCharts, 150));
    // Tables created inside collapsed details need a width calculation on open.
    document.addEventListener('toggle', e => {
      if (e.target.tagName === 'DETAILS' && e.target.open) window.dispatchEvent(new Event('resize'));
    }, true);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup); else setup();
})();
