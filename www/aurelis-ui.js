/* AURELIS v2: no external services, no business calculations, no HTML from data. */
(function () {
  'use strict';
  const chartNotes = {
    exec_revenue_trend: ['Sum · USD · monthly', false],
    exec_top_clients: ['Sum of revenue · USD · top clients', true],
    exec_profit_donut: ['Sum of gross profit · USD · by representative', true],
    exec_country_treemap: ['Sum of revenue · USD · by country', true],
    exec_yoy_comparison: ['Sum of revenue · USD · month / year', false],
    kpi_board_trend: ['Sums · revenue and PO value · USD · labeled axes', false],
    kpi_board_customer_rank: ['Sum of revenue · USD · by client', true],
    kpi_board_buyer_rank: ['Sum of PO value · USD · order count in tooltip', true],
    exec_activity_by_role: ['Count of logged events · by employee / role', false],
    buyer_act_daily_trend: ['Order count + sum of value · USD · daily · labeled axes', true],
    buyer_act_leaderboard: ['Sum of PO value · USD · by buyer', true],
    buyer_act_customer_mix: ['Sum of PO value · USD · by client', true],
    sales_heatmap: ['Count of orders · buyer / month', true],
    sales_buyer_stack: ['Sum of PO value · USD · month / buyer', true],
    sales_monthly_line: ['Sum of PO value · USD · monthly', true],
    perf_rev_cost_trend: ['Monthly sums · revenue, cost and gross profit · USD', false],
    perf_staff_bar: ['Sums of revenue / gross profit · USD · by representative', true],
    perf_margin_gauge: ['Weighted ratio · gross profit / revenue · %', false],
    perf_country_donut: ['Sum of revenue · USD · by country', true],
    perf_country_treemap: ['Sum of revenue · USD · by country', true],
    cust_perf_rank_gauge: ['Client rank · current portfolio', false],
    cust_perf_monthly_trend: ['Sum of relationship value · USD · monthly · page scope', false],
    cust_perf_status_donut: ['Order status distribution · page scope', true],
    cust_perf_annual_story: ['Sum of PO value + order count · annual · labeled axes', false],
    cust_perf_supplier_mix: ['Sum of order value · USD · by supplier', true],
    cust_perf_buyer_mix: ['Sum of PO value · USD · by buyer', true],
    ar_aging_donut: ['Sum of remaining balance · USD · aging bucket', true],
    ar_monthly_trend: ['Sums of invoiced / remaining balance · USD · invoice month', false],
    ar_staff_aging: ['Sum of remaining balance · USD · staff / aging', true],
    ar_exposure_bar: ['Sum of remaining balance · USD · by client', true],
    stmt_aging_donut: ['Sum of remaining balance · USD · aging bucket', true],
    stmt_timeline: ['Invoice amounts / remaining balances · USD · invoice timeline', true],
    tracker_status_donut: ['Delivery status distribution · order lines', true],
    tracker_delivery_timeline: ['Delivery workload · by exact date · source quantities / value', true],
    tracker_delivery_risk: ['Order exposure / delivery timing · line-level risk', true],
    tracker_delivery_heatmap: ['Delivery workload · calendar aggregation', true],
    tracker_customer_exposure: ['Sum of open exposure · USD · client / status', true],
    wh_status_donut: ['Count of stock items · by stock status', true],
    wh_quantity_mix: ['Sum of quantities · units · stock allocation', false],
    wh_reorder_matrix: ['Stock coverage / reorder priority · item level', true],
    wh_value_by_warehouse: ['Sum of inventory value · USD · by warehouse', true],
    inv_supplier_value: ['Sum of open inbound value · USD · by supplier', true],
    inv_client_units: ['Sum of units awaiting receipt · by client', true],
    inv_part_backorders: ['Sum of units awaiting receipt · by part', true],
    inv_timing_donut: ['Inbound timing distribution · page scope', true],
    buyer_supplier_value: ['Sum of open PO value · USD · by supplier', true],
    supplier_compare_score_plot: ['Supplier comparison score · page-defined weights', false],
    pi_landscape_plot: ['Individual catalog records · quoted unit cost / lead days', false],
    quotation_cost_bridge: ['Price bridge · selected scenario · quote currency', false],
    contact_activity_trend: ['Sum of PO value + order count · monthly · labeled axes', true],
    contact_relationship_flow: ['Relationship flow · selected numerical measure', true],
    contact_counterparty_chart: ['Sum of PO value · USD · by counterparty', true],
    atlas_world_map: ['Relationship locations · selected business measure', true],
    atlas_selected_timeline: ['Monthly relationship activity · selected measure', false],
    atlas_country_breakdown: ['Hierarchical geographic totals · selected measure', true],
    atlas_relationship_network: ['Connected activity totals · selected measure', true],
    financial_stats_probability_plot: ['Cumulative probability · selected fitted model', false],
    financial_stats_distribution_plot: ['Observed distribution + fitted model · selected measure', false],
    financial_stats_qq_plot: ['Observed vs theoretical quantiles · selected model', false],
    financial_stats_group_plot: ['Group distributions · selected measure / source units', false],
    'lab-distribution': ['Distribution · filtered observations · selected source units', false],
    'lab-box': ['Median / quartiles / IQR · selected source units', false],
    'lab-qq': ['Observed vs normal quantiles · selected source units', false],
    'lab-correlation': ['Pairwise correlation · coefficient −1 to +1', false],
    'lab-residuals': ['Complete-case model · residuals vs fitted values', false],
    'lab-residual_qq': ['Model residuals vs normal quantiles', false],
    'lab-simulation': ['Empirical resampling · observation-level sensitivity', false]
  };
  function setup() {
    if (document.body.classList.contains('aurelis-v2')) return;
    const body = document.body;
    body.classList.add('aurelis-v2');
    document.title = 'Aurelis | Operations Intelligence';
    const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');
    const systemTheme = window.matchMedia('(prefers-color-scheme: light)');
    let identity = document.getElementById('multi_user_buyer')?.value || 'local';
    let prefix = 'aurelis-v2:' + identity + ':';
    const read = (key, fallback) => { try { const value = JSON.parse(localStorage.getItem(prefix + key)); return Array.isArray(fallback) && !Array.isArray(value) ? fallback : value ?? fallback; } catch (_) { return fallback; } };
    const write = (key, value) => { try { localStorage.setItem(prefix + key, JSON.stringify(value)); } catch (_) {} };
    const send = (id, value) => { if (window.Shiny?.setInputValue) Shiny.setInputValue(id, value, {priority: 'event'}); };
    let themeOverride = read('theme', null);
    let density = read('density', 'comfortable');
    let favorites = read('favorites', []).filter(x => typeof x === 'string').slice(0, 8);
    let recents = read('recent', []).filter(x => typeof x === 'string').slice(0, 6);
    let page = 'exec_overview';
    window.aurelisCurrentPage = page;
    let previousPage = '';
    const restorePage = read('last-page', null);
    let restoredPage = false;
    let searchItems = [];
    let allowedPages = null;
    let active = 0;
    let resizeTimer, chartTimer, searchTimer;
    let forceNextChartTheme = false;
    let summary, quoteSteps = [];
    const pendingTableSearches = new Map();
    let quoteObserver;
    const chartState = new WeakMap();
    const titleOf = id => document.querySelector('.main-sidebar a[data-value="' + CSS.escape(id) + '"] p')?.textContent.trim().replace(/\s+/g, ' ') || id;
    function preferences() { send('v2_preferences', {theme: body.classList.contains('aw-light') ? 'light' : 'dark', density, favorites, page}); }
    function navigate(id, extra) {
      if (allowedPages && !allowedPages.includes(id)) return;
      send('v2_command', Object.assign({page: id}, extra || {}));
      if (innerWidth < 992) body.classList.remove('sidebar-open');
    }
    function setTheme(mode, save) {
      body.classList.toggle('aw-light', mode === 'light');
      if (save) { themeOverride = mode; write('theme', mode); }
      const button = document.querySelector('.aw-theme-toggle');
      if (button) { button.textContent = mode === 'light' ? 'Dark mode' : 'Light mode'; button.setAttribute('aria-label', 'Switch to ' + (mode === 'light' ? 'dark' : 'light') + ' theme'); }
      scheduleCharts(true); preferences();
    }
    function toggleTheme() { setTheme(body.classList.contains('aw-light') ? 'dark' : 'light', true); }
    function setDensity(value) {
      density = value === 'compact' ? 'compact' : 'comfortable'; body.dataset.density = density;
      document.querySelectorAll('[data-v2-action="density"]').forEach(b => { b.textContent = density === 'compact' ? 'Comfortable' : 'Compact'; b.setAttribute('aria-label', 'Use ' + b.textContent.toLowerCase() + ' density'); });
      write('density', density); scheduleResize(); preferences();
    }
    function focusMode(value) {
      body.classList.toggle('v2-focus', value);
      document.querySelectorAll('[data-v2-action="focus"]').forEach(b => { b.setAttribute('aria-pressed', String(value)); b.textContent = value ? 'Exit focus' : 'Focus'; });
      scheduleResize();
    }
    function priorities(value) {
      const panel = document.getElementById('v2-priorities-panel'); if (!panel) return;
      panel.hidden = !value;
      document.querySelectorAll('[data-v2-action="alerts"]').forEach(b => b.setAttribute('aria-expanded', String(value)));
      if (value) panel.querySelector('button')?.focus();
    }
    function updateFreshness() {
      document.querySelectorAll('[data-updated-at]').forEach(e => {
        const age = Math.max(0, Math.floor((Date.now() - Number(e.dataset.updatedAt)) / 1000));
        const label = body.classList.contains('v2-is-busy') ? 'Updating view…' : age < 60 ? 'Source loaded ' + age + 's ago' : age < 3600 ? 'Source loaded ' + Math.floor(age / 60) + 'm ago' : 'Source loaded ' + Math.floor(age / 3600) + 'h ago';
        e.replaceChildren(Object.assign(document.createElement('span'), {className: 'v2-sync-dot'}), document.createTextNode(label));
        e.title = 'File reload time. Source records are synthetic; this is not a live database connection.';
      });
    }
    function tokens() {
      const s = getComputedStyle(body); const get = name => s.getPropertyValue('--v2-' + name).trim();
      return {text: get('text'), muted: get('muted'), grid: get('grid'), panel: get('panel'), line: get('line'), gold: get('gold'), blue: get('blue'), success: get('success'), mauve: get('mauve'), warning: get('warning'), risk: get('risk')};
    }
    function chartNote(plot) {
      if (plot.id === 'exec_revenue_trend') return; // Explicit subtitle in the overview UI.
      let note = chartNotes[plot.id];
      if (plot.id === 'lab-trend') {
        const agg = document.getElementById('lab-aggregation')?.value === 'mean' ? 'Mean' : 'Sum';
        note = [agg + ' · selected date period · linear baseline + prediction interval', false];
      }
      if (plot.id === 'lab-canvas') {
        const kind = document.getElementById('lab-chart_type')?.value;
        note = [kind === 'bar' || kind === 'line' ? 'Mean by selected X · transformed Y if chosen · bars show top 30' : 'Individual observations · selected X / Y · transformation if chosen', false];
      }
      if (!note) return;
      let el = plot.previousElementSibling;
      if (!el?.classList.contains('v2-chart-meta')) { el = document.createElement('div'); el.className = 'v2-chart-meta'; plot.before(el); }
      el.replaceChildren(Object.assign(document.createElement('span'), {textContent: note[0]}));
      if (note[1]) { const hint = document.createElement('span'); hint.className = 'v2-click-hint'; hint.textContent = 'Click to explore'; el.append(hint); }
    }
    function scheduleCharts(force) {
      forceNextChartTheme = forceNextChartTheme || !!force;
      if (chartTimer) return;
      chartTimer = setTimeout(() => { chartTimer = null; const forceTheme = forceNextChartTheme; forceNextChartTheme = false; styleCharts(forceTheme); }, 100);
    }
    // Apply to rendered SVG geometry so older bundled Plotly versions also
    // receive rounded bars. Bounding boxes keep the original value endpoints.
    function finishChart(plot) {
      plot.querySelectorAll('.barlayer .point path').forEach(path => {
        const b = path.getBBox();
        if (!b.width || !b.height) return;
        const r = Math.min(6, b.width / 2, b.height / 2);
        const x = b.x, y = b.y, right = x + b.width, bottom = y + b.height;
        path.setAttribute('d', `M${x+r},${y}H${right-r}Q${right},${y} ${right},${y+r}V${bottom-r}Q${right},${bottom} ${right-r},${bottom}H${x+r}Q${x},${bottom} ${x},${bottom-r}V${y+r}Q${x},${y} ${x+r},${y}Z`);
      });
      if (plot._aurelisHover) return;
      plot._aurelisHover = true;
      plot.setAttribute('tabindex', '0');
      const selector = '.barlayer .point, .pielayer .slice, .sunburstlayer .slice, .treemaplayer .slice, .scatterlayer .point, .scatterlayer .trace, .scattergeolayer .point, .choroplethlayer .choroplethlocation, .boxlayer .box, .heatmaplayer .hm';
      let active;
      const clear = () => {
        active?.classList.remove('v2-mark-active');
        active = null;
        plot.classList.remove('v2-mark-hover');
      };
      plot.addEventListener('pointerover', event => {
        const mark = event.target.closest?.(selector);
        if (mark === active) return;
        clear();
        if (mark) { active = mark; mark.classList.add('v2-mark-active'); plot.classList.add('v2-mark-hover'); }
      });
      plot.addEventListener('pointerleave', clear);
      plot.addEventListener('focusout', clear);
      // Cartesian Plotly charts capture the pointer on a transparent overlay;
      // use their hover event to find the underlying visible SVG mark as well.
      if (typeof plot.on === 'function') {
        plot.on('plotly_hover', event => {
          const point = event.points?.[0];
          if (!point) return;
          const traces = plot.querySelectorAll('.scatterlayer .trace, .barlayer .trace, .scattergeolayer .trace');
          const trace = traces[point.curveNumber];
          const marks = trace?.querySelectorAll('.point');
          const mark = marks?.[point.pointNumber] || trace;
          if (!mark) return;
          clear(); active = mark;
          mark.classList.add('v2-mark-active'); plot.classList.add('v2-mark-hover');
        });
        plot.on('plotly_unhover', clear);
      }
    }
    function styleCharts(force) {
      if (!window.Plotly) return;
      const t = tokens(); const key = (body.classList.contains('aw-light') ? 'light' : 'dark');
      document.querySelectorAll('.js-plotly-plot').forEach(plot => {
        if (!plot._fullLayout || !plot.getBoundingClientRect().width || plot.offsetParent === null || plot.closest('details:not([open])')) return;
        if (!plot._v2AfterPlot && typeof plot.on === 'function') {
          plot._v2AfterPlot = true;
          plot.on('plotly_afterplot', () => { finishChart(plot); scheduleCharts(); });
        }
        finishChart(plot);
        chartNote(plot);
        const state = chartState.get(plot);
        if (!force && state?.layout === plot._fullLayout && state.key === key) return;
        const layout = {
          'font.family': 'Inter, Segoe UI, sans-serif', 'font.size': 12, 'font.color': t.text,
          'legend.font.color': t.muted, paper_bgcolor: 'transparent', plot_bgcolor: 'transparent',
          'hoverlabel.bgcolor': t.panel, 'hoverlabel.bordercolor': t.line,
          'hoverlabel.font.color': t.text, 'hoverlabel.font.family': 'Inter, Segoe UI, sans-serif',
          colorway: [t.gold, t.blue, t.success, t.mauve, t.warning], 'transition.duration': 0,
          bargap: .28, bargroupgap: .12,
          'legend.itemsizing': 'constant', 'legend.tracegroupgap': 12,
          'hoverlabel.font.size': 13, 'hoverlabel.align': 'left'
        };
        for (const axis of ['xaxis', 'yaxis', 'xaxis2', 'yaxis2']) {
          if (!plot._fullLayout[axis]) continue;
          layout[axis + '.tickfont.color'] = t.muted; layout[axis + '.title.font.color'] = t.muted;
          layout[axis + '.gridcolor'] = t.grid; layout[axis + '.zerolinecolor'] = t.grid;
        }
        if (plot._fullLayout.geo) {
          const light = body.classList.contains('aw-light');
          Object.assign(layout, {'geo.bgcolor': 'transparent', 'geo.landcolor': light ? '#e0e8ed' : '#202f40',
            'geo.oceancolor': light ? '#f0f6fa' : '#0c1726', 'geo.lakecolor': light ? '#f0f6fa' : '#0c1726',
            'geo.showocean': true, 'geo.countrycolor': light ? '#b8c9d5' : '#40556b',
            'geo.coastlinecolor': light ? '#a5bac9' : '#4b6278', 'geo.coastlinewidth': .7,
            'geo.lonaxis.showgrid': true, 'geo.lataxis.showgrid': true,
            'geo.lonaxis.gridcolor': t.grid, 'geo.lataxis.gridcolor': t.grid,
            'legend.bgcolor': t.panel, 'legend.font.color': t.text});
        }
        (plot.layout?.annotations || []).forEach((_, i) => { layout['annotations[' + i + '].font.color'] = t.muted; });
        if (plot._fullLayout.title) layout['title.font.color'] = t.text;
        (plot.data || []).forEach((trace, i) => {
          if (trace.type === 'indicator') Plotly.restyle(plot, {'number.font.color': t.text, 'gauge.axis.tickfont.color': t.muted, 'title.font.color': t.muted}, [i]).catch(() => {});
          if (trace.type === 'pie' && trace.hole > 0 && trace.hole !== .58) Plotly.restyle(plot, {hole: .58}, [i]).catch(() => {});
          if (trace.type === 'pie') Plotly.restyle(plot, {
            'outsidetextfont.color': t.text, 'textfont.size': 12,
            'marker.line.color': t.panel, 'marker.line.width': 3
          }, [i]).catch(() => {});
          if (trace.type === 'bar') Plotly.restyle(plot, {
            'marker.line.width': 0, 'textfont.color': t.text
          }, [i]).catch(() => {});
          if (trace.type === 'scatter' || trace.type === 'scattergl') {
            const polish = {};
            if ((trace.mode || '').includes('lines')) polish['line.width'] = Math.max(2.5, trace.line?.width || 0);
            if ((trace.mode || '').includes('markers')) {
              polish['marker.line.width'] = 1.5;
              polish['marker.line.color'] = t.panel;
              if (!trace.marker?.size || typeof trace.marker.size === 'number' && trace.marker.size < 6) polish['marker.size'] = 6;
            }
            if (Object.keys(polish).length) Plotly.restyle(plot, polish, [i]).catch(() => {});
          }
          if (trace.type === 'box') Plotly.restyle(plot, {'line.width': 2, 'marker.opacity': .65}, [i]).catch(() => {});
          if (trace.marker?.colorbar) Plotly.restyle(plot, {'marker.colorbar.tickfont.color': t.muted, 'marker.colorbar.title.font.color': t.muted}, [i]).catch(() => {});
          if (trace.colorbar) Plotly.restyle(plot, {'colorbar.tickfont.color': t.muted, 'colorbar.title.font.color': t.muted}, [i]).catch(() => {});
          if (plot.id.startsWith('lab-') && trace.marker && typeof trace.marker.color === 'string') Plotly.restyle(plot, {'marker.color': trace.type === 'scatter' ? t.blue : t.gold}, [i]).catch(() => {});
        });
        // No automatic type conversion or trace recoloring: status semantics and
        // chart selection keys must survive the visual upgrade. The executive
        // trend has two named, non-semantic series and follows the brand palette.
        if (plot.id === 'exec_revenue_trend' && plot.data?.length === 2) {
          Plotly.restyle(plot, {'line.color': [t.gold, t.blue], fillcolor: body.classList.contains('aw-light') ? 'rgba(128,96,32,.06)' : 'rgba(213,183,127,.05)'}).catch(() => {});
        }
        Plotly.relayout(plot, layout).then(() => { chartState.set(plot, {key, layout: plot._fullLayout}); }).catch(error => {
          window.__aurelisChartErrors = [...(window.__aurelisChartErrors || []), {id: plot.id, message: String(error)}].slice(-10);
        });
      });
    }
    function scheduleResize() {
      clearTimeout(resizeTimer); resizeTimer = setTimeout(() => {
        if (window.Plotly) document.querySelectorAll('.js-plotly-plot').forEach(p => { if (p._fullLayout && p.offsetParent !== null && !p.closest('details:not([open])')) { const resized = Plotly.Plots.resize(p); resized?.catch?.(() => {}); } });
        if (window.jQuery?.fn?.dataTable) jQuery.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
        scheduleCharts();
      }, 160);
    }
    function compactNumbers() {
      document.querySelectorAll('.v2-kpi>strong .shiny-text-output').forEach(e => {
        const raw = e.textContent.trim();
        if (!raw || e.dataset.v2Display === raw) return;
        const amount = Number(raw.replace(/[^\d.\-]/g, ''));
        if (!Number.isFinite(amount)) return;
        const currency = raw.includes('$');
        const compact = Math.abs(amount) >= 1000000000 ? (amount / 1000000000).toLocaleString('en-US', {maximumFractionDigits: 2}) + 'B' : Math.abs(amount) >= 1000000 ? (amount / 1000000).toLocaleString('en-US', {maximumFractionDigits: 2}) + 'M' : Math.abs(amount) >= 10000 ? (amount / 1000).toLocaleString('en-US', {maximumFractionDigits: 1}) + 'K' : amount.toLocaleString('en-US', {maximumFractionDigits: 0});
        const display = (currency ? '$' : '') + compact;
        e.title = raw + ' — exact value'; e.setAttribute('aria-label', raw); e.textContent = display; e.dataset.v2Display = display;
        if (!reducedMotion.matches && e.animate) e.animate([{opacity: .35, transform: 'translateY(3px)'}, {opacity: 1, transform: 'none'}], {duration: 200, easing: 'ease-out'});
      });
    }
    // Keyboard-first command palette with bounded server-side entity lookup.
    const dialog = document.createElement('dialog'); dialog.id = 'aw-command'; dialog.setAttribute('aria-label', 'Search Aurelis');
    dialog.innerHTML = '<div class="aw-command-top"><input id="aw-command-search" aria-label="Search pages, clients, suppliers, orders and invoices" placeholder="Search pages, clients, orders…" autocomplete="off"><button type="button" aria-label="Close search">Esc</button></div><div id="aw-command-results" role="list"></div><div class="aw-command-hint">↑ ↓ navigate · Enter open · Esc close · Entity lookup starts at 2 characters</div>';
    body.append(dialog);
    const search = dialog.querySelector('input'), results = dialog.querySelector('#aw-command-results');
    let opener;
    const fold = s => String(s).normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
    function fuzzy(label, query) {
      label = fold(label); query = fold(query).trim();
      if (!query) return 1;
      if (label.includes(query)) return 100 - label.indexOf(query);
      let pos = -1, gaps = 0;
      for (const char of query) { const next = label.indexOf(char, pos + 1); if (next < 0) return 0; gaps += next - pos - 1; pos = next; }
      return 20 / (1 + gaps);
    }
    function updateActive() {
      results.querySelectorAll('button').forEach((b, i) => { b.classList.toggle('aw-command-active', i === active); b.setAttribute('aria-current', i === active ? 'true' : 'false'); });
      results.querySelector('.aw-command-active')?.scrollIntoView({block: 'nearest'});
    }
    function addResult(label, kind, handler) {
      const button = document.createElement('button'); button.type = 'button';
      button.append(Object.assign(document.createElement('span'), {textContent: label}), Object.assign(document.createElement('small'), {textContent: kind}));
      button.addEventListener('click', () => { dialog.close(); handler(); }); results.append(button);
    }
    function heading(label) { results.append(Object.assign(document.createElement('div'), {className: 'v2-search-heading', textContent: label})); }
    function populate() {
      const needle = search.value.trim(); results.replaceChildren(); active = 0;
      const pages = Array.from(document.querySelectorAll('.main-sidebar a[data-value]')).map(a => ({id: a.dataset.value, label: titleOf(a.dataset.value)})).filter(x => !allowedPages || allowedPages.includes(x.id));
      if (!needle && recents.length) {
        heading('Recent workspaces');
        recents.filter(id => pages.some(p => p.id === id)).slice(0, 4).forEach(id => addResult(titleOf(id), 'Recent', () => navigate(id)));
      }
      const matches = pages.map(p => ({...p, score: fuzzy(p.label, needle)})).filter(p => p.score > 0).sort((a, b) => b.score - a.score).slice(0, needle ? 8 : 7);
      if (matches.length) { heading('Workspaces'); matches.forEach(p => addResult(p.label, 'Page', () => navigate(p.id))); }
      const actions = [
        ['Switch theme', toggleTheme], ['Toggle comfortable / compact density', () => setDensity(density === 'compact' ? 'comfortable' : 'compact')],
        ['Toggle focus mode', () => focusMode(!body.classList.contains('v2-focus'))],
        ['Report / export current page', () => document.getElementById('report_current_page')?.click()]
      ].filter(([label]) => !needle || fuzzy(label, needle) > 0);
      if (actions.length) { heading('Actions'); actions.forEach(([label, action]) => addResult(label, 'Action', action)); }
      if (needle.length >= 2) {
        heading('Clients · suppliers · orders · invoices');
        const entities = searchItems.filter(x => fold(x.label).includes(fold(needle)));
        if (entities.length) entities.forEach(item => addResult(item.label, item.type, () => navigate(item.page, {type: item.type, label: item.label})));
        else results.append(Object.assign(document.createElement('div'), {className: 'v2-search-empty', textContent: search.dataset.resolved === needle ? 'No matching records in the available index.' : 'Searching available records…'}));
      }
      updateActive();
    }
    function open() { opener = document.activeElement; search.value = ''; searchItems = []; search.dataset.resolved = ''; populate(); dialog.showModal(); search.focus(); }
    dialog.querySelector('button').addEventListener('click', () => dialog.close());
    dialog.addEventListener('close', () => opener?.focus());
    dialog.addEventListener('click', e => { if (e.target === dialog) dialog.close(); });
    search.addEventListener('input', () => {
      populate(); clearTimeout(searchTimer);
      if (search.value.trim().length >= 2) searchTimer = setTimeout(() => send('v2_entity_query', {term: search.value.trim(), nonce: Date.now()}), 120);
    });
    search.addEventListener('keydown', e => {
      const buttons = results.querySelectorAll('button');
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') { e.preventDefault(); active = (active + (e.key === 'ArrowDown' ? 1 : -1) + buttons.length) % (buttons.length || 1); updateActive(); }
      if (e.key === 'Enter') { e.preventDefault(); buttons[active]?.click(); }
    });
    document.addEventListener('keydown', e => {
      if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'k') { e.preventDefault(); dialog.open ? dialog.close() : open(); }
      if (e.key === 'Escape') { focusMode(false); priorities(false); }
    }, true);
    const nav = document.querySelector('.main-header .navbar-nav.ml-auto');
    if (nav) {
      const li = document.createElement('li'); li.className = 'nav-item aw-command-nav';
      const button = document.createElement('button'); button.type = 'button'; button.className = 'aw-command-trigger'; button.setAttribute('aria-label', 'Search Aurelis, Control K');
      button.append(document.createTextNode('Search '), Object.assign(document.createElement('kbd'), {textContent: 'Ctrl K'})); button.addEventListener('click', open); li.append(button); nav.prepend(li);
      const themeLi = document.createElement('li'); themeLi.className = 'nav-item aw-command-nav';
      const themeButton = document.createElement('button'); themeButton.type = 'button'; themeButton.className = 'aw-theme-toggle'; themeButton.addEventListener('click', toggleTheme); themeLi.append(themeButton); nav.append(themeLi);
    }
    const favoritesBar = document.createElement('div'); favoritesBar.className = 'v2-favorites'; favoritesBar.setAttribute('aria-label', 'Favorite workspaces');
    document.querySelector('.main-sidebar .sidebar')?.prepend(favoritesBar);
    function renderFavorites() {
      favoritesBar.replaceChildren();
      favorites.filter(id => !allowedPages || allowedPages.includes(id)).forEach(id => {
        const button = document.createElement('button'); button.type = 'button'; button.textContent = titleOf(id); button.addEventListener('click', () => navigate(id)); favoritesBar.append(button);
      });
      document.querySelectorAll('.v2-pin').forEach(b => { b.setAttribute('aria-pressed', String(favorites.includes(b.dataset.page))); b.title = (favorites.includes(b.dataset.page) ? 'Unpin ' : 'Pin ') + titleOf(b.dataset.page); });
    }
    document.querySelectorAll('.main-sidebar a[data-value]').forEach(a => {
      a.title = titleOf(a.dataset.value);
      // A sibling button avoids interactive controls nested inside a link.
      const pin = document.createElement('button'); pin.type = 'button'; pin.className = 'v2-pin'; pin.dataset.page = a.dataset.value; pin.textContent = '☆'; pin.setAttribute('aria-label', 'Pin ' + titleOf(a.dataset.value));
      pin.addEventListener('click', e => { e.preventDefault(); e.stopPropagation(); const id = pin.dataset.page; favorites = favorites.includes(id) ? favorites.filter(x => x !== id) : [...favorites, id].slice(-8); write('favorites', favorites); renderFavorites(); preferences(); });
      // Position the pin beside the anchor, in its navigation list item.
      a.parentElement.classList.add('v2-pinnable'); a.after(pin);
    });
    const mobile = document.createElement('nav'); mobile.className = 'v2-mobile-nav'; mobile.setAttribute('aria-label', 'Primary mobile navigation');
    [['exec_overview', 'Overview', 'fa-gauge-high'], ['order_tracker', 'Delivery', 'fa-truck'], ['quotation_studio', 'Quotes', 'fa-file-signature'], ['analysis_lab', 'Analysis', 'fa-flask']].forEach(([id, label, icon]) => {
      const b = document.createElement('button'); b.type = 'button'; b.dataset.v2Page = id; b.append(Object.assign(document.createElement('i'), {className: 'fas ' + icon}), document.createTextNode(label)); mobile.append(b);
    });
    const more = document.createElement('button'); more.type = 'button'; more.dataset.v2Action = 'menu'; more.append(Object.assign(document.createElement('i'), {className: 'fas fa-bars'}), document.createTextNode('More')); mobile.append(more); body.append(mobile);
    const filterSheet = document.createElement('dialog'); filterSheet.className = 'v2-filter-sheet'; filterSheet.setAttribute('aria-label', 'Global period filters');
    filterSheet.innerHTML = '<header><strong>Global period</strong><button type="button" aria-label="Close period filters">×</button></header><p>These filters apply to the business pages. Analysis Lab keeps its own source and filters.</p>';
    const mobileFilters = document.querySelector('.prism-mobile-filter-row');
    if (mobileFilters) { filterSheet.append(mobileFilters); body.append(filterSheet); }
    filterSheet.querySelector('button').addEventListener('click', () => filterSheet.close());
    filterSheet.addEventListener('click', e => { if (e.target === filterSheet) filterSheet.close(); });
    function quoteSetup() {
      const tab = document.getElementById('shiny-tab-quotation_studio'); if (!tab || tab.querySelector('.v2-quote-stepper')) return;
      const names = ['Identity', 'History', 'Line items', 'Landed cost', 'Finance', 'Pricing', 'Review', 'Export'];
      const stepper = document.createElement('nav'); stepper.className = 'v2-quote-stepper'; stepper.setAttribute('aria-label', 'Quotation sections');
      tab.querySelectorAll('.card-header .card-title').forEach(title => {
        const match = title.textContent.trim().match(/^([1-8])(?:[AB])?\./); if (!match) return;
        const step = Number(match[1]); if (quoteSteps.some(x => x.step === step)) return;
        const card = title.closest('.card'); card.dataset.v2QuoteStep = step; card.classList.add('v2-quote-anchor');
        quoteSteps.push({step, card}); const button = document.createElement('button'); button.type = 'button'; button.dataset.quoteStep = step;
        button.append(Object.assign(document.createElement('b'), {textContent: String(step).padStart(2, '0')}), document.createTextNode(names[step - 1]));
        button.addEventListener('click', () => goQuoteStep(step)); stepper.append(button);
      });
      if (!quoteSteps.length) return;
      tab.prepend(stepper);
      summary = document.createElement('div'); summary.className = 'v2-quote-summary'; summary.setAttribute('aria-label', 'Current quotation total');
      summary.append(Object.assign(document.createElement('span'), {textContent: 'Current quote · customer-facing total'}), Object.assign(document.createElement('strong'), {className: 'v2-quote-total', textContent: 'Calculating…'}));
      const exportButton = document.createElement('button'); exportButton.type = 'button'; exportButton.textContent = 'Review & export →'; exportButton.addEventListener('click', () => goQuoteStep(8)); summary.append(exportButton); tab.append(summary);
      quoteObserver = new IntersectionObserver(entries => { entries.forEach(entry => { if (entry.isIntersecting) setQuoteStep(Number(entry.target.dataset.v2QuoteStep)); }); }, {rootMargin: '-150px 0px -55% 0px', threshold: 0});
      quoteSteps.forEach(({card}) => quoteObserver.observe(card)); updateQuoteSummary(); setQuoteStep(1);
    }
    function setQuoteStep(step) { document.querySelectorAll('[data-quote-step]').forEach(b => { if (Number(b.dataset.quoteStep) === step) b.setAttribute('aria-current', 'step'); else b.removeAttribute('aria-current'); }); }
    function goQuoteStep(step) {
      const target = quoteSteps.find(x => x.step === Number(step));
      if (target) { target.card.scrollIntoView({behavior: reducedMotion.matches ? 'instant' : 'smooth', block: 'start'}); setQuoteStep(Number(step)); }
    }
    function updateQuoteSummary() { const value = document.getElementById('quotation_kpi_total')?.textContent.trim(); if (summary && value) summary.querySelector('strong').textContent = value; }
    function decorateStatuses() {
      const statuses = {'overdue':'risk','out of stock':'risk','due soon':'warning','low stock':'warning','below reorder point':'warning','fully received':'success','paid':'success','in stock':'success'};
      document.querySelectorAll('.dataTable td').forEach(cell => {
        const text = cell.textContent.trim().toLowerCase();
        if (cell.dataset.v2Status === text) return;
        cell.classList.remove('v2-status-risk','v2-status-warning','v2-status-success');
        if (statuses[text]) cell.classList.add('v2-status-' + statuses[text]);
        cell.dataset.v2Status = text;
      });
    }
    document.addEventListener('click', e => {
      const destination = e.target.closest('[data-v2-page]'); if (destination) { navigate(destination.dataset.v2Page); priorities(false); }
      const client = e.target.closest('[data-v2-client]'); if (client) navigate('customer_performance', {type: 'Client', label: client.dataset.v2Client});
      const action = e.target.closest('[data-v2-action]');
      if (action) {
        if (action.dataset.v2Action === 'density') setDensity(density === 'compact' ? 'comfortable' : 'compact');
        if (action.dataset.v2Action === 'focus') focusMode(!body.classList.contains('v2-focus'));
        if (action.dataset.v2Action === 'alerts') priorities(document.getElementById('v2-priorities-panel')?.hidden !== false);
        if (action.dataset.v2Action === 'filters' && !filterSheet.open) filterSheet.showModal();
        if (action.dataset.v2Action === 'menu') body.classList.toggle('sidebar-open');
      } else if (!e.target.closest('#v2-priorities-panel')) priorities(false);
    });
    function tableSearch(message, attempt) {
      const table = document.querySelector('#' + CSS.escape(message.id) + ' table');
      if (table && window.jQuery?.fn?.dataTable?.isDataTable(table)) { jQuery(table).DataTable().search(message.value, false, false).draw(); return; }
      if ((attempt || 0) < 25) setTimeout(() => tableSearch(message, (attempt || 0) + 1), 180);
    }
    function changedPage(id) {
      if (!id || page === id && previousPage) return;
      previousPage = page; page = id;
      window.aurelisCurrentPage = id;
      window.dispatchEvent(new CustomEvent('aurelis:pagechange', {detail: {page: id}}));
      recents = [id, ...recents.filter(x => x !== id)].slice(0, 6); write('recent', recents);
      if (restoredPage) write('last-page', id);
      mobile.querySelectorAll('[data-v2-page]').forEach(b => { if (b.dataset.v2Page === id) b.setAttribute('aria-current', 'page'); else b.removeAttribute('aria-current'); });
      setTimeout(() => { quoteSetup(); scheduleResize(); preferences(); }, 100);
      window.scrollTo({top: 0, behavior: 'instant'});
    }
    function bindShiny() {
      if (!window.Shiny || bindShiny.done) return;
      bindShiny.done = true;
      Shiny.addCustomMessageHandler('aurelis-search-results', message => {
        if (message.query !== search.value.trim()) return;
        searchItems = Array.isArray(message.items) ? message.items : []; search.dataset.resolved = message.query; populate();
      });
      Shiny.addCustomMessageHandler('aurelis-table-search', message => {
        pendingTableSearches.set(message.id, message);
        setTimeout(() => tableSearch(message, 0), 350);
      });
      Shiny.addCustomMessageHandler('aurelis-quote-step', step => setTimeout(() => { quoteSetup(); goQuoteStep(Number(step)); }, 300));
      Shiny.addCustomMessageHandler('aurelis-capabilities', message => {
        allowedPages = message.pages;
        window.aurelisCapabilities = message;
        window.dispatchEvent(new CustomEvent('aurelis:capabilities', {detail: message}));
        body.dataset.workspaceRole = message.role;
        renderFavorites();
        const layouts = {
          CEO: [['exec_overview','Overview','fa-gauge-high'],['kpi_board','KPIs','fa-chart-simple'],['ar_tab','Receivables','fa-file-invoice-dollar'],['order_tracker','Delivery','fa-truck']],
          Manager: [['exec_overview','Overview','fa-gauge-high'],['order_tracker','Delivery','fa-truck'],['inventory_tab','Inventory','fa-warehouse'],['quotation_studio','Quotes','fa-file-signature']],
          Buyer: [['product_intelligence','Products','fa-gem'],['buyer_tool','Suppliers','fa-handshake'],['quotation_studio','Quotes','fa-file-signature'],['order_tracker','Delivery','fa-truck']],
          Analyst: [['exec_overview','Overview','fa-gauge-high'],['analysis_lab','Analysis','fa-flask'],['financial_statistics','Probability','fa-chart-area'],['kpi_board','KPIs','fa-chart-simple']]
        };
        mobile.replaceChildren();
        (layouts[message.role] || layouts.Analyst).filter(([id])=>allowedPages.includes(id)).forEach(([id,label,icon])=>{
          const b=document.createElement('button');b.type='button';b.dataset.v2Page=id;
          b.append(Object.assign(document.createElement('i'),{className:'fas '+icon}),document.createTextNode(label));
          if(id===page)b.setAttribute('aria-current','page');mobile.append(b);
        });
        mobile.append(more);
        if (!restoredPage && allowedPages.length) {
          restoredPage = true;
          const destination = restorePage && allowedPages.includes(restorePage) ? restorePage : message.home;
          setTimeout(() => { navigate(destination); write('last-page', destination); }, 180);
        }
      });
      preferences();
    }
    if (window.jQuery) {
      jQuery(document).on('shiny:connected', () => { bindShiny(); });
      jQuery(document).on('shiny:inputchanged', e => {
        if (e.name === 'sidebar_tabs') changedPage(e.value);
        if (e.name === 'multi_user_buyer' && e.value && e.value !== identity) {
          identity = e.value; prefix = 'aurelis-v2:' + identity + ':'; favorites = read('favorites', []); recents = read('recent', []); themeOverride = read('theme', null);
          setTheme(themeOverride || (systemTheme.matches ? 'light' : 'dark'), false); setDensity(read('density', 'comfortable')); renderFavorites();
        }
      });
      jQuery(document).on('shiny:busy', () => { body.classList.add('v2-is-busy'); updateFreshness(); });
      jQuery(document).on('shiny:idle', () => { body.classList.remove('v2-is-busy'); updateFreshness(); decorateStatuses(); setTimeout(() => scheduleCharts(), 250); });
      jQuery(document).on('draw.dt', () => decorateStatuses());
      jQuery(document).on('shiny:value', event => {
        const pending = pendingTableSearches.get(event.name);
        if (pending) { pendingTableSearches.delete(event.name); setTimeout(() => tableSearch(pending, 0), 200); }
        setTimeout(() => { compactNumbers(); updateQuoteSummary(); updateFreshness(); setDensityLabel(); }, 30); scheduleCharts();
      });
      jQuery(document).on('shown.bs.tab collapsed.lte.pushmenu shown.lte.pushmenu', () => { scheduleResize(); });
      jQuery(document).on('shiny:visualchange', () => scheduleCharts());
    }
    function setDensityLabel() { document.querySelectorAll('[data-v2-action="density"]').forEach(b => { b.textContent = density === 'compact' ? 'Comfortable' : 'Compact'; }); }
    document.addEventListener('toggle', e => { if (e.target.tagName === 'DETAILS' && e.target.open) scheduleResize(); }, true);
    window.addEventListener('resize', scheduleResize);
    if (window.HTMLWidgets?.addPostRenderHandler) HTMLWidgets.addPostRenderHandler(() => scheduleCharts());
    systemTheme.addEventListener('change', () => { if (!themeOverride) setTheme(systemTheme.matches ? 'light' : 'dark', false); });
    const content = document.querySelector('.content-wrapper'); if (content && window.ResizeObserver) new ResizeObserver(scheduleResize).observe(content);
    if (content && window.MutationObserver) {
      // A Shiny value can arrive before a large Plotly widget finishes building.
      // Watch only new plot roots, not every SVG/path mutation from relayout.
      new MutationObserver(records => {
        const newPlot = records.some(record =>
          (record.type === 'attributes' && record.target.classList?.contains('js-plotly-plot')) ||
          [...record.addedNodes].some(node => node.nodeType === 1 &&
            (node.classList?.contains('plot-container') || node.classList?.contains('main-svg') || node.querySelector?.('.plot-container'))));
        if (newPlot) { scheduleCharts(); setTimeout(() => scheduleCharts(), 500); }
      }).observe(content, {childList: true, subtree: true, attributes: true, attributeFilter: ['class']});
    }
    setTheme(themeOverride || (systemTheme.matches ? 'light' : 'dark'), false); setDensity(density); renderFavorites(); quoteSetup(); bindShiny();
    window.AurelisV2.refreshCharts = () => scheduleCharts(true);
    setInterval(updateFreshness, 15000);
    setTimeout(() => { compactNumbers(); scheduleCharts(); }, 500);
  }
  window.AurelisV2 = {setup};
})();
