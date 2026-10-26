/* Local, role-aware navigation help. Search terms stay in the browser. */
(function () {
  'use strict';

  const pages = [
    {id:'exec_overview',group:'Executive Command',title:'Executive Overview',about:'A high-level view of revenue, profit, customer concentration, margin, receivables exposure, and geographic or year-over-year patterns.',example:'Choose a year, compare the revenue trend, then select a client bubble to open its profile.'},
    {id:'kpi_board',group:'Executive Command',title:'KPI Command Center',about:'A management scorecard for revenue, margin, PO value, AR, fulfillment, backorders, customers, and buyer workload.',example:'Compare recognized revenue with PO value, then inspect the customer contribution chart.'},
    {id:'global_network',group:'Executive Command',title:'Global Network Atlas',about:'Explore customer, supplier, and representative relationships by location, measure, map, timeline, and connected records.',example:'Filter to suppliers in one country, set the measure to open exposure, and select a map point.'},
    {id:'executive_observatory',group:'Executive Command',title:'Executive Activity Observatory',about:'Review recorded past activity, tracked demo sessions, future commitments, activity by role, and access governance.',example:'Compare recorded past activity with the future commitments list.'},
    {id:'sale_performance',group:'Commercial Performance',title:'Sales & Profitability',about:'Filter recognized revenue and gross profit by representative, country, or parent company; compare weighted margin with a target.',example:'Select a representative, set a 30% margin target, and inspect the country contribution chart.'},
    {id:'customer_performance',group:'Commercial Performance',title:'Customer Performance',about:'Review one customer’s relationship value, orders, fulfillment measures, trends, portfolio standing, buyers, and suppliers.',example:'Search for a customer, choose full history, and prepare a client-facing relationship review.'},
    {id:'client_statement',group:'Commercial Performance',title:'Client Statements',about:'Inspect one customer’s invoices, amounts paid, open balance, aging, timeline, and detailed account statement.',example:'Choose a client and select an aging segment to locate invoices contributing to the balance.'},
    {id:'monthly_sales',group:'Commercial Performance',title:'POs & Buyers',about:'Compare purchase-order counts and values across buyers, customers, and reporting periods; inspect the order log.',example:'Filter to one buyer and customer, then search the detailed PO log.'},
    {id:'buyer_activity',group:'Commercial Performance',title:'Buyer Activity',about:'Drill buyer workload by year, month, week, and day with PO, value, customer, buyer, and activity-log views.',example:'Choose a month and buyer, then compare the daily trend with the customer mix.'},
    {id:'order_tracker',group:'Supply Chain Control',title:'Orders & Delivery',about:'Filter delivery lines by customer, buyer, supplier, PO/part, status, and due horizon; review open-order KPIs, exact-date workload, risk, calendar, customer exposure, and the prioritized schedule.',example:'Choose Overdue, compare open value with backordered units, and select a workload period to set the global year/month.'},
    {id:'inventory_tab',group:'Supply Chain Control',title:'Inventory & Warehouse',about:'Switch between warehouse availability and inbound/backorder tracking. Physical stock is shown only when verified source data exists; inbound order quantities are separate.',example:'Filter a warehouse and reorder status, then switch to Inbound & Backorders to compare units awaiting receipt.'},
    {id:'buyer_tool',group:'Supply Chain Control',title:'Suppliers & Procurement',about:'Compare suppliers by item, cost, planned lead time, overdue/backorder rate, and open exposure; search product/service records, add a selected item to a quote, and inspect PO lookups.',example:'Enter a part number, choose Fastest planned delivery, and compare the supplier score with the source PO records.'},
    {id:'ar_tab',group:'Supply Chain Control',title:'Accounts Receivable',about:'Prioritize collection work using aging, open invoice balances, customer exposure, staff responsibility, and the full ledger.',example:'Focus on balances over 30 days late, then open a customer’s statement from the exposure view.'},
    {id:'analysis_lab',group:'Analyst Studio',title:'Analysis Lab',about:'Explore catalogued tables or a session-only upload, apply independent filters, inspect data quality, profile measures, test hypotheses, model trends and forecasts, examine drivers, simulate scenarios, and export filtered rows.',example:'Choose the revenue table, filter one country, profile Revenue, then inspect monthly totals.'},
    {id:'financial_statistics',group:'Analyst Studio',title:'Probability Studio',about:'Fit or inspect probability models, calculate tails, intervals and quantiles, and compare groups with tests and confidence intervals.',example:'Choose Revenue and the Empirical model, select Right tail, and enter a threshold.'},
    {id:'presentation_studio',group:'Analyst Studio',title:'Presentation Studio',about:'Turn selected evidence into an editable presentation; choose an audience and decision question, review the draft, edit slides, then export HTML or Markdown.',example:'Group revenue by country, ask which regions need investigation, and generate a local evidence draft.'},
    {id:'multi_user_workspace',group:'Workspace & Studio',title:'My Workspace',about:'Select a demo workspace owner and access profile; review saved quotation drafts, demo sessions, and collaboration activity. A profile selector is not authentication.',example:'Locate a saved quotation in the draft library and review whether it is private or shared.'},
    {id:'quotation_studio',group:'Workspace & Studio',title:'Quotation Studio',about:'Build an editable customer quote through customer history, vendor and landed costs, financing, pricing policy, scenarios, and final export.',example:'Clone a historical PO into a new draft, verify cost assumptions, and review the final price bridge.'},
    {id:'product_intelligence',group:'Workspace & Studio',title:'Product Intelligence',about:'Rank product recommendations by relevance, seller preference, target unit-cost fit, lead time, and availability; explore the catalog and add a selected item to the active quotation.',example:'Search for a pump, set a target unit cost, select In stock, and add a verified catalog recommendation to the quote.'},
    {id:'contacts_tab',group:'Data & Relationships',title:'Directory & Relationships',about:'Explore contacts and counterparties across customer, supplier, buyer, and representative relationships, with connected activity views.',example:'Search a customer contact and follow its activity to the customer workspace.'},
    {id:'misc_tab',group:'Data & Relationships',title:'Demo Data & Process',about:'Test the included synthetic CSV package, inspect source schemas and data contracts, review refresh status and process steps, and record session-only quick notes.',example:'Check whether a quantity is verified warehouse stock or inbound order data before interpreting it.'}
  ];

  const journeys = [
    {title:'Run a management review',summary:'Move from the operating pulse to KPI drivers and recorded activity.',steps:[
      ['exec_overview','Start with the revenue, margin, and client portfolio pulse.'],
      ['kpi_board','Compare the period KPIs and inspect the customer or buyer contribution.'],
      ['sale_performance','Review revenue quality and margin against the target.'],
      ['executive_observatory','Finish by checking recorded activity and planned commitments.']
    ]},
    {title:'Investigate a customer issue',summary:'Trace relationship performance into invoice-level collection detail.',steps:[
      ['exec_overview','Select the client or notice a concentration/exposure signal.'],
      ['customer_performance','Review the customer history, fulfillment measures, and relationship context.'],
      ['client_statement','Inspect the customer’s statement and aging profile.'],
      ['ar_tab','Review the wider receivables exposure and collection priorities.']
    ]},
    {title:'Turn data into a briefing',summary:'Explore a source, answer a statistical question, and prepare a reviewed presentation.',steps:[
      ['analysis_lab','Choose a source table, set Lab filters, and inspect data quality.'],
      ['financial_statistics','Use a probability model only when its assumptions suit the question.'],
      ['presentation_studio','Build and verify an evidence-led briefing for its audience.']
    ]},
    {title:'Move from product to delivery',summary:'Connect product research, quote assumptions, purchasing, and delivery follow-up.',steps:[
      ['product_intelligence','Shortlist products using catalog attributes and lead-time context.'],
      ['quotation_studio','Build and review an editable quotation and price bridge.'],
      ['monthly_sales','Inspect purchase-order activity and buyer workload.'],
      ['order_tracker','Follow delivery timing, statuses, and open customer exposure.']
    ]}
  ];

  const groups = [...new Set(pages.map(page => page.group))];
  const create = (tag, className, text) => {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
  };
  const button = (text, className, attrs) => {
    const node = create('button', className, text);
    node.type = 'button';
    Object.entries(attrs || {}).forEach(([key, value]) => node.setAttribute(key, value));
    return node;
  };

  function setup() {
    const trigger = document.getElementById('aurelis-help-button');
    if (!trigger || document.getElementById('aurelis-help-dialog')) return;

    const mobileTrigger = button('?', 'aurelis-help-mobile-fab', {
      'aria-label':'Open dashboard help',
      'aria-haspopup':'dialog',
      'aria-controls':'aurelis-help-dialog',
      title:'Open the interactive field guide'
    });
    document.body.append(mobileTrigger);

    const dialog = create('dialog', 'aurelis-help-dialog');
    dialog.id = 'aurelis-help-dialog';
    dialog.setAttribute('aria-labelledby', 'aurelis-help-title');
    dialog.innerHTML = `
      <div class="aurelis-help-shell">
        <header class="aurelis-help-header">
          <div class="aurelis-help-brand"><span class="aurelis-help-orbit" aria-hidden="true">A</span><div><span class="aurelis-help-kicker">AURELIS / FIELD GUIDE</span><h2 id="aurelis-help-title">Your next move, made clear.</h2><p>Search the platform, understand this page, or follow a guided route.</p></div></div>
          <button type="button" class="aurelis-help-close" aria-label="Close help">×</button>
        </header>
        <div class="aurelis-help-search"><span aria-hidden="true">⌕</span><input type="search" aria-label="Search help" placeholder="Find a page, metric, or task…" autocomplete="off"><kbd>ESC</kbd></div>
        <nav class="aurelis-help-tabs" aria-label="Help views">
          <button type="button" data-help-view="discover" aria-selected="true">Discover</button>
          <button type="button" data-help-view="journeys" aria-selected="false">Guided routes</button>
          <button type="button" data-help-view="guide" aria-selected="false">All menus</button>
        </nav>
        <section class="aurelis-help-context" aria-live="polite"></section>
        <main class="aurelis-help-main">
          <section class="aurelis-help-pane" data-help-pane="discover">
            <div class="aurelis-help-section-heading"><div><span class="aurelis-help-kicker">EXPLORE THE SYSTEM</span><h3>Find your workspace</h3></div><span class="aurelis-help-count"></span></div>
            <div class="aurelis-help-filters" aria-label="Filter menu group"></div>
            <div class="aurelis-help-results" role="list"></div>
            <p class="aurelis-help-empty" hidden>No matching areas. Try another word or clear the group filter.</p>
          </section>
          <section class="aurelis-help-pane" data-help-pane="journeys" hidden></section>
          <section class="aurelis-help-pane" data-help-pane="guide" hidden></section>
        </main>
        <footer class="aurelis-help-footer"><span><i aria-hidden="true"></i> Private by design · search stays in this browser</span><span>Shortcut <kbd>?</kbd> to reopen</span></footer>
      </div>`;
    document.body.append(dialog);

    const search = dialog.querySelector('input[type="search"]');
    const context = dialog.querySelector('.aurelis-help-context');
    const filters = dialog.querySelector('.aurelis-help-filters');
    const results = dialog.querySelector('.aurelis-help-results');
    const empty = dialog.querySelector('.aurelis-help-empty');
    const count = dialog.querySelector('.aurelis-help-count');
    let activeView = 'discover';
    let activeGroup = 'All areas';
    let currentPage = window.aurelisCurrentPage || document.getElementById('sidebar_tabs')?.value || 'exec_overview';
    let allowedPages = Array.isArray(window.aurelisCapabilities?.pages) ? window.aurelisCapabilities.pages : null;
    let journey = null;
    let journeyStep = 0;
    const available = () => pages.filter(page => !allowedPages || allowedPages.includes(page.id));
    const pageFor = id => pages.find(page => page.id === id);
    const accessiblePage = id => available().some(page => page.id === id);

    function setView(view) {
      activeView = view;
      dialog.querySelectorAll('[data-help-view]').forEach(tab => {
        const selected = tab.dataset.helpView === view;
        tab.setAttribute('aria-selected', String(selected));
      });
      dialog.querySelectorAll('[data-help-pane]').forEach(pane => { pane.hidden = pane.dataset.helpPane !== view; });
      if (view === 'discover') renderContext();
      if (view === 'journeys') renderJourneys();
      if (view === 'guide') renderGuide();
    }

    function renderContext() {
      const page = pageFor(currentPage);
      context.replaceChildren();
      if (!page || !accessiblePage(page.id)) {
        context.hidden = true;
        return;
      }
      context.hidden = false;
      const copy = create('div', 'aurelis-help-context-copy');
      copy.append(create('span', 'aurelis-help-kicker', 'YOU ARE HERE · ' + page.group.toUpperCase()));
      copy.append(create('strong', '', page.title));
      copy.append(create('p', '', page.about));
      const go = button('Open area', 'aurelis-help-context-open', {'data-v2-page':page.id});
      go.setAttribute('aria-label', 'Open ' + page.title);
      context.append(copy, go);
    }

    function renderFilters() {
      filters.replaceChildren();
      ['All areas', ...groups.filter(group => available().some(page => page.group === group))].forEach(group => {
        const chip = button(group, 'aurelis-help-chip' + (activeGroup === group ? ' is-active' : ''), {'aria-pressed':String(activeGroup === group)});
        chip.addEventListener('click', () => { activeGroup = group; renderFilters(); renderResults(); });
        filters.append(chip);
      });
    }

    function renderResults() {
      const term = search.value.trim().toLocaleLowerCase();
      const items = available().filter(page => (activeGroup === 'All areas' || page.group === activeGroup) &&
        (!term || [page.title,page.group,page.about,page.example].some(value => value.toLocaleLowerCase().includes(term))));
      results.replaceChildren();
      items.forEach(page => {
        const card = button('', 'aurelis-help-result' + (page.id === currentPage ? ' is-current' : ''), {'data-v2-page':page.id, role:'listitem'});
        const top = create('span', 'aurelis-help-result-top');
        top.append(create('span', 'aurelis-help-result-group', page.group), create('span', 'aurelis-help-result-arrow', '↗'));
        card.append(top, create('strong', '', page.title), create('span', 'aurelis-help-result-about', page.about), create('span', 'aurelis-help-result-example', 'TRY · ' + page.example));
        results.append(card);
      });
      count.textContent = items.length + (items.length === 1 ? ' area' : ' areas');
      empty.hidden = items.length !== 0;
    }

    function renderJourneys() {
      const pane = dialog.querySelector('[data-help-pane="journeys"]');
      pane.replaceChildren();
      if (!journey) {
        pane.append(create('span','aurelis-help-kicker','A ROUTE THROUGH THE WORKFLOW'), create('h3','aurelis-help-view-title','Choose what you want to accomplish'));
        journeys.forEach((route, index) => {
          const accessibleSteps = route.steps.filter(([id]) => accessiblePage(id));
          if (!accessibleSteps.length) return;
          const card = button('', 'aurelis-help-journey-card', {});
          card.append(create('span','aurelis-help-journey-number',String(index + 1).padStart(2,'0')));
          const copy = create('span','aurelis-help-journey-copy');
          copy.append(create('strong','',route.title), create('span','',route.summary), create('small','',accessibleSteps.length + ' guided steps'));
          card.append(copy, create('span','aurelis-help-result-arrow','→'));
          card.addEventListener('click', () => { journey = {...route, steps:accessibleSteps}; journeyStep = 0; renderJourneys(); });
          pane.append(card);
        });
        return;
      }

      const step = journey.steps[journeyStep];
      const page = pageFor(step[0]);
      pane.append(button('← All guided routes','aurelis-help-back',{}));
      pane.append(create('span','aurelis-help-kicker','GUIDED ROUTE · ' + journey.title.toUpperCase()));
      pane.append(create('h3','aurelis-help-view-title',page.title));
      pane.append(create('p','aurelis-help-route-summary',step[1]));
      const progress = create('div','aurelis-help-progress');
      progress.setAttribute('role','progressbar');
      progress.setAttribute('aria-valuemin','1');
      progress.setAttribute('aria-valuemax',String(journey.steps.length));
      progress.setAttribute('aria-valuenow',String(journeyStep + 1));
      progress.append(create('span',''));
      progress.firstChild.style.width = ((journeyStep + 1) / journey.steps.length * 100) + '%';
      pane.append(progress, create('p','aurelis-help-step-label','STEP ' + String(journeyStep + 1).padStart(2,'0') + ' / ' + String(journey.steps.length).padStart(2,'0')));
      const actions = create('div','aurelis-help-route-actions');
      const open = button('Open ' + page.title,'aurelis-help-open-step',{});
      open.addEventListener('click', () => {
        if (window.Shiny?.setInputValue && accessiblePage(page.id)) {
          Shiny.setInputValue('v2_command',{page:page.id},{priority:'event'});
        }
      });
      actions.append(open);
      if (journeyStep > 0) {
        const previous = button('Previous','aurelis-help-secondary',{});
        previous.addEventListener('click', () => { journeyStep -= 1; renderJourneys(); });
        actions.append(previous);
      }
      if (journeyStep < journey.steps.length - 1) {
        const next = button('Next step →','aurelis-help-secondary',{});
        next.addEventListener('click', () => { journeyStep += 1; renderJourneys(); });
        actions.append(next);
      } else {
        const finish = button('Finish route','aurelis-help-secondary',{});
        finish.addEventListener('click', () => { journey = null; journeyStep = 0; renderJourneys(); });
        actions.append(finish);
      }
      pane.append(actions, create('p','aurelis-help-route-note','You can open this step and keep the guide beside you. The route never changes your data filters.'));
    }

    function renderGuide() {
      const pane = dialog.querySelector('[data-help-pane="guide"]');
      pane.replaceChildren(create('span','aurelis-help-kicker','AURELIS / COMPLETE MENU INDEX'),
        create('h3','aurelis-help-view-title','Every workspace, at a glance'),
        create('p','aurelis-help-route-summary','Choose a menu to navigate there. Search terms and examples are included in the repository’s ABOUT.md field guide.'));
      groups.forEach(group => {
        const items = available().filter(page => page.group === group);
        if (!items.length) return;
        pane.append(create('h4','aurelis-help-guide-group',group));
        items.forEach(page => {
          const card = button('', 'aurelis-help-guide-item', {'data-v2-page':page.id});
          card.append(create('strong','',page.title), create('span','',page.about), create('small','', 'EXAMPLE · ' + page.example));
          pane.append(card);
        });
      });
    }

    function open() {
      if (!dialog.open) dialog.showModal();
      journey = null;
      setView('discover');
      renderFilters();
      renderResults();
      search.focus();
    }

    [trigger,mobileTrigger].forEach(control => control.addEventListener('click', open));
    dialog.querySelector('.aurelis-help-close').addEventListener('click', () => dialog.close());
    dialog.querySelectorAll('[data-help-view]').forEach(tab => tab.addEventListener('click', () => setView(tab.dataset.helpView)));
    search.addEventListener('input', renderResults);
    dialog.addEventListener('click', event => {
      if (event.target === dialog) dialog.close();
      if (event.target.closest('[data-v2-page]')) dialog.close();
      if (event.target.closest('.aurelis-help-back')) { journey = null; journeyStep = 0; renderJourneys(); }
    });
    window.addEventListener('aurelis:pagechange', event => {
      if (!event.detail?.page) return;
      currentPage = event.detail.page;
      if (activeView === 'discover') renderContext();
      renderResults();
    });
    window.addEventListener('aurelis:capabilities', event => {
      allowedPages = Array.isArray(event.detail?.pages) ? event.detail.pages : [];
      if (!accessiblePage(currentPage)) currentPage = event.detail?.home || allowedPages[0] || '';
      renderFilters();
      renderResults();
      if (activeView === 'discover') renderContext();
      if (activeView === 'guide') renderGuide();
      if (activeView === 'journeys') renderJourneys();
    });
    document.addEventListener('keydown', event => {
      const target = event.target;
      const editing = target instanceof HTMLElement && (target.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(target.tagName));
      if (event.key === '?' && !editing && !event.ctrlKey && !event.metaKey && !event.altKey && !dialog.open) {
        event.preventDefault();
        open();
      }
    });
    renderFilters();
    renderResults();
    renderContext();
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup);
  else setup();
})();
