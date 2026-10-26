# Aurelis Operations Intelligence — About & User Guide

This guide explains what each dashboard menu is for, how to work through it,
and an example question to try. The in-dashboard **Help** button opens a
searchable, role-aware field guide and guided task journeys without leaving the
page you are using. Press `?` to open it and `Esc` to close it.

## Before you begin

### Understand the data scope

- **Global year and month** are in the top bar. They apply to most business
  pages. Many pages also have their own filters, so check both before comparing
  figures.
- The **Analysis Lab** has its own source table and filters; the top-bar period
  does not control Lab analyses. Presentation Studio inherits either the Lab
  selection or its chosen dashboard source.
- Some values are snapshots, not historical time series. For example, an
  outstanding receivables balance grouped by invoice month is not a recorded
  month-end AR balance history.
- Charts are interactive: hover for details; click a supported mark to filter,
  drill into a record, or open a related workspace. Look for an on-screen
  interaction hint when unsure. Use a page’s reset control to clear its chart
  selection.
- **Report / Export** uses the current page’s report options. Tables may also
  have their own CSV/Excel downloads.

### Important demo-data notice

This public build is isolated and uses the included **synthetic demonstration
data**. It is not a connection to live accounting, production, warehouse, or
shared-drive systems. A refresh reloads the demonstration package; it does not
contact those systems. Treat generated metrics, quotes, scenario outputs, and
presentations as examples to review, not as verified business records or
financial advice. Check scope, units, definitions, and source quality before
sharing conclusions.

## Global navigation and controls

| Control | What it does | Example |
|---|---|---|
| Sidebar groups | Organize the pages into Executive Command, Commercial Performance, Supply Chain Control, Analyst Studio, Workspace & Studio, and Data & Relationships. Expand a group and select a page. | Open **Supply Chain Control → Orders & Delivery** to investigate late lines. |
| Top-bar year / month | Set the shared reporting period for pages that use the global scope. Page-level filters can narrow it further. | Choose 2025 and March, then review sales for that month. |
| Search | Search available business entities such as a customer, invoice, purchase order, or supplier; results take you to the relevant page. | Search an invoice reference to open its client statement. |
| Refresh | Reload the included demo data package and refresh dependent views. | After a local demo CSV update, refresh to reload the package. |
| Report / Export | Open the current page’s summary or report builder where supported. | Export a filtered receivables aging report. |
| Compact / Focus | Adjust information density or enter a reduced-distraction presentation view. | Use Focus when presenting the KPI page. |
| Priorities | Surface available receivables, overdue-delivery, and stock-reorder signals and jump to their workspaces. | Select an overdue invoice signal to review Accounts Receivable. |
| Help | Open the searchable field guide, see help for the current page, or follow a guided task route. | Search “forecast” or launch “Investigate a dataset.” |

## Executive Command

### Executive Overview

**Purpose:** A broad operating snapshot for revenue, gross profit, customer
concentration, customer margin, receivables exposure, and geographic or
year-over-year patterns. The client portfolio map places gross margin against
revenue share; bubble size represents outstanding balance. Clicking a client
opens its profile.

**Example:** Select one year in the top bar, compare the revenue trend, then
click a high-revenue client bubble to inspect its margin and account profile.

### KPI Command Center

**Purpose:** A management scorecard for recognized revenue, gross margin,
purchase-order value, open AR, fulfillment, backorders, active customers, and
active buyers. Trend, operational-health, customer-contribution, and buyer
workload views help show where a KPI is coming from.

**Example:** Set the reporting year, compare revenue and PO value, then inspect
the customer ranking for concentration. PO value is not the same measure as
recognized accounting revenue.

### Global Network Atlas

**Purpose:** Explore customer, supplier, and representative relationships by
location. Filter entity type, continent, country, region, city, and measure;
search an entity; select a map point or table row to synchronize its profile,
timeline, contacts, and relationship views.

**Example:** Show suppliers in one country, set map intensity to open exposure,
then select a supplier and use **Open Workspace** to investigate the connected
record.

### Executive Activity Observatory

**Purpose:** Review recorded past activity, currently tracked demo sessions,
future commitments, activity by role, and the access-governance view. This is
an operational/demo observability page, not a substitute for a production
identity or audit platform.

**Example:** Compare the past-activity list with future commitments to identify
what is recorded versus planned.

## Commercial Performance

### Sales & Profitability

**Purpose:** Filter recognized revenue by representative, country, and parent
company; compare revenue, gross profit, weighted gross margin, and transaction
counts. The margin target is adjustable. Clicking a country segment focuses
the page on that country; use **Clear Country Focus** to restore the full view.

**Example:** Select a representative and country, set a 30% margin target, and
compare the monthly revenue/cost/profit trend with the weighted-margin gauge.

### Customer Performance

**Purpose:** Build a relationship-level view for one customer: order and
revenue value, fulfillment and service measures supported by available fields,
portfolio standing, monthly trends, suppliers, buyers, and customer-ready
highlights. Choose full relationship history or the global period, then export
the customer file or configure a client-facing relationship PDF.

**Example:** Search for a customer, select full history, review completion
metrics and payment behavior, then prepare a one-page relationship review.

### Client Statements

**Purpose:** Inspect one customer’s invoices, total invoiced, remaining
balance, paid share, invoice count, aging groups, invoice timeline, and detailed
statement. Export the selected statement as CSV or use its report builder.

**Example:** Choose a client, select an overdue aging segment, and inspect the
invoices contributing to that balance.

### POs & Buyers

**Purpose:** Review purchase-order volume and value by buyer, customer, and
period. The heatmap shows PO counts; the buyer/month breakdown and trend show
recorded PO value. Open the detailed log for individual orders.

**Example:** Filter to one buyer and customer, compare monthly PO count with PO
value, then search the detailed log for the order to follow up.

### Buyer Activity

**Purpose:** Drill buyer workload from year to month, week of month, and day.
KPIs summarize distinct POs, value, customers, average PO, active buyers, and
active days; charts and the activity log show who handled what.

**Example:** Select a year and month, choose one buyer, and compare the daily
activity trend with the customer mix.

## Supply Chain Control

### Orders & Delivery

**Purpose:** Track orders and delivery schedules using customer, supplier,
buyer, PO/part text, status, and delivery-horizon filters. The page includes
open-order KPIs, status mix, exact-date workload, a zoomable risk matrix,
delivery calendar, customer exposure, and a prioritized schedule. A calendar
selection can also set the global year/month.

**Example:** Choose **Overdue**, compare open value with backordered units, and
select a workload period to focus the dashboard on that year/month.

### Inventory & Warehouse

**Purpose:** This page has two views:

- **Warehouse Availability** summarizes tracked items, on-hand, allocated, and
  available quantities, status, reorder risk, and warehouse value.
- **Inbound & Backorders** tracks units expected against open orders, by
  customer, supplier, status, delivery date, and part.

**Example:** In Warehouse Availability, filter a warehouse and click a
below-reorder status to inspect affected items. In Inbound & Backorders,
filter to a delivery month and supplier, then compare open value with units
awaiting receipt.

**Data limitation:** Warehouse quantities are shown only when a configured
source provides them. The public demo may explicitly report verified warehouse
stock as unavailable; do not infer physical availability from inbound orders.

### Suppliers & Procurement

**Purpose:** Compare suppliers by item, decision priority, historical median
unit cost, planned lead time, overdue/backorder rate, and open exposure. Search
the product/service directory, add a selected item to the active quotation,
and filter purchase-order lookups by supplier, customer, buyer, or PO/part.
Supplier comparison is a decision-support summary; check its selected
suppliers and scoring assumptions before using it.

**Example:** Enter a part number before comparing two suppliers, choose
**Fastest planned delivery**, inspect the score and underlying PO records, then
add the chosen item to the active quotation if appropriate.

### Accounts Receivable

**Purpose:** Prioritize collection work using open-balance KPIs, aging groups,
invoice trends, balances by responsible staff, customer exposure, and the full
AR ledger. Clicking aging segments filters the AR views; selecting a customer
or ledger row can open the client statement.

**Example:** Select the more-than-30-days-overdue balance, rank the top customer
exposures, then open a customer’s statement to review the invoices.

## Analyst Studio

### Analysis Lab

**Purpose:** An independent data workspace for exploring catalogued local
tables or a session-only upload. Choose a table and measure, search records,
create field filters, inspect schema and data quality, and export the filtered
rows. The tabs provide:

- **Data Explorer:** sortable, searchable records, selected-row inspection,
  schema and completeness checks.
- **Profile:** distribution, descriptive statistics, box plot, and normal Q-Q
  diagnostic.
- **Hypothesis tests:** one-sample and Welch t tests, Mann–Whitney, Welch
  ANOVA, Kruskal–Wallis, and Shapiro–Wilk with group summaries.
- **Trends & forecast:** aggregate a date-based measure by week, month, quarter,
  or year and view the baseline forecast and prediction interval.
- **Drivers:** correlations, regression coefficients, and residual diagnostics.
- **Scenarios:** reproducible empirical resampling with configurable mean
  shift, dispersion, and iterations.
- **Financial metrics:** selected descriptive/financial calculations and
  user-entered investment sensitivity assumptions.

**Example:** Select the revenue table, filter to one country, choose Revenue as
the measure, inspect its profile, and compare monthly totals. Then export the
filtered table if needed.

**Interpretation:** Lab filters are independent of the global year/month.
Forecasts and simulations are analytical sensitivities, not commitments or
validated trading/financial forecasts. Statistical association does not prove
causation; repeated invoice/order rows may not be independent.

### Probability Studio

**Purpose:** A dedicated probability and statistical-comparison workspace.
Choose a dataset, numeric measure, model (Normal, Student t, F, Log-normal,
Exponential, Gamma, Poisson, or Empirical), optional groups, and a probability
question. Review the observed distribution, fitted model, Q-Q check, group
distributions, group tests, summaries, and confidence intervals.

**Example:** Select revenue and the Empirical model, set probability mode to
**Right tail**, and enter a threshold to estimate the share of selected
observations above it. For a fitted Normal model, compare that result with the
model-based estimate.

**Interpretation:** Probability is conditional on the selected observations,
filters, and model assumptions. A fitted distribution is not proof that future
outcomes follow that model.

### Presentation Studio

**Purpose:** Turn selected evidence into an editable presentation. Choose an
audience, decision question, data source, measure, segment, date scope,
aggregation, and optional target; generate a draft; edit slide titles, key
points, and speaker notes; export an HTML deck or editable Markdown outline.

**Example:** Choose the current Lab selection, group Revenue by Country, set the
decision question to “Which regions should we investigate?”, generate a local
draft, verify each slide against the source, then download the outline.

**Privacy:** Local drafting is available. If AI-assisted drafting is enabled,
read the on-screen consent and disclosure before submitting. Only the stated
computed summary, category labels, and brief are sent; do not include sensitive
information. Review all generated claims before presenting.

## Workspace & Studio

### My Workspace

**Purpose:** Select a demo workspace owner and access profile; review saved
quotation drafts, current demo sessions, and recent collaboration activity.
Shared drafts can be shown when the corresponding option is enabled.

**Example:** Inspect the draft library for a saved quotation and check whether
it is private or shared. The demo profile selector illustrates role-aware
navigation when role limits are configured; it does not authenticate a user or
itself guarantee that pages are restricted.

**Access note:** Public demo profiles illustrate role-aware navigation. They
are not production authentication or a guarantee of data confidentiality.

### Quotation Studio

**Purpose:** Prepare an editable customer quotation in a guided sequence:
identify the customer, review history, build item costs, add landed costs,
model financing, apply pricing policy, compare scenarios, and preview/export
the customer document. Drafts may be saved or shared with buyers in the demo
workspace.

**Example:** Select a customer, clone a historical PO into a new editable
quote, verify supplier cost and delivery assumptions, enter order-specific
freight and financing, review the final price bridge, then export the quote.

**Caution:** Cloning copies lines into the draft; it does not change the
historical PO. Confirm every cost, currency, margin, tax, and payment
assumption before sending a real commercial offer.

### Product Intelligence

**Purpose:** Get a ranked product recommendation using a search brief, target
unit-cost slider, category, stock availability, and result count. The ranking
balances relevance, seller preference, price fit, lead time, and availability.
Inspect the recommendation table and cost/delivery landscape, browse the full
catalog, and add a selected recommendation or catalog item to the active quote.

**Example:** Search for “pump”, set the target unit cost, choose **In stock**,
review the leading recommendations, then add a verified catalog item to the
active quotation.

## Data & Relationships

### Directory & Relationships

**Purpose:** Review contacts and business relationships connected to customers,
suppliers, buyers, and representatives; use the activity and counterparty
visualizations to move between a relationship and its business context. Where
the public demo offers client registration, new entries are written only to
the demo client file.

**Example:** Search a customer contact, inspect monthly activity and its
counterparties, then open the related customer workspace.

### Demo Data & Process

**Purpose:** Understand the isolated demo package, test its included CSV
package, inspect source schema and data-contract tables, and check refresh
status. It also contains an operational checklist and a quick-note box; notes
are session-only and are not persisted. External accounting, production
databases, and shared drives are disabled in this public build.

**Example:** Before interpreting an inventory figure, read the data contract
and confirm whether the displayed quantity is verified warehouse stock or
inbound order quantity.

## A few useful journeys

1. **Management review:** Executive Overview → KPI Command Center → Sales &
   Profitability → Executive Activity Observatory.
2. **Customer collection review:** Executive Overview → Customer Performance
   → Client Statements → Accounts Receivable.
3. **Evidence to briefing:** Analysis Lab → Probability Studio (if a
   probability question is relevant) → Presentation Studio.
4. **Quote to fulfilment:** Product Intelligence → Quotation Studio → POs &
   Buyers → Orders & Delivery.

The in-dashboard Help drawer offers these as guided routes with step-by-step
notes and one-click navigation. Pages not enabled for the current access
profile are omitted from its discovery results.
