# AURELIS interface refinement — 17 October 2026

The v2 presentation layer applies the supplied proposals to the existing R Shiny application. It retains the business engines, source CSVs, global year/month filters, independent Analysis Lab scope, existing output IDs and download handlers.

## What changed

- **Design system:** locally hosted Inter variable font, obsidian surfaces, champagne actions/navigation, separate success/warning/risk colors, larger captions, layered borders and restrained motion. Both themes share the tokens in `www/aurelis-theme.css`.
- **Executive overview:** four reusable KPI cards, compact values with exact-value tooltips, monthly sparklines, unit/scope descriptions, period-comparison chips, action links in the decision brief, mobile client cards and deferred supplementary views. More than five representatives use sorted bars rather than an overcrowded profit ring.
- **Scope:** visible global-period chips with individual reset controls; the Lab explicitly states that its own source/filters apply. Page-specific filters continue to apply as before.
- **Navigation:** fuzzy workspace/action search, bounded server-side client/supplier/order/invoice lookup, pinned workspaces, recent pages, theme/density preferences, profile-aware mobile destinations and last-page restoration. Browser preferences are convenience state, not authorization.
- **Operational priorities:** computed past-due balance, overdue delivery-line count and stock lines below reorder point. Each card states its scope and opens its existing detail page. These are deterministic rules, not AI predictions or outbound notifications.
- **Quotation:** a sticky eight-section navigator, accessible links to every existing section, a persistent total copied from the actual calculation, and a successful catalog addition opening line items. No fields are hidden and no pricing rules change.
- **Charts:** shared Plotly defaults for the Lab plus client-side theme synchronization for legacy widgets, chart aggregation/unit captions, themed annotations/indicators, consistent ring proportions and debounced resize. Original selection keys and semantic category colors are preserved.
- **Mobile:** two-column KPI cards, stacked executive client records, a modal period-filter sheet, primary bottom navigation and larger actions. Wider operational tables retain internal horizontal scrolling and their full columns.
- **Lab:** the same theme, a sticky desktop filter-result summary and a populated-cell percentage with an explicit completeness definition. Completeness is not an accuracy or validity score.
- **Local assets:** bundled font/license and Plotly world geometry avoid new runtime font/map CDN requests. The legacy bs4Dash dependency may still reference its own optional external stylesheet assets.

## Period comparisons and statistical integrity

An explicit year compares to the preceding calendar year. An explicit year/month compares to the preceding calendar month, including January rollover. All-years views, absent comparison records and zero denominators show no comparison. The comparison is descriptive, and its direction is not automatically labeled good or bad.

Outstanding receivables have no historical balance delta: the source is an invoice snapshot. Its sparkline groups remaining balance by invoice month and is labeled accordingly. No fake historical cash balance, confidence band, target, AI result or simulation has been added to the executive view.

The original PO KPI still counts its tracked order rows; no formula has been silently changed to a distinct count. The source package has one PO row per tracked order.

## Existing defects repaired during review

- Customer relationship history lacked the `History_Source` column expected by its table/report. The fallback adds a provenance label without changing records or financial calculations.
- The relationship-flow limit was a character select input coalesced with a numeric default, causing a dplyr type error. The default now has the matching type before integer conversion.

## Enable / rollback

V2 is enabled by default. To use the previous interface, set the environment variable **before sourcing or launching the app**:

```r
Sys.setenv(AURELIS_THEME_V2 = "false")
shiny::runApp(".")
```

To return to v2, set it to `"true"` and restart. The presentation assets and new components are gated by this flag. The two data-display bug fixes remain active.

Preferences are stored locally per workspace-owner name. A saved allowed page takes precedence over the profile's initial home; otherwise CEO starts on Overview, Manager on KPI Center, Analyst on Analysis Lab and Buyer on Quotation Studio. Access still uses the application's existing server-side checks.

## Files

| File | Responsibility |
|---|---|
| `R/aurelis_theme.R` | Assets, reusable KPI UI, comparisons, category chart and presentation server commands |
| `www/aurelis-theme.css` | Theme tokens, components, density, responsiveness and reduced motion |
| `www/aurelis-ui.js` | Search, preferences, theme synchronization, presentation mode, quotation navigator and mobile shell |
| `R/overview_server.R` | Existing brief plus action links and mobile client summaries |
| `R/analysis_workspace.R` | V2 UI dispatch, shared plotting helper and completeness badge |
| `app.R` | Small integration points and the two existing display fixes |

No new application package is required for the visual layer. `openxlsx` and `pagedown` were already optional dependencies of the existing exports and were installed in the R 4.5 user library for download verification. Inter is bundled under its SIL Open Font License (`www/Inter-LICENSE.txt`). World geometry was obtained from Plotly's public CDN and derives from Natural Earth.

## Validation commands

```r
source("tests/validate_workspace.R")
source("tests/validate_theme.R")
source("tests/smoke_app.R")
```

Run the rollback check in a separate R process: `Rscript tests/validate_theme_fallback.R`.

`tests/run_preview_v2.R` starts a local review server on port 3876 with activity-file logging disabled, so browser tests do not alter the demo source files. `tests/browser.ps1` talks to a separately launched local Chrome debugging session on port 9224; browser scripts exercise actual Shiny controls and widgets. Review captures live in `exports/`.

## Deliberate limits

This refinement does not implement distributed collaboration, durable per-user preferences, new authentication, generative AI, asynchronous statistical jobs, or an entirely new drag-and-drop pricing engine. No production load or Lighthouse score is claimed. Mobile record cards currently cover the executive client view; large operational ledgers retain their table interactions. Contrast checks cover defined token pairs and are not a full accessibility certification. Forecasting and statistical limitations in the README remain applicable.
