# Aurelis Operations Intelligence

An R Shiny / bs4Dash application. The redesign keeps the existing operations, customer, quotation, warehouse and probability workflows and adds an integrated Analysis Lab. All included company records are synthetic demonstration data.

## Run from R

Open the supplied `.Rproj`, then run:

```r
shiny::runApp(".")
```

Keep `app.R`, `R/`, `www/` and `data/` together. No Node, Python, Supabase account or external AI service is required. The new workspace uses the application's existing R packages.

## Working with data

- Open **Analysis Lab → Data Explorer**. The source selector discovers every supported table in `data/`, including subfolders. **Refresh files** rescans and rereads files; the app's existing refresh cycle also invalidates the Lab's source data.
- Supported: CSV, TSV, Excel workbooks with sheet selection, data-frame RDS, and tabular JSON (`jsonlite`). Other database formats need a connector; this build does not open Access, QuickBooks or a remote database.
- Open another local file using **Open another file / import options**. Imports are private to the current session and never overwrite database files. The interactive file size limit is 100 MB. CSV supports comma, semicolon and tab separators, UTF-8 and Windows-1252 text. ISO `YYYY-MM-DD` columns and Excel dates are recognized automatically.
- Add any number of columns under **Filter by any columns**. Numeric fields get minimum/maximum controls, dates get a range, categories get multiselects, and fields with over 500 unique values get literal text search. Filters combine with AND; the global text search matches any field. Blank category selections mean all values. Missing filter values are excluded unless explicitly enabled.
- Choose visible columns, search individual table columns, Shift-click headers to sort by multiple columns, and select a row for its full record. The table uses server-side paging.
- **Export CSV** includes all rows matching the table search, not just the visible page, and respects the selected columns. Text beginning with spreadsheet formula characters is prefixed with an apostrophe on export. This deliberately changes those exported text cells to prevent automatic formula execution.

## Scope and interpretation

The executive overview and existing pages use the header year/month controls. The Analysis Lab uses its own source and filters; header filters do not silently remove imported records. Structured Lab filters apply to every analysis. Table-only searches apply to the table and CSV export. Outlier removal applies to statistical analyses, not source records, financial ratio totals or the chart builder.

- **Profile:** distribution, box plot, normal Q-Q diagnostic, moment skewness/kurtosis, quantiles, IQR, sample variance and Student-t confidence interval for the mean. Quality checks count missing cells, exact duplicate rows and IQR outliers.
- **Tests:** one-sample t, Welch t, Mann-Whitney, Welch ANOVA, Kruskal-Wallis and Shapiro-Wilk. Alpha and one-/two-sided alternatives are configurable where supported. Cohen's d is reported for t-tests. Groups need at least three finite values. Tests assume appropriate independence; invoice lines from the same order may not be independent. No multiple-testing correction or causal inference is implied.
- **Forecast:** observed date aggregates and a linear baseline with prediction intervals. At least six consecutive calendar periods are required for the forecast. Missing periods are not fabricated; observed values remain available. Seasonality, autocorrelation and incomplete final periods can materially affect accuracy. This is not ARIMA or a validated production forecast.
- **Drivers:** Pearson/Spearman correlation and multiple linear regression (up to eight predictors), coefficient statistics, residual and Q-Q diagnostics. Correlations use pairwise complete rows; regression uses complete finite rows. Avoid putting arithmetic identities or identifiers into driver models.
- **Scenarios:** seeded empirical resampling of individual observations with configurable mean shift and dispersion. Base/best/worst use the selected mean shift and ±10 percentage points. This is a sensitivity model, not portfolio VaR or a cash-flow forecast. The caller's random state is preserved.
- **Financial metrics:** mapped revenue/cost/gross-profit ratios, outstanding invoice exposure and inventory quantity fulfillment. Definitions are visible beside each result. Aging uses the source snapshot's `Days_Past_Due`. Balance-sheet metrics such as ROE, liquidity and runway are omitted because their required data is absent. The investment calculator uses explicit user-entered constant annual cash flows and year-end discounting.
- **Chart builder:** bar/line charts aggregate means (bars show the top 30 categories); scatter uses individual records; box plots require at most 30 categories. Z-score and positive-only log transforms are available. Safe arithmetic formulas create a session-only `Calculated_Metric`; arbitrary R code is never evaluated.
- **Probability studio:** preserves the existing fitted-distribution and tail/quantile tools with their existing controls.

The decision brief and analyst brief are first-class surfaces driven by actual calculations and explicit rules. They are not generative AI. No data is sent to a model. Collaboration, production authentication, external database connections and generated AI require separate integrations.

Press **Ctrl+K** (or Cmd+K) to search and open any workspace. **Light mode / Dark mode** changes the workspace theme and remembers your browser preference; the navigation remains dark. Plotly's toolbar exports charts as images. The existing Report / Export controls remain available on their existing pages. The new Lab exports CSV; it does not promise a PDF or Excel analysis report.

## Files and validation

```text
app.R                     Existing application and integration
R/workspace_engine.R      Pure filtering, file reading, statistics and simulation
R/analysis_workspace.R    Shiny workspace module and executive UI
R/overview_server.R       Executive customer table and computed brief
www/workspace.css         Shared dark design system and responsive layout
www/workspace.js          Accessible keyboard command palette
tests/                    Engine, reactive-module, app and local browser checks
```

From the project root:

```r
source("tests/validate_workspace.R")
source("tests/smoke_app.R")
```

Reference checks cover means, variance, confidence intervals, p-values, compound filters, missing/empty selections, formula restrictions, reproducibility, random-state preservation, forecast values and file reading. Shiny checks exercise source switching, searching, ranges and calculated metrics. The browser helper is optional and expects a separately launched local Chrome debugging session on port 9224.
