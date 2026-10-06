################################################################################
# Title: AURELIS Global Supply — Interactive Operations Dashboard
# Author: Armel Asopjio
# Build Type: Public Advertising / Advisory Demonstration
# Synthetic data range: 2015-01-01 through 2026-08-12
# Public build: Aurelis Obsidian 
################################################################################
# PUBLIC ATLAS BUILD: 2026-08-12
# SECTION 1: LIBRARIES & CONFIGURATION
################################################################################

library(shiny)
library(bs4Dash)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(readxl)
library(stringr)
library(lubridate)
library(scales)
library(DT)
library(shinycssloaders)
library(tibble)
library(treemap)
library(viridis)

AURELIS_MULTIUSER_ENABLED <- TRUE
.aurelis_demo_state <- new.env(parent = emptyenv())
.aurelis_demo_state$sessions <- tibble(session_id=character(),buyer=character(),role=character(),started_at=as.POSIXct(character()),last_seen=as.POSIXct(character()))
.aurelis_demo_state$drafts <- tibble(draft_id=character(),owner=character(),quote_number=character(),customer=character(),is_shared=integer(),version=integer(),saved_at=character(),payload_json=character())
.aurelis_demo_state$activity <- tibble(session_id=character(),buyer=character(),role=character(),event_type=character(),detail=character(),event_at=character())

aurelis_multiuser_register_session <- function(session_id,buyer,role="Buyer") {
  now <- Sys.time()
  .aurelis_demo_state$sessions <- bind_rows(
    .aurelis_demo_state$sessions %>% filter(session_id != !!session_id),
    tibble(session_id=session_id,buyer=buyer,role=role,started_at=now,last_seen=now)
  )
  invisible(TRUE)
}
aurelis_multiuser_touch_session <- function(session_id,buyer,role="Buyer") {
  df <- .aurelis_demo_state$sessions
  if (!session_id %in% df$session_id) return(aurelis_multiuser_register_session(session_id,buyer))
  df$buyer[df$session_id==session_id] <- buyer
  if ("role" %in% names(df)) df$role[df$session_id==session_id] <- role
  df$last_seen[df$session_id==session_id] <- Sys.time()
  .aurelis_demo_state$sessions <- df
  invisible(TRUE)
}
aurelis_multiuser_close_session <- function(session_id) {
  .aurelis_demo_state$sessions <- .aurelis_demo_state$sessions %>% filter(session_id != !!session_id)
  invisible(TRUE)
}
aurelis_multiuser_active_sessions <- function(window_minutes=10) {
  cutoff <- Sys.time() - as.numeric(window_minutes)*60
  .aurelis_demo_state$sessions %>% filter(last_seen >= cutoff) %>% arrange(desc(last_seen))
}
aurelis_multiuser_log <- function(session_id,buyer,event_type,detail="",role="Buyer") {
  event <- tibble(
    session_id=session_id,buyer=buyer,role=role,event_type=event_type,detail=detail,event_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S")
  )
  .aurelis_demo_state$activity <- bind_rows(.aurelis_demo_state$activity, event) %>% tail(500)
  if (exists("AURELIS_DATA_DIR", inherits = TRUE)) {
    path <- file.path(AURELIS_DATA_DIR, "aurelis_activity_log.csv")
    try(
      write.table(event, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path), qmethod = "double"),
      silent = TRUE
    )
  }
  invisible(TRUE)
}
aurelis_multiuser_recent_activity <- function(limit=60) {
  .aurelis_demo_state$activity %>% tail(limit) %>% arrange(desc(event_at))
}
aurelis_multiuser_list_drafts <- function(owner,include_shared=TRUE) {
  df <- .aurelis_demo_state$drafts
  if (include_shared) df %>% filter(owner == !!owner | is_shared == 1L) else df %>% filter(owner == !!owner)
}
aurelis_multiuser_get_draft <- function(draft_id) {
  df <- .aurelis_demo_state$drafts %>% filter(draft_id == !!draft_id)
  if (nrow(df)==0) NULL else df
}
aurelis_multiuser_save_draft <- function(owner,quote_number,customer,payload_json,is_shared=FALSE,draft_id="") {
  df <- .aurelis_demo_state$drafts
  if (is.null(draft_id) || !nzchar(draft_id) || !draft_id %in% df$draft_id) {
    draft_id <- paste0("D-",format(Sys.time(),"%Y%m%d%H%M%S"),"-",sprintf("%04d",sample(1:9999,1)))
    version <- 1L
  } else {
    version <- as.integer(df$version[df$draft_id==draft_id][1]) + 1L
    df <- df %>% filter(draft_id != !!draft_id)
  }
  .aurelis_demo_state$drafts <- bind_rows(df,tibble(
    draft_id=draft_id,owner=owner,quote_number=quote_number,customer=customer,
    is_shared=ifelse(isTRUE(is_shared),1L,0L),version=version,
    saved_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),payload_json=payload_json
  ))
  list(draft_id=draft_id,version=version)
}
aurelis_multiuser_delete_draft <- function(draft_id,owner) {
  df <- .aurelis_demo_state$drafts
  target <- df %>% filter(draft_id == !!draft_id)
  if (nrow(target)==0 || !identical(as.character(target$owner[[1]]),as.character(owner))) return(FALSE)
  .aurelis_demo_state$drafts <- df %>% filter(draft_id != !!draft_id)
  TRUE
}

# Optional packages are loaded only when their feature is used.
# Install once in the R console if needed:
# install.packages(c("openxlsx", "pagedown", "base64enc", "DBI", "odbc", "httr2", "jsonlite", "RSQLite"))
#
# Production data architecture:
#   Microsoft Access = read-only operational/warehouse source through ODBC
#   QuickBooks = deferred until company-file Admin access is available
#   Excel = controlled fallback while live field mappings are validated
#
# Run RUN_ME_FIRST.R once. QuickBooks is skipped in this clean package.
# No QODBC license, API key, OAuth token, or per-request fee is used.

# Aurelis brand-aligned, low-fatigue color system.
# The palette favors navy, ocean blue, muted teal, sage, amber and slate.
# Obsidian Command visualization palette.
brand_navy   <- "#111827"
brand_blue   <- "#FF7A45"
brand_sky    <- "#FFB15C"
brand_teal   <- "#32D1C6"
brand_sage   <- "#63D99A"
brand_amber  <- "#FFC857"
brand_coral  <- "#F0544F"
brand_slate  <- "#8797AD"
brand_ink    <- "#EAF1F7"
brand_mist   <- "#18222E"
brand_cloud  <- "#111922"
brand_border <- "#2A3543"

pal_executive   <- c("#FF7A45", "#FFC857", "#F0544F", "#32D1C6", "#8A72F5", "#63D99A")
pal_sales       <- c("#FF7A45", "#FFB15C", "#FFC857", "#32D1C6", "#63D99A", "#8A72F5")
pal_performance <- c("#FFC857", "#FF7A45", "#32D1C6", "#8A72F5", "#63D99A", "#F0544F")
pal_ar          <- c("#63D99A", "#32D1C6", "#FFC857", "#FF8A5C", "#F0544F", "#8A72F5")
pal_contacts    <- c("#32D1C6", "#FF7A45", "#FFC857", "#63D99A", "#8A72F5")
pal_buyer       <- c("#FFC857", "#FF7A45", "#32D1C6", "#63D99A", "#8A72F5")

# Custom CSS: professional, accessible, responsive and low visual fatigue.
custom_css <- "
:root {
  --aurelis-navy: #17324D;
  --aurelis-blue: #2F6EA5;
  --aurelis-sky: #6FA8D6;
  --aurelis-teal: #3C7474;
  --aurelis-sage: #5F7D67;
  --aurelis-amber: #9A6A27;
  --aurelis-coral: #A65359;
  --aurelis-slate: #64748B;
  --aurelis-ink: #1F2937;
  --aurelis-muted: #5E6B78;
  --aurelis-mist: #EEF4F8;
  --aurelis-cloud: #F7FAFC;
  --aurelis-border: #D7E1EA;
}

html, body, .wrapper {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
  color: var(--aurelis-ink);
}

.content-wrapper {
  background: linear-gradient(180deg, #F8FBFD 0%, #EEF4F8 100%);
  min-height: 100vh;
}

.content {
  padding-top: 16px;
}

/* Wider desktop sidebar so business labels remain readable. */
@media (min-width: 992px) {
  body:not(.sidebar-collapse) .main-sidebar,
  body:not(.sidebar-collapse) .main-sidebar::before {
    width: 332px !important;
  }

  body:not(.sidebar-collapse) .content-wrapper,
  body:not(.sidebar-collapse) .main-header,
  body:not(.sidebar-collapse) .main-footer {
    margin-left: 332px !important;
  }
}

.main-sidebar {
  background: #FFFFFF !important;
  border-right: 1px solid var(--aurelis-border) !important;
  box-shadow: 3px 0 18px rgba(23, 50, 77, 0.06);
}

.brand-link {
  height: 74px !important;
  padding: 10px 14px !important;
  border-bottom: 1px solid var(--aurelis-border) !important;
  display: flex !important;
  align-items: center !important;
}

.brand-link .brand-image {
  float: none !important;
  max-height: 49px !important;
  width: auto !important;
  margin: 0 10px 0 0 !important;
  opacity: 1 !important;
}

.brand-link .brand-text {
  color: var(--aurelis-navy) !important;
  font-size: 16px;
  font-weight: 700 !important;
  letter-spacing: 0.2px;
  white-space: normal;
  line-height: 1.15;
}

.nav-sidebar .nav-link {
  color: #425466 !important;
  font-weight: 600;
  padding: 10px 14px;
  border-radius: 9px;
  margin: 3px 9px;
  transition: background-color .18s ease, color .18s ease, transform .18s ease;
  white-space: normal;
  line-height: 1.25;
}

.nav-sidebar .nav-link:hover {
  background: #EAF2F8 !important;
  color: var(--aurelis-blue) !important;
  transform: translateX(2px);
}

.nav-sidebar .nav-link.active {
  background: var(--aurelis-navy) !important;
  color: #FFFFFF !important;
  box-shadow: 0 5px 14px rgba(23, 50, 77, 0.18);
}

.nav-sidebar .nav-link i {
  width: 22px;
  margin-right: 9px;
  text-align: center;
}

.main-header {
  background: rgba(255,255,255,.97) !important;
  border-bottom: 1px solid var(--aurelis-border) !important;
  box-shadow: 0 2px 10px rgba(23, 50, 77, 0.05);
}

.main-header .navbar-nav .nav-link {
  color: #425466 !important;
}

.selectize-dropdown,
.selectize-dropdown-content,
.selectize-control .selectize-dropdown {
  z-index: 9999 !important;
}

.selectize-dropdown-content {
  max-height: 320px !important;
  overflow-y: auto !important;
}

.modal, .modal-content, .modal-body, .modal-dialog,
.card, .card-body, .row,
.col-sm-3, .col-md-3, .col-lg-3, .col-3 {
  overflow: visible !important;
}

.card {
  background: #FFFFFF;
  border: 1px solid rgba(215, 225, 234, .9);
  border-radius: 13px;
  box-shadow: 0 3px 14px rgba(23, 50, 77, .055);
  margin-bottom: 19px;
  transition: box-shadow .2s ease, transform .2s ease;
}

.card:hover {
  box-shadow: 0 8px 24px rgba(23, 50, 77, .09);
  transform: translateY(-1px);
}

.card-header {
  background: #FFFFFF;
  border-bottom: 1px solid #E7EEF4;
  padding: 15px 18px;
  color: var(--aurelis-navy);
  border-radius: 13px 13px 0 0 !important;
}

.card-header .card-title {
  color: var(--aurelis-navy);
  font-weight: 700;
  font-size: 1.03rem;
}

.kpi-card {
  background: #FFFFFF !important;
  border: 1px solid var(--aurelis-border);
  border-left: 5px solid var(--kpi-accent, var(--aurelis-blue));
  border-radius: 13px;
  padding: 20px 18px;
  min-height: 130px;
  color: var(--aurelis-ink) !important;
  box-shadow: 0 3px 13px rgba(23, 50, 77, .06);
  margin-bottom: 15px;
  transition: box-shadow .2s ease, transform .2s ease;
}

.kpi-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 22px rgba(23, 50, 77, .10);
}

.kpi-card h2 {
  color: var(--aurelis-navy) !important;
  font-size: 2.05rem;
  font-weight: 750;
  margin: 7px 0 3px;
  letter-spacing: -.35px;
}

.kpi-card p {
  color: #586878 !important;
  font-size: .79rem;
  font-weight: 700;
  margin: 0;
  text-transform: uppercase;
  letter-spacing: .65px;
}

.kpi-card .sub-text {
  color: #718096 !important;
  font-size: .77rem;
  font-weight: 500;
  text-transform: none;
  letter-spacing: 0;
  margin-top: 4px;
}

.kpi-blue  { --kpi-accent: var(--aurelis-blue); }
.kpi-teal  { --kpi-accent: var(--aurelis-teal); }
.kpi-sage  { --kpi-accent: var(--aurelis-sage); }
.kpi-amber { --kpi-accent: var(--aurelis-amber); }
.kpi-coral { --kpi-accent: var(--aurelis-coral); }
.kpi-navy  { --kpi-accent: var(--aurelis-navy); }

.small-box, .info-box {
  border-radius: 12px;
  box-shadow: 0 3px 12px rgba(23, 50, 77, .06);
  transition: transform .2s ease, box-shadow .2s ease;
}

.small-box:hover, .info-box:hover {
  transform: translateY(-1px);
  box-shadow: 0 7px 20px rgba(23, 50, 77, .09);
}

.filter-panel {
  background: #FFFFFF;
  border: 1px solid var(--aurelis-border);
  border-radius: 12px;
  padding: 15px 18px;
  margin-bottom: 18px;
}

.selectize-input, .form-control {
  border-radius: 8px !important;
  border-color: #C9D6E1 !important;
}

.selectize-input.focus, .form-control:focus {
  border-color: var(--aurelis-blue) !important;
  box-shadow: 0 0 0 3px rgba(47, 110, 165, .13) !important;
}

.btn {
  border-radius: 8px !important;
  font-weight: 650 !important;
  transition: transform .15s ease, box-shadow .15s ease !important;
}

.btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 4px 10px rgba(23, 50, 77, .12);
}

.btn-primary {
  background: var(--aurelis-blue) !important;
  border-color: var(--aurelis-blue) !important;
}

.btn-success {
  background: var(--aurelis-teal) !important;
  border-color: var(--aurelis-teal) !important;
}

.btn-warning {
  background: var(--aurelis-amber) !important;
  border-color: var(--aurelis-amber) !important;
  color: #FFFFFF !important;
}

.btn-danger {
  background: var(--aurelis-coral) !important;
  border-color: var(--aurelis-coral) !important;
}

.navbar-report-button {
  margin-right: 10px !important;
  color: #FFFFFF !important;
  background: var(--aurelis-blue) !important;
  border: none !important;
  box-shadow: 0 4px 11px rgba(47, 110, 165, .22);
}

.report-action-bar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
  padding: 12px 14px;
  margin: 13px 0 4px;
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  background: #F6F9FC;
}

.report-action-bar .report-help {
  color: #5F6F7F;
  font-size: .87rem;
  margin: 0;
  flex: 1 1 340px;
}

.report-action-buttons {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}

.interaction-tip {
  display: flex;
  align-items: flex-start;
  gap: 9px;
  padding: 10px 13px;
  margin: 6px 0 15px;
  border-left: 4px solid var(--aurelis-blue);
  border-radius: 7px;
  background: #F1F6FA;
  color: #526273;
  font-size: .86rem;
}

.metric-definition {
  padding: 11px 14px;
  border-radius: 8px;
  background: #F5F8FB;
  border: 1px solid #DCE7F0;
  color: #485A6C;
  font-size: .88rem;
  line-height: 1.45;
}

.definition-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(225px, 1fr));
  gap: 10px;
  margin: 8px 0 16px;
}

.definition-card {
  background: #FFFFFF;
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  padding: 12px 13px;
}

.definition-card strong {
  display: block;
  color: var(--aurelis-navy);
  margin-bottom: 3px;
}

.status-chip {
  display: inline-block;
  border-radius: 999px;
  padding: 4px 9px;
  font-size: .78rem;
  font-weight: 700;
  background: #EAF2F8;
  color: var(--aurelis-navy);
}

.delivery-filter-note, .data-quality-note {
  padding: 10px 14px;
  margin-top: 5px;
  background: #F5F8FB;
  border-left: 4px solid var(--aurelis-blue);
  border-radius: 7px;
  color: #526273;
  font-size: .87rem;
}

.data-quality-note.warning {
  border-left-color: var(--aurelis-amber);
  background: #FBF8F1;
}

.report-modal-status {
  padding: 10px 12px;
  border-radius: 8px;
  background: #F1F5F9;
  color: #334155;
  margin-bottom: 12px;
}

.dataTables_wrapper {
  padding: 8px 0;
}

.dataTables_wrapper .dataTables_filter input {
  border-radius: 8px !important;
  border: 1px solid #C9D6E1 !important;
  padding: 6px 10px !important;
}

table.dataTable thead th {
  color: var(--aurelis-navy);
  background: #F4F8FB;
}

.nav-tabs .nav-link {
  border-radius: 8px 8px 0 0 !important;
  color: #526273 !important;
  font-weight: 650 !important;
}

.nav-tabs .nav-link.active {
  color: var(--aurelis-blue) !important;
  border-bottom: 3px solid var(--aurelis-blue) !important;
}

.plotly-container {
  border-radius: 8px;
}

::-webkit-scrollbar {
  width: 7px;
  height: 7px;
}

::-webkit-scrollbar-track {
  background: #EDF2F6;
}

::-webkit-scrollbar-thumb {
  background: #AEBECC;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: #8FA3B3;
}


.profile-panel {
  background: #F8FAFC;
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  padding: 13px 14px;
  margin-bottom: 10px;
}

.profile-label {
  color: #64748B;
  font-size: .75rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .55px;
  margin-bottom: 2px;
}

.profile-value {
  color: var(--aurelis-navy);
  font-weight: 700;
  margin-bottom: 10px;
  word-break: break-word;
}

.source-badge {
  display: inline-block;
  margin: 2px 5px 2px 0;
  padding: 4px 8px;
  border-radius: 999px;
  background: #EAF2F8;
  color: var(--aurelis-navy);
  font-size: .76rem;
  font-weight: 700;
}

.directory-note {
  padding: 10px 13px;
  margin-top: 7px;
  border-radius: 8px;
  background: #F1F6FA;
  border-left: 4px solid var(--aurelis-teal);
  color: #526273;
  font-size: .86rem;
}

.highlight-list {
  padding-left: 20px;
  margin-bottom: 0;
}

.highlight-list li {
  margin-bottom: 7px;
  color: #455769;
}

.connection-code {
  display: block;
  max-height: 310px;
  overflow: auto;
  padding: 13px;
  border-radius: 9px;
  background: #172B3E;
  color: #E6EEF5;
  font-size: .79rem;
  white-space: pre-wrap;
}

@media (max-width: 991px) {
  .brand-link { height: 62px !important; }
  .kpi-card h2 { font-size: 1.75rem; }
}

@media (max-width: 768px) {
  .card { margin-bottom: 14px; }
  .report-action-bar { align-items: stretch; }
  .report-action-buttons { width: 100%; }
  .report-action-buttons .btn { flex: 1; }
}

/* -------------------------------------------------------------------------
   GLOBAL SELECT / DROPDOWN LAYER FIX
   Selectize menus must float above every KPI, card, plot and table. Raising
   only the dropdown itself is insufficient when a parent card creates a CSS
   stacking context, so the active card and row are elevated as well.
   ------------------------------------------------------------------------- */
.content-wrapper,
.content-wrapper > .content,
.content,
.container-fluid,
.card,
.card-body,
.row,
[class*='col-'] {
  overflow: visible !important;
}

.card,
.kpi-card,
.small-box,
.info-box,
.plotly,
.html-widget,
.dataTables_wrapper {
  position: relative;
  z-index: 1;
}

.selectize-control {
  position: relative !important;
  z-index: 40 !important;
}

.selectize-control.dropdown-active,
.selectize-control.input-active {
  z-index: 2147483600 !important;
}

.selectize-dropdown {
  position: absolute !important;
  z-index: 2147483647 !important;
  box-shadow: 0 16px 38px rgba(23, 50, 77, .20) !important;
  border: 1px solid #B9C9D7 !important;
  background: #FFFFFF !important;
}

.selectize-host-active {
  position: relative !important;
  z-index: 2147483500 !important;
  overflow: visible !important;
}

.js-plotly-plot .slice,
.js-plotly-plot .point,
.js-plotly-plot .heatmaplayer {
  cursor: pointer;
}

.card:has(.selectize-control.dropdown-active),
.row:has(.selectize-control.dropdown-active),
[class*='col-']:has(.selectize-control.dropdown-active) {
  position: relative !important;
  z-index: 2147483500 !important;
  overflow: visible !important;
}

/* -------------------------------------------------------------------------
   AURELIS BRAND PLACEMENT
   The owner-supplied logo is used in the sidebar, navbar, footer and as a
   subtle non-interactive content watermark.
   ------------------------------------------------------------------------- */
.brand-link .brand-image {
  max-height: 52px !important;
  max-width: 184px !important;
  object-fit: contain;
}

.brand-link .brand-text {
  font-size: 14px !important;
  max-width: 105px;
}

.header-logo-lockup {
  gap: 9px;
  margin-right: 18px;
  padding: 5px 12px;
  border-right: 1px solid var(--aurelis-border);
}

.header-logo-lockup img {
  width: 92px;
  height: 34px;
  object-fit: contain;
}

.header-logo-lockup span {
  color: var(--aurelis-navy);
  font-weight: 700;
  font-size: .82rem;
  white-space: nowrap;
}

.aurelis-content-watermark {
  position: fixed;
  right: 28px;
  bottom: 42px;
  width: 300px;
  max-width: 24vw;
  opacity: .025;
  pointer-events: none;
  z-index: 0;
  filter: grayscale(15%);
}

.aurelis-content-watermark img {
  display: block;
  width: 100%;
  height: auto;
}

.content-wrapper > .content,
.content-wrapper > .content-header {
  position: relative;
  z-index: 1;
}

.footer-brand {
  display: inline-flex;
  align-items: center;
  gap: 9px;
}

.footer-brand img {
  width: 78px;
  height: 29px;
  object-fit: contain;
}

@media (max-width: 1199px) {
  .header-logo-lockup { display: none !important; }
}

@media (max-width: 768px) {
  .aurelis-content-watermark { display: none; }
  .brand-link .brand-text { display: none !important; }
}

/* -------------------------------------------------------------------------
   FINAL BRAND POLISH
   The sidebar logo fills the complete brand block, the desktop navbar lockup
   stays balanced, and each page receives one restrained watermark.
   ------------------------------------------------------------------------- */
.brand-link {
  height: 118px !important;
  min-height: 118px !important;
  padding: 6px 2px !important;
  justify-content: center !important;
  overflow: hidden !important;
  background: #FFFFFF !important;
  border-bottom: 3px solid var(--aurelis-blue) !important;
}

.brand-link .brand-image {
  float: none !important;
  display: block !important;
  width: calc(100% - 2px) !important;
  max-width: calc(100% - 2px) !important;
  height: auto !important;
  max-height: 102px !important;
  margin: 0 auto !important;
  object-fit: contain !important;
  object-position: center !important;
}

.brand-link .brand-text {
  position: absolute !important;
  width: 1px !important;
  height: 1px !important;
  overflow: hidden !important;
  clip: rect(0 0 0 0) !important;
  white-space: nowrap !important;
}

.main-header.navbar {
  min-height: 66px !important;
  position: relative !important;
}

.header-logo-lockup {
  position: relative !important;
  left: auto !important;
  top: auto !important;
  transform: none !important;
  display: inline-flex !important;
  align-items: center !important;
  justify-content: center !important;
  flex: 0 0 344px !important;
  gap: 10px !important;
  width: 344px !important;
  min-width: 344px !important;
  max-width: 344px !important;
  height: 58px !important;
  margin: 0 16px 0 0 !important;
  padding: 4px 12px !important;
  border: 1px solid rgba(53,197,244,.18) !important;
  background: rgba(255,255,255,.96) !important;
  border-radius: 14px !important;
  box-shadow: 0 10px 28px rgba(23,50,77,.08) !important;
  pointer-events: none;
}

.header-logo-lockup img {
  width: 146px !important;
  height: 44px !important;
  max-width: 146px !important;
  object-fit: contain !important;
}

.header-logo-lockup span {
  color: #0E6B67 !important;
  font-size: .74rem !important;
  font-weight: 800 !important;
  letter-spacing: .18px !important;
  text-transform: uppercase !important;
  white-space: nowrap !important;
}

.aurelis-content-watermark {
  right: 4.5vw !important;
  bottom: 6vh !important;
  width: 430px !important;
  max-width: 30vw !important;
  opacity: .028 !important;
  filter: grayscale(20%) blur(.25px) !important;
}

/* The report builder must stay open while users interact with its controls. */
#shiny-modal .modal-dialog {
  max-width: 1120px !important;
}

#shiny-modal,
#shiny-modal .modal-content,
#shiny-modal .modal-body {
  overflow: visible !important;
}

#shiny-modal .modal-header {
  background: linear-gradient(135deg, #17324D 0%, #2F6EA5 100%);
  color: #FFFFFF;
  border-bottom: 0;
}

#shiny-modal .modal-header .modal-title,
#shiny-modal .modal-header .close {
  color: #FFFFFF !important;
}

#shiny-modal .modal-footer {
  background: #F6F9FC;
  border-top: 1px solid var(--aurelis-border);
}

/* Country analysis cards stretch together while their internals size to data. */
.country-analysis-row {
  display: flex;
  flex-wrap: wrap;
  align-items: stretch;
}

.country-analysis-row > [class*='col-'] {
  display: flex;
  flex-direction: column;
}

.country-analysis-row .card {
  width: 100%;
  height: 100%;
}

.country-overflow-note {
  margin: 6px 0 10px 0;
  padding: 8px 12px;
  border-radius: 12px;
  background: rgba(255,255,255,.72);
  border: 1px solid rgba(47,110,165,.10);
  color: #54677A;
  font-size: .82rem;
  line-height: 1.45;
}

.country-overflow-note strong {
  color: #17324D;
  margin-right: 6px;
}

.country-business-summary {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 10px;
  margin: 4px 0 14px;
}

.country-business-metric {
  padding: 11px 12px;
  border: 1px solid var(--aurelis-border);
  border-radius: 9px;
  background: #F7FAFC;
}

.country-business-metric span {
  display: block;
  color: #64748B;
  font-size: .72rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .45px;
}

.country-business-metric strong {
  display: block;
  margin-top: 3px;
  color: var(--aurelis-navy);
  font-size: 1.08rem;
}

.country-detail-shell {
  overflow: hidden;
}

.chart-toolbar-note {
  color: #64748B;
  font-size: .78rem;
  margin: 2px 0 8px;
}

.schedule-export-bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 9px;
  padding: 11px 12px;
  margin-top: 12px;
  border: 1px solid var(--aurelis-border);
  border-radius: 9px;
  background: #F6F9FC;
}

.schedule-export-bar .schedule-export-actions {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}

@media (max-width: 1500px) {
  .header-logo-lockup { min-width: 360px !important; }
  .header-logo-lockup img { width: 152px !important; height: 52px !important; }
  .header-logo-lockup span { font-size: .86rem !important; }
}

@media (max-width: 1199px) {
  .header-logo-lockup { display: none !important; }
}

@media (max-width: 768px) {
  .brand-link { height: 72px !important; }
  .brand-link .brand-image { height: 62px !important; max-height: 62px !important; width: auto !important; }
  .country-business-summary { grid-template-columns: 1fr; }
}


/* -------------------------------------------------------------------------
   DEFINITIVE SIDEBAR BRAND CORRECTION
   Keep the owner-supplied logo in its original rectangular proportions,
   remove AdminLTE's automatic img-circle treatment, and reserve enough
   vertical space so the Global Year label can never sit behind the brand.
   ------------------------------------------------------------------------- */
.brand-link {
  width: 332px !important;
  max-width: 100% !important;
  height: 116px !important;
  min-height: 116px !important;
  padding: 5px 4px !important;
  justify-content: center !important;
  overflow: hidden !important;
  background: #FFFFFF !important;
  border-bottom: 3px solid var(--aurelis-blue) !important;
  box-sizing: border-box !important;
}

.brand-link .brand-image,
.brand-link img.brand-image,
.brand-link .img-circle,
.brand-link .elevation-3 {
  float: none !important;
  display: block !important;
  width: 100% !important;
  max-width: 324px !important;
  height: 104px !important;
  max-height: 104px !important;
  margin: 0 auto !important;
  padding: 0 !important;
  object-fit: contain !important;
  object-position: center !important;
  border-radius: 0 !important;
  box-shadow: none !important;
  filter: none !important;
  opacity: 1 !important;
}

.main-sidebar .sidebar {
  padding-top: 0 !important;
}

.global-filter-panel {
  position: relative !important;
  z-index: 5 !important;
  padding: 22px 18px 12px !important;
  background: #FFFFFF !important;
}

.global-filter-panel > label {
  display: block !important;
  position: relative !important;
  z-index: 6 !important;
  margin: 0 0 8px 0 !important;
  opacity: 1 !important;
  color: #526273 !important;
  font-size: .78rem !important;
  font-weight: 750 !important;
  line-height: 1.25 !important;
  letter-spacing: .12px !important;
}

.global-filter-panel .form-group {
  margin-bottom: 18px !important;
}

.global-filter-panel .selectize-control,
.global-filter-panel .selectize-input {
  width: 100% !important;
}

@media (max-width: 768px) {
  .brand-link {
    height: 86px !important;
    min-height: 86px !important;
    padding: 8px 14px !important;
  }
  .brand-link .brand-image,
  .brand-link img.brand-image,
  .brand-link .img-circle,
  .brand-link .elevation-3 {
    height: 64px !important;
    max-height: 64px !important;
    width: 100% !important;
    max-width: calc(100% - 12px) !important;
    border-radius: 0 !important;
    box-shadow: none !important;
  }
  .global-filter-panel {
    padding-top: 13px !important;
  }
}


/* -------------------------------------------------------------------------
   AURELIS OPERATIONS GRID — FUTURE INTERFACE
   A business-safe futuristic layer: luminous ocean accents, restrained glass
   surfaces, contextual page intelligence and user-controlled display modes.
   ------------------------------------------------------------------------- */
:root {
  --future-deep: #071A2B;
  --future-navy: #0B2942;
  --future-blue: #1479C9;
  --future-cyan: #35C5F4;
  --future-aqua: #20BFA9;
  --future-violet: #8A63FF;
  --future-emerald: #1ED8B5;
  --future-ice: #EAF7FF;
  --future-panel: rgba(255,255,255,.88);
  --future-line: rgba(38,126,178,.20);
  --future-shadow: 0 18px 46px rgba(10,45,72,.11);
}

body.aurelis-future {
  background: #EAF3F8 !important;
}

body.aurelis-future .content-wrapper {
  position: relative !important;
  background:
    radial-gradient(circle at 86% 10%, rgba(53,197,244,.20), transparent 26%),
    radial-gradient(circle at 12% 82%, rgba(138,99,255,.12), transparent 28%),
    radial-gradient(circle at 52% 14%, rgba(30,216,181,.10), transparent 20%),
    linear-gradient(145deg, #FBFDFF 0%, #F0F8FC 42%, #E7F1F8 100%) !important;
}

body.aurelis-future .content-wrapper::before {
  content: '';
  position: fixed;
  inset: 66px 0 0 332px;
  pointer-events: none;
  z-index: 0;
  opacity: .28;
  background-image:
    linear-gradient(rgba(26,112,166,.055) 1px, transparent 1px),
    linear-gradient(90deg, rgba(26,112,166,.055) 1px, transparent 1px);
  background-size: 34px 34px;
  mask-image: linear-gradient(to bottom, rgba(0,0,0,.65), transparent 86%);
}

body.aurelis-future .main-sidebar {
  background: linear-gradient(180deg, #071A2B 0%, #0B2942 58%, #0C3652 100%) !important;
  border-right: 1px solid rgba(84,197,244,.24) !important;
  box-shadow: 10px 0 36px rgba(7,26,43,.18) !important;
}

body.aurelis-future .brand-link {
  background: linear-gradient(155deg, rgba(255,255,255,.98), rgba(231,247,255,.96)) !important;
  border-bottom: 3px solid var(--future-cyan) !important;
  box-shadow: inset 0 -1px 0 rgba(255,255,255,.8), 0 10px 28px rgba(2,20,35,.20) !important;
}

body.aurelis-future .global-filter-panel {
  background: rgba(7,26,43,.92) !important;
  border-bottom: 1px solid rgba(84,197,244,.16) !important;
  padding-top: 50px !important;
}

body.aurelis-future .global-filter-panel > label {
  color: #D8ECF7 !important;
}

.global-filter-panel .global-filter-label {
  display: block !important;
  visibility: visible !important;
  opacity: 1 !important;
  position: relative !important;
  z-index: 8 !important;
  margin: 0 0 9px 0 !important;
  padding: 0 2px !important;
  color: #D8ECF7 !important;
  font-size: .80rem !important;
  font-weight: 800 !important;
  line-height: 1.35 !important;
  letter-spacing: .12px !important;
  white-space: normal !important;
}

.global-filter-panel .global-filter-label.mt-2 {
  margin-top: 13px !important;
}

.global-filter-panel > .global-filter-label:first-child {
  display: block !important;
  visibility: visible !important;
  opacity: 1 !important;
  margin-top: 0 !important;
  color: #D8ECF7 !important;
}

body.aurelis-future .global-filter-panel .selectize-input,
body.aurelis-future .global-filter-panel select {
  background: rgba(255,255,255,.96) !important;
  border-color: rgba(93,199,244,.30) !important;
  box-shadow: 0 8px 20px rgba(0,0,0,.12) !important;
}

body.aurelis-future .nav-sidebar .nav-link {
  color: #C9DCE8 !important;
  border: 1px solid transparent !important;
  background: transparent !important;
}

body.aurelis-future .nav-sidebar .nav-link:hover {
  color: #FFFFFF !important;
  background: linear-gradient(90deg, rgba(53,197,244,.16), rgba(111,106,248,.10)) !important;
  border-color: rgba(53,197,244,.20) !important;
  transform: translateX(4px) !important;
}

body.aurelis-future .nav-sidebar .nav-link.active {
  color: #FFFFFF !important;
  background: linear-gradient(105deg, #1479C9 0%, #20BFA9 55%, #35C5F4 100%) !important;
  border-color: rgba(255,255,255,.28) !important;
  box-shadow: 0 10px 28px rgba(22,145,191,.28), inset 0 1px 0 rgba(255,255,255,.25) !important;
}

body.aurelis-future .main-header {
  background: rgba(247,252,255,.82) !important;
  backdrop-filter: blur(18px) saturate(145%) !important;
  border-bottom: 1px solid rgba(47,110,165,.16) !important;
  box-shadow: 0 8px 28px rgba(16,55,82,.08) !important;
}

body.aurelis-future .header-logo-lockup {
  background: rgba(255,255,255,.72) !important;
  border: 1px solid rgba(53,197,244,.22) !important;
  box-shadow: 0 10px 34px rgba(20,91,132,.10) !important;
  backdrop-filter: blur(14px) !important;
}

.future-status-chip {
  margin-right: 8px !important;
  padding: 5px 10px !important;
  border: 1px solid rgba(32,191,169,.30) !important;
  border-radius: 999px !important;
  background: rgba(230,255,251,.82) !important;
  color: #0B665B !important;
  font-size: .72rem !important;
  font-weight: 800 !important;
  letter-spacing: .45px !important;
  text-transform: uppercase !important;
  white-space: nowrap !important;
}

.future-status-chip .future-signal-dot,
.future-page-hero .future-signal-dot {
  width: 8px;
  height: 8px;
  display: inline-block;
  margin-right: 7px;
  border-radius: 50%;
  background: var(--future-aqua);
  box-shadow: 0 0 0 5px rgba(32,191,169,.12), 0 0 16px rgba(32,191,169,.70);
  animation: futurePulse 2.2s ease-in-out infinite;
}

@keyframes futurePulse {
  0%, 100% { transform: scale(.92); opacity: .82; }
  50% { transform: scale(1.12); opacity: 1; }
}

.future-control-button {
  margin: 0 3px !important;
  padding: 8px 11px !important;
  border: 1px solid rgba(47,110,165,.18) !important;
  border-radius: 9px !important;
  background: rgba(255,255,255,.70) !important;
  color: #174D70 !important;
  font-size: .76rem !important;
  font-weight: 750 !important;
  box-shadow: 0 6px 16px rgba(23,50,77,.06) !important;
}

.future-control-button:hover {
  color: #FFFFFF !important;
  background: linear-gradient(110deg, var(--future-blue), var(--future-aqua)) !important;
  border-color: transparent !important;
}

.future-control-item-first {
  margin-left: 0 !important;
}

.future-control-item .future-control-button {
  position: relative !important;
  z-index: 3 !important;
}

.future-page-hero-enterprise::after {
  content: '';
  position: absolute;
  inset: 0;
  pointer-events: none;
  background: linear-gradient(90deg, transparent 0%, rgba(255,255,255,.06) 48%, transparent 100%);
  opacity: .8;
}

.future-page-hero {
  position: relative;
  z-index: 2;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 22px;
  margin: 0 0 18px 0;
  padding: 20px 24px;
  overflow: hidden;
  border: 1px solid rgba(53,197,244,.22);
  border-radius: 18px;
  background:
    linear-gradient(115deg, rgba(7,26,43,.96) 0%, rgba(11,58,89,.94) 58%, rgba(20,121,201,.86) 100%);
  box-shadow: 0 20px 48px rgba(7,35,56,.16), inset 0 1px 0 rgba(255,255,255,.13);
  color: #FFFFFF;
}

.future-page-hero::before {
  content: '';
  position: absolute;
  inset: 0;
  background:
    radial-gradient(circle at 90% 20%, rgba(53,197,244,.28), transparent 30%),
    linear-gradient(90deg, transparent, rgba(255,255,255,.03), transparent);
  pointer-events: none;
}

.future-page-hero::after {
  content: '';
  position: absolute;
  right: -45px;
  top: -72px;
  width: 210px;
  height: 210px;
  border: 1px solid rgba(255,255,255,.10);
  border-radius: 50%;
  box-shadow: 0 0 0 28px rgba(255,255,255,.025), 0 0 0 58px rgba(255,255,255,.018);
}

.future-page-hero > * {
  position: relative;
  z-index: 1;
}

.future-hero-eyebrow {
  margin-bottom: 4px;
  color: #8EE7FF;
  font-size: .70rem;
  font-weight: 850;
  letter-spacing: 1.6px;
  text-transform: uppercase;
}

.future-page-hero h2 {
  margin: 0;
  color: #FFFFFF;
  font-size: 1.52rem;
  font-weight: 760;
  letter-spacing: -.25px;
}

.future-page-hero p {
  margin: 6px 0 0;
  max-width: 760px;
  color: #C8E2F0;
  font-size: .88rem;
}

.future-hero-status {
  min-width: 205px;
  padding: 12px 14px;
  border: 1px solid rgba(142,231,255,.20);
  border-radius: 13px;
  background: rgba(255,255,255,.07);
  backdrop-filter: blur(10px);
  text-align: right;
}

.future-hero-status strong {
  display: block;
  color: #FFFFFF;
  font-size: .77rem;
  letter-spacing: .35px;
}

.future-hero-status span:last-child {
  display: block;
  margin-top: 4px;
  color: #9CCDE2;
  font-size: .73rem;
}

body.aurelis-future .card {
  border: 1px solid rgba(48,126,177,.14) !important;
  border-radius: 16px !important;
  background: var(--future-panel) !important;
  backdrop-filter: blur(13px) saturate(125%) !important;
  box-shadow: var(--future-shadow) !important;
}

body.aurelis-future .card:hover {
  border-color: rgba(53,197,244,.30) !important;
  box-shadow: 0 22px 52px rgba(10,45,72,.15) !important;
}

body.aurelis-future .card-header {
  border-bottom: 1px solid rgba(48,126,177,.13) !important;
  background: linear-gradient(90deg, rgba(236,248,255,.88), rgba(255,255,255,.72)) !important;
}

body.aurelis-future .card-header .card-title {
  color: #102F49 !important;
  font-weight: 780 !important;
}

body.aurelis-future .kpi-card {
  overflow: hidden !important;
  border: 1px solid rgba(255,255,255,.42) !important;
  border-radius: 18px !important;
  box-shadow: 0 18px 38px rgba(13,56,85,.14) !important;
}

body.aurelis-future .kpi-card::before {
  content: '';
  position: absolute;
  inset: 0;
  background: linear-gradient(120deg, transparent 0%, rgba(255,255,255,.14) 42%, transparent 74%);
  transform: translateX(-110%);
  transition: transform .65s ease;
}

body.aurelis-future .kpi-card:hover::before {
  transform: translateX(110%);
}

body.aurelis-future .kpi-card:hover {
  transform: translateY(-5px) scale(1.012) !important;
}

body.aurelis-future .plotly .modebar {
  background: rgba(255,255,255,.80) !important;
  border: 1px solid rgba(47,110,165,.15) !important;
  border-radius: 8px !important;
  box-shadow: 0 6px 18px rgba(23,50,77,.08) !important;
}

body.aurelis-future .dataTables_wrapper,
body.aurelis-future .dataTables_scroll,
body.aurelis-future table.dataTable {
  border-radius: 12px !important;
}

body.aurelis-future .report-action-bar,
body.aurelis-future .schedule-export-bar,
body.aurelis-future .interaction-tip {
  border-color: rgba(53,197,244,.20) !important;
  background: linear-gradient(110deg, rgba(238,250,255,.92), rgba(245,246,255,.88)) !important;
}

body.aurelis-ambient .content-wrapper {
  background:
    radial-gradient(circle at 88% 10%, rgba(53,197,244,.22), transparent 28%),
    radial-gradient(circle at 10% 86%, rgba(111,106,248,.17), transparent 32%),
    linear-gradient(145deg, #DCECF5 0%, #E9F4F8 48%, #DDE8F1 100%) !important;
}

body.aurelis-ambient .card {
  background: rgba(255,255,255,.82) !important;
}

body.aurelis-compact .card-body {
  padding: 12px 14px !important;
}

body.aurelis-compact .card {
  margin-bottom: 12px !important;
}

body.aurelis-compact .kpi-card {
  padding: 16px 17px !important;
}

body.aurelis-compact .kpi-card h2 {
  font-size: 2.02rem !important;
}

body.aurelis-compact .content {
  padding-top: 10px !important;
}

@media (max-width: 1600px) {
  .future-status-chip { display: none !important; }
  .future-control-button { padding: 8px 9px !important; font-size: 0 !important; }
  .future-control-button i { font-size: .86rem !important; margin: 0 !important; }
}

body.sidebar-collapse.aurelis-future .content-wrapper::before {
  inset: 66px 0 0 74px;
}

@media (max-width: 991px) {
  body.aurelis-future .content-wrapper::before { inset: 66px 0 0 0; }
  .future-page-hero { padding: 16px 18px; border-radius: 14px; }
  .future-page-hero h2 { font-size: 1.25rem; }
  .future-hero-status { display: none; }
}


/* Quotation Studio */
.quote-toolbar {
  display: flex;
  flex-wrap: wrap;
  gap: 9px;
  align-items: center;
  margin-bottom: 12px;
}

.quote-toolbar .btn,
.quote-toolbar .form-group {
  margin-bottom: 0 !important;
}

.quote-input-matrix {
  width: 100%;
  border-collapse: separate;
  border-spacing: 0;
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  overflow: hidden;
  background: #FFFFFF;
}

.quote-input-matrix th {
  background: var(--aurelis-navy);
  color: #FFFFFF;
  padding: 10px 11px;
  text-align: center;
  font-size: .82rem;
  letter-spacing: .25px;
}

.quote-input-matrix th:first-child,
.quote-input-matrix td:first-child {
  text-align: left;
  min-width: 245px;
}

.quote-input-matrix td {
  padding: 7px 9px;
  border-top: 1px solid #E7EEF4;
  vertical-align: middle;
}

.quote-input-matrix td .form-group {
  margin-bottom: 0 !important;
}

.quote-input-matrix td input {
  text-align: right;
}

.quote-scenario-min { border-top: 4px solid #5F7D67; }
.quote-scenario-average { border-top: 4px solid #F0A36D; }
.quote-scenario-max { border-top: 4px solid #7B3F18; }

.quote-status-strip {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 10px;
  margin: 12px 0;
}

.quote-status-pill {
  border: 1px solid var(--aurelis-border);
  border-left: 5px solid var(--aurelis-blue);
  border-radius: 10px;
  padding: 11px 13px;
  background: #F8FBFD;
  line-height: 1.35;
}

.quote-status-pill.good { border-left-color: var(--aurelis-sage); }
.quote-status-pill.warn { border-left-color: var(--aurelis-amber); }
.quote-status-pill.danger { border-left-color: var(--aurelis-coral); }

.quote-preview-sheet {
  background: #FFFFFF;
  color: #1F2937;
  border: 1px solid #CBD7E2;
  border-radius: 12px;
  box-shadow: 0 7px 28px rgba(23, 50, 77, .09);
  padding: 34px 38px;
  max-width: 1080px;
  margin: 0 auto;
}

.quote-preview-header {
  display: flex;
  justify-content: space-between;
  gap: 28px;
  align-items: flex-start;
  border-bottom: 3px solid var(--aurelis-navy);
  padding-bottom: 17px;
  margin-bottom: 22px;
}

.quote-preview-header img {
  max-width: 220px;
  max-height: 72px;
  object-fit: contain;
}

.quote-preview-title {
  text-align: right;
}

.quote-preview-title h1 {
  margin: 0;
  color: var(--aurelis-navy);
  font-size: 2rem;
  letter-spacing: 1.2px;
}

.quote-preview-meta {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 14px 25px;
  margin-bottom: 22px;
}

.quote-preview-panel {
  background: #F7FAFC;
  border: 1px solid #DDE7EF;
  border-radius: 9px;
  padding: 13px 15px;
}

.quote-preview-panel strong {
  color: var(--aurelis-navy);
}

.quote-preview-table {
  width: 100%;
  border-collapse: collapse;
  margin: 18px 0;
  font-size: .88rem;
}

.quote-preview-table th {
  background: var(--aurelis-navy);
  color: #FFFFFF;
  padding: 10px 8px;
  text-align: left;
}

.quote-preview-table td {
  border-bottom: 1px solid #DDE7EF;
  padding: 9px 8px;
  vertical-align: top;
}

.quote-preview-table .num {
  text-align: right;
  white-space: nowrap;
}

.quote-preview-summary {
  width: min(100%, 470px);
  margin-left: auto;
  border-collapse: collapse;
}

.quote-preview-summary td {
  padding: 7px 9px;
  border-bottom: 1px solid #DDE7EF;
}

.quote-preview-summary td:last-child {
  text-align: right;
  font-weight: 700;
}

.quote-preview-summary tr.total td {
  background: var(--aurelis-navy);
  color: #FFFFFF;
  font-size: 1.05rem;
  border-bottom: none;
}

.quote-preview-notes {
  margin-top: 24px;
  border-top: 1px solid #DDE7EF;
  padding-top: 16px;
  white-space: pre-wrap;
}

.quote-math-note {
  border: 1px solid #CFE0EC;
  border-left: 5px solid var(--aurelis-blue);
  background: #F4F9FC;
  border-radius: 10px;
  padding: 13px 15px;
  margin-top: 12px;
  color: #425466;
}


.quote-process-strip {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  gap: 8px;
  margin: 4px 0 18px;
}

.quote-process-step {
  background: #F7FAFC;
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  padding: 10px 12px;
  color: #526273;
  font-size: .81rem;
  line-height: 1.25;
}

.quote-process-step strong {
  color: var(--aurelis-navy);
  display: block;
  margin-bottom: 3px;
}

.quote-help-grid,
.customer-milestone-grid,
.customer-resume-grid,
.kpi-health-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 11px;
}

.quote-help-card,
.customer-milestone-card,
.customer-resume-card,
.kpi-health-card {
  border: 1px solid var(--aurelis-border);
  border-radius: 10px;
  background: #F8FBFD;
  padding: 12px 14px;
}

.quote-help-card strong,
.customer-milestone-card strong,
.customer-resume-card strong,
.kpi-health-card strong {
  display: block;
  color: var(--aurelis-navy);
  margin-bottom: 4px;
}

.customer-milestone-value {
  font-size: 1.45rem;
  font-weight: 750;
  color: var(--aurelis-navy);
}

.quote-bridge-wrap {
  width: 100%;
  overflow: visible;
  min-height: 455px;
}

.customer-resume-preview {
  border: 1px solid #D7E1EA;
  border-radius: 13px;
  padding: 22px;
  background: #FFFFFF;
}

.customer-resume-preview h3 {
  color: var(--aurelis-navy);
  margin-top: 0;
}

.buyer-drill-path {
  display: flex;
  flex-wrap: wrap;
  gap: 7px;
  margin: 5px 0 12px;
}

.buyer-drill-chip {
  border: 1px solid var(--aurelis-border);
  background: #F7FAFC;
  color: #526273;
  border-radius: 999px;
  padding: 6px 10px;
  font-size: .78rem;
  font-weight: 650;
}

.metric-definition-note {
  color: #607284;
  font-size: .79rem;
  margin-top: 7px;
}

@media (max-width: 767px) {
  .quote-process-strip { grid-template-columns: 1fr 1fr; }
}

@media (max-width: 767px) {
  .quote-preview-sheet { padding: 20px 17px; }
  .quote-preview-header { display: block; }
  .quote-preview-title { text-align: left; margin-top: 16px; }
  .quote-preview-meta { grid-template-columns: 1fr; }
  .quote-input-matrix th:first-child,
  .quote-input-matrix td:first-child { min-width: 180px; }
}

.multiuser-banner {
  background: #F4F8FB;
  border: 1px solid var(--aurelis-border);
  border-left: 5px solid var(--aurelis-blue);
  border-radius: 12px;
  padding: 14px 16px;
  margin-bottom: 16px;
}

.multiuser-session-id {
  font-family: Consolas, 'Courier New', monospace;
  font-size: .85rem;
  color: var(--aurelis-slate);
}

.multiuser-status-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
  gap: 12px;
  margin: 12px 0 16px;
}

.multiuser-status-card {
  border: 1px solid var(--aurelis-border);
  border-radius: 11px;
  padding: 13px;
  background: #FFFFFF;
}

.multiuser-status-card strong {
  display: block;
  color: var(--aurelis-navy);
  margin-bottom: 5px;
}

"


# JavaScript companion for the global selectize layering fix. It elevates the
# active control and all relevant parent containers, then releases them after
# the menu closes. This applies automatically to every menu and modal.

# ------------------------------------------------------------------------------
# ARME L SIGNATURE COMMAND FABRIC
# A unique visual system that combines:
# - a midnight enterprise shell,
# - a clean high-readability analytics canvas,
# - luminous page-aware accents,
# - a command/search rail,
# - focusable analytical cards,
# - consistent creator signature on every workspace.
# ------------------------------------------------------------------------------

signature_css <- "
:root {
  --sig-bg: #EAF0F6;
  --sig-bg-soft: #F5F8FC;
  --sig-shell: #08131F;
  --sig-shell-2: #0D2235;
  --sig-panel: rgba(255,255,255,.94);
  --sig-panel-strong: #FFFFFF;
  --sig-ink: #10243A;
  --sig-muted: #66778A;
  --sig-line: rgba(35,84,126,.14);
  --sig-blue: #1877D2;
  --sig-cyan: #22C3D6;
  --sig-teal: #18A999;
  --sig-violet: #7A5AF8;
  --sig-amber: #E3A23B;
  --sig-rose: #D55F73;
  --sig-green: #45B77D;
  --sig-accent: var(--sig-blue);
  --sig-accent-soft: rgba(24,119,210,.12);
  --sig-radius-xl: 22px;
  --sig-radius-lg: 17px;
  --sig-radius-md: 12px;
  --sig-shadow: 0 16px 44px rgba(17,50,78,.10);
  --sig-shadow-hover: 0 24px 62px rgba(17,50,78,.16);
}

body.aurelis-signature[data-aurelis-page='exec_overview'] { --sig-accent: #1877D2; --sig-accent-soft: rgba(24,119,210,.13); }
body.aurelis-signature[data-aurelis-page='kpi_board'] { --sig-accent: #7A5AF8; --sig-accent-soft: rgba(122,90,248,.12); }
body.aurelis-signature[data-aurelis-page='sale_performance'] { --sig-accent: #18A999; --sig-accent-soft: rgba(24,169,153,.12); }
body.aurelis-signature[data-aurelis-page='quotation_studio'] { --sig-accent: #E3A23B; --sig-accent-soft: rgba(227,162,59,.14); }
body.aurelis-signature[data-aurelis-page='customer_performance'] { --sig-accent: #45B77D; --sig-accent-soft: rgba(69,183,125,.12); }
body.aurelis-signature[data-aurelis-page='client_statement'] { --sig-accent: #22C3D6; --sig-accent-soft: rgba(34,195,214,.12); }
body.aurelis-signature[data-aurelis-page='monthly_sales'] { --sig-accent: #1877D2; --sig-accent-soft: rgba(24,119,210,.12); }
body.aurelis-signature[data-aurelis-page='buyer_activity'] { --sig-accent: #D55F73; --sig-accent-soft: rgba(213,95,115,.12); }
body.aurelis-signature[data-aurelis-page='order_tracker'] { --sig-accent: #E3A23B; --sig-accent-soft: rgba(227,162,59,.14); }
body.aurelis-signature[data-aurelis-page='inventory_tab'] { --sig-accent: #18A999; --sig-accent-soft: rgba(24,169,153,.12); }
body.aurelis-signature[data-aurelis-page='buyer_tool'] { --sig-accent: #7A5AF8; --sig-accent-soft: rgba(122,90,248,.12); }
body.aurelis-signature[data-aurelis-page='ar_tab'] { --sig-accent: #D55F73; --sig-accent-soft: rgba(213,95,115,.12); }
body.aurelis-signature[data-aurelis-page='contacts_tab'] { --sig-accent: #22C3D6; --sig-accent-soft: rgba(34,195,214,.12); }
body.aurelis-signature[data-aurelis-page='multi_user_workspace'] { --sig-accent: #7A5AF8; --sig-accent-soft: rgba(122,90,248,.12); }
body.aurelis-signature[data-aurelis-page='misc_tab'] { --sig-accent: #66778A; --sig-accent-soft: rgba(102,119,138,.12); }
body.aurelis-signature[data-aurelis-page='product_intelligence'] { --sig-accent: #27D5B2; --sig-accent-soft: rgba(39,213,178,.14); }

body.aurelis-signature {
  background: var(--sig-shell) !important;
}

body.aurelis-signature .content-wrapper {
  background:
    radial-gradient(circle at 88% 7%, var(--sig-accent-soft), transparent 23%),
    radial-gradient(circle at 18% 94%, rgba(122,90,248,.07), transparent 24%),
    linear-gradient(145deg, #F8FBFE 0%, #EDF3F8 48%, #E7EEF5 100%) !important;
}

body.aurelis-signature .content-wrapper::before {
  opacity: .16 !important;
  background-size: 42px 42px !important;
}

body.aurelis-signature .main-sidebar {
  width: 282px !important;
  background:
    radial-gradient(circle at 10% 0%, rgba(34,195,214,.12), transparent 24%),
    linear-gradient(180deg, #07111B 0%, #091C2C 55%, #0D2538 100%) !important;
  border-right: 1px solid rgba(77,167,219,.16) !important;
  box-shadow: 12px 0 42px rgba(5,18,31,.28) !important;
}

body.aurelis-signature .brand-link {
  height: 86px !important;
  background: rgba(255,255,255,.98) !important;
  border-bottom: 1px solid rgba(34,195,214,.32) !important;
  box-shadow: 0 14px 36px rgba(0,0,0,.22) !important;
}

body.aurelis-signature .brand-link .brand-image {
  max-height: 58px !important;
}

body.aurelis-signature .global-filter-panel {
  position: relative;
  margin: 14px 12px 10px !important;
  padding: 18px 15px !important;
  border: 1px solid rgba(91,177,225,.14) !important;
  border-radius: 16px !important;
  background:
    linear-gradient(145deg, rgba(255,255,255,.075), rgba(255,255,255,.025)) !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,.06);
}

body.aurelis-signature .global-filter-panel::before {
  content: 'GLOBAL LENS';
  display: block;
  margin-bottom: 13px;
  color: #82DFFA;
  font-size: .61rem;
  font-weight: 900;
  letter-spacing: 1.8px;
}

body.aurelis-signature .nav-sidebar {
  padding: 5px 5px 40px !important;
}

body.aurelis-signature .nav-sidebar .nav-link {
  position: relative;
  min-height: 45px;
  display: flex !important;
  align-items: center;
  margin: 4px 8px !important;
  padding: 10px 13px !important;
  border-radius: 12px !important;
  color: #C5D6E2 !important;
  font-size: .88rem !important;
}

body.aurelis-signature .nav-sidebar .nav-link::after {
  content: '';
  position: absolute;
  right: 10px;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background: transparent;
  box-shadow: none;
  transition: all .18s ease;
}

body.aurelis-signature .nav-sidebar .nav-link:hover::after,
body.aurelis-signature .nav-sidebar .nav-link.active::after {
  background: var(--sig-accent);
  box-shadow: 0 0 0 5px var(--sig-accent-soft), 0 0 14px var(--sig-accent);
}

body.aurelis-signature .nav-sidebar .nav-link.active {
  background:
    linear-gradient(105deg, rgba(255,255,255,.12), rgba(255,255,255,.05)) !important;
  border: 1px solid rgba(255,255,255,.10) !important;
  border-left: 3px solid var(--sig-accent) !important;
  box-shadow: 0 10px 28px rgba(0,0,0,.16) !important;
  color: #FFFFFF !important;
}

body.aurelis-signature .nav-sidebar .nav-link.active i {
  color: var(--sig-accent) !important;
  filter: drop-shadow(0 0 8px var(--sig-accent-soft));
}

body.aurelis-signature .main-header {
  min-height: 64px !important;
  background: rgba(248,251,254,.86) !important;
  backdrop-filter: blur(22px) saturate(155%) !important;
  border-bottom: 1px solid rgba(35,84,126,.10) !important;
  box-shadow: 0 12px 34px rgba(17,50,78,.07) !important;
}

body.aurelis-signature .future-control-button,
body.aurelis-signature .navbar-report-button {
  border-radius: 11px !important;
  border: 1px solid rgba(35,84,126,.12) !important;
  background: rgba(255,255,255,.78) !important;
}

body.aurelis-signature .future-control-button:hover,
body.aurelis-signature .navbar-report-button:hover {
  background: var(--sig-shell-2) !important;
  color: #FFFFFF !important;
  border-color: transparent !important;
}

body.aurelis-signature .signature-command-button {
  color: #FFFFFF !important;
  background: linear-gradient(115deg, var(--sig-shell-2), #163E5B) !important;
  border-color: rgba(34,195,214,.22) !important;
  box-shadow: 0 8px 24px rgba(7,27,44,.16) !important;
}

body.aurelis-signature .signature-command-button i {
  color: var(--sig-cyan) !important;
}

body.aurelis-signature .content {
  position: relative;
  z-index: 2;
  padding: 12px 20px 24px !important;
}

body.aurelis-signature .future-page-hero {
  min-height: 148px;
  padding: 24px 28px !important;
  border: 1px solid rgba(255,255,255,.11) !important;
  border-radius: var(--sig-radius-xl) !important;
  background:
    radial-gradient(circle at 90% 14%, var(--sig-accent-soft), transparent 34%),
    radial-gradient(circle at 66% 130%, rgba(34,195,214,.16), transparent 42%),
    linear-gradient(116deg, #07131F 0%, #0B2538 54%, #113D58 100%) !important;
  box-shadow: 0 24px 58px rgba(5,27,43,.18) !important;
}

body.aurelis-signature .future-page-hero::after {
  right: 6%;
  top: -82px;
  width: 250px;
  height: 250px;
  border-color: rgba(255,255,255,.07);
  box-shadow:
    0 0 0 34px rgba(255,255,255,.022),
    0 0 0 70px rgba(255,255,255,.013);
}

.signature-hero-layout {
  width: 100%;
  display: grid;
  grid-template-columns: minmax(360px, 1.5fr) minmax(320px, 1fr);
  gap: 26px;
  align-items: center;
}

.signature-hero-copy {
  min-width: 0;
}

.signature-hero-kicker {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 7px;
  color: #91DFF5;
  font-size: .65rem;
  font-weight: 900;
  letter-spacing: 1.9px;
  text-transform: uppercase;
}

.signature-hero-kicker::before {
  content: '';
  width: 22px;
  height: 2px;
  border-radius: 99px;
  background: var(--sig-accent);
  box-shadow: 0 0 12px var(--sig-accent);
}

.signature-hero-copy h2 {
  margin: 0;
  color: #FFFFFF;
  font-size: clamp(1.45rem, 2.2vw, 2.15rem);
  font-weight: 780;
  letter-spacing: -.45px;
}

.signature-hero-copy p {
  max-width: 790px;
  margin: 8px 0 0;
  color: #BED4E3;
  font-size: .88rem;
  line-height: 1.58;
}

.signature-hero-side {
  display: grid;
  gap: 10px;
}

.signature-hero-chips {
  display: grid;
  grid-template-columns: repeat(3, minmax(88px, 1fr));
  gap: 8px;
}

.signature-hero-chip {
  min-height: 62px;
  padding: 10px 11px;
  border: 1px solid rgba(255,255,255,.09);
  border-radius: 13px;
  background: rgba(255,255,255,.055);
  backdrop-filter: blur(10px);
}

.signature-hero-chip span {
  display: block;
  color: #7FA4BC;
  font-size: .58rem;
  font-weight: 850;
  letter-spacing: 1px;
  text-transform: uppercase;
}

.signature-hero-chip strong {
  display: block;
  margin-top: 4px;
  color: #FFFFFF;
  font-size: .78rem;
  font-weight: 760;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.signature-hero-shortcuts {
  display: flex;
  flex-wrap: wrap;
  gap: 7px;
}

.signature-tab-shortcut {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 7px 10px;
  border: 1px solid rgba(255,255,255,.10);
  border-radius: 999px;
  color: #DCEAF2 !important;
  background: rgba(255,255,255,.05);
  font-size: .68rem;
  font-weight: 700;
  text-decoration: none !important;
  transition: all .18s ease;
}

.signature-tab-shortcut:hover {
  color: #FFFFFF !important;
  border-color: var(--sig-accent);
  background: var(--sig-accent-soft);
  transform: translateY(-1px);
}

body.aurelis-signature .card {
  position: relative;
  overflow: visible !important;
  border: 1px solid rgba(42,89,126,.11) !important;
  border-radius: var(--sig-radius-lg) !important;
  background: var(--sig-panel) !important;
  box-shadow: var(--sig-shadow) !important;
  transition: transform .20s ease, box-shadow .20s ease, border-color .20s ease !important;
}

body.aurelis-signature .card::before {
  content: '';
  position: absolute;
  top: 0;
  left: 18px;
  right: 18px;
  height: 2px;
  border-radius: 0 0 8px 8px;
  background: linear-gradient(90deg, transparent, var(--sig-accent), transparent);
  opacity: .38;
  pointer-events: none;
}

body.aurelis-signature .card:hover {
  border-color: rgba(42,89,126,.18) !important;
  box-shadow: var(--sig-shadow-hover) !important;
  transform: translateY(-2px) !important;
}

body.aurelis-signature .card-header {
  min-height: 52px;
  display: flex;
  align-items: center;
  border-bottom: 1px solid rgba(42,89,126,.09) !important;
  background: linear-gradient(90deg, rgba(247,250,253,.96), rgba(255,255,255,.83)) !important;
}

body.aurelis-signature .card-header .card-title {
  color: var(--sig-ink) !important;
  font-size: .93rem !important;
  font-weight: 780 !important;
  letter-spacing: -.10px;
}

body.aurelis-signature .kpi-card {
  position: relative !important;
  min-height: 124px !important;
  overflow: hidden !important;
  border: 1px solid rgba(42,89,126,.10) !important;
  border-left: 0 !important;
  border-radius: 16px !important;
  background:
    radial-gradient(circle at 90% 14%, var(--sig-accent-soft), transparent 35%),
    linear-gradient(145deg, rgba(255,255,255,.98), rgba(247,250,253,.96)) !important;
  box-shadow: 0 12px 34px rgba(17,50,78,.08) !important;
}

body.aurelis-signature .kpi-card::after {
  content: '';
  position: absolute;
  left: 0;
  bottom: 0;
  width: 46%;
  height: 4px;
  border-radius: 0 8px 0 0;
  background: var(--kpi-accent, var(--sig-accent));
  box-shadow: 0 0 18px var(--sig-accent-soft);
}

body.aurelis-signature .kpi-card h2 {
  color: var(--sig-ink) !important;
  font-size: clamp(1.65rem, 2vw, 2.25rem) !important;
  font-weight: 790 !important;
}

body.aurelis-signature .kpi-card p {
  color: #5B6E81 !important;
}

body.aurelis-signature .form-control,
body.aurelis-signature .selectize-input,
body.aurelis-signature select,
body.aurelis-signature textarea {
  border-radius: 10px !important;
  border-color: rgba(42,89,126,.14) !important;
  background: rgba(255,255,255,.94) !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,.8), 0 3px 10px rgba(17,50,78,.035) !important;
}

body.aurelis-signature .form-control:focus,
body.aurelis-signature .selectize-input.focus {
  border-color: var(--sig-accent) !important;
  box-shadow: 0 0 0 3px var(--sig-accent-soft) !important;
}

body.aurelis-signature .btn-primary,
body.aurelis-signature .btn-info {
  border-color: transparent !important;
  background: linear-gradient(115deg, var(--sig-accent), #1F86C7) !important;
  box-shadow: 0 7px 18px var(--sig-accent-soft) !important;
}

body.aurelis-signature table.dataTable thead th {
  background: #F0F5F9 !important;
  color: #41586E !important;
  border-bottom-color: rgba(42,89,126,.13) !important;
  font-size: .73rem;
  letter-spacing: .25px;
}

body.aurelis-signature table.dataTable tbody tr {
  transition: background-color .16s ease, transform .16s ease;
}

body.aurelis-signature table.dataTable tbody tr:hover {
  background: var(--sig-accent-soft) !important;
}

body.aurelis-signature .plotly .modebar {
  opacity: .72;
  transition: opacity .18s ease;
}

body.aurelis-signature .card:hover .plotly .modebar {
  opacity: 1;
}

.signature-command-overlay {
  position: fixed;
  inset: 0;
  z-index: 1045;
  display: none;
  background: rgba(4,14,24,.34);
  backdrop-filter: blur(4px);
}

.signature-command-panel {
  position: fixed;
  top: 74px;
  right: 18px;
  z-index: 1050;
  width: min(420px, calc(100vw - 28px));
  max-height: calc(100vh - 94px);
  overflow-y: auto;
  display: none;
  padding: 18px;
  border: 1px solid rgba(82,174,222,.19);
  border-radius: 20px;
  background:
    radial-gradient(circle at 100% 0%, rgba(122,90,248,.14), transparent 28%),
    linear-gradient(160deg, rgba(8,22,34,.98), rgba(12,39,58,.98));
  color: #FFFFFF;
  box-shadow: 0 28px 80px rgba(3,14,24,.36);
}

.signature-command-panel.open,
.signature-command-overlay.open {
  display: block;
}

.signature-command-head {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 14px;
  margin-bottom: 15px;
}

.signature-command-head strong {
  display: block;
  font-size: 1.02rem;
  letter-spacing: -.2px;
}

.signature-command-head span {
  display: block;
  margin-top: 3px;
  color: #8AA8BC;
  font-size: .72rem;
}

.signature-command-close {
  border: 0;
  background: rgba(255,255,255,.07);
  color: #DCEAF2;
  width: 34px;
  height: 34px;
  border-radius: 50%;
}

.signature-command-panel label {
  color: #A9C3D3 !important;
  font-size: .72rem;
}

.signature-command-panel .form-control {
  background: rgba(255,255,255,.09) !important;
  border-color: rgba(255,255,255,.11) !important;
  color: #FFFFFF !important;
}

.signature-command-panel .form-control::placeholder {
  color: #7693A6;
}

.signature-command-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 8px;
  margin: 13px 0 15px;
}

.signature-command-tile {
  display: flex;
  flex-direction: column;
  gap: 4px;
  min-height: 72px;
  padding: 11px 12px;
  border: 1px solid rgba(255,255,255,.09);
  border-radius: 13px;
  color: #EAF5FB !important;
  background: rgba(255,255,255,.045);
  text-decoration: none !important;
  transition: all .18s ease;
}

.signature-command-tile i {
  color: var(--sig-cyan);
}

.signature-command-tile span {
  color: #7FA4BC;
  font-size: .65rem;
}

.signature-command-tile:hover {
  transform: translateY(-2px);
  border-color: var(--sig-accent);
  background: var(--sig-accent-soft);
}

.signature-command-health {
  padding: 12px 13px;
  border: 1px solid rgba(255,255,255,.08);
  border-radius: 13px;
  background: rgba(255,255,255,.04);
}

.signature-command-health strong {
  display: block;
  color: #A7E8F5;
  font-size: .68rem;
  letter-spacing: 1px;
  text-transform: uppercase;
}

.signature-command-health span {
  display: block;
  margin-top: 4px;
  color: #D8E7F0;
  font-size: .76rem;
  line-height: 1.45;
}

.aurelis-signature-footer {
  width: 100%;
  clear: both;
  margin: 18px 0 8px;
  padding: 13px 4px 2px;
  border-top: 1px solid rgba(42,89,126,.10);
  color: #8392A2;
  font-size: .66rem;
  font-weight: 550;
  letter-spacing: .30px;
  text-align: right;
}

.aurelis-signature-footer::before {
  content: '';
  display: inline-block;
  width: 18px;
  height: 1px;
  margin: 0 7px 3px 0;
  background: var(--sig-accent);
  opacity: .75;
}

.aurelis-card-focus-backdrop {
  position: fixed;
  inset: 0;
  z-index: 1080;
  display: none;
  background: rgba(3,14,24,.52);
  backdrop-filter: blur(5px);
}

body.aurelis-card-focus-mode .aurelis-card-focus-backdrop {
  display: block;
}

body.aurelis-card-focus-mode .card.aurelis-card-focus {
  position: fixed !important;
  inset: 82px 28px 24px 360px !important;
  z-index: 1090 !important;
  overflow: auto !important;
  margin: 0 !important;
  transform: none !important;
  box-shadow: 0 30px 90px rgba(0,0,0,.34) !important;
}

body.sidebar-collapse.aurelis-card-focus-mode .card.aurelis-card-focus {
  left: 96px !important;
}

body.aurelis-card-focus-mode .card.aurelis-card-focus .card-body {
  min-height: calc(100vh - 180px);
}

.aurelis-busy-bar {
  position: fixed;
  top: 0;
  left: 0;
  z-index: 1200;
  width: 100%;
  height: 3px;
  pointer-events: none;
  opacity: 0;
  background: linear-gradient(90deg, var(--sig-cyan), var(--sig-violet), var(--sig-accent), var(--sig-cyan));
  background-size: 240% 100%;
  transition: opacity .15s ease;
}

body.aurelis-shiny-busy .aurelis-busy-bar {
  opacity: 1;
  animation: sigBusy 1.2s linear infinite;
}

@keyframes sigBusy {
  0% { background-position: 0% 50%; }
  100% { background-position: 240% 50%; }
}

@media (max-width: 1200px) {
  .signature-hero-layout { grid-template-columns: 1fr; }
  .signature-hero-side { display: none; }
}

@media (max-width: 991px) {
  body.aurelis-card-focus-mode .card.aurelis-card-focus {
    inset: 76px 12px 14px 12px !important;
  }
  .signature-command-panel {
    top: 68px;
    right: 8px;
  }
  body.aurelis-signature .content {
    padding-left: 10px !important;
    padding-right: 10px !important;
  }
}

@media (prefers-reduced-motion: reduce) {
  body.aurelis-signature *,
  body.aurelis-signature *::before,
  body.aurelis-signature *::after {
    animation-duration: .01ms !important;
    transition-duration: .01ms !important;
  }
}

@media (max-width: 575px) {
  body.aurelis-signature .main-header.navbar {
    padding-right: 4px !important;
    overflow: visible !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto {
    min-width: 0 !important;
    width: auto !important;
    flex: 1 1 auto !important;
    justify-content: flex-end !important;
    gap: 2px !important;
    overflow: visible !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .nav-item.dropdown:not(.future-control-item) {
    display: none !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .nav-item:not(.dropdown),
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .custom-control {
    display: none !important;
  }
  body.aurelis-signature .main-header .future-control-item,
  body.aurelis-signature .main-header .future-control-item > a,
  body.aurelis-signature .main-header .navbar-report-button {
    box-sizing: border-box !important;
    width: 38px !important;
    min-width: 38px !important;
    max-width: 38px !important;
    height: 38px !important;
    margin: 0 !important;
    padding: 8px !important;
    font-size: 0 !important;
    justify-content: center !important;
  }
  body.aurelis-signature .main-header .future-control-item > a i,
  body.aurelis-signature .main-header .navbar-report-button i {
    font-size: .95rem !important;
  }
}
"


# ------------------------------------------------------------------------------
# AURELIS ATLAS INTERFACE v5
# Unique shell inspired only by the user's uploaded dashboard references:
# modular executive cards, pill controls, dark command surfaces, bright finance
# canvases, layered depth, concise typography and high-density visual analytics.
# ------------------------------------------------------------------------------

atlas_v5_css <- "
:root {
  --atlas-black: #070A0F;
  --atlas-night: #0C111C;
  --atlas-night-2: #12192A;
  --atlas-card-dark: #151C2D;
  --atlas-pearl: #F4F7FB;
  --atlas-white: #FFFFFF;
  --atlas-ink: #111827;
  --atlas-muted: #7A8798;
  --atlas-line: rgba(55,76,101,.13);
  --atlas-purple: #8067F2;
  --atlas-lilac: #B79CF7;
  --atlas-blue: #268BFF;
  --atlas-cyan: #24CED2;
  --atlas-mint: #64E6AA;
  --atlas-yellow: #FFD76B;
  --atlas-coral: #FF7389;
  --atlas-orange: #FF9A5C;
  --atlas-panel-radius: 20px;
  --atlas-control-radius: 999px;
}

body.aurelis-signature[data-aurelis-page='global_network'] {
  --sig-accent: #8067F2;
  --sig-accent-soft: rgba(128,103,242,.13);
}

/* ---- Whole application frame ---- */
body.aurelis-signature .wrapper {
  background: #DCE5F1 !important;
}

body.aurelis-signature .content-wrapper {
  min-height: calc(100vh - 64px) !important;
  background:
    radial-gradient(circle at 94% 1%, rgba(183,156,247,.20), transparent 24%),
    radial-gradient(circle at 58% 8%, rgba(36,206,210,.10), transparent 23%),
    radial-gradient(circle at 13% 91%, rgba(38,139,255,.10), transparent 24%),
    linear-gradient(145deg, #F8FAFD 0%, #EEF3F9 54%, #E9EEF6 100%) !important;
}

body.aurelis-signature .main-sidebar {
  width: 282px !important;
  background:
    radial-gradient(circle at 14% 8%, rgba(128,103,242,.18), transparent 21%),
    linear-gradient(180deg, #090D16 0%, #101827 58%, #131E2F 100%) !important;
}

@media (min-width: 992px) {
  body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
  body:not(.sidebar-collapse).aurelis-signature .main-header {
    margin-left: 282px !important;
  }
}

body.aurelis-signature .brand-link {
  height: 92px !important;
  padding: 10px 16px !important;
  background: rgba(10,14,23,.96) !important;
  border-bottom: 1px solid rgba(255,255,255,.07) !important;
  box-shadow: none !important;
}

body.aurelis-signature .brand-link .brand-image {
  max-height: 58px !important;
  filter: drop-shadow(0 4px 14px rgba(36,206,210,.11));
}

body.aurelis-signature .global-filter-panel {
  margin: 13px 12px 13px !important;
  border-color: rgba(255,255,255,.075) !important;
  background:
    linear-gradient(145deg, rgba(255,255,255,.065), rgba(255,255,255,.022)) !important;
  box-shadow:
    inset 0 1px 0 rgba(255,255,255,.06),
    0 12px 34px rgba(0,0,0,.08) !important;
}

body.aurelis-signature .global-filter-panel::before {
  content: 'OPERATING LENS' !important;
  color: #9EAFF7 !important;
}

body.aurelis-signature .nav-sidebar .nav-link {
  min-height: 46px !important;
  margin: 3px 9px !important;
  border-radius: 13px !important;
  color: #AAB8C9 !important;
  font-size: .84rem !important;
}

body.aurelis-signature .nav-sidebar .nav-link:hover {
  transform: translateX(2px) !important;
  color: #FFFFFF !important;
  background: rgba(255,255,255,.055) !important;
}

body.aurelis-signature .nav-sidebar .nav-link.active {
  border-left: 0 !important;
  background:
    linear-gradient(100deg, var(--sig-accent-soft), rgba(255,255,255,.045)) !important;
  box-shadow:
    inset 0 0 0 1px rgba(255,255,255,.07),
    0 10px 25px rgba(0,0,0,.14) !important;
}

body.aurelis-signature .nav-sidebar .nav-link.active::before {
  content: '';
  position: absolute;
  left: -2px;
  top: 11px;
  bottom: 11px;
  width: 3px;
  border-radius: 99px;
  background: var(--sig-accent);
  box-shadow: 0 0 12px var(--sig-accent);
}

/* ---- Floating, finance-style top command bar ---- */
body.aurelis-signature .main-header {
  min-height: 70px !important;
  margin-top: 0 !important;
  padding: 7px 11px !important;
  background: rgba(247,250,253,.91) !important;
  border-bottom: 1px solid rgba(54,76,101,.09) !important;
  backdrop-filter: blur(25px) saturate(150%) !important;
}

body.aurelis-signature .main-header .navbar-nav {
  gap: 4px;
}

body.aurelis-signature .future-control-button,
body.aurelis-signature .navbar-report-button {
  min-height: 38px;
  padding: 9px 12px !important;
  border-radius: 999px !important;
  background: rgba(255,255,255,.76) !important;
  border: 1px solid rgba(45,67,92,.09) !important;
  color: #4D6074 !important;
  box-shadow: 0 5px 16px rgba(25,48,72,.045) !important;
}

body.aurelis-signature .signature-command-button {
  background: linear-gradient(110deg, #12192A, #1B2841) !important;
  color: #FFFFFF !important;
  border-color: rgba(128,103,242,.24) !important;
}

/* ---- Page title / command hero ---- */
body.aurelis-signature .future-page-hero {
  min-height: 134px !important;
  margin: 13px 0 18px !important;
  padding: 22px 25px !important;
  border-radius: 22px !important;
  background:
    radial-gradient(circle at 90% -20%, var(--sig-accent-soft), transparent 42%),
    radial-gradient(circle at 72% 160%, rgba(36,206,210,.14), transparent 42%),
    linear-gradient(112deg, #0A0F18 0%, #131B2B 56%, #182740 100%) !important;
  border: 1px solid rgba(255,255,255,.065) !important;
  box-shadow: 0 22px 54px rgba(17,34,54,.16) !important;
}

.signature-hero-copy h2 {
  font-weight: 720 !important;
  letter-spacing: -.65px !important;
}

.signature-hero-chip {
  min-height: 58px !important;
  border-radius: 15px !important;
  background: rgba(255,255,255,.045) !important;
}

.signature-tab-shortcut {
  border-radius: 999px !important;
  background: rgba(255,255,255,.042) !important;
}

/* ---- Modular cards: less generic Bootstrap, more designed system ---- */
body.aurelis-signature .card {
  border-radius: 20px !important;
  border: 1px solid rgba(56,79,105,.10) !important;
  background: rgba(255,255,255,.94) !important;
  box-shadow:
    0 10px 30px rgba(21,44,68,.065),
    0 1px 0 rgba(255,255,255,.92) inset !important;
}

body.aurelis-signature .card::before {
  left: 22px !important;
  right: auto !important;
  width: 58px !important;
  height: 3px !important;
  background: linear-gradient(90deg, var(--sig-accent), rgba(36,206,210,.86)) !important;
  opacity: .92 !important;
}

body.aurelis-signature .card:hover {
  transform: translateY(-3px) !important;
  box-shadow:
    0 21px 54px rgba(21,44,68,.12),
    0 1px 0 rgba(255,255,255,.92) inset !important;
}

body.aurelis-signature .card-header {
  min-height: 54px !important;
  padding: 13px 16px !important;
  border-radius: 20px 20px 0 0 !important;
  background: rgba(255,255,255,.64) !important;
}

body.aurelis-signature .card-header .card-title {
  font-size: .89rem !important;
  font-weight: 720 !important;
}

/* ---- KPI geometry inspired by the bright modular references ---- */
body.aurelis-signature .kpi-card {
  min-height: 118px !important;
  padding: 17px 18px !important;
  border-radius: 18px !important;
  border: 1px solid rgba(48,69,94,.09) !important;
  background:
    radial-gradient(circle at 92% 10%, var(--sig-accent-soft), transparent 34%),
    linear-gradient(145deg, #FFFFFF 0%, #F8FAFD 100%) !important;
  box-shadow: 0 10px 27px rgba(23,48,73,.065) !important;
}

body.aurelis-signature .kpi-card::after {
  left: 17px !important;
  bottom: 12px !important;
  width: 34px !important;
  height: 3px !important;
  border-radius: 99px !important;
}

body.aurelis-signature .kpi-card h2 {
  font-size: clamp(1.52rem, 2vw, 2.08rem) !important;
  letter-spacing: -.65px;
}

body.aurelis-signature .kpi-card .sub-text {
  margin-top: 13px !important;
  font-size: .64rem !important;
  letter-spacing: .25px !important;
  text-transform: none !important;
  font-weight: 560 !important;
}

/* ---- Segmented / pill controls used by the Atlas and high-level filters ---- */
.atlas-segmented-shell {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-wrap: wrap;
  padding: 6px;
  border-radius: 15px;
  background: #EEF2F7;
  border: 1px solid rgba(57,77,100,.08);
}

.atlas-filter-note {
  margin: 7px 0 0;
  color: #738295;
  font-size: .68rem;
  line-height: 1.5;
}

.atlas-map-shell {
  position: relative;
  overflow: hidden;
  border-radius: 18px;
  background:
    radial-gradient(circle at 50% 45%, rgba(128,103,242,.08), transparent 33%),
    linear-gradient(145deg, #111827, #0B1220);
  min-height: 600px;
}

.atlas-map-shell .plotly.html-widget {
  min-height: 590px;
}

.atlas-entity-summary {
  min-height: 156px;
  padding: 17px;
  border: 1px solid rgba(56,79,105,.10);
  border-radius: 17px;
  background:
    radial-gradient(circle at 94% 5%, var(--sig-accent-soft), transparent 42%),
    linear-gradient(145deg, #FFFFFF, #F7F9FC);
}

.atlas-entity-summary .atlas-type {
  color: var(--sig-accent);
  font-size: .63rem;
  font-weight: 850;
  letter-spacing: 1.1px;
  text-transform: uppercase;
}

.atlas-entity-summary h3 {
  margin: 5px 0 3px;
  color: #16283A;
  font-size: 1.03rem;
  font-weight: 760;
}

.atlas-entity-summary .atlas-place {
  color: #728094;
  font-size: .74rem;
}

.atlas-mini-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0,1fr));
  gap: 8px;
  margin-top: 13px;
}

.atlas-mini-stat {
  min-height: 65px;
  padding: 9px 10px;
  border-radius: 12px;
  background: rgba(240,244,249,.86);
  border: 1px solid rgba(54,76,101,.07);
}

.atlas-mini-stat span {
  display: block;
  color: #7A8797;
  font-size: .56rem;
  font-weight: 760;
  text-transform: uppercase;
  letter-spacing: .7px;
}

.atlas-mini-stat strong {
  display: block;
  margin-top: 3px;
  color: #1A2C3D;
  font-size: .82rem;
  font-weight: 760;
}

.atlas-network-legend {
  display: flex;
  flex-wrap: wrap;
  gap: 7px;
  margin-top: 8px;
}

.atlas-network-legend span {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  padding: 5px 8px;
  border-radius: 999px;
  background: rgba(238,243,249,.85);
  color: #607083;
  font-size: .62rem;
  font-weight: 680;
}

.atlas-network-legend i {
  width: 8px;
  height: 8px;
  display: inline-block;
  border-radius: 50%;
}

/* ---- Midnight analytical canvas; reuses the existing Ambient persistence ---- */
body.aurelis-signature.aurelis-ambient .content-wrapper {
  background:
    radial-gradient(circle at 92% 0%, rgba(128,103,242,.18), transparent 28%),
    radial-gradient(circle at 12% 90%, rgba(36,206,210,.10), transparent 27%),
    linear-gradient(145deg, #0D1320 0%, #111827 50%, #131B2A 100%) !important;
  color: #DDE6F1 !important;
}

body.aurelis-signature.aurelis-ambient .main-header {
  background: rgba(9,14,24,.90) !important;
  border-color: rgba(255,255,255,.06) !important;
}

body.aurelis-signature.aurelis-ambient .future-control-button,
body.aurelis-signature.aurelis-ambient .navbar-report-button {
  background: rgba(255,255,255,.055) !important;
  border-color: rgba(255,255,255,.07) !important;
  color: #C6D3E2 !important;
}

body.aurelis-signature.aurelis-ambient .card {
  background: rgba(20,27,43,.96) !important;
  border-color: rgba(255,255,255,.07) !important;
  box-shadow: 0 16px 42px rgba(0,0,0,.23) !important;
  color: #DCE6F0 !important;
}

body.aurelis-signature.aurelis-ambient .card-header {
  background: rgba(255,255,255,.025) !important;
  border-color: rgba(255,255,255,.055) !important;
}

body.aurelis-signature.aurelis-ambient .card-header .card-title,
body.aurelis-signature.aurelis-ambient .card h1,
body.aurelis-signature.aurelis-ambient .card h2,
body.aurelis-signature.aurelis-ambient .card h3,
body.aurelis-signature.aurelis-ambient .card h4,
body.aurelis-signature.aurelis-ambient .card strong {
  color: #F5F8FC !important;
}

body.aurelis-signature.aurelis-ambient .kpi-card {
  background:
    radial-gradient(circle at 94% 7%, var(--sig-accent-soft), transparent 37%),
    linear-gradient(145deg, #151D2E, #121928) !important;
  border-color: rgba(255,255,255,.065) !important;
  box-shadow: 0 13px 34px rgba(0,0,0,.20) !important;
}

body.aurelis-signature.aurelis-ambient .kpi-card h2 {
  color: #FFFFFF !important;
}

body.aurelis-signature.aurelis-ambient .kpi-card p,
body.aurelis-signature.aurelis-ambient .kpi-card .sub-text,
body.aurelis-signature.aurelis-ambient .metric-definition-note,
body.aurelis-signature.aurelis-ambient .chart-toolbar-note {
  color: #94A6BA !important;
}

body.aurelis-signature.aurelis-ambient .form-control,
body.aurelis-signature.aurelis-ambient .selectize-input,
body.aurelis-signature.aurelis-ambient select,
body.aurelis-signature.aurelis-ambient textarea {
  background: rgba(255,255,255,.055) !important;
  border-color: rgba(255,255,255,.075) !important;
  color: #E8EEF5 !important;
}

body.aurelis-signature.aurelis-ambient .selectize-dropdown {
  background: #172133 !important;
  color: #DDE6F1 !important;
  border-color: rgba(255,255,255,.08) !important;
}

body.aurelis-signature.aurelis-ambient .selectize-dropdown .option {
  color: #DDE6F1 !important;
}

body.aurelis-signature.aurelis-ambient .selectize-dropdown .active {
  background: rgba(128,103,242,.18) !important;
}

body.aurelis-signature.aurelis-ambient table.dataTable,
body.aurelis-signature.aurelis-ambient table.dataTable tbody td {
  color: #D7E1EC !important;
  background: #151D2E !important;
}

body.aurelis-signature.aurelis-ambient table.dataTable thead th {
  color: #AFC0D0 !important;
  background: #101827 !important;
  border-color: rgba(255,255,255,.06) !important;
}

body.aurelis-signature.aurelis-ambient .atlas-entity-summary {
  background:
    radial-gradient(circle at 94% 5%, var(--sig-accent-soft), transparent 42%),
    linear-gradient(145deg, #151D2E, #121928);
  border-color: rgba(255,255,255,.065);
}

body.aurelis-signature.aurelis-ambient .atlas-entity-summary h3,
body.aurelis-signature.aurelis-ambient .atlas-mini-stat strong {
  color: #FFFFFF;
}

body.aurelis-signature.aurelis-ambient .atlas-mini-stat,
body.aurelis-signature.aurelis-ambient .atlas-network-legend span {
  background: rgba(255,255,255,.045);
  border-color: rgba(255,255,255,.06);
  color: #AAB9C9;
}

body.aurelis-signature.aurelis-ambient .aurelis-signature-footer {
  border-top-color: rgba(255,255,255,.06);
  color: #6F8194;
}

@media (max-width: 1500px) {
  body.aurelis-signature .main-sidebar { width: 260px !important; }
}

@media (min-width: 992px) and (max-width: 1500px) {
  body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
  body:not(.sidebar-collapse).aurelis-signature .main-header {
    margin-left: 260px !important;
  }
}

@media (max-width: 767px) {
  .atlas-mini-grid { grid-template-columns: 1fr; }
  .atlas-map-shell { min-height: 500px; }
}
"


# ------------------------------------------------------------------------------
# ARMEL PRISM FUSION v6 — COMPLETE VISIBLE TEMPLATE REPLACEMENT
# ------------------------------------------------------------------------------

prism_v6_css <- "
:root {
  --pf-bg:#eef3f9; --pf-shell:#0d1320; --pf-surface:#fff; --pf-surface-2:#f7f9fc;
  --pf-text:#172435; --pf-muted:#748296; --pf-border:rgba(42,64,88,.10);
  --pf-accent:#6f63f6; --pf-accent-2:#25c5d9; --pf-accent-soft:rgba(111,99,246,.12);
  --pf-positive:#5bd394; --pf-warning:#f8c95d; --pf-danger:#ff6f87;
  --pf-radius-xl:26px; --pf-radius-lg:19px; --pf-radius-md:13px;
  --pf-shadow:0 18px 55px rgba(21,39,60,.09); --pf-shadow-strong:0 26px 75px rgba(21,39,60,.16);
}

/* PAGE-WIDE PALETTES */
body.aurelis-signature[data-aurelis-page='exec_overview'] {
  --pf-bg:#101524; --pf-surface:#171e31; --pf-surface-2:#12192a; --pf-text:#f4f6fb;
  --pf-muted:#92a0b5; --pf-border:rgba(255,255,255,.07); --pf-accent:#8a6df5;
  --pf-accent-2:#25c9d7; --pf-accent-soft:rgba(138,109,245,.16);
}
body.aurelis-signature[data-aurelis-page='global_network'] {
  --pf-bg:#080b11; --pf-surface:#111620; --pf-surface-2:#161c29; --pf-text:#f7f4ff;
  --pf-muted:#9b9cac; --pf-border:rgba(255,255,255,.07); --pf-accent:#b291ff;
  --pf-accent-2:#ffd76b; --pf-accent-soft:rgba(178,145,255,.16);
}
body.aurelis-signature[data-aurelis-page='multi_user_workspace'] {
  --pf-bg:#f4f7fb; --pf-surface:#fff; --pf-surface-2:#f7f9fc; --pf-text:#172435;
  --pf-muted:#748296; --pf-border:rgba(42,64,88,.10); --pf-accent:#f24e86;
  --pf-accent-2:#43c6df; --pf-accent-soft:rgba(242,78,134,.11);
}
body.aurelis-signature[data-aurelis-page='kpi_board'] {
  --pf-bg:#f7f8fb; --pf-surface:#fff; --pf-surface-2:#f3f6fb; --pf-text:#1d2a3b;
  --pf-muted:#7a8798; --pf-border:rgba(39,63,91,.09); --pf-accent:#6948ec;
  --pf-accent-2:#18b9de; --pf-accent-soft:rgba(105,72,236,.10);
}
body.aurelis-signature[data-aurelis-page='sale_performance'] {
  --pf-bg:#fff3ee; --pf-surface:#fffaf7; --pf-surface-2:#f8efff; --pf-text:#32283a;
  --pf-muted:#8f7d8d; --pf-border:rgba(91,65,93,.09); --pf-accent:#7d63f5;
  --pf-accent-2:#ff8b6d; --pf-accent-soft:rgba(125,99,245,.11);
}
body.aurelis-signature[data-aurelis-page='quotation_studio'] {
  --pf-bg:#0c0d10; --pf-surface:#171717; --pf-surface-2:#222224; --pf-text:#f6f3ec;
  --pf-muted:#a7a097; --pf-border:rgba(255,255,255,.07); --pf-accent:#f3b65c;
  --pf-accent-2:#caa576; --pf-accent-soft:rgba(243,182,92,.14);
}
body.aurelis-signature[data-aurelis-page='customer_performance'] {
  --pf-bg:#eef9f6; --pf-surface:#fff; --pf-surface-2:#f6fbfa; --pf-text:#18343b;
  --pf-muted:#6f8a8c; --pf-border:rgba(35,95,95,.09); --pf-accent:#18b7a4;
  --pf-accent-2:#4a8df7; --pf-accent-soft:rgba(24,183,164,.11);
}
body.aurelis-signature[data-aurelis-page='client_statement'] {
  --pf-bg:#edf4fb; --pf-surface:#fff; --pf-surface-2:#f4f8fc; --pf-text:#19314c;
  --pf-muted:#74889e; --pf-border:rgba(43,83,123,.10); --pf-accent:#2f7bd8;
  --pf-accent-2:#21bcd1; --pf-accent-soft:rgba(47,123,216,.11);
}
body.aurelis-signature[data-aurelis-page='monthly_sales'] {
  --pf-bg:#222f55; --pf-surface:#29426f; --pf-surface-2:#20375f; --pf-text:#f4f8ff;
  --pf-muted:#b3c3dc; --pf-border:rgba(255,255,255,.08); --pf-accent:#55abff;
  --pf-accent-2:#70e8ba; --pf-accent-soft:rgba(85,171,255,.15);
}
body.aurelis-signature[data-aurelis-page='buyer_activity'] {
  --pf-bg:#edf4ff; --pf-surface:#fff; --pf-surface-2:#f7f9ff; --pf-text:#24314a;
  --pf-muted:#7c879c; --pf-border:rgba(60,78,110,.09); --pf-accent:#5c6ee8;
  --pf-accent-2:#f1719d; --pf-accent-soft:rgba(92,110,232,.11);
}
body.aurelis-signature[data-aurelis-page='order_tracker'] {
  --pf-bg:#24477d; --pf-surface:#2c548d; --pf-surface-2:#274b82; --pf-text:#f7faff;
  --pf-muted:#bfd0e6; --pf-border:rgba(255,255,255,.08); --pf-accent:#61b5ff;
  --pf-accent-2:#ffcf6c; --pf-accent-soft:rgba(97,181,255,.15);
}
body.aurelis-signature[data-aurelis-page='inventory_tab'] {
  --pf-bg:#11182a; --pf-surface:#182239; --pf-surface-2:#121b30; --pf-text:#f4f8ff;
  --pf-muted:#9aaac2; --pf-border:rgba(255,255,255,.07); --pf-accent:#20d5d2;
  --pf-accent-2:#6867ff; --pf-accent-soft:rgba(32,213,210,.14);
}
body.aurelis-signature[data-aurelis-page='buyer_tool'] {
  --pf-bg:#100d1a; --pf-surface:#181426; --pf-surface-2:#201831; --pf-text:#fbf8ff;
  --pf-muted:#a9a0ba; --pf-border:rgba(255,255,255,.07); --pf-accent:#a06cf7;
  --pf-accent-2:#ff7ec3; --pf-accent-soft:rgba(160,108,247,.15);
}
body.aurelis-signature[data-aurelis-page='ar_tab'] {
  --pf-bg:#fff4f4; --pf-surface:#fff; --pf-surface-2:#fff8f8; --pf-text:#3a292e;
  --pf-muted:#8f7b82; --pf-border:rgba(104,62,74,.09); --pf-accent:#f06078;
  --pf-accent-2:#7a6cf5; --pf-accent-soft:rgba(240,96,120,.11);
}
body.aurelis-signature[data-aurelis-page='contacts_tab'] {
  --pf-bg:#191d31; --pf-surface:#22283d; --pf-surface-2:#1b2135; --pf-text:#f8f7ff;
  --pf-muted:#a6abc0; --pf-border:rgba(255,255,255,.07); --pf-accent:#d454f1;
  --pf-accent-2:#29d9db; --pf-accent-soft:rgba(212,84,241,.15);
}
body.aurelis-signature[data-aurelis-page='misc_tab'] {
  --pf-bg:#f0f4f8; --pf-surface:#fff; --pf-surface-2:#f7f9fb; --pf-text:#263547;
  --pf-muted:#7d8998; --pf-border:rgba(47,68,91,.09); --pf-accent:#53667a;
  --pf-accent-2:#27b7aa; --pf-accent-soft:rgba(83,102,122,.10);
}

/* COMPLETELY NEW COMPACT ICON RAIL */
body.aurelis-signature, body.aurelis-signature .wrapper { background:var(--pf-bg) !important; color:var(--pf-text) !important; }
body.aurelis-signature .main-sidebar {
  width:118px !important;
  background:radial-gradient(circle at 30% 2%,rgba(138,109,245,.16),transparent 17%),linear-gradient(180deg,#090d16 0%,#111827 100%) !important;
  border-right:1px solid rgba(255,255,255,.06) !important; box-shadow:14px 0 42px rgba(3,9,17,.20) !important;
}
@media (min-width:992px) {
  body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
  body:not(.sidebar-collapse).aurelis-signature .main-header { margin-left:118px !important; }
}
body.aurelis-signature .brand-link {
  height:90px !important; padding:13px 9px !important; display:flex !important; align-items:center; justify-content:center;
  background:transparent !important; border-bottom:0 !important; box-shadow:none !important;
}
body.aurelis-signature .brand-link .brand-image { max-height:56px !important; max-width:90px !important; margin:0 !important; float:none !important; }
body.aurelis-signature .sidebar { padding:3px 6px 34px !important; }
body.aurelis-signature .global-filter-panel, body.aurelis-signature .sidebar > hr { display:none !important; }
body.aurelis-signature .nav-sidebar { padding:0 !important; }
body.aurelis-signature .nav-sidebar .nav-link {
  width:94px !important; min-height:66px !important; margin:5px auto !important; padding:9px 7px 8px !important;
  display:flex !important; flex-direction:column; justify-content:center; align-items:center; gap:5px;
  border:1px solid transparent !important; border-radius:16px !important; color:#8d9bae !important;
  text-align:center !important; font-size:.58rem !important; font-weight:650 !important; line-height:1.05 !important;
  white-space:normal !important; transition:transform .18s ease,background .18s ease,color .18s ease !important;
}
body.aurelis-signature .nav-sidebar .nav-link p { display:block !important; margin:0 !important; max-width:84px; font-size:.58rem !important; line-height:1.05 !important; white-space:normal !important; }
body.aurelis-signature .nav-sidebar .nav-icon { width:auto !important; margin:0 !important; font-size:1.05rem !important; }
body.aurelis-signature .nav-sidebar .nav-link:hover { transform:translateY(-2px) !important; background:rgba(255,255,255,.055) !important; color:#fff !important; }
body.aurelis-signature .nav-sidebar .nav-link.active {
  color:#fff !important; border-color:rgba(255,255,255,.08) !important;
  background:radial-gradient(circle at 50% 18%,var(--pf-accent-soft),transparent 45%),rgba(255,255,255,.07) !important;
  box-shadow:0 12px 30px rgba(0,0,0,.16) !important;
}
body.aurelis-signature .nav-sidebar .nav-link.active .nav-icon { color:var(--pf-accent) !important; filter:drop-shadow(0 0 9px var(--pf-accent-soft)); }

/* SLIM TOP BAR */
body.aurelis-signature .main-header {
  min-height:66px !important; padding:7px 16px !important;
  background:color-mix(in srgb,var(--pf-surface) 90%,transparent) !important;
  border-bottom:1px solid var(--pf-border) !important; box-shadow:0 10px 30px rgba(20,39,61,.06) !important;
  backdrop-filter:blur(24px) saturate(150%) !important;
}
body.aurelis-signature .future-control-button, body.aurelis-signature .navbar-report-button {
  min-height:38px !important; padding:8px 12px !important; border:1px solid var(--pf-border) !important;
  border-radius:999px !important; background:var(--pf-surface-2) !important; color:var(--pf-text) !important;
  box-shadow:none !important; font-size:.72rem !important;
}
body.aurelis-signature .signature-command-button {
  color:#fff !important; border-color:transparent !important;
  background:linear-gradient(110deg,var(--pf-accent),var(--pf-accent-2)) !important;
}
body.aurelis-signature .header-logo-lockup { display:none !important; }

/* FLOATING APPLICATION CANVAS */
body.aurelis-signature .content-wrapper {
  position:relative; min-height:calc(100vh - 66px) !important; padding:14px 16px 28px !important;
  background:radial-gradient(circle at 96% 0%,var(--pf-accent-soft),transparent 24%),var(--pf-bg) !important;
}
body.aurelis-signature .content-wrapper::before, body.aurelis-signature .aurelis-content-watermark { display:none !important; }
body.aurelis-signature .content { max-width:1900px; margin:0 auto; padding:0 !important; }

/* NEW GLOBAL FILTER DOCK */
.prism-filter-dock {
  position:sticky; top:72px; z-index:100; display:grid;
  grid-template-columns:minmax(150px,210px) minmax(150px,210px) 1fr auto;
  gap:10px; align-items:center; margin:0 0 13px; padding:9px 11px;
  border:1px solid var(--pf-border); border-radius:18px;
  background:color-mix(in srgb,var(--pf-surface) 91%,transparent);
  box-shadow:0 12px 32px rgba(21,41,63,.07); backdrop-filter:blur(22px) saturate(150%);
}
.prism-filter-dock .form-group { margin-bottom:0 !important; }
.prism-filter-dock .control-label {
  margin:0 0 3px !important; color:var(--pf-muted) !important; font-size:.56rem !important;
  font-weight:800 !important; letter-spacing:.9px; text-transform:uppercase;
}
.prism-filter-context { min-width:0; padding:2px 11px; }
.prism-filter-context span { display:block; color:var(--pf-muted); font-size:.57rem; font-weight:800; letter-spacing:.9px; text-transform:uppercase; }
.prism-filter-context strong { display:block; margin-top:2px; overflow:hidden; color:var(--pf-text); font-size:.76rem; font-weight:720; white-space:nowrap; text-overflow:ellipsis; }
.prism-mode-pill { display:flex; align-items:center; gap:7px; padding:8px 11px; border-radius:999px; background:var(--pf-accent-soft); color:var(--pf-accent); font-size:.66rem; font-weight:760; }

/* NEW COMPACT PAGE MASTHEAD */
body.aurelis-signature .future-page-hero {
  min-height:0 !important; margin:0 0 15px !important; padding:18px 20px !important;
  border:1px solid var(--pf-border) !important; border-radius:22px !important; overflow:hidden;
  background:radial-gradient(circle at 92% 15%,var(--pf-accent-soft),transparent 35%),var(--pf-surface) !important;
  box-shadow:var(--pf-shadow) !important;
}
.prism-masthead { display:grid; grid-template-columns:minmax(0,1fr) auto; gap:18px; align-items:center; }
.prism-masthead-kicker { display:flex; align-items:center; gap:7px; margin-bottom:4px; color:var(--pf-accent); font-size:.58rem; font-weight:850; letter-spacing:1.15px; text-transform:uppercase; }
.prism-masthead-kicker::before { content:''; width:18px; height:3px; border-radius:99px; background:linear-gradient(90deg,var(--pf-accent),var(--pf-accent-2)); }
.prism-masthead h2 { margin:0; color:var(--pf-text); font-size:clamp(1.35rem,2vw,2rem); font-weight:720; letter-spacing:-.65px; }
.prism-masthead p { max-width:900px; margin:5px 0 0; color:var(--pf-muted); font-size:.75rem; line-height:1.5; }
.prism-masthead-stats { display:flex; gap:8px; align-items:stretch; }
.prism-masthead-stat { min-width:105px; padding:9px 11px; border:1px solid var(--pf-border); border-radius:14px; background:var(--pf-surface-2); }
.prism-masthead-stat span { display:block; color:var(--pf-muted); font-size:.52rem; font-weight:800; text-transform:uppercase; letter-spacing:.75px; }
.prism-masthead-stat strong { display:block; margin-top:3px; color:var(--pf-text); font-size:.72rem; font-weight:760; }

/* MODULAR REFERENCE-STYLE CARDS */
body.aurelis-signature .card {
  position:relative; overflow:hidden !important; border:1px solid var(--pf-border) !important;
  border-radius:var(--pf-radius-lg) !important; background:var(--pf-surface) !important;
  color:var(--pf-text) !important; box-shadow:var(--pf-shadow) !important;
  transition:transform .18s ease,box-shadow .18s ease !important;
}
body.aurelis-signature .card::before {
  content:''; position:absolute; inset:0 auto auto 0; width:100%; height:3px;
  background:linear-gradient(90deg,var(--pf-accent),var(--pf-accent-2),transparent 72%); opacity:.75;
}
body.aurelis-signature .card:hover { transform:translateY(-3px) !important; box-shadow:var(--pf-shadow-strong) !important; }
body.aurelis-signature .card-header { min-height:50px !important; padding:13px 16px !important; border-bottom:1px solid var(--pf-border) !important; background:transparent !important; }
body.aurelis-signature .card-header .card-title { color:var(--pf-text) !important; font-size:.84rem !important; font-weight:740 !important; }
body.aurelis-signature .card-body { background:transparent !important; }

body.aurelis-signature .kpi-card {
  position:relative; min-height:112px !important; overflow:hidden; padding:15px 16px 17px !important;
  border:1px solid var(--pf-border) !important; border-left:0 !important; border-radius:17px !important;
  background:radial-gradient(circle at 88% 13%,var(--pf-accent-soft),transparent 34%),var(--pf-surface) !important;
  box-shadow:0 13px 34px rgba(20,38,59,.07) !important;
}
body.aurelis-signature .kpi-card::before {
  content:''; position:absolute; right:14px; top:14px; width:34px; height:34px; border-radius:50%;
  border:7px solid color-mix(in srgb,var(--pf-accent) 22%,transparent); border-top-color:var(--pf-accent);
}
body.aurelis-signature .kpi-card::after { left:16px !important; bottom:11px !important; width:30px !important; height:3px !important; background:linear-gradient(90deg,var(--pf-accent),var(--pf-accent-2)) !important; }
body.aurelis-signature .kpi-card h2 { margin-top:6px !important; color:var(--pf-text) !important; font-size:clamp(1.45rem,1.9vw,2rem) !important; font-weight:760 !important; letter-spacing:-.7px; }
body.aurelis-signature .kpi-card p, body.aurelis-signature .kpi-card .sub-text,
body.aurelis-signature .metric-definition-note, body.aurelis-signature .chart-toolbar-note { color:var(--pf-muted) !important; }

/* FORMS / TABLES / BUTTONS */
body.aurelis-signature .form-control, body.aurelis-signature .selectize-input, body.aurelis-signature select, body.aurelis-signature textarea {
  min-height:36px; border:1px solid var(--pf-border) !important; border-radius:11px !important;
  background:var(--pf-surface-2) !important; color:var(--pf-text) !important; box-shadow:none !important;
}
body.aurelis-signature .selectize-dropdown { border-color:var(--pf-border) !important; background:var(--pf-surface) !important; color:var(--pf-text) !important; }
body.aurelis-signature .selectize-dropdown .option { color:var(--pf-text) !important; }
body.aurelis-signature .selectize-dropdown .active { background:var(--pf-accent-soft) !important; color:var(--pf-text) !important; }
body.aurelis-signature .btn-primary, body.aurelis-signature .btn-info {
  border-color:transparent !important; border-radius:999px !important;
  background:linear-gradient(105deg,var(--pf-accent),var(--pf-accent-2)) !important; box-shadow:none !important;
}
body.aurelis-signature .btn-outline-secondary, body.aurelis-signature .btn-outline-primary { border-radius:999px !important; }
body.aurelis-signature table.dataTable { color:var(--pf-text) !important; background:transparent !important; }
body.aurelis-signature table.dataTable thead th {
  padding-top:11px !important; padding-bottom:11px !important; border-bottom:1px solid var(--pf-border) !important;
  background:var(--pf-surface-2) !important; color:var(--pf-muted) !important; font-size:.66rem !important;
  font-weight:800 !important; letter-spacing:.35px; text-transform:uppercase;
}
body.aurelis-signature table.dataTable tbody td { border-top:1px solid var(--pf-border) !important; background:transparent !important; color:var(--pf-text) !important; }
body.aurelis-signature table.dataTable tbody tr:hover td { background:var(--pf-accent-soft) !important; }
body.aurelis-signature .plot-container, body.aurelis-signature .svg-container, body.aurelis-signature .plotly { background:transparent !important; }

/* COMMAND PALETTE */
.signature-command-overlay { background:rgba(3,7,13,.50) !important; backdrop-filter:blur(7px) !important; }
.signature-command-panel {
  top:78px !important; right:18px !important; width:min(430px,calc(100vw - 24px)) !important;
  border:1px solid rgba(255,255,255,.08) !important; border-radius:24px !important;
  background:radial-gradient(circle at 100% 0%,rgba(138,109,245,.22),transparent 30%),linear-gradient(155deg,#0d1320,#171f31) !important;
  box-shadow:0 34px 95px rgba(0,0,0,.40) !important;
}

/* ATLAS */
.atlas-map-shell {
  border:1px solid rgba(255,255,255,.06) !important; border-radius:20px !important;
  background:radial-gradient(circle at 50% 50%,rgba(138,109,245,.11),transparent 36%),#090d16 !important;
}
.atlas-entity-summary { border:1px solid var(--pf-border) !important; border-radius:17px !important; background:var(--pf-surface-2) !important; }
.atlas-entity-summary h3, .atlas-mini-stat strong { color:var(--pf-text) !important; }
.atlas-mini-stat, .atlas-network-legend span { border-color:var(--pf-border) !important; background:var(--pf-surface-2) !important; color:var(--pf-muted) !important; }

/* FOOTER / FOCUS */
.aurelis-signature-footer { margin:18px 0 2px !important; padding:11px 4px 1px !important; border-top:1px solid var(--pf-border) !important; color:var(--pf-muted) !important; font-size:.61rem !important; }
.aurelis-card-focus-backdrop { background:rgba(3,8,14,.58) !important; backdrop-filter:blur(8px) !important; }
body.aurelis-card-focus-mode .card.aurelis-card-focus { inset:82px 24px 20px 142px !important; border-radius:24px !important; box-shadow:0 38px 110px rgba(0,0,0,.38) !important; }
body.sidebar-collapse.aurelis-card-focus-mode .card.aurelis-card-focus { left:24px !important; }

/* CONTRAST TOGGLE: enhancement only, not an old alternate template */
body.aurelis-signature.aurelis-ambient { filter:saturate(1.07); }
body.aurelis-signature.aurelis-ambient .card,
body.aurelis-signature.aurelis-ambient .future-page-hero,
body.aurelis-signature.aurelis-ambient .prism-filter-dock { box-shadow:0 22px 64px rgba(0,0,0,.18) !important; }

/* RESPONSIVE */
@media (max-width:1300px) {
  .prism-filter-dock { grid-template-columns:minmax(135px,180px) minmax(135px,180px) 1fr; }
  .prism-filter-dock .prism-mode-pill { display:none; }
}
@media (max-width:991px) {
  body.aurelis-signature .main-sidebar { width:250px !important; }
  body.aurelis-signature .nav-sidebar .nav-link {
    width:auto !important; min-height:44px !important; flex-direction:row !important;
    justify-content:flex-start !important; padding:9px 12px !important; text-align:left !important; font-size:.76rem !important;
  }
  body.aurelis-signature .nav-sidebar .nav-link p { max-width:none !important; font-size:.76rem !important; }
  .prism-filter-dock { position:relative; top:auto; grid-template-columns:1fr 1fr; }
  .prism-filter-context { grid-column:1 / -1; }
  .prism-masthead { grid-template-columns:1fr; }
  .prism-masthead-stats { flex-wrap:wrap; }
  body.aurelis-card-focus-mode .card.aurelis-card-focus { inset:75px 10px 12px 10px !important; }
}
@media (max-width:620px) {
  .prism-filter-dock { grid-template-columns:1fr; }
  .prism-masthead-stats { display:none; }
  body.aurelis-signature .content-wrapper { padding:8px 8px 20px !important; }
}
"
custom_css <- paste0(custom_css, prism_v6_css)

obsidian_v7_css <- "
/* ================================================================
   OBSIDIAN COMMAND v7
   Unified dark dashboard language across every business menu.
   ================================================================ */

:root {
  --pf-bg:#0B1119;
  --pf-surface:#121A24;
  --pf-surface-2:#17212D;
  --pf-text:#EEF4FA;
  --pf-muted:#9AABBD;
  --pf-border:rgba(177,198,218,.10);
  --pf-accent:#FF7A45;
  --pf-accent-2:#FFC857;
  --pf-accent-soft:rgba(255,122,69,.14);
}

/* Same dark foundation for all pages; accents still communicate function. */
body.aurelis-signature[data-aurelis-page] {
  --pf-bg:#0B1119;
  --pf-surface:#121A24;
  --pf-surface-2:#17212D;
  --pf-text:#EEF4FA;
  --pf-muted:#9AABBD;
  --pf-border:rgba(177,198,218,.10);
  --pf-accent:#FF7A45;
  --pf-accent-2:#FFC857;
  --pf-accent-soft:rgba(255,122,69,.14);
}
body.aurelis-signature[data-aurelis-page='global_network'] {
  --pf-accent:#FFC857; --pf-accent-2:#32D1C6; --pf-accent-soft:rgba(255,200,87,.13);
}
body.aurelis-signature[data-aurelis-page='inventory_tab'],
body.aurelis-signature[data-aurelis-page='contacts_tab'] {
  --pf-accent:#32D1C6; --pf-accent-2:#8A72F5; --pf-accent-soft:rgba(50,209,198,.13);
}
body.aurelis-signature[data-aurelis-page='ar_tab'] {
  --pf-accent:#F0544F; --pf-accent-2:#FFC857; --pf-accent-soft:rgba(240,84,79,.13);
}
body.aurelis-signature[data-aurelis-page='quotation_studio'],
body.aurelis-signature[data-aurelis-page='buyer_tool'] {
  --pf-accent:#FFC857; --pf-accent-2:#FF7A45; --pf-accent-soft:rgba(255,200,87,.13);
}
body.aurelis-signature[data-aurelis-page='customer_performance'],
body.aurelis-signature[data-aurelis-page='sale_performance'] {
  --pf-accent:#63D99A; --pf-accent-2:#32D1C6; --pf-accent-soft:rgba(99,217,154,.12);
}

body.aurelis-signature,
body.aurelis-signature .wrapper {
  background:#0B1119 !important;
  color:#EEF4FA !important;
}
body.aurelis-signature .content-wrapper {
  padding-top:12px !important;
  background:
    radial-gradient(circle at 86% -5%,rgba(255,122,69,.08),transparent 25%),
    radial-gradient(circle at 35% 120%,rgba(50,209,198,.045),transparent 28%),
    #0B1119 !important;
}
body.aurelis-signature .content {
  max-width:1920px !important;
}

/* Dense reference-style modular panels. */
body.aurelis-signature .card {
  margin-bottom:14px !important;
  background:linear-gradient(155deg,#141D28 0%,#101821 100%) !important;
  border-color:rgba(177,198,218,.10) !important;
  box-shadow:0 14px 38px rgba(0,0,0,.24) !important;
}
body.aurelis-signature .card:hover {
  box-shadow:0 22px 54px rgba(0,0,0,.34) !important;
}
body.aurelis-signature .card-header {
  min-height:45px !important;
  padding:11px 15px !important;
  background:rgba(255,255,255,.012) !important;
}
body.aurelis-signature .card-body {
  padding:14px 15px !important;
}
body.aurelis-signature .card::before {
  background:linear-gradient(90deg,var(--pf-accent),var(--pf-accent-2),transparent 76%) !important;
}
body.aurelis-signature .card-header .card-title,
body.aurelis-signature .card h1,
body.aurelis-signature .card h2,
body.aurelis-signature .card h3,
body.aurelis-signature .card h4,
body.aurelis-signature .card strong {
  color:#F2F6FA !important;
}

body.aurelis-signature .kpi-card {
  min-height:106px !important;
  background:
    radial-gradient(circle at 91% 10%,var(--pf-accent-soft),transparent 38%),
    linear-gradient(150deg,#182330,#111923) !important;
  border-color:rgba(177,198,218,.10) !important;
  box-shadow:0 12px 32px rgba(0,0,0,.22) !important;
}
body.aurelis-signature .kpi-card h2 {
  color:#F5F8FC !important;
}
body.aurelis-signature .kpi-card p,
body.aurelis-signature .kpi-card .sub-text,
body.aurelis-signature .metric-definition-note,
body.aurelis-signature .chart-toolbar-note {
  color:#9AABBD !important;
}

/* Forms and tables remain readable on dark panels. */
body.aurelis-signature .form-control,
body.aurelis-signature .selectize-input,
body.aurelis-signature select,
body.aurelis-signature textarea {
  background:#17212D !important;
  color:#E7EEF6 !important;
  border-color:rgba(177,198,218,.12) !important;
}
body.aurelis-signature .selectize-dropdown {
  background:#17212D !important;
  color:#E7EEF6 !important;
  border-color:rgba(177,198,218,.12) !important;
}
body.aurelis-signature .selectize-dropdown .option {
  color:#E7EEF6 !important;
}
body.aurelis-signature .selectize-dropdown .active {
  background:var(--pf-accent-soft) !important;
}
body.aurelis-signature table.dataTable thead th {
  background:#182330 !important;
  color:#AFC0D1 !important;
}
body.aurelis-signature table.dataTable tbody td {
  background:transparent !important;
  color:#E6EDF5 !important;
}
body.aurelis-signature table.dataTable tbody tr:hover td {
  background:rgba(255,122,69,.08) !important;
}

/* KPI content begins immediately below navbar. */
body.aurelis-signature .tab-content > .tab-pane {
  padding-top:0 !important;
}
body.aurelis-signature .interaction-tip {
  margin-top:4px !important;
}

/* Logo locations */
body.aurelis-signature .header-logo-lockup {
  display:flex !important;
  min-width:170px;
  gap:9px;
  padding:4px 11px;
  margin-right:8px;
  border-radius:12px;
  background:rgba(255,255,255,.035);
  border:1px solid rgba(255,255,255,.055);
}
body.aurelis-signature .header-logo-lockup img {
  display:block !important;
  width:76px !important;
  max-width:76px !important;
  height:auto !important;
  object-fit:contain;
}
body.aurelis-signature .header-logo-lockup span {
  display:block !important;
  max-width:165px;
  overflow:hidden;
  color:#AAB9C8 !important;
  font-size:.60rem !important;
  font-weight:720 !important;
  line-height:1.15;
  white-space:nowrap;
  text-overflow:ellipsis;
}

/* Year/Month stay in navbar without pushing business KPIs downward.
   IMPORTANT: bs4Dash validates custom navbar <li> items and requires the
   Bootstrap `dropdown` class on these wrappers. Do not remove it. */
.prism-navbar-filter {
  margin-right:6px;
}

.prism-navbar-filter.dropdown > .dropdown-toggle::after,
.prism-navbar-filter.dropdown::after {
  display: none !important;
}

.prism-navbar-filter .form-group,
.prism-navbar-filter .selectize-control {
  margin:0 !important;
}
.prism-navbar-filter .selectize-input,
.prism-navbar-filter select {
  min-height:34px !important;
  height:34px !important;
  padding-top:6px !important;
  padding-bottom:6px !important;
  border-radius:999px !important;
  background:#151F2B !important;
}

/* Mobile filter fallback. */
.prism-mobile-filter-row {
  display:grid;
  grid-template-columns:1fr 1fr;
  gap:8px;
  margin:0 0 10px;
  padding:8px;
  border-radius:14px;
  border:1px solid var(--pf-border);
  background:#121A24;
}
.prism-mobile-filter-row .form-group {
  margin-bottom:0 !important;
}

/* Third logo location: subtle working-canvas seal. */
body.aurelis-signature .aurelis-content-watermark {
  display:block !important;
  position:fixed !important;
  right:24px !important;
  bottom:18px !important;
  width:86px !important;
  height:auto !important;
  opacity:.075 !important;
  z-index:0 !important;
  pointer-events:none !important;
}
body.aurelis-signature .aurelis-content-watermark img {
  width:100% !important;
  height:auto !important;
}

/* Atlas reset control at far right. */
.atlas-breakdown-reset-floating {
  position:absolute;
  top:8px;
  right:14px;
  z-index:8;
}
.atlas-breakdown-reset-floating .btn {
  border-radius:999px !important;
  color:#FFC857 !important;
  border-color:rgba(255,200,87,.44) !important;
  background:rgba(255,200,87,.06) !important;
}
.atlas-breakdown-reset-floating .btn:hover {
  color:#111827 !important;
  background:#FFC857 !important;
}

/* Neat equal-height blocks where columns directly contain cards. */
body.aurelis-signature .row {
  align-items:stretch;
}
body.aurelis-signature .row > [class*='col-'] > .card {
  height:calc(100% - 12px);
}

/* Plotly modebar */
body.aurelis-signature .modebar-btn path {
  fill:#9DAFC1 !important;
}
body.aurelis-signature .modebar-btn:hover path {
  fill:#FFC857 !important;
}

/* Navbar remains dark across all pages. */
body.aurelis-signature .main-header {
  background:rgba(12,18,27,.95) !important;
  border-bottom-color:rgba(177,198,218,.08) !important;
}
body.aurelis-signature .future-control-button,
body.aurelis-signature .navbar-report-button {
  color:#AFC0D1 !important;
  background:#151F2B !important;
  border-color:rgba(177,198,218,.09) !important;
}

body.aurelis-signature.aurelis-ambient {
  filter:saturate(1.06) contrast(1.015);
}
body.aurelis-signature.aurelis-ambient .card {
  box-shadow:0 20px 55px rgba(0,0,0,.34) !important;
}

@media (max-width:991px) {
  body.aurelis-signature .header-logo-lockup {
    display:none !important;
  }
}
@media (max-width:560px) {
  .prism-mobile-filter-row {
    grid-template-columns:1fr;
  }
}
"

custom_css <- paste0(custom_css, obsidian_v7_css)

obsidian_v72_css <- "
/* ================================================================
   OBSIDIAN COMMAND v7.2 — readability / brand / layering hardening
   ================================================================ */

/* --- High-definition branding ------------------------------------------------ */
body.aurelis-signature .brand-link {
  height: 112px !important;
  min-height: 112px !important;
  padding: 3px !important;
  background:
    radial-gradient(circle at 50% 20%, rgba(50,209,198,.10), transparent 44%),
    #09111B !important;
  border-bottom: 1px solid rgba(177,198,218,.08) !important;
  overflow: hidden !important;
}
body.aurelis-signature .brand-link .brand-image {
  width: 108px !important;
  max-width: 108px !important;
  height: 104px !important;
  max-height: 104px !important;
  margin: 0 auto !important;
  object-fit: contain !important;
  filter: drop-shadow(0 6px 18px rgba(28,183,221,.16));
}

body.aurelis-signature .header-logo-lockup {
  flex: 0 0 330px !important;
  width: 330px !important;
  min-width: 330px !important;
  max-width: 330px !important;
  height: 60px !important;
  padding: 4px 12px !important;
  margin-right: 12px !important;
  gap: 10px !important;
  justify-content: flex-start !important;
  background:
    radial-gradient(circle at 18% 50%, rgba(50,209,198,.10), transparent 43%),
    linear-gradient(135deg,#121C28,#0E1620) !important;
  border: 1px solid rgba(85,183,244,.18) !important;
  border-radius: 14px !important;
  box-shadow: 0 10px 30px rgba(0,0,0,.22) !important;
}
body.aurelis-signature .header-logo-lockup img {
  width: 188px !important;
  min-width: 188px !important;
  max-width: 188px !important;
  height: 51px !important;
  object-fit: contain !important;
  filter: drop-shadow(0 4px 16px rgba(50,209,198,.12));
}
body.aurelis-signature .header-logo-lockup span {
  flex: 1 1 auto !important;
  max-width: 112px !important;
  color: #B7C5D4 !important;
  font-size: .60rem !important;
  font-weight: 760 !important;
  text-transform: uppercase !important;
  letter-spacing: .45px !important;
  line-height: 1.18 !important;
  white-space: normal !important;
}

body.aurelis-signature .aurelis-content-watermark {
  width: 220px !important;
  max-width: 16vw !important;
  right: 28px !important;
  bottom: 22px !important;
  opacity: .055 !important;
  filter: none !important;
}

/* --- Legacy light panels: convert operational UI to dark readable surfaces --- */
body.aurelis-signature .multiuser-banner,
body.aurelis-signature .multiuser-status-card,
body.aurelis-signature .definition-card,
body.aurelis-signature .metric-definition,
body.aurelis-signature .profile-panel,
body.aurelis-signature .delivery-filter-note,
body.aurelis-signature .data-quality-note,
body.aurelis-signature .report-modal-status,
body.aurelis-signature .schedule-export-bar,
body.aurelis-signature .quote-process-step,
body.aurelis-signature .quote-help-card,
body.aurelis-signature .quote-status-pill,
body.aurelis-signature .quote-math-note,
body.aurelis-signature .buyer-drill-chip,
body.aurelis-signature .country-business-metric,
body.aurelis-signature .kpi-health-card,
body.aurelis-signature .customer-milestone-card {
  background: #17212D !important;
  color: #C7D3E0 !important;
  border-color: rgba(177,198,218,.12) !important;
}

body.aurelis-signature .multiuser-banner {
  border-left-color: #FF7A45 !important;
}
body.aurelis-signature .multiuser-banner strong,
body.aurelis-signature .multiuser-status-card strong,
body.aurelis-signature .definition-card strong,
body.aurelis-signature .profile-value,
body.aurelis-signature .quote-process-step strong,
body.aurelis-signature .quote-help-card strong,
body.aurelis-signature .customer-milestone-card strong,
body.aurelis-signature .country-business-metric strong,
body.aurelis-signature .kpi-health-card strong {
  color: #F1F6FB !important;
}
body.aurelis-signature .country-business-metric span,
body.aurelis-signature .profile-label {
  color: #92A5B9 !important;
}
body.aurelis-signature .customer-milestone-value {
  color: #F7FAFC !important;
}

/* Quotation matrices are interactive controls, not paper previews. */
body.aurelis-signature .quote-input-matrix {
  background: #101821 !important;
  border-color: rgba(177,198,218,.14) !important;
}
body.aurelis-signature .quote-input-matrix th {
  background: #1B2A3A !important;
  color: #F2F6FA !important;
  border-color: rgba(177,198,218,.10) !important;
}
body.aurelis-signature .quote-input-matrix td {
  background: #121C27 !important;
  color: #D6E1EC !important;
  border-top-color: rgba(177,198,218,.09) !important;
}
body.aurelis-signature .quote-input-matrix tr:nth-child(even) td {
  background: #15202C !important;
}
body.aurelis-signature .quote-input-matrix td:first-child {
  color: #DDE7F1 !important;
  font-weight: 620 !important;
}

/* Customer-facing / quotation previews deliberately look like paper.
   Explicit dark typography prevents global dark-theme rules from washing them out. */
body.aurelis-signature .customer-resume-preview,
body.aurelis-signature .quote-preview-sheet {
  background: #F7FAFC !important;
  color: #25384B !important;
  border-color: #CBD8E4 !important;
}
body.aurelis-signature .customer-resume-preview h1,
body.aurelis-signature .customer-resume-preview h2,
body.aurelis-signature .customer-resume-preview h3,
body.aurelis-signature .customer-resume-preview strong,
body.aurelis-signature .customer-resume-preview p,
body.aurelis-signature .customer-resume-preview small,
body.aurelis-signature .quote-preview-sheet h1,
body.aurelis-signature .quote-preview-sheet h2,
body.aurelis-signature .quote-preview-sheet h3,
body.aurelis-signature .quote-preview-sheet strong,
body.aurelis-signature .quote-preview-sheet p,
body.aurelis-signature .quote-preview-sheet div,
body.aurelis-signature .quote-preview-sheet td {
  color: #25384B !important;
}
body.aurelis-signature .customer-resume-card,
body.aurelis-signature .quote-preview-panel {
  background: #FFFFFF !important;
  color: #25384B !important;
  border-color: #D4E0EA !important;
}
body.aurelis-signature .customer-resume-card strong,
body.aurelis-signature .quote-preview-panel strong {
  color: #17324D !important;
}
body.aurelis-signature .quote-preview-table th,
body.aurelis-signature .quote-preview-summary tr.total td {
  background: #17324D !important;
  color: #FFFFFF !important;
}
body.aurelis-signature .customer-resume-empty {
  min-height: 135px;
  display: flex;
  flex-direction: column;
  justify-content: center;
}

/* --- Dropdown / scrolling layering ----------------------------------------- */
/* Cards may contain Selectize menus. Do not create a clipping boundary. */
body.aurelis-signature .card,
body.aurelis-signature .card-body,
body.aurelis-signature .tab-pane,
body.aurelis-signature .row,
body.aurelis-signature [class*='col-'] {
  overflow: visible !important;
}
body.aurelis-signature .selectize-host-active {
  z-index: 2147483600 !important;
  transform: none !important;
  overflow: visible !important;
}
body.aurelis-signature .selectize-control.dropdown-active,
body.aurelis-signature .selectize-control.input-active {
  position: relative !important;
  z-index: 2147483645 !important;
}
body.aurelis-signature .selectize-dropdown {
  z-index: 2147483647 !important;
  background: #17212D !important;
  border-color: rgba(177,198,218,.18) !important;
  box-shadow: 0 24px 70px rgba(0,0,0,.48) !important;
}
body.aurelis-signature .selectize-dropdown-content {
  max-height: 340px !important;
  overflow-y: auto !important;
}

/* Dark scrollbars throughout the application. */
body.aurelis-signature ::-webkit-scrollbar {
  width: 8px !important;
  height: 8px !important;
}
body.aurelis-signature ::-webkit-scrollbar-track {
  background: #0B1119 !important;
}
body.aurelis-signature ::-webkit-scrollbar-thumb {
  background: #3A4A5D !important;
  border-radius: 99px !important;
  border: 2px solid #0B1119 !important;
}
body.aurelis-signature ::-webkit-scrollbar-thumb:hover {
  background: #60758B !important;
}

/* AdminLTE / OverlayScrollbars sidebar: keep the rail inside the sidebar and
   reveal it only when useful, instead of showing a bright bar behind menu items. */
body.aurelis-signature .main-sidebar .os-scrollbar {
  z-index: 99999 !important;
  opacity: .10 !important;
  transition: opacity .18s ease !important;
}
body.aurelis-signature .main-sidebar:hover .os-scrollbar {
  opacity: .72 !important;
}
body.aurelis-signature .main-sidebar .os-scrollbar-vertical {
  right: 2px !important;
  width: 6px !important;
}
body.aurelis-signature .main-sidebar .os-scrollbar-handle {
  background: rgba(184,202,220,.48) !important;
  border-radius: 99px !important;
}

/* --- Adaptive KPI values --------------------------------------------------- */
body.aurelis-signature .kpi-card h2 {
  max-width: 100% !important;
  font-size: clamp(1.08rem, 1.42vw, 1.86rem) !important;
  line-height: 1.02 !important;
  letter-spacing: -.85px !important;
  white-space: nowrap !important;
}
body.aurelis-signature .kpi-card-long-value h2 {
  font-size: clamp(.98rem, 1.18vw, 1.48rem) !important;
  letter-spacing: -1px !important;
}

/* Country summary metrics no longer become white-on-white. */
body.aurelis-signature .country-business-summary {
  gap: 9px !important;
}

/* Avoid placeholder-looking empty card expanses. */
body.aurelis-signature .quote-help-grid,
body.aurelis-signature .customer-resume-grid,
body.aurelis-signature .kpi-health-grid,
body.aurelis-signature .customer-milestone-grid {
  align-items: stretch !important;
}
body.aurelis-signature .quote-help-card,
body.aurelis-signature .customer-resume-card,
body.aurelis-signature .kpi-health-card,
body.aurelis-signature .customer-milestone-card {
  min-height: 112px;
}

/* Keep the reset control aligned to the extreme right of the Atlas hierarchy. */
body.aurelis-signature .atlas-breakdown-reset-floating {
  top: 7px !important;
  right: 12px !important;
}

@media (max-width: 1500px) {
  body.aurelis-signature .header-logo-lockup {
    flex-basis: 285px !important;
    width: 285px !important;
    width: 248px !important;
    background: linear-gradient(180deg, #071522 0%, #050D17 100%) !important;
    box-shadow: 10px 0 30px rgba(0,0,0,.24) !important;
  }

  body:not(.sidebar-collapse).aurelis-signature .main-header,
  body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
  body:not(.sidebar-collapse).aurelis-signature .main-footer {
    margin-left: 248px !important;
  }

  body.aurelis-signature .brand-link {
    height: 92px !important;
    min-height: 92px !important;
    padding: 8px !important;
    border-bottom: 1px solid rgba(98,231,241,.18) !important;
  }

  body.aurelis-signature .brand-link .brand-image {
    width: 132px !important;
    max-width: 132px !important;
    max-height: 78px !important;
  }

  body.aurelis-signature .main-sidebar .sidebar {
    padding: 7px 7px 28px !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) {
    margin: 9px 0 0 !important;
    padding: 0 0 6px !important;
    border: 0 !important;
    border-bottom: 1px solid rgba(98,231,241,.13) !important;
    border-radius: 0 !important;
    background: transparent !important;
    box-shadow: none !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview)::before {
    display: none !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link {
    min-height: 34px !important;
    margin: 0 3px 4px !important;
    padding: 7px 9px !important;
    border: 0 !important;
    border-left: 2px solid #27D5B2 !important;
    border-radius: 4px !important;
    background: linear-gradient(90deg, rgba(39,213,178,.13), transparent) !important;
    box-shadow: none !important;
    font-size: .60rem !important;
    letter-spacing: .8px !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link::after {
    content: '' !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview {
    margin: 0 2px !important;
    padding: 0 0 0 8px !important;
    border-left: 1px solid rgba(98,231,241,.23) !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link {
    min-height: 31px !important;
    margin: 1px 0 !important;
    padding: 6px 7px 6px 10px !important;
    border-left: 0 !important;
    border-radius: 6px !important;
    font-size: .69rem !important;
    line-height: 1.05 !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active::after {
    content: '' !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active {
    box-shadow: inset 2px 0 0 #27D5B2, 0 0 12px rgba(39,213,178,.10) !important;
  }

  body.aurelis-signature .main-sidebar .nav-sidebar .nav-icon {
    width: 20px !important;
    margin-right: 5px !important;
    font-size: .76rem !important;
  }

  body.sidebar-collapse.aurelis-signature .main-sidebar {
    width: 78px !important;
  }

  body.sidebar-collapse.aurelis-signature .main-header,
  body.sidebar-collapse.aurelis-signature .content-wrapper,
  body.sidebar-collapse.aurelis-signature .main-footer {
    margin-left: 78px !important;
  }

  @media (max-width: 991px) {
    body.aurelis-signature .main-sidebar { width: 248px !important; }
    body.aurelis-signature .main-header,
    body.aurelis-signature .content-wrapper,
    body.aurelis-signature .main-footer { margin-left: 0 !important; }
  }

  body.aurelis-signature .main-sidebar {
    max-width: 285px !important;
  }
  body.aurelis-signature .header-logo-lockup img {
    width: 160px !important;
    min-width: 160px !important;
    max-width: 160px !important;
  }
}

@media (max-width: 1199px) {
  body.aurelis-signature .header-logo-lockup {
    display: none !important;
  }
}
"

custom_css <- paste0(custom_css, obsidian_v72_css)


# ==============================================================================
# LEFT SIDEBAR LOGO SIZE / POSITION FIX
# ==============================================================================
# This changes ONLY the logo at the top of the left menu.
# It does NOT change the larger logo in the top navbar.
# It does NOT change the "Created by Armel Asopjio" signature.

small_sidebar_logo_css <- "

/* --------------------------------------------------------------
   TOP-LEFT SIDEBAR BRAND AREA
   -------------------------------------------------------------- */

body.aurelis-signature .main-sidebar .brand-link {

  width: 100% !important;
  max-width: 100% !important;

  /* Small top section so menu begins immediately underneath */
  height: 76px !important;
  min-height: 76px !important;
  max-height: 76px !important;

  padding: 4px 5px !important;
  margin: 0 !important;

  display: flex !important;
  align-items: center !important;
  justify-content: center !important;

  overflow: hidden !important;

  background: #09111B !important;

  border-bottom:
    1px solid rgba(177,198,218,.08) !important;

  box-sizing: border-box !important;
}


/* --------------------------------------------------------------
   ACTUAL LOGO IMAGE
   -------------------------------------------------------------- */

body.aurelis-signature .main-sidebar .brand-link .brand-image,
body.aurelis-signature .main-sidebar .brand-link img.brand-image {

  width: calc(100% - 20px) !important;
  max-width: 228px !important;
  height: auto !important;
  max-height: 64px !important;

  margin: 0 auto !important;
  padding: 0 !important;

  float: none !important;
  display: block !important;

  object-fit: contain !important;
  object-position: center center !important;

  border-radius: 0 !important;

  box-shadow: none !important;

  opacity: 1 !important;

  filter:
    drop-shadow(
      0 3px 7px rgba(32,180,215,.15)
    ) !important;
}


/* --------------------------------------------------------------
   MAKE MENU START DIRECTLY UNDER LOGO
   -------------------------------------------------------------- */

body.aurelis-signature .main-sidebar .sidebar {
  padding-top: 84px !important;
  margin-top: 0 !important;
}

body.aurelis-signature
.main-sidebar
.nav-sidebar {
  margin-top: 0 !important;
  padding-top: 0 !important;
}

body.aurelis-signature
.main-sidebar
.nav-sidebar
.nav-item:first-child {
  margin-top: 0 !important;
}

/* --------------------------------------------------------------
   SMALL SCREEN ADAPTATION
   -------------------------------------------------------------- */

@media (max-width: 991px) {

  body.aurelis-signature
  .main-sidebar
  .brand-link {

    height: 70px !important;
    min-height: 70px !important;
    max-height: 70px !important;
  }


  body.aurelis-signature
  .main-sidebar
  .brand-link
  .brand-image,

  body.aurelis-signature
  .main-sidebar
  .brand-link
  img.brand-image {

    width: 60px !important;
    max-width: 60px !important;

    height: 60px !important;
    max-height: 60px !important;
  }
}

"

custom_css <- paste0(
  custom_css,
  small_sidebar_logo_css
)


# ==============================================================================
# FINAL NAVBAR POSITION / CENTER LOGO FIX
#
# Desired layout:
#
# YEAR -> MONTH -------- [ LARGE AURELIS LOGO ] -------- REFRESH -> REPORT
#
# This changes ONLY the top navbar.
# It does NOT change the left sidebar logo.
# It does NOT change the signature/footer.
# ==============================================================================

final_navbar_position_css <- "

/* ========================================================================
   TOP NAVBAR
   ======================================================================== */

body.aurelis-signature .main-header.navbar {

  height: 78px !important;
  min-height: 78px !important;

  display: flex !important;
  align-items: center !important;

  padding-top: 0 !important;
  padding-bottom: 0 !important;

  padding-left: 8px !important;
  padding-right: 10px !important;

  background: #09111B !important;
}


/* ========================================================================
   HAMBURGER AREA

   Keep only a small amount of width for the hamburger.
   This removes the large empty space before YEAR.
   ======================================================================== */

body.aurelis-signature
.main-header
.navbar-nav:first-child {

  flex: 0 0 46px !important;

  width: 46px !important;
  min-width: 46px !important;
  max-width: 46px !important;

  margin: 0 !important;
  padding: 0 !important;
}


/* ========================================================================
   MAIN NAVBAR ELEMENT ROW

   bs4Dash normally pushes rightUi to the far right using ml-auto.

   This removes that big automatic gap.
   ======================================================================== */

body.aurelis-signature
.main-header
.navbar-nav.ml-auto {

  margin-left: 4px !important;
  margin-right: 0 !important;

  padding-left: 0 !important;
  padding-right: 0 !important;

  flex: 1 1 auto !important;

  width: auto !important;
  min-width: 0 !important;

  display: flex !important;
  flex-direction: row !important;

  align-items: center !important;
  justify-content: flex-start !important;

  gap: 5px !important;
}


/* ========================================================================
   YEAR + MONTH
   ======================================================================== */

body.aurelis-signature
.prism-navbar-filter {

  flex: 0 0 auto !important;

  height: 54px !important;

  display: flex !important;
  align-items: center !important;

  margin: 0 2px !important;
  padding: 0 !important;
}


body.aurelis-signature
.prism-year-filter {

  margin-left: 0 !important;
}


body.aurelis-signature
.prism-month-filter {

  margin-right: 7px !important;
}


body.aurelis-signature
.prism-navbar-filter
.form-group {

  margin: 0 !important;
}


body.aurelis-signature
.prism-navbar-filter
.selectize-control {

  margin: 0 !important;
}


body.aurelis-signature
.prism-navbar-filter
.selectize-input {

  height: 40px !important;
  min-height: 40px !important;

  display: flex !important;
  align-items: center !important;

  padding-left: 14px !important;
  padding-right: 29px !important;

  border-radius: 12px !important;

  background: #17212D !important;

  border:
    1px solid rgba(177,198,218,.12) !important;

  color: #EEF4FA !important;

  box-shadow: none !important;
}


/* Do NOT remove the dropdown class from the R code.
   bs4Dash requires it.
   We only hide its extra visual arrow here. */

body.aurelis-signature
.prism-navbar-filter.dropdown::after,

body.aurelis-signature
.prism-navbar-filter.dropdown
.dropdown-toggle::after {

  display: none !important;
}


/* ========================================================================
   CENTER LOGO BLOCK

   This block automatically expands and uses the available center space.
   ======================================================================== */

body.aurelis-signature
.aurelis-center-header-logo {

  flex: 1 1 auto !important;

  width: auto !important;

  min-width: 400px !important;
  max-width: none !important;

  height: 66px !important;

  margin-left: 5px !important;
  margin-right: 10px !important;

  padding: 2px 20px !important;

  display: flex !important;

  align-items: center !important;
  justify-content: center !important;

  position: relative !important;

  overflow: hidden !important;

  border-radius: 15px !important;

  background:
    radial-gradient(
      ellipse at center,
      rgba(50,209,198,.09),
      transparent 72%
    ),
    linear-gradient(
      135deg,
      #13202D 0%,
      #0D1721 100%
    ) !important;

  border:
    1px solid rgba(85,183,244,.20) !important;

  box-shadow:
    0 7px 25px rgba(0,0,0,.18) !important;

  pointer-events: none !important;
}


/* ========================================================================
   ACTUAL AURELIS LOGO

   Your SVG contains quite a lot of internal horizontal space.
   Increasing only CSS width does not therefore make the visible artwork
   much larger.

   transform: scale() enlarges the actual visible Aurelis artwork.
   ======================================================================== */

body.aurelis-signature
.aurelis-center-header-logo
img {

  width: 430px !important;

  max-width: 72% !important;

  height: 58px !important;
  max-height: 58px !important;

  margin: 0 auto !important;
  padding: 0 !important;

  display: block !important;

  object-fit: contain !important;
  object-position: center center !important;

transform:
  translateX(26px)
  scale(1.38) !important;

transform-origin:
  center center !important;
  filter:
    drop-shadow(
      0 4px 13px rgba(50,209,198,.17)
    ) !important;
}


/* ========================================================================
   REMOVE PAGE-TITLE TEXT FROM THE CENTER BLOCK

   Your existing R code has:

   textOutput('obsidian_header_page')

   We KEEP that output alive so the server is not touched.
   We simply make it invisible so the logo can occupy the entire block.
   ======================================================================== */

body.aurelis-signature
.aurelis-center-header-logo
span {

  position: absolute !important;

  width: 1px !important;
  height: 1px !important;

  overflow: hidden !important;

  opacity: 0 !important;

  pointer-events: none !important;
}


/* ========================================================================
   REFRESH
   ======================================================================== */

body.aurelis-signature
.aurelis-center-header-logo
+
.future-control-item {

  margin-left: 0 !important;
}


body.aurelis-signature
.future-control-item {

  flex: 0 0 auto !important;

  margin-left: 0 !important;
  margin-right: 3px !important;
}


/* ========================================================================
   REPORT / EXPORT
   ======================================================================== */

body.aurelis-signature
.navbar-report-button {

  flex: 0 0 auto !important;

  margin-left: 0 !important;
}


/* ========================================================================
   NORMAL LAPTOP / RSTUDIO WINDOW
   ======================================================================== */

@media (max-width: 1550px) {

  body.aurelis-signature
  .main-header
  .navbar-nav.ml-auto {

    margin-left: 2px !important;

    gap: 3px !important;
  }


  body.aurelis-signature
  .aurelis-center-header-logo {

    min-width: 340px !important;

    margin-left: 4px !important;
    margin-right: 6px !important;
  }


  body.aurelis-signature
  .aurelis-center-header-logo
  img {

    width: 380px !important;

    max-width: 74% !important;

transform:
  translateX(52px)
  scale(1.32) !important;
  }
}


/* ========================================================================
   LARGE MONITOR
   ======================================================================== */

@media (min-width: 1700px) {

  body.aurelis-signature
  .aurelis-center-header-logo {

    min-width: 520px !important;
  }


  body.aurelis-signature
  .aurelis-center-header-logo
  img {

    width: 470px !important;

    transform:
      scale(1.45) !important;
  }
}


/* ========================================================================
   SMALLER DESKTOP
   ======================================================================== */

@media (max-width: 1300px) {

  body.aurelis-signature
  .aurelis-center-header-logo {

    min-width: 250px !important;
  }


  body.aurelis-signature
  .aurelis-center-header-logo
  img {

    width: 290px !important;

    transform:
      scale(1.20) !important;
  }
}


/* ========================================================================
   MOBILE
   ======================================================================== */

@media (max-width: 991px) {

  body.aurelis-signature
  .aurelis-center-header-logo,

  body.aurelis-signature
  .prism-navbar-filter {

    display: none !important;
  }
}

"

custom_css <- paste0(
  custom_css,
  final_navbar_position_css
)

# Final product skin: a denser, darker command-center shell with a branded
# light field for charts and tables so operational data remains readable.
prism_ultimate_css <- "
:root {
  --aurelis-prism-bg: #07111D;
  --aurelis-prism-panel: #0C1B2B;
  --aurelis-prism-panel-2: #10263A;
  --aurelis-prism-line: rgba(105,220,239,.18);
  --aurelis-prism-cyan: #62E7F1;
  --aurelis-prism-blue: #4C8DFF;
  --aurelis-prism-mint: #27D5B2;
}

body.aurelis-signature .main-sidebar {
  width: 282px !important;
  background:
    linear-gradient(180deg, rgba(10,28,44,.98), rgba(4,12,22,.99)),
    repeating-linear-gradient(135deg, rgba(98,231,241,.035) 0 1px, transparent 1px 14px) !important;
  border-right: 1px solid var(--aurelis-prism-line) !important;
}

body:not(.sidebar-collapse).aurelis-signature .main-header,
body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
body:not(.sidebar-collapse).aurelis-signature .main-footer {
  margin-left: 282px !important;
}

body.sidebar-collapse.aurelis-signature .main-sidebar {
  width: 78px !important;
}

body.sidebar-collapse.aurelis-signature .main-header,
body.sidebar-collapse.aurelis-signature .content-wrapper,
body.sidebar-collapse.aurelis-signature .main-footer {
  margin-left: 78px !important;
}

body.aurelis-signature .brand-link {
  height: 112px !important;
  min-height: 112px !important;
  background:
    radial-gradient(circle at 50% 40%, rgba(55,213,218,.13), transparent 58%),
    #071522 !important;
  border-bottom: 1px solid rgba(98,231,241,.32) !important;
}

body.aurelis-signature .brand-link .brand-image {
  width: 160px !important;
  max-width: 160px !important;
  max-height: 100px !important;
  filter: drop-shadow(0 0 15px rgba(98,231,241,.24));
}

body.aurelis-signature .main-header.navbar {
  background: rgba(7,17,29,.88) !important;
  border-bottom: 1px solid rgba(98,231,241,.18) !important;
  box-shadow: 0 14px 42px rgba(1,7,15,.28) !important;
}

body.aurelis-signature .header-logo-lockup {
  background: linear-gradient(110deg, rgba(9,27,43,.98), rgba(15,48,69,.92)) !important;
  border: 1px solid rgba(98,231,241,.28) !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,.06), 0 0 22px rgba(39,213,178,.08) !important;
}

body.aurelis-signature .header-logo-lockup img {
  width: 184px !important;
  height: 48px !important;
  filter: drop-shadow(0 0 11px rgba(98,231,241,.30));
}

body.aurelis-signature .nav-sidebar .nav-item > .nav-link:not([href]) {
  min-height: 36px !important;
  margin-top: 11px !important;
  color: #6FE5EF !important;
  background: rgba(98,231,241,.055) !important;
  border: 1px solid rgba(98,231,241,.10) !important;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-size: .64rem !important;
  font-weight: 850 !important;
}

/* Module rail: make the five primary domains unmistakable containers. */
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) {
  position: relative;
  margin: 13px 5px 0 !important;
  padding: 7px 0 8px;
  border: 1px solid rgba(98,231,241,.13);
  border-radius: 15px;
  background: linear-gradient(145deg, rgba(98,231,241,.055), rgba(2,12,22,.16));
  box-shadow: inset 0 1px 0 rgba(255,255,255,.035);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview)::before {
  content: '';
  position: absolute;
  top: -8px;
  left: 15px;
  right: 15px;
  height: 1px;
  background: linear-gradient(90deg, transparent, rgba(98,231,241,.28), transparent);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link {
  min-height: 34px !important;
  margin: 0 7px 6px !important;
  padding: 7px 10px !important;
  color: #83EDF3 !important;
  background: linear-gradient(90deg, rgba(98,231,241,.12), rgba(98,231,241,.025)) !important;
  border: 1px solid rgba(98,231,241,.19) !important;
  border-radius: 10px !important;
  box-shadow: 0 0 18px rgba(98,231,241,.05);
  font-size: .63rem !important;
  font-weight: 900 !important;
  letter-spacing: 1.15px !important;
  text-transform: uppercase;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link::after {
  content: 'MODULE';
  position: absolute;
  right: 29px;
  color: rgba(150,230,239,.52);
  font-size: .47rem;
  font-weight: 800;
  letter-spacing: .9px;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link .right {
  color: #65E7EF !important;
  font-size: .62rem !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview {
  position: relative;
  display: block;
  margin: 0 6px !important;
  padding: 1px 0 0 8px !important;
  border-left: 1px solid rgba(98,231,241,.26);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview::before {
  content: '';
  position: absolute;
  top: 0;
  bottom: 9px;
  left: -1px;
  width: 1px;
  background: linear-gradient(180deg, #62E7F1, rgba(98,231,241,0));
  box-shadow: 0 0 8px rgba(98,231,241,.55);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-item {
  position: relative;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-item::before {
  content: '';
  position: absolute;
  left: -8px;
  top: 50%;
  width: 8px;
  height: 1px;
  background: rgba(98,231,241,.30);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link {
  margin: 2px 4px !important;
  padding: 8px 9px 8px 12px !important;
  border: 1px solid transparent !important;
  border-radius: 9px !important;
  color: #B5CAD7 !important;
  background: transparent !important;
  font-size: .73rem !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link:hover {
  color: #F3FDFF !important;
  background: rgba(98,231,241,.10) !important;
  border-color: rgba(98,231,241,.17) !important;
  transform: translateX(2px);
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active {
  color: #FFFFFF !important;
  background: linear-gradient(90deg, rgba(39,213,178,.22), rgba(76,141,255,.12)) !important;
  border-color: rgba(39,213,178,.34) !important;
  box-shadow: 0 0 18px rgba(39,213,178,.10), inset 3px 0 0 #27D5B2 !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active::after {
  content: 'LIVE';
  float: right;
  color: #6EF4C5;
  font-size: .46rem;
  font-weight: 900;
  letter-spacing: .8px;
}

body.aurelis-signature .nav-sidebar .nav-treeview .nav-link {
  min-height: 38px !important;
  margin: 2px 8px 2px 18px !important;
  padding: 8px 10px !important;
  font-size: .76rem !important;
  border-left: 1px solid rgba(98,231,241,.12) !important;
}

body.aurelis-signature .content-wrapper {
  background:
    radial-gradient(circle at 83% 5%, rgba(76,141,255,.13), transparent 25%),
    radial-gradient(circle at 12% 88%, rgba(39,213,178,.08), transparent 22%),
    linear-gradient(145deg, #081522 0%, #0B1C2C 42%, #102438 100%) !important;
}

body.aurelis-signature .content-wrapper::after {
  content: '';
  position: fixed;
  inset: 78px 0 0 118px;
  pointer-events: none;
  opacity: .12;
  background-image: linear-gradient(rgba(98,231,241,.08) 1px, transparent 1px), linear-gradient(90deg, rgba(98,231,241,.08) 1px, transparent 1px);
  background-size: 44px 44px;
  mask-image: linear-gradient(to bottom, black, transparent 75%);
  z-index: 0;
}

body.aurelis-signature .content,
body.aurelis-signature .content-wrapper > .content {
  max-width: 2040px !important;
  padding: 18px 24px 34px !important;
}

body.aurelis-signature .card {
  background: linear-gradient(145deg, rgba(15,38,58,.94), rgba(8,23,37,.96)) !important;
  border: 1px solid rgba(98,231,241,.15) !important;
  box-shadow: 0 18px 48px rgba(0,7,15,.24), inset 0 1px 0 rgba(255,255,255,.035) !important;
}

body.aurelis-signature .card-header {
  background: linear-gradient(90deg, rgba(98,231,241,.07), transparent) !important;
  border-bottom: 1px solid rgba(98,231,241,.12) !important;
}

body.aurelis-signature .card-header .card-title,
body.aurelis-signature .card h1,
body.aurelis-signature .card h2,
body.aurelis-signature .card h3,
body.aurelis-signature .card h4,
body.aurelis-signature .card h5,
body.aurelis-signature .card strong {
  color: #E9F7FF !important;
}

body.aurelis-signature .card p,
body.aurelis-signature .card label,
body.aurelis-signature .card .small-tag,
body.aurelis-signature .card .sub-text {
  color: #9AB6C9 !important;
}

body.aurelis-signature .form-control,
body.aurelis-signature .selectize-input,
body.aurelis-signature select,
body.aurelis-signature textarea {
  color: #E7F6FF !important;
  background: rgba(2,12,22,.72) !important;
  border-color: rgba(98,231,241,.20) !important;
}

body.aurelis-signature table.dataTable,
body.aurelis-signature table.dataTable tbody td,
body.aurelis-signature .dataTables_wrapper {
  color: #D7EBF5 !important;
  background: transparent !important;
}

body.aurelis-signature table.dataTable thead th {
  color: #71E5EF !important;
  background: rgba(98,231,241,.07) !important;
  border-bottom: 1px solid rgba(98,231,241,.20) !important;
}

body.aurelis-signature table.dataTable tbody tr:hover,
body.aurelis-signature table.dataTable tbody tr.selected {
  background: rgba(76,141,255,.15) !important;
}

body.aurelis-signature .btn-primary,
body.aurelis-signature .btn-info {
  background: linear-gradient(120deg, #1A8ED1, #1FBFA8) !important;
  border: 0 !important;
  box-shadow: 0 8px 22px rgba(31,191,168,.18) !important;
}

body.aurelis-signature .aurelis-content-watermark {
  right: 5vw !important;
  bottom: 5vh !important;
  width: 520px !important;
  max-width: 36vw !important;
  opacity: .075 !important;
  filter: saturate(1.15) drop-shadow(0 0 18px rgba(98,231,241,.18)) !important;
}

.aurelis-access-portal {
  position: fixed;
  inset: 0;
  z-index: 2147483000;
  display: grid;
  place-items: center;
  padding: 24px;
  background: radial-gradient(circle at 50% 25%, rgba(39,213,178,.16), transparent 32%), linear-gradient(145deg, #050C16, #0B2135 55%, #07111D);
}

.aurelis-access-portal-card {
  width: min(460px, 100%);
  padding: 30px;
  border: 1px solid rgba(98,231,241,.28);
  border-radius: 24px;
  background: linear-gradient(145deg, rgba(15,45,66,.97), rgba(5,18,30,.98));
  box-shadow: 0 30px 100px rgba(0,0,0,.45), inset 0 1px 0 rgba(255,255,255,.08);
}

.aurelis-access-portal-card img {
  display: block;
  width: 230px;
  max-width: 100%;
  margin: 0 auto 20px;
  filter: drop-shadow(0 0 18px rgba(98,231,241,.28));
}

.aurelis-access-portal-card h1 { color: #F0FBFF; font-size: 1.5rem; }
.aurelis-access-portal-card p { color: #A7C3D1; line-height: 1.55; }
.aurelis-access-portal-card label { color: #8CEAF0; font-size: .78rem; }
.aurelis-access-portal-card .form-control { color: #E8FAFF; background: rgba(2,12,22,.68); border-color: rgba(98,231,241,.22); }
.aurelis-access-portal-card .btn { width: 100%; margin-top: 8px; }
.aurelis-access-portal-note { margin-top: 14px; color: #77DCD2 !important; font-size: .72rem; }

body.aurelis-signature .prism-masthead {
  padding: 22px 24px !important;
  border: 1px solid rgba(98,231,241,.16) !important;
  border-radius: 20px !important;
  background: linear-gradient(125deg, rgba(12,38,58,.96), rgba(8,21,35,.86)) !important;
  box-shadow: 0 20px 55px rgba(0,8,16,.28), inset 0 1px 0 rgba(255,255,255,.05) !important;
}

body.aurelis-signature .pi-priority-score,
body.aurelis-signature .pi-inquiry-result,
body.aurelis-signature .pi-brief-note {
  border: 1px solid rgba(98,231,241,.18) !important;
  border-radius: 14px !important;
  background: rgba(98,231,241,.06) !important;
  color: #BFEAF0 !important;
  padding: 12px 14px !important;
}

body.aurelis-signature .future-command-deck {
  display: grid;
  grid-template-columns: 1fr 1.35fr 1.1fr;
  gap: 10px;
}

body.aurelis-signature .future-command-cell {
  min-height: 118px;
  padding: 15px 17px;
  border: 1px solid rgba(98,231,241,.16);
  border-radius: 16px;
  background: linear-gradient(140deg, rgba(98,231,241,.075), rgba(2,12,22,.34));
  box-shadow: inset 0 1px 0 rgba(255,255,255,.04);
}

body.aurelis-signature .future-command-cell.future-command-next {
  border-color: rgba(39,213,178,.28);
  background: linear-gradient(140deg, rgba(39,213,178,.13), rgba(2,12,22,.34));
}

body.aurelis-signature .future-command-cell.future-command-risk {
  border-color: rgba(255,180,75,.25);
  background: linear-gradient(140deg, rgba(255,180,75,.10), rgba(2,12,22,.34));
}

body.aurelis-signature .future-command-kicker {
  display: flex;
  align-items: center;
  gap: 7px;
  color: #65E7EF;
  font-size: .60rem;
  font-weight: 850;
  letter-spacing: 1.25px;
}

body.aurelis-signature .future-command-cell strong {
  display: block;
  margin-top: 7px;
  color: #F0FBFF !important;
  font-size: 1.06rem;
}

body.aurelis-signature .future-command-cell p {
  min-height: 30px;
  margin: 5px 0 9px;
  color: #9AB6C9 !important;
  font-size: .72rem;
  line-height: 1.35;
}

@media (max-width: 991px) {
  body.aurelis-signature .future-command-deck { grid-template-columns: 1fr; }
}

@media (max-width: 991px) {
  body.aurelis-signature .main-sidebar { width: 248px !important; }
  body.aurelis-signature .main-header,
  body.aurelis-signature .content-wrapper,
  body.aurelis-signature .main-footer { margin-left: 0 !important; }
  body.aurelis-signature .content,
  body.aurelis-signature .content-wrapper > .content { padding: 12px 10px 22px !important; }
  body.aurelis-signature .content-wrapper::after { inset: 70px 0 0 0; }
  body.aurelis-signature .aurelis-content-watermark { max-width: 70vw !important; opacity: .045 !important; }
}

/* Narrow navigation geometry: remove unused rail space beside the menu. */
body.aurelis-signature .main-sidebar { width: 164px !important; }
body:not(.sidebar-collapse).aurelis-signature .main-header,
body:not(.sidebar-collapse).aurelis-signature .content-wrapper,
body:not(.sidebar-collapse).aurelis-signature .main-footer { margin-left: 164px !important; }
body.sidebar-collapse.aurelis-signature .main-sidebar { width: 78px !important; }
body.sidebar-collapse.aurelis-signature .main-header,
body.sidebar-collapse.aurelis-signature .content-wrapper,
body.sidebar-collapse.aurelis-signature .main-footer { margin-left: 78px !important; }
body.aurelis-signature .brand-link { height: 72px !important; min-height: 72px !important; padding: 5px !important; }
body.aurelis-signature .brand-link .brand-image { width: 102px !important; max-width: 102px !important; max-height: 62px !important; }
body.aurelis-signature .main-sidebar .sidebar { padding: 4px 5px 20px !important; }
body.aurelis-signature .main-sidebar .nav-sidebar .nav-link { width: auto !important; max-width: none !important; }
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) {
  margin: 6px 0 0 !important; padding: 0 0 4px !important; border: 0 !important;
  border-bottom: 1px solid rgba(98,231,241,.13) !important; border-radius: 0 !important;
  background: transparent !important; box-shadow: none !important;
}
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link {
  min-height: 27px !important; margin: 0 2px 2px !important; padding: 5px 7px !important;
  border: 0 !important; border-left: 2px solid #27D5B2 !important; border-radius: 4px !important;
  background: linear-gradient(90deg, rgba(39,213,178,.13), transparent) !important;
  box-shadow: none !important; font-size: .53rem !important; letter-spacing: .55px !important;
}
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link::after { content: '' !important; }
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview {
  margin: 0 2px !important; padding: 0 0 0 8px !important; border-left: 1px solid rgba(98,231,241,.23) !important;
}
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link {
  min-height: 27px !important; margin: 0 !important; padding: 4px 5px 4px 8px !important;
  border-left: 0 !important; border-radius: 5px !important; font-size: .61rem !important; line-height: 1 !important;
}
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active::after { content: '' !important; }
body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link.active {
  box-shadow: inset 2px 0 0 #27D5B2, 0 0 12px rgba(39,213,178,.10) !important;
}
body.aurelis-signature .main-sidebar .nav-sidebar .nav-icon { width: 17px !important; margin-right: 3px !important; font-size: .65rem !important; }
body.aurelis-signature .main-sidebar .nav-sidebar .nav-link::after,
body.aurelis-signature .main-sidebar .nav-sidebar .nav-link:hover::after,
body.aurelis-signature .main-sidebar .nav-sidebar .nav-link.active::after {
  display: none !important;
  content: none !important;
  width: 0 !important;
  box-shadow: none !important;
}
body.aurelis-signature .brand-link {
  height: 64px !important;
  min-height: 64px !important;
  max-height: 64px !important;
  width: 100% !important;
  max-width: none !important;
  position: relative !important;
  top: 0 !important;
  left: 0 !important;
  float: none !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  overflow: hidden !important;
  background: linear-gradient(110deg, rgba(10,35,52,.98), rgba(5,17,28,.98)) !important;
  border-bottom: 1px solid rgba(98,231,241,.24) !important;
  box-shadow: inset 0 -1px 0 rgba(39,213,178,.10) !important;
}
body.aurelis-signature .brand-link .brand-image {
  width: 300px !important;
  max-width: none !important;
  height: 58px !important;
  max-height: 58px !important;
  object-fit: contain !important;
  object-position: center !important;
  filter: drop-shadow(0 0 10px rgba(98,231,241,.22)) !important;
}
body.aurelis-signature .main-sidebar .sidebar { margin-top: 0 !important; }
body.aurelis-signature .main-sidebar > .brand-link + .sidebar {
  position: relative !important;
  top: 0 !important;
  clear: both !important;
  padding-top: 8px !important;
}
body.aurelis-signature .main-sidebar > .brand-link + .sidebar > .nav-sidebar > .nav-item:first-child {
  margin-top: 0 !important;
}
@media (max-width: 991px) {
  body.aurelis-signature .main-sidebar { width: 164px !important; }
  body.aurelis-signature .main-header,
  body.aurelis-signature .content-wrapper,
  body.aurelis-signature .main-footer { margin-left: 0 !important; }
}
"

custom_css <- paste0(custom_css, prism_ultimate_css)

responsive_dashboard_css <- "
/* Keep the sidebar wordmark inside the complete brand block. */
body.aurelis-signature .main-sidebar > .brand-link {
  box-sizing: border-box !important;
  width: 100% !important;
  max-width: 100% !important;
  height: 78px !important;
  min-height: 78px !important;
  max-height: 78px !important;
  padding: 7px 10px !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  overflow: hidden !important;
}

body.aurelis-signature .main-sidebar > .brand-link .brand-image {
  box-sizing: border-box !important;
  display: block !important;
  float: none !important;
  width: calc(100% - 16px) !important;
  max-width: 228px !important;
  height: auto !important;
  max-height: 62px !important;
  margin: 0 auto !important;
  object-fit: contain !important;
  object-position: center !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item > .nav-link {
  min-height: 40px !important;
  padding: 8px 10px !important;
  font-size: .82rem !important;
  line-height: 1.2 !important;
  white-space: normal !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-link {
  min-height: 40px !important;
  font-size: .82rem !important;
  line-height: 1.2 !important;
}

body.aurelis-signature .main-sidebar .nav-sidebar .nav-treeview .nav-link {
  min-height: 36px !important;
  padding: 7px 8px 7px 12px !important;
  font-size: .82rem !important;
  line-height: 1.2 !important;
  white-space: normal !important;
  overflow-wrap: anywhere;
}

body.aurelis-signature .main-sidebar .nav-sidebar > .nav-item:has(> .nav-treeview) > .nav-treeview .nav-link {
  min-height: 36px !important;
  font-size: .82rem !important;
  line-height: 1.2 !important;
  white-space: normal !important;
  overflow-wrap: anywhere;
}

body.aurelis-signature .main-sidebar .nav-sidebar .nav-link .nav-text {
  min-width: 0;
  white-space: normal !important;
  overflow-wrap: anywhere;
}

.content-wrapper,
.main-header,
.main-footer {
  transition: margin-left .2s ease, width .2s ease;
}

.content-wrapper,
.content,
.content-header,
.main-footer,
.card,
.card-body,
.row > [class*='col-'] {
  min-width: 0;
  max-width: 100%;
}

.financial-stats-summary {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(min(100%, 150px), 1fr));
  gap: 10px;
  margin: 8px 0 16px;
}

.financial-stats-summary-card {
  min-width: 0;
  padding: 14px;
  border: 1px solid rgba(98, 231, 241, .2);
  border-radius: 12px;
  background: rgba(8, 26, 40, .72);
}

.financial-stats-summary-card span {
  display: block;
  color: #91a6b8;
  font-size: .74rem;
  font-weight: 700;
  letter-spacing: .04em;
  text-transform: uppercase;
}

.financial-stats-summary-card strong {
  display: block;
  margin-top: 5px;
  color: #eef5fb;
  font-size: clamp(1rem, 2vw, 1.35rem);
  overflow-wrap: anywhere;
}

.financial-stats-note {
  margin: 10px 0;
  color: #91a6b8;
  font-size: .84rem;
}

.js-plotly-plot,
.plotly.html-widget,
.plotly .plot-container,
.plotly .svg-container,
.plotly .gl-container {
  box-sizing: border-box !important;
  width: 100% !important;
  max-width: 100% !important;
  min-width: 0 !important;
}

@media (min-width: 1200px) {
  body.aurelis-signature:not(.sidebar-collapse) .main-sidebar,
  body.aurelis-signature:not(.sidebar-collapse) .main-sidebar::before {
    width: 164px !important;
  }
  body.aurelis-signature:not(.sidebar-collapse) .main-header,
  body.aurelis-signature:not(.sidebar-collapse) .content-wrapper,
  body.aurelis-signature:not(.sidebar-collapse) .main-footer {
    margin-left: 164px !important;
  }
}

@media (min-width: 992px) and (max-width: 1199px) {
  body.aurelis-signature:not(.sidebar-collapse) .main-sidebar,
  body.aurelis-signature:not(.sidebar-collapse) .main-sidebar::before {
    width: 180px !important;
  }
  body.aurelis-signature:not(.sidebar-collapse) .main-header,
  body.aurelis-signature:not(.sidebar-collapse) .content-wrapper,
  body.aurelis-signature:not(.sidebar-collapse) .main-footer {
    margin-left: 180px !important;
  }
}

@media (max-width: 991px) {
  body.aurelis-signature .main-sidebar,
  body.aurelis-signature .main-sidebar::before {
    width: min(280px, 86vw) !important;
    max-width: 86vw !important;
  }
  body.aurelis-signature .main-header,
  body.aurelis-signature .content-wrapper,
  body.aurelis-signature .main-footer {
    margin-left: 0 !important;
  }
  body.sidebar-collapse.aurelis-signature .main-header,
  body.sidebar-collapse.aurelis-signature .content-wrapper,
  body.sidebar-collapse.aurelis-signature .main-footer {
    margin-left: 0 !important;
  }
  body.aurelis-signature .main-sidebar > .brand-link {
    height: 72px !important;
    min-height: 72px !important;
    max-height: 72px !important;
  }
  body.aurelis-signature .main-sidebar > .brand-link .brand-image {
    max-height: 56px !important;
  }
  .financial-stats-summary {
    grid-template-columns: repeat(auto-fit, minmax(min(100%, 130px), 1fr));
  }
}

@media (max-width: 575px) {
  html,
  body,
  body.aurelis-signature .wrapper {
    max-width: 100% !important;
    overflow-x: hidden !important;
  }
  body.aurelis-signature .main-header.navbar {
    padding-right: 4px !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto {
    min-width: 0 !important;
    width: auto !important;
    flex: 1 1 auto !important;
    justify-content: flex-end !important;
    gap: 2px !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .nav-item.dropdown:not(.future-control-item):not(.navbar-report-item),
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .nav-item:not(.dropdown),
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .custom-control {
    display: none !important;
  }
  body.aurelis-signature .main-header .navbar-nav.ml-auto > .navbar-report-item {
    box-sizing: border-box !important;
    width: 38px !important;
    min-width: 38px !important;
    max-width: 38px !important;
    margin: 0 !important;
  }
  body.aurelis-signature .main-header .future-control-item,
  body.aurelis-signature .main-header .future-control-item > a,
  body.aurelis-signature .main-header .navbar-report-button {
    box-sizing: border-box !important;
    width: 38px !important;
    min-width: 38px !important;
    max-width: 38px !important;
    height: 38px !important;
    margin: 0 !important;
    padding: 8px !important;
    font-size: 0 !important;
    justify-content: center !important;
  }
  body.aurelis-signature .main-header .future-control-item > a i,
  body.aurelis-signature .main-header .navbar-report-button i {
    font-size: .95rem !important;
  }
}
"

responsive_dashboard_js <- "
(function() {
  var resizeTimer;
  function resizeDashboardWidgets() {
    window.clearTimeout(resizeTimer);
    resizeTimer = window.setTimeout(function() {
      if (window.Plotly) {
        document.querySelectorAll('.js-plotly-plot').forEach(function(plot) {
          if (plot.offsetWidth > 0 && plot.offsetHeight > 0) {
            window.Plotly.Plots.resize(plot);
          }
        });
      }
      if (window.jQuery && window.jQuery.fn.dataTable) {
        window.jQuery('.dataTable').each(function() {
          if (window.jQuery.fn.dataTable.isDataTable(this)) {
            window.jQuery(this).DataTable().columns.adjust();
          }
        });
      }
    }, 120);
  }
  window.addEventListener('resize', resizeDashboardWidgets);
  window.addEventListener('orientationchange', resizeDashboardWidgets);
  if (window.jQuery) {
    window.jQuery(document).on(
      'expanded.pushMenu collapsed.pushMenu expanded.lte.pushmenu collapsed.lte.pushmenu shown.bs.tab',
      resizeDashboardWidgets
    );
  }
})();
"

custom_css <- paste0(custom_css, responsive_dashboard_css)


# ==============================================================================
# KEEP YOUR SIGNATURE / INTERACTION SYSTEM
# ==============================================================================

signature_ui_js <- "
(function() {
  function getActiveTab() {
    var active = $('.nav-sidebar .nav-link.active').first();
    var value = active.attr('data-value') || active.data('value') || '';
    if (!value) {
      var href = active.attr('href') || '';
      value = href.replace('#shiny-tab-', '').replace('#', '');
    }
    return value || 'exec_overview';
  }

  function applySignature() {
    $('body').addClass('aurelis-signature');
    $('body').attr('data-aurelis-page', getActiveTab());

    if ($('.aurelis-busy-bar').length === 0) {
      $('body').append('<div class=\"aurelis-busy-bar\"></div>');
    }
    if ($('.aurelis-card-focus-backdrop').length === 0) {
      $('body').append('<div class=\"aurelis-card-focus-backdrop\"></div>');
    }
    ensureFooters();
    stabilizeModuleRail();
  }

  function ensureFooters() {
    $('.tab-pane').each(function() {
      var pane = $(this);
      if (pane.find('> .aurelis-signature-footer').length === 0) {
        pane.append('<div class=\"aurelis-signature-footer\">Created by Armel Asopjio</div>');
      }
    });
  }

  function closeCommand() {
    $('#aurelis-command-panel, #aurelis-command-overlay').removeClass('open');
  }

  function stabilizeModuleRail() {
    var active = $('.nav-sidebar .nav-treeview .nav-link.active').first();
    if (!active.length) return;
    var group = active.closest('.nav-item').closest('.nav-treeview').closest('.nav-item');
    if (!group.length) return;
    if (group.attr('data-manual-collapsed') === 'true') return;
    group.addClass('menu-open');
    group.children('.nav-treeview').show();
    group.children('a.nav-link').attr('aria-expanded', 'true');
  }

  function expandAllModules() {
    $('.nav-sidebar > .nav-item').has('.nav-treeview').each(function() {
      var group = $(this);
      group.addClass('menu-open').removeAttr('data-manual-collapsed');
      group.children('.nav-treeview').show();
      group.children('a.nav-link').attr('aria-expanded', 'true');
    });
  }

  $(document).on('click', '.nav-sidebar > .nav-item > .nav-link', function(e) {
    var group = $(this).closest('.nav-item');
    var tree = group.children('.nav-treeview');
    if (!tree.length) return;

    e.preventDefault();
    e.stopImmediatePropagation();

    var isOpen = group.hasClass('menu-open');
    $('.nav-sidebar > .nav-item.menu-open').not(group).each(function() {
      var other = $(this);
      other.removeClass('menu-open').attr('data-manual-collapsed', 'true');
      other.children('.nav-treeview').stop(true, true).slideUp(140);
      other.children('a.nav-link').attr('aria-expanded', 'false');
    });

    if (isOpen) {
      group.removeClass('menu-open').attr('data-manual-collapsed', 'true');
      tree.stop(true, true).slideUp(140);
      $(this).attr('aria-expanded', 'false');
    } else {
      group.addClass('menu-open').removeAttr('data-manual-collapsed');
      tree.stop(true, true).slideDown(140);
      $(this).attr('aria-expanded', 'true');
    }
  });

  $(document).on('click', '.nav-sidebar .nav-treeview .nav-link', function() {
    $(this).closest('.nav-treeview').closest('.nav-item')
      .removeAttr('data-manual-collapsed')
      .addClass('menu-open')
      .children('.nav-treeview').stop(true, true).show();
  });

  function closeFocus() {
    $('.card.aurelis-card-focus').removeClass('aurelis-card-focus');
    $('body').removeClass('aurelis-card-focus-mode');
    window.dispatchEvent(new Event('resize'));
  }

  function openTab(tab) {
    var target = $('.nav-sidebar .nav-link[data-value=\"' + tab + '\"]');
    if (!target.length) target = $('a[href=\"#shiny-tab-' + tab + '\"]');
    if (target.length) {
      target.first().trigger('click');
      closeCommand();
      setTimeout(function() {
        $('body').attr('data-aurelis-page', tab);
        window.dispatchEvent(new Event('resize'));
      }, 80);
    }
  }

  $(document).on('shiny:connected', function() {
    applySignature();
    expandAllModules();
  });

  $(document).ready(function() {
    applySignature();
    expandAllModules();
    setTimeout(ensureFooters, 300);
  });

  $(document).on('shown.bs.tab click', '.nav-sidebar .nav-link', function() {
    setTimeout(function() {
      $('body').attr('data-aurelis-page', getActiveTab());
      ensureFooters();
      stabilizeModuleRail();
    }, 40);
  });

  $(document).on('click', '#signature_command_toggle', function(e) {
    e.preventDefault();
    $('#aurelis-command-panel, #aurelis-command-overlay').addClass('open');
    setTimeout(function() {
      $('#signature_global_search').trigger('focus');
    }, 80);
  });

  $(document).on(
    'click',
    '#signature_command_close, #aurelis-command-overlay',
    function(e) {
      e.preventDefault();
      closeCommand();
    }
  );

  $(document).on(
    'click',
    '.signature-tab-shortcut, .signature-command-tile',
    function(e) {
      var tab = $(this).attr('data-tab');
      if (!tab) return;
      e.preventDefault();
      openTab(tab);
    }
  );

  $(document).on('dblclick', '.card-header', function(e) {

    if (
      $(e.target)
        .closest(
          'button,a,input,select,.card-tools'
        )
        .length
    ) return;

    var card = $(this).closest('.card');

    if (card.hasClass('aurelis-card-focus')) {

      closeFocus();

    } else {

      closeFocus();

      card.addClass('aurelis-card-focus');

      $('body')
        .addClass('aurelis-card-focus-mode');

      setTimeout(function() {
        window.dispatchEvent(
          new Event('resize')
        );
      }, 120);
    }
  });

  $(document).on(
    'click',
    '.aurelis-card-focus-backdrop',
    function() {
      closeFocus();
    }
  );

  $(document).on('keydown', function(e) {

    var tag =
      (
        document.activeElement &&
        document.activeElement.tagName ||
        ''
      ).toLowerCase();

    var typing =
      ['input','textarea','select']
        .indexOf(tag) >= 0 ||
      $(document.activeElement)
        .hasClass('selectize-input');

    if (e.key === 'Escape') {
      closeCommand();
      closeFocus();
    }

  });

  $(document).on('shiny:busy', function() {

    $('body')
      .addClass('aurelis-shiny-busy');

  });

  $(document).on('shiny:idle', function() {

    $('body')
      .removeClass('aurelis-shiny-busy');

    ensureFooters();

  });

  function registerSignatureHandlers() {

    if (
      !window.Shiny ||
      window.__aurelisSignatureHandlersRegistered
    ) return;

    window.__aurelisSignatureHandlersRegistered = true;

    Shiny.addCustomMessageHandler(
      'signatureCloseCommand',
      function(message) {
        closeCommand();
      }
    );
  }

  $(document).on(
    'shiny:connected',
    registerSignatureHandlers
  );

  $(document).ready(function() {

    setTimeout(
      registerSignatureHandlers,
      250
    );

  });

  function obsidianRestylePlots() {

    if (!window.Plotly) return;

    $('.js-plotly-plot').each(function() {

      var gd = this;

      if (
        !gd ||
        !gd.layout
      ) return;

      var update = {

        'paper_bgcolor':
          'rgba(0,0,0,0)',

        'plot_bgcolor':
          'rgba(0,0,0,0)',

        'font.color':
          '#C7D3E0'

      };

      if (gd.layout.xaxis) {

        update['xaxis.color'] =
          '#AAB8C7';

        update['xaxis.gridcolor'] =
          'rgba(255,255,255,.065)';

        update['xaxis.zerolinecolor'] =
          'rgba(255,255,255,.10)';
      }

      if (gd.layout.yaxis) {

        update['yaxis.color'] =
          '#AAB8C7';

        update['yaxis.gridcolor'] =
          'rgba(255,255,255,.065)';

        update['yaxis.zerolinecolor'] =
          'rgba(255,255,255,.10)';
      }

      if (gd.layout.yaxis2) {

        update['yaxis2.color'] =
          '#AAB8C7';

        update['yaxis2.gridcolor'] =
          'rgba(255,255,255,.035)';
      }

      if (gd.layout.legend) {

        update['legend.font.color'] =
          '#C7D3E0';

      }

      try {

        Plotly.relayout(
          gd,
          update
        );

      } catch(e) {}

    });
  }

  $(document).on(
    'shiny:idle shown.bs.tab',
    function() {

      setTimeout(
        obsidianRestylePlots,
        120
      );

    }
  );

  var observer =
    new MutationObserver(
      function() {
        ensureFooters();
      }
    );

  $(document).ready(function() {

    var root =
      document.querySelector(
        '.content-wrapper'
      );

    if (root) {

      observer.observe(
        root,
        {
          childList: true,
          subtree: true
        }
      );

    }
  });

})();
"

dropdown_layer_js <- "
(function() {
  function clearElevatedSelects() {
    $('.selectize-host-active').removeClass('selectize-host-active');
  }

  function elevateSelect(control) {
    clearElevatedSelects();
    var $control = $(control);
    $control.addClass('selectize-host-active');
    $control.closest('.card').addClass('selectize-host-active');
    $control.closest('.card-body').addClass('selectize-host-active');
    $control.closest('.row').addClass('selectize-host-active');
    $control.closest('[class*=\"col-\"]').addClass('selectize-host-active');
    $control.closest('.modal').addClass('selectize-host-active');
    $control.closest('.modal-content').addClass('selectize-host-active');
    $control.closest('.modal-body').addClass('selectize-host-active');
  }

  $(document).on('mousedown focusin click', '.selectize-control, .selectize-input', function() {
    var control = $(this).closest('.selectize-control');
    elevateSelect(control);
  });

  $(document).on('mouseenter mousedown', '.selectize-dropdown', function() {
    var control = $(this).closest('.selectize-control');
    if (!control.length) control = $('.selectize-control.dropdown-active').first();
    if (control.length) elevateSelect(control);
  });

  $(document).on('click', '.selectize-dropdown-content .option', function() {
    window.setTimeout(function() {
      if ($('.selectize-control.dropdown-active').length === 0) clearElevatedSelects();
    }, 180);
  });

  $(document).on('mousedown', function(event) {
    if ($(event.target).closest('.selectize-control, .selectize-dropdown').length === 0) {
      window.setTimeout(clearElevatedSelects, 20);
    }
  });

  var observer = new MutationObserver(function() {
    var active = $('.selectize-control.dropdown-active').first();
    if (active.length && !active.hasClass('selectize-host-active')) {
      elevateSelect(active);
    }
  });

  $(document).on('shiny:connected', function() {
    observer.observe(document.body, { attributes: true, subtree: true, attributeFilter: ['class'] });
  });

  /* Selectize menus may be rendered outside the modal DOM tree. Prevent their
     clicks from reaching the Bootstrap backdrop while the report builder is open. */
  $(document).on('mousedown click', '.selectize-dropdown, .selectize-input, .selectize-control', function(event) {
    if ($('.modal.show').length > 0) event.stopPropagation();
  });

})();
"


################################################################################
# REUSABLE UI HELPERS
################################################################################

report_action_bar <- function(
    button_id,
    help_text = "Export a concise page summary or build a configurable detailed report."
) {
  summary_id <- sub("^report_", "summary_", button_id)
  
  div(
    class = "report-action-bar",
    tags$p(class = "report-help", icon("info-circle"), paste0(" ", help_text)),
    div(
      class = "report-action-buttons",
      downloadButton(
        summary_id,
        "Export Summary",
        icon = icon("file-excel"),
        class = "btn-success"
      ),
      actionButton(
        button_id,
        "Report & Export",
        icon = icon("file-export"),
        class = "btn-primary"
      )
    )
  )
}

interaction_tip <- function(text) {
  div(
    class = "interaction-tip",
    icon("mouse-pointer"),
    tags$span(text)
  )
}

metric_definition <- function(title, text) {
  div(
    class = "definition-card",
    tags$strong(title),
    tags$span(text)
  )
}

safe_divide <- function(numerator, denominator, default = 0) {
  ifelse(
    is.na(denominator) | denominator == 0,
    default,
    numerator / denominator
  )
}


future_ui_js <- "
(function() {
  function safeStore(key, value) {
    try { window.localStorage.setItem(key, value); } catch (e) {}
  }
  function safeRead(key) {
    try { return window.localStorage.getItem(key); } catch (e) { return null; }
  }
  function applyFutureState() {
    $('body').addClass('aurelis-future');
    if (safeRead('aurelisAmbient') === '1') $('body').addClass('aurelis-ambient');
    if (safeRead('aurelisCompact') === '1') $('body').addClass('aurelis-compact');
  }
  $(document).on('shiny:connected', applyFutureState);
  $(document).ready(applyFutureState);

  $(document).on('click', '#future_theme_toggle', function(e) {
    e.preventDefault();
    $('body').toggleClass('aurelis-ambient');
    safeStore('aurelisAmbient', $('body').hasClass('aurelis-ambient') ? '1' : '0');
    window.dispatchEvent(new Event('resize'));
  });

  $(document).on('click', '#future_density_toggle', function(e) {
    e.preventDefault();
    $('body').toggleClass('aurelis-compact');
    safeStore('aurelisCompact', $('body').hasClass('aurelis-compact') ? '1' : '0');
    window.dispatchEvent(new Event('resize'));
  });
})();
"


# ------------------------------------------------------------------------------
# Quiet Excel import and reliable Plotly event helpers
# ------------------------------------------------------------------------------

# Read every Excel field as text first. The dashboard already performs its own
# explicit date and numeric conversion, so this avoids readxl type-guessing
# warnings and preserves identifiers exactly as stored in the workbooks.
read_excel_quiet <- function(...) {
  suppressMessages(
    withCallingHandlers(
      suppressWarnings(
        readxl::read_excel(
          ...,
          col_types = "text",
          progress = FALSE,
          .name_repair = "unique_quiet"
        )
      ),
      message = function(m) {
        msg <- conditionMessage(m)
        if (grepl("^New names:", msg) || grepl("coercing text to numeric", msg, ignore.case = TRUE)) {
          invokeRestart("muffleMessage")
        }
      },
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("coercing text to numeric", msg, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
}

# Explicitly register click events on every interactive Plotly object.
register_plotly_click <- function(chart) {
  chart <- plotly::event_register(chart, "plotly_click")
  chart
}

# event_data() can be evaluated once while the Shiny session is starting,
# before an off-screen tab has rendered its chart. Suppressing that transient
# startup warning keeps the console clean; the charts themselves are still
# explicitly registered with register_plotly_click().
safe_plotly_event_data <- function(
    event = "plotly_click",
    source = "A",
    priority = c("input", "event"),
    session = shiny::getDefaultReactiveDomain()
) {
  priority <- match.arg(priority)
  withCallingHandlers(
    plotly::event_data(
      event = event,
      source = source,
      session = session,
      priority = priority
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("not registered", msg, ignore.case = TRUE)) invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("not registered", msg, ignore.case = TRUE)) invokeRestart("muffleMessage")
    }
  )
}

################################################################################
# SECTION 2: PUBLIC SYNTHETIC DATA LOADER
################################################################################

clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("","NA","N/A","NULL","-","null")] <- NA_character_
  x
}

safe_report_filename <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x,"[^A-Za-z0-9_-]+","_")
  x <- str_replace_all(x,"_+","_")
  x <- str_remove_all(x,"^_|_$")
  ifelse(is.na(x)|x=="","Aurelis_Report",x)
}

report_display_value <- function(x) {
  if (inherits(x,"Date")) return(format(x,"%Y-%m-%d"))
  if (inherits(x,"POSIXt")) return(format(x,"%Y-%m-%d %H:%M"))
  if (is.factor(x)) return(as.character(x))
  if (is.numeric(x)) return(ifelse(is.na(x),"",format(round(x,2),big.mark=",",scientific=FALSE,trim=TRUE)))
  y <- as.character(x); y[is.na(y)] <- ""; y
}
prepare_report_dataframe <- function(df) {
  df <- as.data.frame(df,stringsAsFactors=FALSE)
  if (ncol(df)==0) return(df)
  df[] <- lapply(df,report_display_value)
  df
}
parse_report_pages <- function(specification,total_pages) {
  if (total_pages<=0) return(integer(0))
  specification <- str_to_lower(str_squish(coalesce(specification,"all")))
  if (specification %in% c("","all","all pages","*")) return(seq_len(total_pages))
  pieces <- str_split(specification,",",simplify=FALSE)[[1]]
  pages <- integer(0)
  for (piece in pieces) {
    piece <- str_squish(piece)
    if (str_detect(piece,"^[0-9]+$")) pages <- c(pages,as.integer(piece))
    else if (str_detect(piece,"^[0-9]+\\s*-\\s*[0-9]+$")) {
      limits <- as.integer(str_extract_all(piece,"[0-9]+")[[1]])
      pages <- c(pages,seq(min(limits),max(limits)))
    } else stop("Invalid PDF page selection. Use All or a range such as 1-3,5.",call.=FALSE)
  }
  pages <- sort(unique(pages))
  if (any(pages<1|pages>total_pages)) stop(paste0("PDF pages must be between 1 and ",total_pages,"."),call.=FALSE)
  pages
}
html_report_table <- function(df,empty_message="No records are available.") {
  df <- prepare_report_dataframe(df)
  if (nrow(df)==0 || ncol(df)==0) return(paste0("<p class='empty-message'>",htmltools::htmlEscape(empty_message),"</p>"))
  header <- paste0("<tr>",paste0("<th>",htmltools::htmlEscape(names(df)),"</th>",collapse=""),"</tr>")
  rows <- apply(df,1,function(r) paste0("<tr>",paste0("<td>",htmltools::htmlEscape(r),"</td>",collapse=""),"</tr>"))
  paste0("<table class='report-table'><thead>",header,"</thead><tbody>",paste(rows,collapse=""),"</tbody></table>")
}

aurelis_logo_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='820' height='220' viewBox='0 0 820 220'>",
  "<rect width='820' height='220' rx='28' fill='white'/>",
  "<path d='M62 150 L120 45 L178 150 Z' fill='#1479C9'/>",
  "<path d='M93 150 L120 101 L147 150 Z' fill='#20BFA9'/>",
  "<text x='210' y='122' font-family='Segoe UI,Arial' font-size='68' font-weight='800' fill='#071A2B'>AURELIS</text>",
  "<text x='214' y='164' font-family='Segoe UI,Arial' font-size='24' font-weight='600' letter-spacing='5' fill='#1479C9'>GLOBAL SUPPLY</text>",
  "</svg>"
)
aurelis_logo_source <- paste0("data:image/svg+xml;utf8,",utils::URLencode(aurelis_logo_svg,reserved=TRUE))

# High-definition dark-interface logo variants. These are SVG vectors, so they
# remain perfectly sharp at any monitor scale.
aurelis_logo_header_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='900' height='260' viewBox='0 0 900 260'>",
  "<defs>",
  "<linearGradient id='triA' x1='0' y1='0' x2='1' y2='1'>",
  "<stop offset='0%' stop-color='#7EF5FF'/>",
  "<stop offset='48%' stop-color='#2CD5D2'/>",
  "<stop offset='100%' stop-color='#2374E1'/>",
  "</linearGradient>",
  "<linearGradient id='wordA' x1='0' y1='0' x2='1' y2='1'>",
  "<stop offset='0%' stop-color='#FFFFFF'/>",
  "<stop offset='100%' stop-color='#B5D7EF'/>",
  "</linearGradient>",
  "<filter id='glowA'><feGaussianBlur stdDeviation='7' result='b'/><feMerge><feMergeNode in='b'/><feMergeNode in='SourceGraphic'/></feMerge></filter>",
  "</defs>",
  "<ellipse cx='128' cy='130' rx='111' ry='49' fill='none' stroke='#5CE7F2' stroke-opacity='.42' stroke-width='3' transform='rotate(-22 128 130)'/>",
  "<path d='M48 198 L128 52 L208 198 Z' fill='url(#triA)' filter='url(#glowA)'/>",
  "<path d='M89 198 L128 127 L167 198 Z' fill='#23D7B7'/>",
  "<path d='M128 52 L128 198 M78 145 L178 145' stroke='#D9FFFF' stroke-opacity='.48' stroke-width='2'/>",
  "<circle cx='128' cy='53' r='7' fill='#FFFFFF'/>",
  "<text x='255' y='145' font-family='Segoe UI,Arial' font-size='88' font-weight='800' letter-spacing='3' fill='url(#wordA)'>AURELIS</text>",
  "<text x='261' y='195' font-family='Segoe UI,Arial' font-size='30' font-weight='650' letter-spacing='7' fill='#61E1EF'>GLOBAL SUPPLY  /  INTELLIGENCE</text>",
  "</svg>"
)
aurelis_logo_header_source <- paste0(
  "data:image/svg+xml;utf8,",
  utils::URLencode(aurelis_logo_header_svg, reserved = TRUE)
)

aurelis_logo_sidebar_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='420' height='420' viewBox='0 0 420 420'>",
  "<defs>",
  "<linearGradient id='triB' x1='0' y1='0' x2='1' y2='1'>",
  "<stop offset='0%' stop-color='#40D4E4'/>",
  "<stop offset='100%' stop-color='#1479C9'/>",
  "</linearGradient>",
  "</defs>",
  "<ellipse cx='210' cy='190' rx='170' ry='72' fill='none' stroke='#5CE7F2' stroke-opacity='.38' stroke-width='4' transform='rotate(-24 210 190)'/>",
  "<path d='M108 214 L210 40 L312 214 Z' fill='url(#triB)'/>",
  "<path d='M160 214 L210 126 L260 214 Z' fill='#22D9B8'/>",
  "<path d='M210 40 L210 214 M145 165 L275 165' stroke='#E7FFFF' stroke-opacity='.45' stroke-width='3'/>",
  "<circle cx='210' cy='40' r='9' fill='#FFFFFF'/>",
  "<text x='210' y='290' text-anchor='middle' font-family='Segoe UI,Arial' font-size='54' font-weight='800' fill='#F7FAFC'>AURELIS</text>",
  "<text x='210' y='335' text-anchor='middle' font-family='Segoe UI,Arial' font-size='20' font-weight='650' letter-spacing='4' fill='#55B7F4'>GLOBAL SUPPLY</text>",
  "</svg>"
)
aurelis_logo_sidebar_source <- paste0(
  "data:image/svg+xml;utf8,",
  utils::URLencode(aurelis_logo_sidebar_svg, reserved = TRUE)
)
report_logo_file <- function() ""
report_logo_data_uri <- function() aurelis_logo_source

build_aurelis_pdf_html <- function(
    title,page_name,filters,summary,detail,notes="",
    content_mode="summary_details",rows_per_page=25,
    orientation="landscape",page_specification="all",paper_size="A4",
    logo_uri=report_logo_data_uri()) {
  title <- ifelse(is.null(title)||str_squish(title)=="","Aurelis Business Report",title)
  page_name <- ifelse(is.null(page_name)||str_squish(page_name)=="","Operations Intelligence",page_name)
  rows_per_page <- max(5,min(100,as.integer(rows_per_page)))
  include_summary <- content_mode %in% c("summary_details","summary_only")
  include_detail <- content_mode %in% c("summary_details","details_only")
  orientation <- ifelse(orientation=="portrait","portrait","landscape")
  paper_size <- ifelse(toupper(paper_size)=="LETTER","Letter","A4")
  pages <- list(); titles <- character(0)
  
  ps <- prepare_report_dataframe(summary)
  cards <- if (nrow(ps)>0 && ncol(ps)>=2) paste0(
    "<div class='summary-grid'>",
    paste0("<div class='summary-card'><span>",htmltools::htmlEscape(ps[[1]]),"</span><strong>",
           htmltools::htmlEscape(ps[[2]]),"</strong></div>",collapse=""),
    "</div>") else "<p class='empty-message'>No summary metrics are available.</p>"
  
  if (include_summary) {
    pages <- append(pages,list(paste0(
      "<section class='report-page'><header><img src='",logo_uri,"' alt='Aurelis logo'><div><strong>AURELIS GLOBAL SUPPLY, INC.</strong><span>Public Advisory Demonstration</span></div></header>",
      "<div class='accent'></div><div class='eyebrow'>BUSINESS REPORT</div><h1>",htmltools::htmlEscape(title),"</h1>",
      "<div class='subtitle'>",htmltools::htmlEscape(page_name),"</div>",
      if (str_squish(notes)!="") paste0("<div class='note'>",htmltools::htmlEscape(notes),"</div>") else "",
      "<h2>Executive summary</h2>",cards,"<h2>Active filters</h2>",html_report_table(filters,"No page filters are active."),"</section>"
    )))
    titles <- c(titles,"Executive Summary")
  }
  if (include_detail) {
    detail <- as.data.frame(detail,stringsAsFactors=FALSE)
    chunks <- if (nrow(detail)==0) list(detail) else split(detail,ceiling(seq_len(nrow(detail))/rows_per_page))
    for (i in seq_along(chunks)) {
      pages <- append(pages,list(paste0(
        "<section class='report-page'><header><img src='",logo_uri,"' alt='Aurelis logo'><div><strong>",
        htmltools::htmlEscape(page_name),"</strong><span>",htmltools::htmlEscape(title),"</span></div></header>",
        "<div class='accent'></div><h2>Detailed records</h2>",html_report_table(chunks[[i]],"No detailed records match the selected filters."),"</section>"
      )))
      titles <- c(titles,paste0("Details ",i))
    }
  }
  total <- length(pages); selected <- parse_report_pages(page_specification,total); pages <- pages[selected]
  for (i in seq_along(pages)) {
    original <- selected[[i]]
    footer <- paste0("<footer><span>Aurelis Global Supply, Inc. · Synthetic public demo</span><span>Page ",
                     original," of ",total," · ",htmltools::htmlEscape(titles[[original]]),"</span></footer>")
    pages[[i]] <- str_replace(pages[[i]],"</section>$",paste0(footer,"</section>"))
  }
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><style>",
    "@page{size:",paper_size," ",orientation,";margin:10mm}*{box-sizing:border-box}body{font-family:'Segoe UI',Arial,sans-serif;color:#1F2937;margin:0}",
    ".report-page{position:relative;min-height:180mm;page-break-after:always;padding-bottom:12mm}.report-page:last-child{page-break-after:auto}",
    "header{display:flex;justify-content:space-between;align-items:center}header img{width:48mm;height:15mm;object-fit:contain;object-position:left}",
    "header div{display:flex;flex-direction:column;text-align:right}header strong{color:#071A2B;font-size:11px}header span{color:#64748B;font-size:8px}",
    ".accent{height:4px;background:linear-gradient(90deg,#071A2B,#1479C9,#20BFA9);margin:5mm 0 8mm;border-radius:4px}",
    ".eyebrow{color:#1479C9;font-size:8px;font-weight:800;letter-spacing:1.4px}h1{color:#071A2B;font-size:27px;margin:2mm 0}h2{color:#071A2B;font-size:15px}",
    ".subtitle{color:#526273}.note{background:#EEF7FB;border-left:4px solid #20BFA9;padding:3mm;margin:5mm 0}",
    ".summary-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:3mm}.summary-card{border:1px solid #D7E1EA;border-left:3px solid #1479C9;border-radius:7px;padding:3mm;background:#F8FBFD}",
    ".summary-card span{display:block;font-size:7px;color:#64748B;text-transform:uppercase;font-weight:800}.summary-card strong{display:block;color:#071A2B;font-size:14px;margin-top:2mm}",
    ".report-table{width:100%;border-collapse:collapse;table-layout:fixed;font-size:8px}.report-table th{background:#071A2B;color:white;text-align:left;padding:5px;border:1px solid #071A2B;word-break:break-word}",
    ".report-table td{padding:4px;border:1px solid #D7E1EA;vertical-align:top;word-break:break-word}.report-table tbody tr:nth-child(even){background:#F7FAFC}",
    ".empty-message{color:#64748B;border:1px dashed #CBD5E1;padding:5mm;text-align:center}.report-page footer{position:absolute;bottom:1mm;left:0;right:0;border-top:1px solid #CBD5E1;padding-top:2mm;display:flex;justify-content:space-between;font-size:7px;color:#64748B}",
    "</style></head><body>",paste(pages,collapse=""),"</body></html>"
  )
}

find_aurelis_app_dir <- function() {
  candidates <- character(0)
  appdir <- getOption("shiny.appDir","")
  if (!is.null(appdir)&&length(appdir)>0&&nzchar(appdir[[1]])) candidates <- c(candidates,appdir[[1]])
  frame_files <- vapply(sys.frames(),function(frame) {
    v <- frame$ofile
    if (is.null(v)||length(v)==0) NA_character_ else as.character(v[[1]])
  },character(1))
  frame_files <- frame_files[!is.na(frame_files)&nzchar(frame_files)]
  if (length(frame_files)>0) candidates <- c(candidates,dirname(frame_files))
  if (requireNamespace("rstudioapi",quietly=TRUE)&&rstudioapi::isAvailable()) {
    f <- tryCatch(rstudioapi::getActiveDocumentContext()$path,error=function(e)"")
    if (nzchar(f)) candidates <- c(candidates,dirname(f))
  }
  candidates <- unique(c(candidates,getwd()))
  candidates <- normalizePath(candidates[nzchar(candidates)],winslash="/",mustWork=FALSE)
  matching <- candidates[dir.exists(file.path(candidates,"data")) &
                           file.exists(file.path(candidates,"data","Aurelis_Daily_PO.csv"))]
  if (length(matching)==0) stop("Keep the data folder beside Aurelis_Public_Advisory_Dashboard.R.",call.=FALSE)
  matching[[1]]
}

AURELIS_APP_DIR <- find_aurelis_app_dir()
AURELIS_APP_ENV <- environment()
AURELIS_DATA_DIR <- file.path(AURELIS_APP_DIR,"data")
AURELIS_DATA_SOURCE_MODE <- "SYNTHETIC_DEMO"
AURELIS_ACCESS_DB_PATH <- file.path(AURELIS_DATA_DIR,"Aurelis_Daily_PO.csv")
AURELIS_ALLOW_EXCEL_FALLBACK <- FALSE
AURELIS_QUICKBOOKS_ENABLED <- FALSE
AURELIS_AUTO_REFRESH_SECONDS <- 300L
AURELIS_QB_SYNC_SECONDS <- 300L
AURELIS_ACCESS_CONTROL_ENABLED <- FALSE
AURELIS_ACCESS_DIRECTORY <- data.frame(
  email = c("ceo@aurelis.local", "analyst@aurelis.local", "manager@aurelis.local", "buyer@aurelis.local"),
  role = c("CEO", "Analyst", "Manager", "Buyer"),
  access_key = c("CEO-FULL", "ANALYST-FULL", "MANAGER-OPS", "BUYER-CLIENTS"),
  stringsAsFactors = FALSE
)
AURELIS_SHARED_REFRESH_STATE <- new.env(parent=emptyenv())
AURELIS_SHARED_REFRESH_STATE$version <- 1L
AURELIS_SHARED_REFRESH_STATE$last_refresh <- Sys.time()
AURELIS_SHARED_REFRESH_STATE$running <- FALSE
AURELIS_DEMO_LOADED_AT <- Sys.time()

# ------------------------------------------------------------------------------
# Adaptive dataset reading and schema safety
# ------------------------------------------------------------------------------

AURELIS_DATA_DIAGNOSTICS <- new.env(parent = emptyenv())
AURELIS_DATA_DIAGNOSTICS$messages <- character(0)

data_diag <- function(message) {
  message <- str_squish(as.character(message))
  if (!nzchar(message)) return(invisible(FALSE))
  AURELIS_DATA_DIAGNOSTICS$messages <- unique(c(
    tail(AURELIS_DATA_DIAGNOSTICS$messages, 99),
    message
  ))
  invisible(TRUE)
}

normalize_field_key <- function(x) {
  str_to_lower(str_replace_all(as.character(x), "[^A-Za-z0-9]+", ""))
}

safe_character <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_squish(x)
  x
}

safe_numeric <- function(x, default = NA_real_) {
  if (is.numeric(x)) {
    out <- as.numeric(x)
  } else {
    raw <- safe_character(x)
    negative_parentheses <- str_detect(raw, "^\\(.*\\)$")
    raw <- str_replace_all(raw, "[,$% ]", "")
    raw <- str_replace_all(raw, "^\\((.*)\\)$", "\\1")
    out <- suppressWarnings(as.numeric(raw))
    out[negative_parentheses & is.finite(out)] <- -abs(out[negative_parentheses & is.finite(out)])
  }
  if (!is.na(default)) out[!is.finite(out)] <- default
  out
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  raw <- safe_character(x)
  raw[raw == ""] <- NA_character_
  
  direct <- suppressWarnings(as.Date(raw))
  missing <- is.na(direct) & !is.na(raw)
  
  if (any(missing)) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        raw[missing],
        orders = c(
          "ymd", "mdy", "dmy",
          "Ymd HMS", "mdY HMS", "dmY HMS",
          "Y-m-d H:M:S", "m/d/Y H:M:S", "d/m/Y H:M:S"
        ),
        quiet = TRUE,
        tz = "UTC"
      )
    )
    direct[missing] <- as.Date(parsed)
  }
  
  direct
}

read_demo_csv <- function(name) {
  path <- file.path(AURELIS_DATA_DIR, name)
  if (!file.exists(path)) {
    stop(paste0("Required public-demo dataset is missing: ", name), call. = FALSE)
  }
  
  attempts <- list(
    list(fileEncoding = "UTF-8-BOM", sep = ","),
    list(fileEncoding = "UTF-8", sep = ","),
    list(fileEncoding = "latin1", sep = ","),
    list(fileEncoding = "UTF-8", sep = ";")
  )
  
  last_error <- NULL
  for (attempt in attempts) {
    result <- tryCatch(
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = attempt$fileEncoding,
        sep = attempt$sep,
        na.strings = c("", "NA", "N/A", "NULL", "null")
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(result) && ncol(result) > 0) {
      names(result) <- str_squish(str_replace(names(result), "^\\ufeff", ""))
      return(result)
    }
  }
  
  stop(
    paste0(
      "Could not read ", name, ". ",
      if (is.null(last_error)) "Unknown parsing error." else conditionMessage(last_error)
    ),
    call. = FALSE
  )
}

adapt_schema <- function(df, aliases, defaults = list(), dataset_name = "dataset") {
  if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  existing_keys <- normalize_field_key(names(df))
  
  for (canonical in names(aliases)) {
    if (canonical %in% names(df)) next
    
    candidate_keys <- unique(normalize_field_key(c(canonical, aliases[[canonical]])))
    hit <- which(existing_keys %in% candidate_keys)
    
    if (length(hit) > 0) {
      old_name <- names(df)[hit[[1]]]
      names(df)[hit[[1]]] <- canonical
      existing_keys[hit[[1]]] <- normalize_field_key(canonical)
      data_diag(paste0(dataset_name, ": mapped '", old_name, "' -> '", canonical, "'."))
    } else {
      default <- if (canonical %in% names(defaults)) defaults[[canonical]] else NA
      df[[canonical]] <- rep(default, nrow(df))
      existing_keys <- c(existing_keys, normalize_field_key(canonical))
      data_diag(paste0(dataset_name, ": missing '", canonical, "'; safe default was supplied."))
    }
  }
  
  df
}

to_num <- function(x) safe_numeric(x)

load_aurelis_demo_data <- function(target_env=AURELIS_APP_ENV) {
  AURELIS_DATA_DIAGNOSTICS$messages <- character(0)
  
  daily_po <- read_demo_csv("Aurelis_Daily_PO.csv") %>%
    adapt_schema(
      aliases = list(
        Date = c("Order_Date", "PO_Date", "Transaction_Date"),
        Year = c("Order_Year"),
        Month = c("Order_Month"),
        Month_Name = c("Order_Month_Name"),
        Month_Abbr = c("Month_Abbreviation"),
        Quarter = c("Order_Quarter"),
        Total_PO = c("PO_Total", "PO_Value", "Order_Total", "Total"),
        Client_ID = c("Customer_ID", "Customer_Code", "ClientCode"),
        Client = c("Customer", "Customer_Name", "Client_Name"),
        Buyer = c("Purchaser", "Buyer_Name", "Procurement_Buyer"),
        PO_Number = c("PO", "PONumber", "Purchase_Order", "Order_Number")
      ),
      defaults = list(
        Total_PO = 0,
        Year = NA_real_,
        Month = NA_real_,
        Month_Name = "",
        Month_Abbr = "",
        Quarter = ""
      ),
      dataset_name = "Daily PO"
    ) %>%
    mutate(
      Date = safe_date(Date),
      Client_ID = safe_character(Client_ID),
      Client = safe_character(Client),
      Buyer = safe_character(Buyer),
      PO_Number = safe_character(PO_Number),
      across(c(Year, Month, Total_PO), to_num),
      Year = ifelse(!is.finite(Year) & !is.na(Date), lubridate::year(Date), Year),
      Month = ifelse(!is.finite(Month) & !is.na(Date), lubridate::month(Date), Month),
      Month_Name = ifelse(
        is.na(Month_Name) | Month_Name == "",
        ifelse(is.finite(Month), month.name[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Name
      ),
      Month_Abbr = ifelse(
        is.na(Month_Abbr) | Month_Abbr == "",
        ifelse(is.finite(Month), month.abb[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Abbr
      ),
      Quarter = ifelse(
        is.na(Quarter) | Quarter == "",
        ifelse(is.finite(Month), paste0("Q", ((as.integer(Month) - 1L) %/% 3L) + 1L), ""),
        Quarter
      )
    )
  
  revenue_data <- read_demo_csv("Aurelis_Revenue.csv") %>%
    adapt_schema(
      aliases = list(
        Date = c("Invoice_Date", "Txn_Date", "Transaction_Date"),
        Year = c("Revenue_Year"),
        Month = c("Revenue_Month"),
        Month_Name = c("Revenue_Month_Name"),
        Month_Abbr = c("Month_Abbreviation"),
        Quarter = c("Revenue_Quarter"),
        Revenue = c("Sales", "Sales_Amount", "Net_Sales"),
        Cost = c("COGS", "Direct_Cost", "Cost_Of_Goods"),
        Gross_Profit = c("GrossProfit", "GP", "Profit"),
        Margin_Pct = c("Gross_Margin", "Margin", "Margin_Percent"),
        Staff = c("Sales_Rep", "Representative", "Rep"),
        Customer = c("Client", "Customer_Name", "Entity"),
        Customer_ID = c("Client_ID", "Customer_Code"),
        Entity = c("Billing_Entity", "Customer_Entity", "Client"),
        Parent_Company = c("Parent", "ParentCompany"),
        Country = c("Customer_Country", "Bill_Country"),
        Invoice_Ref = c("Invoice", "Invoice_Number", "Txn_Number"),
        PO_Number = c("PO", "PONumber", "Purchase_Order")
      ),
      defaults = list(
        Revenue = 0,
        Cost = 0,
        Gross_Profit = 0,
        Margin_Pct = NA_real_,
        Year = NA_real_,
        Month = NA_real_,
        Month_Name = "",
        Month_Abbr = "",
        Quarter = "",
        Entity = ""
      ),
      dataset_name = "Revenue"
    ) %>%
    mutate(
      Date = safe_date(Date),
      Customer_ID = safe_character(Customer_ID),
      Customer = safe_character(Customer),
      Entity = ifelse(safe_character(Entity) == "", Customer, safe_character(Entity)),
      Staff = safe_character(Staff),
      PO_Number = safe_character(PO_Number),
      Invoice_Ref = safe_character(Invoice_Ref),
      across(c(Year, Month, Revenue, Cost, Gross_Profit, Margin_Pct), to_num),
      Year = ifelse(!is.finite(Year) & !is.na(Date), lubridate::year(Date), Year),
      Month = ifelse(!is.finite(Month) & !is.na(Date), lubridate::month(Date), Month),
      Month_Name = ifelse(
        is.na(Month_Name) | Month_Name == "",
        ifelse(is.finite(Month), month.name[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Name
      ),
      Month_Abbr = ifelse(
        is.na(Month_Abbr) | Month_Abbr == "",
        ifelse(is.finite(Month), month.abb[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Abbr
      ),
      Quarter = ifelse(
        is.na(Quarter) | Quarter == "",
        ifelse(is.finite(Month), paste0("Q", ((as.integer(Month) - 1L) %/% 3L) + 1L), ""),
        Quarter
      )
    )
  
  ar_data <- read_demo_csv("Aurelis_Accounts_Receivable.csv") %>%
    adapt_schema(
      aliases = list(
        Txn_Date = c("Invoice_Date", "Transaction_Date", "Date"),
        Ship_Date = c("Shipment_Date", "Delivery_Date"),
        Due_Date = c("Payment_Due_Date", "Due"),
        Year = c("Invoice_Year", "Txn_Year"),
        Month = c("Invoice_Month", "Txn_Month"),
        Month_Name = c("Invoice_Month_Name"),
        Invoice_Amount = c("Invoice_Total", "Amount", "Original_Amount"),
        Balance_Remaining = c("Open_Balance", "Balance", "Outstanding"),
        Customer = c("Client", "Customer_Name"),
        Customer_ID = c("Client_ID", "Customer_Code"),
        Staff = c("Sales_Rep", "Representative"),
        PO_Number = c("PO", "PONumber", "Purchase_Order"),
        Invoice_Ref = c("Invoice", "Invoice_Number", "Txn_Number"),
        Terms = c("Payment_Terms"),
        Days_Outstanding = c("Days_Open", "Outstanding_Days"),
        Days_Until_Due = c("Due_In_Days"),
        Days_Past_Due = c("Past_Due_Days", "Overdue_Days"),
        Aging_Bucket = c("Aging", "Age_Bucket", "Aging_Group")
      ),
      defaults = list(
        Invoice_Amount = 0,
        Balance_Remaining = 0,
        Days_Outstanding = 0,
        Days_Until_Due = 0,
        Days_Past_Due = 0,
        Aging_Bucket = "Current",
        Year = NA_real_,
        Month = NA_real_,
        Month_Name = ""
      ),
      dataset_name = "Accounts Receivable"
    ) %>%
    mutate(
      Txn_Date = safe_date(Txn_Date),
      Ship_Date = safe_date(Ship_Date),
      Due_Date = safe_date(Due_Date),
      Customer_ID = safe_character(Customer_ID),
      Customer = safe_character(Customer),
      Staff = safe_character(Staff),
      PO_Number = safe_character(PO_Number),
      Invoice_Ref = safe_character(Invoice_Ref),
      across(c(Year,Month,Invoice_Amount,Balance_Remaining,Days_Outstanding,Days_Until_Due,Days_Past_Due),to_num),
      Year = ifelse(!is.finite(Year) & !is.na(Txn_Date), lubridate::year(Txn_Date), Year),
      Month = ifelse(!is.finite(Month) & !is.na(Txn_Date), lubridate::month(Txn_Date), Month),
      Month_Name = ifelse(
        is.na(Month_Name) | Month_Name == "",
        ifelse(is.finite(Month), month.name[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Name
      )
    )
  ar_data$Aging_Bucket <- factor(ar_data$Aging_Bucket,levels=c("Paid","Current","1-30 Days","31-60 Days","61-90 Days","90+ Days"))
  
  inventory_data <- read_demo_csv("Aurelis_Inventory.csv") %>%
    adapt_schema(
      aliases = list(
        Order_Date = c("Date", "PO_Date", "Purchase_Date"),
        Delivery_Date = c("Expected_Delivery", "ETA", "Due_Date"),
        PO_Number = c("PO", "PONumber", "Purchase_Order", "Order_Number"),
        Client_ID = c("Customer_ID", "Customer_Code"),
        Client = c("Customer", "Customer_Name", "Client_Name"),
        Buyer = c("Purchaser", "Buyer_Name"),
        Supplier = c("Vendor", "Seller", "Supplier_Name"),
        Memo = c("Notes", "Line_Memo"),
        Part_Number = c("Part", "Item_Code", "SKU"),
        Order_Description = c("Description", "Item_Description", "Product_Description"),
        Item_Description = c("Description", "Order_Description", "Product_Description"),
        Line_Type = c("Type", "Item_Type"),
        Quantity = c("Qty", "Ordered_Qty"),
        Unit_Price = c("UnitPrice", "Price", "Unit_Cost"),
        Received_Qty = c("Qty_Received", "Received", "Delivered_Qty"),
        Backordered_Qty = c("Backorder_Qty", "Open_Qty", "Qty_Backordered"),
        Amount = c("Line_Total", "Extended_Amount", "Value"),
        Open_Balance = c("Open_Value", "Outstanding_Value", "Remaining_Value"),
        PO_Total = c("Total_PO", "Order_Total", "Purchase_Order_Total"),
        Days_To_Delivery = c("Days_Until_Delivery", "Lead_Days_Remaining"),
        Order_Status = c("Status", "Delivery_Status"),
        Year = c("Order_Year"),
        Month = c("Order_Month"),
        Month_Name = c("Order_Month_Name"),
        Delivery_Year = c("ETA_Year"),
        Delivery_Month = c("ETA_Month"),
        Delivery_Month_Name = c("ETA_Month_Name"),
        Delivery_Week = c("ETA_Week", "Delivery_Week_Number"),
        Delivery_Week_Label = c("ETA_Week_Label")
      ),
      defaults = list(
        Quantity = 0,
        Unit_Price = 0,
        Received_Qty = 0,
        Backordered_Qty = 0,
        Amount = 0,
        Open_Balance = 0,
        PO_Total = 0,
        Line_Type = "Product",
        Order_Status = "Open",
        Year = NA_real_,
        Month = NA_real_,
        Month_Name = "",
        Delivery_Year = NA_real_,
        Delivery_Month = NA_real_,
        Delivery_Month_Name = "",
        Delivery_Week = NA_real_,
        Delivery_Week_Label = ""
      ),
      dataset_name = "Inventory / Delivery"
    ) %>%
    mutate(
      Order_Date = safe_date(Order_Date),
      Delivery_Date = safe_date(Delivery_Date),
      PO_Number = safe_character(PO_Number),
      Client_ID = safe_character(Client_ID),
      Client = safe_character(Client),
      Buyer = safe_character(Buyer),
      Supplier = safe_character(Supplier),
      Part_Number = safe_character(Part_Number),
      Order_Description = safe_character(Order_Description),
      Item_Description = safe_character(Item_Description),
      Line_Type = safe_character(Line_Type),
      Order_Status = safe_character(Order_Status),
      across(
        c(
          Quantity,Unit_Price,Received_Qty,Backordered_Qty,Amount,Open_Balance,PO_Total,
          Days_To_Delivery,Year,Month,Delivery_Year,Delivery_Month,Delivery_Week
        ),
        to_num
      ),
      Year = ifelse(!is.finite(Year) & !is.na(Order_Date), lubridate::year(Order_Date), Year),
      Month = ifelse(!is.finite(Month) & !is.na(Order_Date), lubridate::month(Order_Date), Month),
      Month_Name = ifelse(
        is.na(Month_Name) | Month_Name == "",
        ifelse(is.finite(Month), month.name[pmax(1, pmin(12, as.integer(Month)))], ""),
        Month_Name
      ),
      Delivery_Year = ifelse(
        !is.finite(Delivery_Year) & !is.na(Delivery_Date),
        lubridate::year(Delivery_Date),
        Delivery_Year
      ),
      Delivery_Month = ifelse(
        !is.finite(Delivery_Month) & !is.na(Delivery_Date),
        lubridate::month(Delivery_Date),
        Delivery_Month
      ),
      Delivery_Month_Name = ifelse(
        is.na(Delivery_Month_Name) | Delivery_Month_Name == "",
        ifelse(
          is.finite(Delivery_Month),
          month.name[pmax(1, pmin(12, as.integer(Delivery_Month)))],
          ""
        ),
        Delivery_Month_Name
      ),
      Delivery_Week = ifelse(
        !is.finite(Delivery_Week) & !is.na(Delivery_Date),
        pmin(5, ceiling(lubridate::day(Delivery_Date) / 7)),
        Delivery_Week
      ),
      Delivery_Week_Label = ifelse(
        is.na(Delivery_Week_Label) | Delivery_Week_Label == "",
        ifelse(is.finite(Delivery_Week), paste0("Week ", as.integer(Delivery_Week)), ""),
        Delivery_Week_Label
      ),
      Days_To_Delivery = ifelse(
        !is.finite(Days_To_Delivery) & !is.na(Delivery_Date),
        as.numeric(Delivery_Date - Sys.Date()),
        Days_To_Delivery
      )
    )
  
  inventory_data$Order_Status <- factor(
    inventory_data$Order_Status,
    levels = c("Overdue","Due Soon","Open","Fully Received","Information")
  )
  
  warehouse_inventory_data <- read_demo_csv("Aurelis_Warehouse.csv") %>%
    adapt_schema(
      aliases = list(
        Item_Code = c("Part_Number", "SKU", "Item"),
        Item_Description = c("Description", "Product_Description"),
        Warehouse = c("Location", "Warehouse_Name"),
        Bin_Location = c("Bin", "Shelf", "Storage_Bin"),
        Supplier = c("Vendor", "Seller"),
        On_Hand = c("Qty_On_Hand", "Stock_On_Hand"),
        Allocated = c("Committed", "Reserved", "Allocated_Qty"),
        Available = c("Qty_Available", "Free_Stock"),
        On_Order = c("Qty_On_Order", "Inbound_Qty"),
        Reorder_Point = c("Min_Stock", "Reorder_Level"),
        Unit_Cost = c("Cost", "Average_Cost"),
        Monthly_Demand = c("Demand", "Avg_Monthly_Demand"),
        Months_Of_Cover = c("Stock_Cover_Months", "Coverage_Months"),
        Inventory_Value = c("Stock_Value", "On_Hand_Value"),
        Last_Updated = c("Updated_At", "Snapshot_Time"),
        Stock_Status = c("Status", "Inventory_Status")
      ),
      defaults = list(
        On_Hand = 0,
        Allocated = 0,
        Available = 0,
        On_Order = 0,
        Reorder_Point = 0,
        Unit_Cost = 0,
        Monthly_Demand = 0,
        Months_Of_Cover = 0,
        Inventory_Value = 0,
        Stock_Status = "Available"
      ),
      dataset_name = "Warehouse"
    ) %>%
    mutate(
      Last_Updated = suppressWarnings(as.POSIXct(Last_Updated, tz = "UTC")),
      Item_Code = safe_character(Item_Code),
      Item_Description = safe_character(Item_Description),
      Warehouse = safe_character(Warehouse),
      Supplier = safe_character(Supplier),
      Stock_Status = safe_character(Stock_Status),
      across(
        c(
          On_Hand,Allocated,Available,On_Order,Reorder_Point,Unit_Cost,
          Monthly_Demand,Months_Of_Cover,Inventory_Value
        ),
        to_num
      )
    )
  
  contacts_data <- read_demo_csv("Aurelis_Contacts.csv") %>%
    adapt_schema(
      aliases = list(
        Entity = c("Company", "Customer", "Organization"),
        Country = c("Location_Country"),
        Function = c("Role_Function", "Business_Function"),
        Department = c("Dept"),
        Position = c("Title", "Job_Title"),
        Contact_Name = c("Name", "Contact"),
        AURELIS_Rep = c("Representative", "Account_Manager", "Sales_Rep")
      ),
      dataset_name = "Contacts"
    )
  
  relationship_directory <- read_demo_csv("Aurelis_Directory.csv") %>%
    adapt_schema(
      aliases = list(
        Contact_Type = c("Type", "Entity_Type"),
        Entity = c("Company", "Organization"),
        Contact_Name = c("Name", "Contact"),
        Country = c("Location_Country"),
        Function = c("Business_Function"),
        Department = c("Dept"),
        Position = c("Title", "Job_Title"),
        Email = c("Email_Address"),
        Phone = c("Telephone", "Phone_Number"),
        Address = c("Billing_Address", "Office_Address"),
        AURELIS_Rep = c("Representative", "Account_Manager"),
        Directory_Source = c("Source")
      ),
      dataset_name = "Relationship Directory"
    )
  
  seller_catalog_data <- read_demo_csv("Aurelis_Seller_Catalog.csv") %>%
    adapt_schema(
      aliases = list(
        Supplier_ID = c("Vendor_ID", "Seller_ID"),
        Supplier = c("Vendor", "Seller", "Supplier_Name"),
        Country = c("Supplier_Country"),
        Product_Service_Type = c("Type", "Offering_Type"),
        Category = c("Product_Category", "Service_Category"),
        Product_Code = c("Part_Number", "SKU", "Service_Code"),
        Product_Service = c("Description", "Offering", "Product_Description", "Service_Description"),
        Manufacturer = c("Brand", "OEM"),
        Preferred_Rank = c("Rank", "Preference_Rank"),
        Min_Order_Qty = c("MOQ", "Minimum_Order_Quantity"),
        Typical_Lead_Days = c("Lead_Time_Days", "Lead_Days"),
        Last_Quoted_Unit_Cost = c("Quoted_Cost", "Unit_Cost", "Last_Cost"),
        Currency = c("Currency_Code"),
        Active = c("Status", "Is_Active")
      ),
      defaults = list(
        Preferred_Rank = 1,
        Min_Order_Qty = 1,
        Typical_Lead_Days = 0,
        Last_Quoted_Unit_Cost = 0,
        Currency = "USD",
        Active = "Yes"
      ),
      dataset_name = "Seller Catalog"
    ) %>%
    mutate(
      Supplier_ID = safe_character(Supplier_ID),
      Supplier = safe_character(Supplier),
      Product_Service_Type = safe_character(Product_Service_Type),
      Category = safe_character(Category),
      Product_Code = safe_character(Product_Code),
      Product_Service = safe_character(Product_Service),
      Manufacturer = safe_character(Manufacturer),
      Currency = safe_character(Currency),
      Active = safe_character(Active),
      across(c(Preferred_Rank,Min_Order_Qty,Typical_Lead_Days,Last_Quoted_Unit_Cost),to_num)
    )

  products_data <- read_demo_csv("Aurelis_Products.csv") %>%
    adapt_schema(
      aliases = list(
        Part_Number = c("Product_Code", "SKU", "Item_Code"),
        Description = c("Product", "Product_Description", "Product_Service"),
        Category = c("Product_Category"),
        Preferred_Supplier = c("Supplier", "Preferred_Vendor"),
        Base_Unit_Cost = c("Unit_Cost", "Cost", "Last_Quoted_Unit_Cost"),
        Unit = c("UOM", "Unit_Of_Measure")
      ),
      defaults = list(Base_Unit_Cost = 0, Unit = "EA"),
      dataset_name = "Product Master"
    ) %>%
    mutate(
      Part_Number = safe_character(Part_Number),
      Description = safe_character(Description),
      Category = safe_character(Category),
      Preferred_Supplier = safe_character(Preferred_Supplier),
      Unit = safe_character(Unit),
      Base_Unit_Cost = to_num(Base_Unit_Cost)
    )

  suppliers_data <- read_demo_csv("Aurelis_Suppliers.csv") %>%
    adapt_schema(
      aliases = list(
        Supplier_ID = c("Vendor_ID", "Seller_ID"),
        Supplier = c("Vendor", "Seller", "Supplier_Name"),
        Country = c("Supplier_Country", "Nation"),
        Reliability_Index = c("Reliability", "Reliability_Score"),
        Active_Since = c("Start_Year", "Founded_Year")
      ),
      defaults = list(Reliability_Index = NA_real_, Active_Since = NA_real_),
      dataset_name = "Supplier Master"
    ) %>%
    mutate(
      Supplier_ID = safe_character(Supplier_ID),
      Supplier = safe_character(Supplier),
      Country = safe_character(Country),
      Reliability_Index = to_num(Reliability_Index),
      Active_Since = to_num(Active_Since)
    )
  
  
  geography_data <- read_demo_csv("Aurelis_Geography.csv") %>%
    adapt_schema(
      aliases = list(
        Entity_Type = c("Type", "Relationship_Type"),
        Entity_ID = c("ID", "Customer_ID", "Supplier_ID", "Representative_ID"),
        Entity = c("Company", "Customer", "Supplier", "Representative", "Name"),
        Continent = c("Global_Region", "World_Region"),
        Country = c("Nation"),
        Region = c("State", "Province", "Area"),
        City = c("Town", "Municipality"),
        Latitude = c("Lat"),
        Longitude = c("Lon", "Lng", "Long"),
        Representative = c("Account_Manager", "Sales_Representative"),
        Location_Source = c("Source")
      ),
      defaults = list(
        Entity_Type = "Unknown",
        Entity_ID = "",
        Entity = "",
        Continent = "Global",
        Country = "Unknown",
        Region = "",
        City = "",
        Latitude = 0,
        Longitude = 0,
        Representative = "",
        Location_Source = "Imported geography"
      ),
      dataset_name = "Geography"
    ) %>%
    mutate(
      Entity_Type = safe_character(Entity_Type),
      Entity_ID = safe_character(Entity_ID),
      Entity = safe_character(Entity),
      Continent = safe_character(Continent),
      Country = safe_character(Country),
      Region = safe_character(Region),
      City = safe_character(City),
      Representative = {
        representative <- safe_character(Representative)
        missing_representative <- !nzchar(representative)
        if (any(missing_representative)) {
          data_diag(paste0(
            "Geography: ", sum(missing_representative),
            " record(s) had no assigned representative; labeled 'Unassigned' rather than inventing an owner."
          ))
        }
        ifelse(missing_representative, "Unassigned", representative)
      },
      Location_Source = safe_character(Location_Source),
      Latitude = safe_numeric(Latitude, 0),
      Longitude = safe_numeric(Longitude, 0)
    ) %>%
    filter(
      Entity != "",
      is.finite(Latitude),
      is.finite(Longitude),
      Latitude >= -90,
      Latitude <= 90,
      Longitude >= -180,
      Longitude <= 180
    )
  
  quotation_customer_reference <- read_demo_csv("Aurelis_Customer_Reference.csv") %>%
    adapt_schema(
      aliases = list(
        Customer_Code = c("Customer_ID", "Client_ID", "Account_Number"),
        Company = c("Customer", "Client", "Billing_Name", "Company_Name"),
        Terms = c("Payment_Terms"),
        Payment_Min_Months = c("Fast_Payment_Months", "Min_Payment_Months"),
        Payment_Average_Months = c("Average_Payment_Months", "Avg_Payment_Months"),
        Payment_Max_Months = c("Slow_Payment_Months", "Max_Payment_Months")
      ),
      defaults = list(
        Terms = "NET 30 DAYS",
        Payment_Min_Months = 1,
        Payment_Average_Months = 1.5,
        Payment_Max_Months = 3
      ),
      dataset_name = "Customer Reference"
    ) %>%
    mutate(
      Customer_Code = safe_character(Customer_Code),
      Company = safe_character(Company),
      Terms = safe_character(Terms),
      across(c(Payment_Min_Months,Payment_Average_Months,Payment_Max_Months),to_num)
    )
  
  # Locally registered demo clients are stored only in this public package.
  # They never touch production Excel, Access or QuickBooks sources.
  registered_client_file <- file.path(AURELIS_DATA_DIR,"Aurelis_User_Clients.csv")
  registered_clients <- if (file.exists(registered_client_file)) {
    tryCatch(read_demo_csv("Aurelis_User_Clients.csv"), error = function(e) data.frame())
  } else data.frame()
  
  if (nrow(registered_clients) > 0) {
    registered_clients <- registered_clients %>%
      adapt_schema(
        aliases = list(
          Customer_Code = c("Customer_ID", "Client_ID", "Account_Number"),
          Company = c("Customer", "Client", "Billing_Name", "Company_Name"),
          Country = c("Customer_Country"),
          Region = c("State", "Province", "Area"),
          City = c("Town", "Municipality"),
          Parent_Company = c("Parent", "ParentCompany"),
          Terms = c("Payment_Terms"),
          Payment_Min_Months = c("Fast_Payment_Months", "Min_Payment_Months"),
          Payment_Average_Months = c("Average_Payment_Months", "Avg_Payment_Months"),
          Payment_Max_Months = c("Slow_Payment_Months", "Max_Payment_Months"),
          Contact_Name = c("Contact", "Name"),
          Email = c("Email_Address"),
          Phone = c("Telephone", "Phone_Number"),
          Address = c("Billing_Address", "Office_Address"),
          Department = c("Dept"),
          Position = c("Title", "Job_Title"),
          AURELIS_Rep = c("Representative", "Account_Manager"),
          Registered_At = c("Created_At", "Registration_Date")
        ),
        defaults = list(
          Country = "",
          Region = "",
          City = "",
          Parent_Company = "Independent Entity",
          Terms = "NET 30 DAYS",
          Payment_Min_Months = 1,
          Payment_Average_Months = 1.5,
          Payment_Max_Months = 3,
          Contact_Name = "",
          Email = "",
          Phone = "",
          Address = "",
          Department = "",
          Position = "",
          AURELIS_Rep = "",
          Registered_At = ""
        ),
        dataset_name = "Locally Registered Clients"
      ) %>%
      mutate(
        Customer_Code = safe_character(Customer_Code),
        Company = safe_character(Company),
        Country = safe_character(Country),
        Region = safe_character(Region),
        City = safe_character(City),
        Terms = safe_character(Terms),
        across(c(Payment_Min_Months,Payment_Average_Months,Payment_Max_Months),to_num)
      )
    
    registered_directory <- registered_clients %>%
      transmute(
        Contact_Type = "Customer",
        Entity = Company,
        Contact_Name = Contact_Name,
        Country = Country,
        Function = "Customer",
        Department = Department,
        Position = Position,
        Email = Email,
        Phone = Phone,
        Address = Address,
        AURELIS_Rep = AURELIS_Rep,
        Directory_Source = "Locally registered public-demo client"
      )
    
    registered_reference <- registered_clients %>%
      transmute(
        Customer_Code = as.character(Customer_Code),
        Company = Company,
        Terms = Terms,
        Payment_Min_Months = Payment_Min_Months,
        Payment_Average_Months = Payment_Average_Months,
        Payment_Max_Months = Payment_Max_Months
      )
    
    relationship_directory <- bind_rows(relationship_directory, registered_directory) %>%
      distinct(Contact_Type, Entity, Contact_Name, Email, .keep_all = TRUE)
    
    quotation_customer_reference <- bind_rows(
      quotation_customer_reference,
      registered_reference
    ) %>%
      distinct(Customer_Code, .keep_all = TRUE)
  }
  
  quotation_terms_reference <- read_demo_csv("Aurelis_Terms_Reference.csv") %>%
    adapt_schema(
      aliases = list(
        Terms = c("Payment_Terms"),
        Advance_Pct = c("Advance", "Advance_Percent"),
        Day30_Pct = c("Day_30", "D30_Pct"),
        Day60_Pct = c("Day_60", "D60_Pct"),
        Day90_Pct = c("Day_90", "D90_Pct"),
        Day120_Pct = c("Day_120", "D120_Pct"),
        Day150_Pct = c("Day_150", "D150_Pct")
      ),
      defaults = list(
        Advance_Pct = 0,
        Day30_Pct = 0,
        Day60_Pct = 0,
        Day90_Pct = 0,
        Day120_Pct = 0,
        Day150_Pct = 0
      ),
      dataset_name = "Payment Terms Reference"
    ) %>%
    mutate(
      Terms = safe_character(Terms),
      across(c(Advance_Pct,Day30_Pct,Day60_Pct,Day90_Pct,Day120_Pct,Day150_Pct),to_num)
    )
  
  # --------------------------------------------------------------------------
  # Runtime package integrity checks
  # These checks protect the dashboard from silent cross-file drift. Critical
  # empty datasets stop the load; non-critical mismatches are surfaced in the
  # Data & Process diagnostics instead of causing unexplained downstream errors.
  # --------------------------------------------------------------------------
  
  require_nonempty_dataset <- function(df, dataset_name) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      stop(
        paste0(
          "The required dataset '", dataset_name,
          "' is empty. Restore the public-demo CSV before running the dashboard."
        ),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }
  
  require_nonempty_dataset(daily_po, "Daily PO")
  require_nonempty_dataset(revenue_data, "Revenue")
  require_nonempty_dataset(ar_data, "Accounts Receivable")
  require_nonempty_dataset(inventory_data, "Inventory / Delivery")
  require_nonempty_dataset(quotation_customer_reference, "Customer Reference")
  require_nonempty_dataset(geography_data, "Geography")
  require_nonempty_dataset(products_data, "Product Master")
  require_nonempty_dataset(suppliers_data, "Supplier Master")

  catalog_product_gaps <- setdiff(products_data$Part_Number, seller_catalog_data$Product_Code)
  if (length(catalog_product_gaps) > 0) {
    data_diag(paste0("Product Master: ", length(catalog_product_gaps), " product(s) have no seller-catalog record."))
  }
  catalog_supplier_gaps <- setdiff(seller_catalog_data$Supplier, suppliers_data$Supplier)
  if (length(catalog_supplier_gaps) > 0) {
    data_diag(paste0("Supplier Master: ", length(catalog_supplier_gaps), " catalog supplier(s) have no supplier-master record."))
  }
  
  valid_po_numbers <- unique(daily_po$PO_Number[daily_po$PO_Number != ""])
  
  inventory_orphans <- inventory_data %>%
    filter(
      PO_Number != "",
      !PO_Number %in% valid_po_numbers
    ) %>%
    distinct(PO_Number)
  
  if (nrow(inventory_orphans) > 0) {
    data_diag(
      paste0(
        "Inventory / Delivery: ",
        format(nrow(inventory_orphans), big.mark = ","),
        " PO reference(s) do not exist in Daily PO."
      )
    )
  }
  
  revenue_orphans <- revenue_data %>%
    filter(
      PO_Number != "",
      !PO_Number %in% valid_po_numbers
    ) %>%
    distinct(PO_Number)
  
  if (nrow(revenue_orphans) > 0) {
    data_diag(
      paste0(
        "Revenue: ",
        format(nrow(revenue_orphans), big.mark = ","),
        " PO reference(s) do not exist in Daily PO."
      )
    )
  }
  
  ar_orphans <- ar_data %>%
    filter(
      PO_Number != "",
      !PO_Number %in% valid_po_numbers
    ) %>%
    distinct(PO_Number)
  
  if (nrow(ar_orphans) > 0) {
    data_diag(
      paste0(
        "Accounts Receivable: ",
        format(nrow(ar_orphans), big.mark = ","),
        " PO reference(s) do not exist in Daily PO."
      )
    )
  }
  
  duplicate_customer_ids <- quotation_customer_reference %>%
    filter(Customer_Code != "") %>%
    count(Customer_Code, name = "Rows") %>%
    filter(Rows > 1)
  
  if (nrow(duplicate_customer_ids) > 0) {
    data_diag(
      paste0(
        "Customer Reference: ",
        nrow(duplicate_customer_ids),
        " duplicate customer ID(s) were found. The first matching profile is used by quotation lookup."
      )
    )
  }
  
  geography_duplicate_ids <- geography_data %>%
    filter(Entity_ID != "") %>%
    count(Entity_Type, Entity_ID, name = "Rows") %>%
    filter(Rows > 1)
  
  if (nrow(geography_duplicate_ids) > 0) {
    data_diag(
      paste0(
        "Geography: ",
        nrow(geography_duplicate_ids),
        " duplicate entity ID/location combination(s) were found."
      )
    )
  }
  
  invalid_revenue_rows <- revenue_data %>%
    filter(
      !is.finite(Revenue) |
        !is.finite(Cost) |
        !is.finite(Gross_Profit)
    )
  
  if (nrow(invalid_revenue_rows) > 0) {
    data_diag(
      paste0(
        "Revenue: ",
        format(nrow(invalid_revenue_rows), big.mark = ","),
        " row(s) contain non-finite financial values after sanitization."
      )
    )
  }
  
  missing_order_dates <- sum(is.na(daily_po$Date))
  if (missing_order_dates > 0) {
    data_diag(
      paste0(
        "Daily PO: ",
        format(missing_order_dates, big.mark = ","),
        " order date(s) could not be parsed."
      )
    )
  }
  
  missing_delivery_dates <- sum(
    is.na(inventory_data$Delivery_Date) &
      inventory_data$Line_Type != "Information"
  )
  if (missing_delivery_dates > 0) {
    data_diag(
      paste0(
        "Inventory / Delivery: ",
        format(missing_delivery_dates, big.mark = ","),
        " product line(s) do not have a valid planned delivery date."
      )
    )
  }
  
  mapped_customer_entities <- geography_data %>%
    filter(Entity_Type == "Customer") %>%
    pull(Entity) %>%
    unique()
  
  customer_location_gaps <- setdiff(
    unique(quotation_customer_reference$Company[quotation_customer_reference$Company != ""]),
    mapped_customer_entities
  )
  
  if (length(customer_location_gaps) > 0) {
    data_diag(
      paste0(
        "Geography: ",
        length(customer_location_gaps),
        " customer reference(s) currently have no Atlas location."
      )
    )
  }
  
  all_years <- sort(unique(na.omit(c(daily_po$Year,revenue_data$Year,ar_data$Year,inventory_data$Year))))
  all_buyers <- sort(unique(na.omit(daily_po$Buyer)))
  all_clients_po <- sort(unique(na.omit(daily_po$Client)))
  all_staff_rev <- sort(unique(na.omit(revenue_data$Staff)))
  all_countries <- sort(unique(na.omit(revenue_data$Country)))
  all_parents <- sort(unique(na.omit(revenue_data$Parent_Company)))
  all_clients_ar <- sort(unique(na.omit(ar_data$Customer)))
  all_inventory_clients <- sort(unique(na.omit(inventory_data$Client)))
  all_inventory_buyers <- sort(unique(na.omit(inventory_data$Buyer)))
  all_inventory_suppliers <- sort(unique(na.omit(inventory_data$Supplier)))
  all_inventory_statuses <- levels(inventory_data$Order_Status)
  all_delivery_years <- sort(unique(na.omit(inventory_data$Delivery_Year)))
  warehouse_inventory_available <- nrow(warehouse_inventory_data)>0
  customer_performance_clients <- sort(unique(na.omit(c(daily_po$Client,revenue_data$Customer,ar_data$Customer,inventory_data$Client))))
  relationship_order_history <- inventory_data
  
  customer_portfolio <- full_join(
    daily_po %>% group_by(Customer=Client) %>% summarise(
      First_PO_Date=min(Date,na.rm=TRUE),Last_PO_Date=max(Date,na.rm=TRUE),
      PO_Count=n_distinct(PO_Number),PO_Value=sum(Total_PO,na.rm=TRUE),
      Average_PO_Value=mean(Total_PO,na.rm=TRUE),Buyer_Count=n_distinct(Buyer),.groups="drop"),
    revenue_data %>% group_by(Customer) %>% summarise(
      First_Revenue_Date=min(Date,na.rm=TRUE),Last_Revenue_Date=max(Date,na.rm=TRUE),
      Revenue=sum(Revenue,na.rm=TRUE),Gross_Profit=sum(Gross_Profit,na.rm=TRUE),
      Invoice_Count=n(),.groups="drop"),by="Customer") %>%
    full_join(
      inventory_data %>% filter(Line_Type!="Information") %>% group_by(Customer=Client) %>% summarise(
        Ordered_Quantity=sum(Quantity,na.rm=TRUE),Received_Quantity=sum(Received_Qty,na.rm=TRUE),
        Backordered_Quantity=sum(Backordered_Qty,na.rm=TRUE),
        Average_Execution_Days=mean(as.numeric(difftime(Delivery_Date,Order_Date,units="days")),na.rm=TRUE),
        Supplier_Count=n_distinct(Supplier),.groups="drop"),by="Customer") %>%
    full_join(
      ar_data %>% group_by(Customer) %>% summarise(
        Total_Invoiced_AR=sum(Invoice_Amount,na.rm=TRUE),
        Outstanding_AR=sum(pmax(Balance_Remaining,0),na.rm=TRUE),.groups="drop"),by="Customer") %>%
    mutate(
      Completion_Rate=if_else(Ordered_Quantity>0,pmin(Received_Quantity/Ordered_Quantity,1),NA_real_),
      Backorder_Rate=if_else(Ordered_Quantity>0,pmax(Backordered_Quantity/Ordered_Quantity,0),NA_real_),
      Gross_Margin=if_else(Revenue>0,Gross_Profit/Revenue,NA_real_),
      Paid_Rate=if_else(Total_Invoiced_AR>0,pmax(0,pmin(1,1-Outstanding_AR/Total_Invoiced_AR)),NA_real_),
      Revenue_Rank=min_rank(desc(coalesce(Revenue,0))),
      PO_Value_Rank=min_rank(desc(coalesce(PO_Value,0))),
      Relationship_Score=coalesce(percent_rank(coalesce(Revenue,0)+coalesce(PO_Value,0)),1)
    ) %>% arrange(desc(Revenue),desc(PO_Value))
  
  list2env(list(
    daily_po=daily_po,revenue_data=revenue_data,ar_data=ar_data,inventory_data=inventory_data,
    warehouse_inventory_data=warehouse_inventory_data,contacts_data=contacts_data,
    relationship_directory=relationship_directory,seller_catalog_data=seller_catalog_data,
    products_data=products_data,suppliers_data=suppliers_data,
    geography_data=geography_data,
    quotation_customer_reference=quotation_customer_reference,
    quotation_terms_reference=quotation_terms_reference,all_years=all_years,all_buyers=all_buyers,
    all_clients_po=all_clients_po,all_staff_rev=all_staff_rev,all_countries=all_countries,all_parents=all_parents,
    all_clients_ar=all_clients_ar,all_inventory_clients=all_inventory_clients,
    all_inventory_buyers=all_inventory_buyers,all_inventory_suppliers=all_inventory_suppliers,
    all_inventory_statuses=all_inventory_statuses,all_delivery_years=all_delivery_years,
    warehouse_inventory_available=warehouse_inventory_available,customer_performance_clients=customer_performance_clients,
    relationship_order_history=relationship_order_history,customer_portfolio=customer_portfolio
  ),envir=target_env)
  AURELIS_DEMO_LOADED_AT <<- Sys.time()
  invisible(TRUE)
}

source_registry_table <- function() tibble(
  Dataset=c("DAILY_PO","REVENUE","AR","INVENTORY","WAREHOUSE_INVENTORY","CONTACTS","DIRECTORY","SELLER_CATALOG","PRODUCTS","SUPPLIERS","CUSTOMER_REFERENCE","TERMS_REFERENCE","GEOGRAPHY","USER_CLIENTS"),
  Source=rep("Synthetic Demo CSV Package",14),
  Detail=c(
    "Aurelis_Daily_PO.csv · fictional purchase orders",
    "Aurelis_Revenue.csv · fictional accounting revenue",
    "Aurelis_Accounts_Receivable.csv · fictional invoice aging",
    "Aurelis_Inventory.csv · fictional order and delivery lines",
    "Aurelis_Warehouse.csv · fictional warehouse stock",
    "Aurelis_Contacts.csv · fictional customer contacts",
    "Aurelis_Directory.csv · fictional customer / buyer / supplier relationships",
    "Aurelis_Seller_Catalog.csv · searchable seller product and service catalog",
    "Aurelis_Products.csv · synthetic product master and base commercial attributes",
    "Aurelis_Suppliers.csv · synthetic supplier reference and procurement coverage",
    "Aurelis_Customer_Reference.csv · quotation and client reference choices",
    "Aurelis_Terms_Reference.csv · quotation payment and commercial terms",
    "Aurelis_Geography.csv · customer / supplier / representative operating locations",
    "Aurelis_User_Clients.csv · locally registered public-demo clients only"
  ),
  Loaded_At=rep(AURELIS_DEMO_LOADED_AT,14)
)

list_access_tables <- function() c("Daily_PO","Revenue","Accounts_Receivable","Inventory","Warehouse","Contacts","Directory","Seller_Catalog","Products","Suppliers","Customer_Reference","Terms_Reference","Geography","User_Clients")
list_quickbooks_tables <- function() character(0)
qb_sdk_state <- function() list(last_success_utc=NA_character_)
qb_sdk_authorize <- function() stop("External accounting connections are disabled in the public demo.",call.=FALSE)

load_aurelis_demo_data(AURELIS_APP_ENV)
quotation_customer_choices <- sort(unique(na.omit(c(customer_performance_clients,quotation_customer_reference$Company))))
quotation_po_choices <- sort(unique(na.omit(inventory_data$PO_Number)))
quotation_supplier_choices <- sort(unique(na.omit(inventory_data$Supplier)))

aurelis_stats_dataset_choices <- c(
  "Revenue & profitability" = "revenue",
  "Accounts receivable" = "ar",
  "Purchase orders" = "orders",
  "Inventory" = "inventory"
)

aurelis_stats_source_frame <- function(source) {
  switch(
    as.character(source),
    revenue = revenue_data,
    ar = ar_data,
    orders = daily_po,
    inventory = inventory_data,
    stop("Choose a supported financial-statistics dataset.", call. = FALSE)
  )
}

aurelis_stats_numeric_fields <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0) return(character(0))
  numeric_field <- vapply(data, function(column) {
    is.numeric(column) && any(is.finite(column))
  }, logical(1))
  names(data)[numeric_field]
}

aurelis_stats_group_fields <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0) return(character(0))
  group_field <- vapply(data, function(column) {
    (is.character(column) || is.factor(column)) &&
      dplyr::n_distinct(column, na.rm = TRUE) > 1 &&
      dplyr::n_distinct(column, na.rm = TRUE) <= 50
  }, logical(1))
  names(data)[group_field]
}

aurelis_stats_prepare <- function(
    data,
    measure,
    group_by = "",
    groups = character(0),
    missing_policy = "exclude"
) {
  if (!is.data.frame(data) ||
      length(measure) != 1 ||
      is.na(measure) ||
      !measure %in% names(data)) {
    stop("Select a numeric measure available in the chosen dataset.", call. = FALSE)
  }
  if (!is.numeric(data[[measure]])) {
    stop("The selected financial-statistics measure is not numeric.", call. = FALSE)
  }
  if (length(missing_policy) != 1 || is.na(missing_policy) ||
      !missing_policy %in% c("exclude", "median")) {
    stop("Choose a supported missing-value policy.", call. = FALSE)
  }
  if (!is.null(group_by) && nzchar(group_by) && !group_by %in% names(data)) {
    stop("The selected comparison field is not available in this dataset.", call. = FALSE)
  }

  values <- as.numeric(data[[measure]])
  finite <- is.finite(values)
  group_values <- if (is.null(group_by) || !nzchar(group_by)) {
    rep("All records", length(values))
  } else {
    as.character(data[[group_by]])
  }
  group_values[is.na(group_values) | !nzchar(trimws(group_values))] <- "Unspecified"
  selected <- if (length(groups) > 0 && !is.null(group_by) && nzchar(group_by)) {
    group_values %in% as.character(groups)
  } else {
    rep(TRUE, length(values))
  }

  missing_count <- sum(selected & !finite)
  imputed_count <- 0L
  if (identical(missing_policy, "median") && missing_count > 0L && any(selected & finite)) {
    fill_value <- stats::median(values[selected & finite])
    values[selected & !finite] <- fill_value
    finite <- is.finite(values)
    imputed_count <- missing_count
  }

  keep <- selected & finite
  result <- tibble::tibble(
    value = values[keep],
    group = factor(group_values[keep], levels = unique(group_values[keep]))
  )
  attr(result, "missing_count") <- as.integer(missing_count)
  attr(result, "imputed_count") <- as.integer(imputed_count)
  attr(result, "excluded_count") <- as.integer(missing_count - imputed_count)
  result
}

aurelis_stats_distribution_fit <- function(values, distribution, degrees_freedom = 5) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) stop("At least two usable observations are required.", call. = FALSE)
  if (!distribution %in% c("Normal", "Student t", "Log-normal", "Exponential", "Gamma", "Poisson", "Empirical")) {
    stop("Choose a supported statistical distribution.", call. = FALSE)
  }

  location <- mean(values)
  spread <- stats::sd(values)
  if (distribution %in% c("Normal", "Student t") && (!is.finite(spread) || spread <= 0)) {
    stop("A fitted continuous distribution requires values with non-zero variation.", call. = FALSE)
  }

  if (distribution == "Normal") {
    return(list(
      discrete = FALSE,
      density = function(x) stats::dnorm(x, mean = location, sd = spread),
      quantile = function(p) stats::qnorm(p, mean = location, sd = spread)
    ))
  }
  if (distribution == "Student t") {
    df_input <- suppressWarnings(as.numeric(degrees_freedom))
    df <- if (length(df_input) == 0 || !is.finite(df_input[[1]])) 5 else df_input[[1]]
    df <- max(2.01, df)
    if (!is.finite(df)) df <- 5
    scale <- spread * sqrt((df - 2) / df)
    return(list(
      discrete = FALSE,
      density = function(x) stats::dt((x - location) / scale, df = df) / scale,
      quantile = function(p) location + scale * stats::qt(p, df = df)
    ))
  }
  if (distribution == "Log-normal") {
    if (any(values <= 0)) stop("Log-normal fitting requires strictly positive observations.", call. = FALSE)
    log_values <- log(values)
    log_mean <- mean(log_values)
    log_sd <- stats::sd(log_values)
    if (!is.finite(log_sd) || log_sd <= 0) stop("Log-normal fitting requires variation in positive observations.", call. = FALSE)
    return(list(
      discrete = FALSE,
      density = function(x) stats::dlnorm(x, meanlog = log_mean, sdlog = log_sd),
      quantile = function(p) stats::qlnorm(p, meanlog = log_mean, sdlog = log_sd)
    ))
  }
  if (distribution == "Exponential") {
    if (any(values < 0) || location <= 0) stop("Exponential fitting requires non-negative observations with a positive mean.", call. = FALSE)
    rate <- 1 / location
    return(list(
      discrete = FALSE,
      density = function(x) stats::dexp(x, rate = rate),
      quantile = function(p) stats::qexp(p, rate = rate)
    ))
  }
  if (distribution == "Gamma") {
    variance <- stats::var(values)
    if (any(values <= 0) || !is.finite(variance) || variance <= 0) {
      stop("Gamma fitting requires strictly positive observations with non-zero variation.", call. = FALSE)
    }
    shape <- location ^ 2 / variance
    rate <- location / variance
    return(list(
      discrete = FALSE,
      density = function(x) stats::dgamma(x, shape = shape, rate = rate),
      quantile = function(p) stats::qgamma(p, shape = shape, rate = rate)
    ))
  }
  if (distribution == "Poisson") {
    if (any(values < 0) || any(abs(values - round(values)) > 1e-8) || location <= 0) {
      stop("Poisson fitting requires non-negative whole-number observations with a positive mean.", call. = FALSE)
    }
    lambda <- location
    return(list(
      discrete = TRUE,
      density = function(x) stats::dpois(round(x), lambda = lambda),
      quantile = function(p) stats::qpois(p, lambda = lambda)
    ))
  }

  list(discrete = FALSE, density = NULL, quantile = NULL)
}

################################################################################
# SECTION 3: UI DEFINITION
################################################################################

ui <- bs4DashPage(
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # --- 3.1 Header ---
  header = bs4DashNavbar(
    title = dashboardBrand(
      title = "Aurelis",
      color = "primary",
      image = aurelis_logo_header_source
    ),
    fixed = TRUE,
    
    rightUi = tagList(
      
      # ========================================================================
      # YEAR
      # ========================================================================
      tags$li(
        class = "nav-item dropdown d-none d-lg-flex align-items-center prism-navbar-filter prism-year-filter",
        
        selectInput(
          "global_year",
          NULL,
          
          choices = c(
            "All Years" = "all",
            all_years
          ),
          
          selected = "all",
          width = "128px"
        )
      ),
      
      
      # ========================================================================
      # MONTH
      # ========================================================================
      tags$li(
        class = "nav-item dropdown d-none d-lg-flex align-items-center prism-navbar-filter prism-month-filter",
        
        selectInput(
          "global_month",
          NULL,
          
          choices = c(
            "All Months" = "all",
            setNames(
              1:12,
              month.name
            )
          ),
          
          selected = "all",
          width = "142px"
        )
      ),
      
      
      # ========================================================================
      # CENTER AURELIS LOGO
      # ========================================================================
      tags$li(
        class = "nav-item dropdown d-none d-lg-flex align-items-center header-logo-lockup aurelis-center-header-logo",
        
        tags$img(
          src = aurelis_logo_header_source,
          alt = "Aurelis Global Supply"
        ),
        
        # Keep this existing Shiny output alive.
        # CSS will hide the text but NOT remove the output.
        tags$span(
          textOutput(
            "obsidian_header_page",
            inline = TRUE
          )
        )
      ),
      
      
      # ========================================================================
      # REFRESH
      # ========================================================================
      tags$li(
        class = "nav-item dropdown d-flex align-items-center future-control-item future-control-item-first",
        
        actionLink(
          inputId = "refresh_live_data",
          label = "Refresh",
          icon = icon("sync-alt"),
          
          class = "nav-link future-control-button",
          
          title = "Refresh dashboard data now"
        )
      ),
      
      
      # ========================================================================
      # REPORT / EXPORT
      # ========================================================================
      tags$li(
        class = "nav-item dropdown d-flex align-items-center navbar-report-item",
        
        actionLink(
          inputId = "report_current_page",
          label = "Report / Export",
          icon = icon("file-export"),
          
          class = "nav-link navbar-report-button"
        )
      ),
      
      
      # ========================================================================
      # INFORMATION
      # ========================================================================
      dropdownMenu(
        type = "notifications",
        icon = icon("info-circle"),
        badgeStatus = "info",
        
        notificationItem(
          text = paste0(
            "Data updated: ",
            format(
              Sys.Date(),
              "%B %d, %Y"
            )
          ),
          
          icon = icon("calendar"),
          status = "info"
        )
      )
      
    )
  ),
  
  # ========================================================================
  # LEAVE YOUR COMPACT / REFRESH / REPORT / OTHER CONTROLS BELOW THIS POINT
  # ========================================================================
  
  # --- Sidebar Navigation ---
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "navy",
    elevation = 0,
    fixed = TRUE,
    width = 164,
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = FALSE,
    bs4SidebarMenu(
      id = "sidebar_tabs",
      bs4SidebarMenuItem(
        "Command Center", icon = icon("bullseye"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("Executive Overview", tabName = "exec_overview", icon = icon("tachometer-alt")),
        bs4SidebarMenuSubItem("KPI Command Center", tabName = "kpi_board", icon = icon("th-large")),
        bs4SidebarMenuSubItem("Global Network Atlas", tabName = "global_network", icon = icon("globe-americas")),
        bs4SidebarMenuSubItem("Executive Activity Observatory", tabName = "executive_observatory", icon = icon("eye"))
      ),
      bs4SidebarMenuItem(
        "Commercial Intelligence", icon = icon("chart-line"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("Sales & Profitability", tabName = "sale_performance", icon = icon("chart-line")),
        bs4SidebarMenuSubItem("Customer Performance", tabName = "customer_performance", icon = icon("award")),
        bs4SidebarMenuSubItem("Client Statements", tabName = "client_statement", icon = icon("user-tie")),
        bs4SidebarMenuSubItem("POs & Buyers", tabName = "monthly_sales", icon = icon("shopping-cart")),
        bs4SidebarMenuSubItem("Buyer Activity", tabName = "buyer_activity", icon = icon("user-clock")),
        bs4SidebarMenuSubItem("Financial Statistics", tabName = "financial_statistics", icon = icon("chart-area"))
      ),
      bs4SidebarMenuItem(
        "Supply Chain Control", icon = icon("project-diagram"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("Orders & Delivery", tabName = "order_tracker", icon = icon("shipping-fast")),
        bs4SidebarMenuSubItem("Inventory & Warehouse", tabName = "inventory_tab", icon = icon("warehouse")),
        bs4SidebarMenuSubItem("Suppliers & Procurement", tabName = "buyer_tool", icon = icon("handshake")),
        bs4SidebarMenuSubItem("Accounts Receivable", tabName = "ar_tab", icon = icon("file-invoice-dollar"))
      ),
      bs4SidebarMenuItem(
        "Workspace & Studio", icon = icon("magic"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("My Workspace", tabName = "multi_user_workspace", icon = icon("users-cog")),
        bs4SidebarMenuSubItem("Quotation Studio", tabName = "quotation_studio", icon = icon("file-signature")),
        bs4SidebarMenuSubItem("Product Intelligence", tabName = "product_intelligence", icon = icon("gem"))
      ),
      bs4SidebarMenuItem(
        "Data & Relationships", icon = icon("database"), startExpanded = TRUE,
        bs4SidebarMenuSubItem("Directory & Relationships", tabName = "contacts_tab", icon = icon("address-book")),
        bs4SidebarMenuSubItem("Demo Data & Process", tabName = "misc_tab", icon = icon("database"))
      )
    )
  ),
  
  # --- Body ---
  body = bs4DashBody(
    tags$head(
      tags$title("Aurelis Obsidian Command — Business Operations Intelligence"),
      tags$meta(name = "description", content = "Aurelis Obsidian Command dark interactive business operations intelligence platform"),
      tags$style(HTML(custom_css)),
      tags$script(HTML(responsive_dashboard_js)),
      tags$script(HTML(dropdown_layer_js)),
      tags$script(HTML(future_ui_js)),
      tags$script(HTML(signature_ui_js))
    ),
    
    div(
      class = "aurelis-content-watermark",
      tags$img(src = aurelis_logo_header_source, alt = "")
    ),

    uiOutput("aurelis_access_portal"),
    
    div(
      class = "prism-mobile-filter-row d-lg-none",
      selectInput(
        "global_year_mobile", "Year",
        choices = c("All Years" = "all", all_years),
        selected = "all", width = "100%"
      ),
      selectInput(
        "global_month_mobile", "Month",
        choices = c("All Months" = "all", setNames(1:12, month.name)),
        selected = "all", width = "100%"
      )
    ),
    
    
    bs4TabItems(
      
      ########################################################################
      # 3.1 MULTI-USER WORKSPACE
      ########################################################################
      bs4TabItem(
        tabName = "multi_user_workspace",
        fluidRow(
          bs4Card(
            title = "My Aurelis Workspace", width = 12,
            status = "primary", solidHeader = TRUE,
            div(
              class = "multiuser-banner",
              tags$strong("Public multi-workspace demo mode is active."),
              tags$br(),
              tags$span("Each browser has its own filters and quotation work-in-progress. Shared data refreshes are coordinated, while saved quotation drafts are stored centrally for authorized Aurelis buyers.")
            ),
            fluidRow(
              column(3, selectizeInput(
                "multi_user_buyer", "Current buyer / workspace owner:",
                choices = sort(unique(na.omit(all_buyers))),
                selected = if (length(all_buyers) > 0) all_buyers[[1]] else "",
                options = list(create = TRUE, placeholder = "Select or enter your name")
              )),
              column(3, selectInput(
                "multi_user_role", "Demo access profile:",
                choices = c("CEO / Executive" = "CEO", "Manager" = "Manager", "Buyer" = "Buyer", "Analyst" = "Analyst"),
                selected = "Analyst"
              )),
              column(3, checkboxInput("multi_user_include_shared", "Show shared drafts from other buyers", value = TRUE)),
              column(3, uiOutput("multi_user_session_status"))
            ),
            div(
              class = "multiuser-status-grid",
              div(class = "multiuser-status-card", tags$strong("Active Sessions"), textOutput("multi_user_active_count"), tags$small("Active on this central host during the recent session window.")),
              div(class = "multiuser-status-card", tags$strong("My Available Drafts"), textOutput("multi_user_draft_count"), tags$small("Private drafts plus shared drafts when enabled.")),
              div(class = "multiuser-status-card", tags$strong("Shared Data Refresh"), textOutput("multi_user_refresh_status"), tags$small("Automatic Access reloads are reused across concurrent sessions."))
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Quotation Draft Library", width = 7,
            status = "info", solidHeader = TRUE,
            selectizeInput("multi_user_draft_id", "Saved quotation draft:", choices = c("No saved drafts" = ""), selected = "", options = list(placeholder = "Choose a saved draft")),
            fluidRow(
              column(6, actionButton("multi_user_load_draft", "Open Draft in Quotation Studio", icon = icon("folder-open"), class = "btn-primary btn-block")),
              column(6, actionButton("multi_user_delete_draft", "Delete My Draft", icon = icon("trash"), class = "btn-outline-danger btn-block"))
            ),
            br(),
            DTOutput("multi_user_draft_table")
          ),
          bs4Card(
            title = "Concurrent Buyer Sessions", width = 5,
            status = "success", solidHeader = TRUE,
            DTOutput("multi_user_session_table"),
            tags$p(class = "metric-definition-note", "The buyer selector identifies the workspace owner for drafts and activity logs. It is not a security login. For external or internet-facing deployment, place the dashboard behind your company authentication / reverse proxy or Posit authentication layer.")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Recent Collaboration Activity", width = 12,
            status = "gray-dark", solidHeader = TRUE,
            DTOutput("multi_user_activity_table")
          )
        )
      ),
      
      ########################################################################
      # 3.2 EXECUTIVE ACTIVITY OBSERVATORY
      ########################################################################
      bs4TabItem(
        tabName = "executive_observatory",
        fluidRow(
          column(
            width = 12,
            div(
              class = "prism-masthead",
              div(
                class = "prism-masthead-copy",
                div(class = "prism-masthead-kicker", icon("eye"), "EXECUTIVE ACTIVITY OBSERVATORY"),
                tags$h2("See what is happening across the operating floor."),
                tags$p("A CEO-level view of past actions, live sessions, and future delivery commitments. Demo access is role-aware; production access should be connected to company authentication."),
                div(class = "prism-mode-pill", icon("lock"), tags$span(textOutput("access_profile_badge", inline = TRUE)))
              ),
              div(class = "prism-masthead-stats",
                  div(class = "prism-masthead-stat", tags$span("Live sessions"), tags$strong(textOutput("exec_live_sessions", inline = TRUE))),
                  div(class = "prism-masthead-stat", tags$span("Events logged"), tags$strong(textOutput("exec_logged_events", inline = TRUE))),
                  div(class = "prism-masthead-stat", tags$span("Future commitments"), tags$strong(textOutput("exec_future_commitments", inline = TRUE))))
            )
          )
        ),
        fluidRow(
          column(4, bs4Card(title = "Past activity", width = 12, status = "info", solidHeader = TRUE, DTOutput("exec_past_activity"))),
          column(4, bs4Card(title = "Live employee sessions", width = 12, status = "success", solidHeader = TRUE, DTOutput("exec_live_activity"))),
          column(4, bs4Card(title = "Future commitments", width = 12, status = "warning", solidHeader = TRUE, DTOutput("exec_future_activity")))
        ),
        fluidRow(
          column(7, bs4Card(title = "Activity by employee and role", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("exec_activity_by_role", height = "330px"))),
          column(5, bs4Card(title = "Access governance", width = 12, status = "secondary", solidHeader = TRUE, uiOutput("exec_access_governance")))
        )
      ),
      
      ########################################################################
      # 3.3 EXECUTIVE OVERVIEW TAB
      ########################################################################
      bs4TabItem(
        tabName = "exec_overview",
        
        # KPI Row
        fluidRow(
          column(3, div(class = "kpi-card kpi-blue",
                        tags$p("TOTAL REVENUE"), tags$h2(textOutput("kpi_total_revenue")), 
                        tags$p(class = "sub-text", "Filtered Period"))),
          column(3, div(class = "kpi-card kpi-teal",
                        tags$p("GROSS PROFIT"), tags$h2(textOutput("kpi_total_profit")), 
                        tags$p(class = "sub-text", "Filtered Period"))),
          column(3, div(class = "kpi-card kpi-coral",
                        tags$p("OUTSTANDING AR"), tags$h2(textOutput("kpi_total_ar")), 
                        tags$p(class = "sub-text", "Balance Remaining"))),
          column(3, div(class = "kpi-card kpi-navy",
                        tags$p("PURCHASE ORDERS"), tags$h2(textOutput("kpi_total_po")), 
                        tags$p(class = "sub-text", "Orders Tracked")))
        ),

        fluidRow(
          bs4Card(
            title = "AURELIS LIVE OPERATING POSTURE", width = 12,
            status = "primary", solidHeader = TRUE,
            div(
              class = "future-command-deck",
              div(class = "future-command-cell future-command-health",
                  tags$span(class = "future-command-kicker", icon("heartbeat"), "NOW"),
                  tags$strong(textOutput("future_now_signal")),
                  tags$p(textOutput("future_now_detail"))),
              div(class = "future-command-cell future-command-next",
                  tags$span(class = "future-command-kicker", icon("random"), "NEXT BEST MOVE"),
                  tags$strong(textOutput("future_next_move")),
                  tags$p(textOutput("future_next_detail")),
                  actionButton("future_open_next", "Open workspace", icon = icon("arrow-right"), class = "btn-primary btn-sm")),
              div(class = "future-command-cell future-command-risk",
                  tags$span(class = "future-command-kicker", icon("exclamation-triangle"), "WATCHLIST"),
                  tags$strong(textOutput("future_risk_signal")),
                  tags$p(textOutput("future_risk_detail")),
                  actionButton("future_open_risk", "Inspect risk", icon = icon("crosshairs"), class = "btn-outline-info btn-sm"))
            )
          )
        ),
        
        interaction_tip(
          "Click a top-customer bar to open that customer's performance profile. Click a sales-representative segment to open Sales & Profitability for that representative."
        ),
        
        # Revenue Trend full width
        fluidRow(
          bs4Card(title = "Monthly Revenue and Gross Profit", width = 12, 
                  status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("exec_revenue_trend", height = "350px") %>% withSpinner(color = "#2F6EA5"))
        ),
        
        # Top 10 Clients Bar + Profit Margin Donut (side by side)
        fluidRow(
          bs4Card(title = "Top 10 Clients by Revenue", width = 7, 
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("exec_top_clients", height = "350px") %>% withSpinner(color = "#9A6A27")),
          bs4Card(title = "Gross Profit Contribution by Sales Representative", width = 5, 
                  status = "success", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("exec_profit_donut", height = "350px") %>% withSpinner(color = "#3C7474"))
        ),
        
        # Country Treemap full width
        fluidRow(
          bs4Card(title = "Revenue by Country", width = 12, 
                  status = "info", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("exec_country_treemap", height = "380px") %>% withSpinner(color = "#6FA8D6"))
        ),
        
        # Year-over-Year comparison at bottom
        fluidRow(
          bs4Card(title = "Year-over-Year Revenue Comparison", width = 12, 
                  status = "secondary", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("exec_yoy_comparison", height = "320px") %>% withSpinner(color = "#607D8B"),
                  report_action_bar("report_exec", "Build an executive report from the current global filters."))
        )
      ),
      
      
      ########################################################################
      # 3.3 GLOBAL NETWORK ATLAS
      ########################################################################
      bs4TabItem(
        tabName = "global_network",
        
        fluidRow(
          column(
            3,
            div(
              class = "kpi-card",
              style = "--kpi-accent:#8067F2;",
              tags$p("VISIBLE LOCATIONS"),
              tags$h2(textOutput("atlas_kpi_locations")),
              tags$p(class = "sub-text", "Mapped customer, supplier and representative locations")
            )
          ),
          column(
            3,
            div(
              class = "kpi-card",
              style = "--kpi-accent:#268BFF;",
              tags$p("COUNTRIES"),
              tags$h2(textOutput("atlas_kpi_countries")),
              tags$p(class = "sub-text", "Countries represented after filters")
            )
          ),
          column(
            3,
            div(
              class = "kpi-card",
              style = "--kpi-accent:#24CED2;",
              tags$p("CURRENT ACTIVITY"),
              tags$h2(textOutput("atlas_kpi_activity")),
              tags$p(class = "sub-text", "Revenue / sourcing value represented on the map")
            )
          ),
          column(
            3,
            div(
              class = "kpi-card",
              style = "--kpi-accent:#FF7389;",
              tags$p("OPEN EXPOSURE"),
              tags$h2(textOutput("atlas_kpi_exposure")),
              tags$p(class = "sub-text", "Customer AR + supplier open procurement exposure")
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Global Operating Lens",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                3,
                selectizeInput(
                  "atlas_entity_types",
                  "Relationship type:",
                  choices = c("Customer", "Supplier", "Representative"),
                  selected = c("Customer", "Supplier", "Representative"),
                  multiple = TRUE,
                  options = list(plugins = list("remove_button"), placeholder = "Choose relationship types")
                )
              ),
              column(
                2,
                selectInput(
                  "atlas_continent",
                  "Continent:",
                  choices = c("All Continents" = "all", setNames(sort(unique(na.omit(geography_data$Continent))), sort(unique(na.omit(geography_data$Continent))))),
                  selected = "all"
                )
              ),
              column(
                2,
                selectizeInput(
                  "atlas_country",
                  "Country:",
                  choices = c("All Countries" = "all", setNames(sort(unique(na.omit(geography_data$Country))), sort(unique(na.omit(geography_data$Country))))),
                  selected = "all",
                  options = list(maxOptions = 500)
                )
              ),
              column(
                2,
                selectizeInput(
                  "atlas_region",
                  "Region / state:",
                  choices = c("All Regions" = "all", setNames(sort(unique(na.omit(geography_data$Region))), sort(unique(na.omit(geography_data$Region))))),
                  selected = "all",
                  options = list(maxOptions = 1000)
                )
              ),
              column(
                3,
                selectizeInput(
                  "atlas_city",
                  "City / town:",
                  choices = c("All Cities" = "all", setNames(sort(unique(na.omit(geography_data$City))), sort(unique(na.omit(geography_data$City))))),
                  selected = "all",
                  options = list(maxOptions = 2000, placeholder = "All cities")
                )
              )
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  "atlas_metric",
                  "Map intensity / bubble size:",
                  choices = c(
                    "Business activity value" = "activity",
                    "Order / transaction count" = "count",
                    "Open exposure" = "exposure",
                    "Gross profit" = "profit"
                  ),
                  selected = "activity"
                )
              ),
              column(
                3,
                selectInput(
                  "atlas_projection",
                  "World projection:",
                  choices = c(
                    "Natural Earth" = "natural earth",
                    "Mercator" = "mercator",
                    "Equirectangular" = "equirectangular",
                    "Orthographic Globe" = "orthographic"
                  ),
                  selected = "natural earth"
                )
              ),
              column(
                3,
                textInput(
                  "atlas_search",
                  "Entity / ID lookup:",
                  value = "",
                  placeholder = "Customer, supplier, representative, ID..."
                )
              ),
              column(
                3,
                div(
                  class = "atlas-segmented-shell",
                  actionButton("atlas_reset", "Reset Atlas", icon = icon("undo"), class = "btn-sm btn-outline-secondary"),
                  actionButton("atlas_focus_selected", "Focus Selection", icon = icon("crosshairs"), class = "btn-sm btn-outline-primary"),
                  actionButton("atlas_open_workspace", "Open Workspace", icon = icon("external-link-alt"), class = "btn-sm btn-primary")
                ),
                tags$p(
                  class = "atlas-filter-note",
                  "Click a point to select an entity. Filters drill from continent → country → region/city while all current year/month filters remain active."
                )
              )
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Interactive World Relationship Map",
            width = 8,
            status = "navy",
            solidHeader = TRUE,
            div(
              class = "atlas-map-shell",
              plotlyOutput("atlas_world_map", height = "610px") %>% withSpinner(color = "#8067F2")
            ),
            div(
              class = "atlas-network-legend",
              tags$span(tags$i(style = "background:#268BFF;"), "Customer"),
              tags$span(tags$i(style = "background:#8067F2;"), "Supplier"),
              tags$span(tags$i(style = "background:#24CED2;"), "Representative"),
              tags$span(icon("mouse-pointer"), "Click = entity drilldown"),
              tags$span(icon("search-plus"), "Zoom / pan enabled")
            )
          ),
          bs4Card(
            title = "Selected Relationship",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            uiOutput("atlas_selected_entity"),
            br(),
            plotlyOutput("atlas_selected_timeline", height = "250px") %>% withSpinner(color = "#24CED2"),
            DTOutput("atlas_selected_contacts")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Geographic Business Breakdown",
            width = 12,
            status = "navy",
            solidHeader = TRUE,
            div(
              class = "atlas-breakdown-reset-floating",
              actionButton(
                "atlas_breakdown_reset",
                "Reset Breakdown",
                icon = icon("undo-alt"),
                class = "btn-sm btn-outline-warning"
              )
            ),
            plotlyOutput("atlas_country_breakdown", height = "470px") %>% withSpinner(color = "#FFC857")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Location Drilldown",
            width = 12,
            status = "navy",
            solidHeader = TRUE,
            DTOutput("atlas_location_table") %>% withSpinner(color = "#FF7A45"),
            tags$p(
              class = "metric-definition-note",
              "Rows are selectable. Selecting a row synchronizes the world map, relationship profile and connected business views."
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Relationship Connections",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            plotlyOutput("atlas_relationship_network", height = "390px") %>% withSpinner(color = "#8067F2"),
            report_action_bar("report_atlas", "Build a geographic relationship report using the active year/month and Atlas filters.")
          )
        )
      ),
      
      
      ########################################################################
      # 3.4 KPI COMMAND CENTER
      ########################################################################
      bs4TabItem(
        tabName = "kpi_board",
        
        fluidRow(
          column(3, div(class = "kpi-card kpi-blue",
                        tags$p("RECOGNIZED REVENUE"), tags$h2(textOutput("kpi_board_revenue")),
                        tags$p(class = "sub-text", "Accounting revenue in the current global period"))),
          column(3, div(class = "kpi-card kpi-teal",
                        tags$p("GROSS MARGIN"), tags$h2(textOutput("kpi_board_margin")),
                        tags$p(class = "sub-text", "Gross profit ÷ recognized revenue"))),
          column(3, div(class = "kpi-card kpi-navy",
                        tags$p("PO VALUE"), tags$h2(textOutput("kpi_board_po_value")),
                        tags$p(class = "sub-text", "Purchase-order value in the current period"))),
          column(3, div(class = "kpi-card kpi-coral",
                        tags$p("OUTSTANDING AR"), tags$h2(textOutput("kpi_board_ar")),
                        tags$p(class = "sub-text", "Open customer balance")))
        ),
        fluidRow(
          column(3, div(class = "kpi-card kpi-sage",
                        tags$p("QUANTITY FULFILLMENT"), tags$h2(textOutput("kpi_board_completion")),
                        tags$p(class = "sub-text", "Received quantity ÷ ordered quantity"))),
          column(3, div(class = "kpi-card kpi-amber",
                        tags$p("BACKORDER RATE"), tags$h2(textOutput("kpi_board_backorder")),
                        tags$p(class = "sub-text", "Backordered quantity ÷ ordered quantity"))),
          column(3, div(class = "kpi-card", style = "--kpi-accent:#6FA8D6;",
                        tags$p("ACTIVE CUSTOMERS"), tags$h2(textOutput("kpi_board_customers")),
                        tags$p(class = "sub-text", "Customers with activity in the selected period"))),
          column(3, div(class = "kpi-card", style = "--kpi-accent:#64748B;",
                        tags$p("ACTIVE BUYERS"), tags$h2(textOutput("kpi_board_buyers")),
                        tags$p(class = "sub-text", "Aurelis buyers with PO activity")))
        ),
        
        fluidRow(
          bs4Card(
            title = "Management KPI Trend", width = 8,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("kpi_board_trend", height = "390px") %>% withSpinner(color = brand_blue)
          ),
          bs4Card(
            title = "Operational KPI Health", width = 4,
            status = "success", solidHeader = TRUE,
            DTOutput("kpi_board_health") %>% withSpinner(color = brand_sage),
            tags$p(
              class = "metric-definition-note",
              "Customer-service metrics are calculated only from fields available in the current Aurelis data. The dashboard does not label a metric as OTIF or Perfect Order unless actual delivery, completeness, damage and documentation fields support that definition."
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Top Customer Contribution", width = 6,
            status = "info", solidHeader = TRUE,
            plotlyOutput("kpi_board_customer_rank", height = "390px") %>% withSpinner(color = brand_sky)
          ),
          bs4Card(
            title = "Buyer Workload and PO Value", width = 6,
            status = "warning", solidHeader = TRUE,
            plotlyOutput("kpi_board_buyer_rank", height = "390px") %>% withSpinner(color = brand_amber)
          )
        ),
        fluidRow(
          bs4Card(
            title = NULL, width = 12, status = "white",
            report_action_bar("report_kpi_board", "Export the current management KPI command-center view.")
          )
        )
      ),
      
      ########################################################################
      # 3.4 BUYER ACTIVITY INTELLIGENCE
      ########################################################################
      bs4TabItem(
        tabName = "buyer_activity",
        
        fluidRow(
          bs4Card(
            title = "Buyer Activity Drill-Down", width = 12,
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(2, selectInput("buyer_act_year", "Year:", choices = c("All Years" = "all", all_years), selected = "all")),
              column(2, selectInput("buyer_act_month", "Month:", choices = c("All Months" = "all", setNames(1:12, month.name)), selected = "all")),
              column(2, selectInput("buyer_act_week", "Week of month:", choices = c("All Weeks" = "all", setNames(1:5, paste("Week", 1:5))), selected = "all")),
              column(2, selectInput("buyer_act_day", "Day:", choices = c("All Days" = "all"), selected = "all")),
              column(3, selectizeInput("buyer_act_buyer", "Buyer:", choices = c("All Buyers" = "all", setNames(all_buyers, all_buyers)), selected = "all", options = list(placeholder = "All buyers / choose one"))),
              column(1, br(), actionButton("buyer_act_reset", NULL, icon = icon("undo"), class = "btn-outline-secondary btn-block", title = "Reset buyer activity filters"))
            ),
            uiOutput("buyer_act_drill_path")
          )
        ),
        
        fluidRow(
          column(2, div(class = "kpi-card kpi-blue", tags$p("POs"), tags$h2(textOutput("buyer_act_kpi_pos")), tags$p(class = "sub-text", "Distinct purchase orders"))),
          column(2, div(class = "kpi-card kpi-teal kpi-card-long-value", tags$p("PO VALUE"), tags$h2(textOutput("buyer_act_kpi_value")), tags$p(class = "sub-text", "Purchase-order value"))),
          column(2, div(class = "kpi-card kpi-sage", tags$p("CUSTOMERS"), tags$h2(textOutput("buyer_act_kpi_customers")), tags$p(class = "sub-text", "Distinct customers served"))),
          column(2, div(class = "kpi-card kpi-amber", tags$p("AVERAGE PO"), tags$h2(textOutput("buyer_act_kpi_avg")), tags$p(class = "sub-text", "Average value per PO"))),
          column(2, div(class = "kpi-card kpi-navy", tags$p("ACTIVE BUYERS"), tags$h2(textOutput("buyer_act_kpi_buyers")), tags$p(class = "sub-text", "Buyers active in selection"))),
          column(2, div(class = "kpi-card", style = "--kpi-accent:#64748B;", tags$p("ACTIVE DAYS"), tags$h2(textOutput("buyer_act_kpi_days")), tags$p(class = "sub-text", "Days with PO activity")))
        ),
        
        fluidRow(
          bs4Card(
            title = "Daily Activity — PO Count and Value", width = 7,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("buyer_act_daily_trend", height = "380px") %>% withSpinner(color = brand_blue)
          ),
          bs4Card(
            title = "Buyer Activity Leaderboard", width = 5,
            status = "warning", solidHeader = TRUE,
            plotlyOutput("buyer_act_leaderboard", height = "380px") %>% withSpinner(color = brand_amber)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Customer Mix for Selected Buyer / Period", width = 5,
            status = "info", solidHeader = TRUE,
            plotlyOutput("buyer_act_customer_mix", height = "390px") %>% withSpinner(color = brand_sky)
          ),
          bs4Card(
            title = "Buyer Activity Log", width = 7,
            status = "gray-dark", solidHeader = TRUE,
            DTOutput("buyer_act_table") %>% withSpinner(color = brand_slate),
            report_action_bar("report_buyer_activity", "Export the current buyer activity drill-down.")
          )
        )
      ),
      
      ########################################################################
      # 3.4 PURCHASE ORDERS & BUYER PERFORMANCE TAB
      ########################################################################
      bs4TabItem(
        tabName = "monthly_sales",
        
        # Filters specific to this section
        fluidRow(
          bs4Card(title = NULL, width = 12, status = "white",
                  fluidRow(
                    column(3, selectInput("sales_buyer", "Buyer:", 
                                          choices = c("All Buyers" = "all", all_buyers), selected = "all")),
                    column(3, selectInput("sales_client", "Customer:", 
                                          choices = c("All Clients" = "all", all_clients_po), selected = "all")),
                    column(3, selectInput("sales_year_specific", "Compare Year:", 
                                          choices = c("Use Global" = "global", all_years), selected = "global")),
                    column(3, br(), actionButton("sales_reset", "Reset PO Filters", 
                                                 class = "btn-outline-secondary btn-block", icon = icon("undo")))
                  )
          )
        ),
        
        # Filter-responsive purchase-order summary
        fluidRow(
          column(3, div(
            class = "kpi-card kpi-blue",
            tags$p("PURCHASE ORDERS"),
            tags$h2(textOutput("sales_kpi_po_count")),
            tags$p(class = "sub-text", "Distinct PO numbers in the current filters")
          )),
          column(3, div(
            class = "kpi-card kpi-teal",
            tags$p("TOTAL PO VALUE"),
            tags$h2(textOutput("sales_kpi_total_value")),
            tags$p(class = "sub-text", "Commercial value recorded on selected POs")
          )),
          column(3, div(
            class = "kpi-card kpi-amber",
            tags$p("AVERAGE PO VALUE"),
            tags$h2(textOutput("sales_kpi_avg_value")),
            tags$p(class = "sub-text", "Average value per distinct PO")
          )),
          column(3, div(
            class = "kpi-card kpi-sage",
            tags$p("CUSTOMERS SERVED"),
            tags$h2(textOutput("sales_kpi_customers")),
            tags$p(class = "sub-text", "Distinct customers represented")
          ))
        ),
        
        div(
          class = "definition-grid",
          metric_definition(
            "Total PO Value",
            "The sum of values recorded in Daily PO for the selected purchase orders. It is not the same as recognized accounting revenue."
          ),
          metric_definition(
            "Purchase Orders",
            "The number of distinct Aurelis PO numbers after applying the global period, buyer, customer and comparison-year filters."
          )
        ),
        
        # Monthly Heatmap + Buyer Breakdown
        fluidRow(
          bs4Card(title = "Purchase Order Volume by Buyer and Month", width = 7, 
                  status = "primary", solidHeader = TRUE,
                  plotlyOutput("sales_heatmap", height = "380px") %>% withSpinner(color = "#2F6EA5")),
          bs4Card(title = "Purchase Order Value by Buyer and Month", width = 5, 
                  status = "indigo", solidHeader = TRUE,
                  plotlyOutput("sales_buyer_stack", height = "380px") %>% withSpinner(color = "#17324D"))
        ),
        
        # Monthly trend line + PO table
        fluidRow(
          bs4Card(title = "Purchase Order Value Trend", width = 6, 
                  status = "success", solidHeader = TRUE,
                  plotlyOutput("sales_monthly_line", height = "300px") %>% withSpinner(color = "#5F7D67")),
          bs4Card(title = "Buyer Purchase-Order Summary", width = 6, 
                  status = "orange", solidHeader = TRUE,
                  DTOutput("sales_buyer_summary_table") %>% withSpinner(color = "#9A6A27"))
        ),
        
        # Detailed PO Table
        fluidRow(
          bs4Card(title = "Detailed Purchase Orders Log", width = 12, 
                  status = "gray-dark", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  DTOutput("sales_po_detail_table") %>% withSpinner(color = "#424242"),
                  report_action_bar("report_sales"))
        )
      ),
      
      ########################################################################
      # 3.3 SALES, REVENUE & PROFITABILITY TAB
      ########################################################################
      bs4TabItem(
        tabName = "sale_performance",
        
        fluidRow(
          bs4Card(
            title = NULL, width = 12, status = "white",
            fluidRow(
              column(2, selectInput(
                "perf_staff", "Sales Representative:",
                choices = c("All Representatives" = "all", all_staff_rev),
                selected = "all"
              )),
              column(2, selectInput(
                "perf_country", "Country:",
                choices = c("All Countries" = "all", all_countries),
                selected = "all"
              )),
              column(3, selectInput(
                "perf_parent", "Parent Company:",
                choices = c("All Parent Companies" = "all", all_parents),
                selected = "all"
              )),
              column(2, numericInput(
                "perf_margin_target", "Margin Target (%):",
                value = 30, min = 0, max = 100, step = 1
              )),
              column(
                3, br(),
                actionButton(
                  "perf_reset", "Reset Sales Filters",
                  icon = icon("undo"),
                  class = "btn-outline-secondary btn-block"
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(3, bs4ValueBox(
            value = textOutput("perf_kpi_rev"), subtitle = "Recognized Revenue",
            icon = icon("dollar-sign"), color = "primary", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("perf_kpi_profit"), subtitle = "Gross Profit",
            icon = icon("chart-line"), color = "success", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("perf_kpi_margin"), subtitle = "Weighted Gross Margin",
            icon = icon("percentage"), color = "warning", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("perf_kpi_deals"), subtitle = "Revenue Transactions",
            icon = icon("file-invoice"), color = "info", width = 12
          ))
        ),
        
        div(
          class = "definition-grid",
          metric_definition(
            "Weighted Gross Margin",
            "Total gross profit divided by total revenue for the selected filters. A value of 32% means $32 of gross profit for every $100 of revenue, before operating expenses."
          ),
          metric_definition(
            "Target Comparison",
            "The gauge target is adjustable above. The displayed delta is the number of percentage points above or below that target."
          ),
          metric_definition(
            "Revenue Transaction",
            "One loaded revenue record. This is not automatically the same as one unique customer order."
          ),
          metric_definition(
            "QuickBooks Cost Basis",
            "The free SDK uses an invoice custom total-cost field when available. Otherwise, it estimates cost from the item purchase or average cost; the detailed data keeps the CostMethod used."
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Monthly Revenue, Cost and Gross Profit", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("perf_rev_cost_trend", height = "370px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Revenue and Gross Profit by Sales Representative", width = 7,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("perf_staff_bar", height = "410px") %>%
              withSpinner(color = brand_blue)
          ),
          bs4Card(
            title = "Weighted Gross Margin vs Target", width = 5,
            status = "success", solidHeader = TRUE,
            plotlyOutput("perf_margin_gauge", height = "335px") %>%
              withSpinner(color = brand_teal),
            div(
              class = "metric-definition",
              htmlOutput("perf_margin_explanation")
            )
          )
        ),
        
        interaction_tip(
          "Click a country segment to focus the complete Sales & Profitability page on that country. Click the selected segment again or use Clear Country Focus to restore the full view."
        ),
        
        fluidRow(
          class = "country-analysis-row",
          bs4Card(
            title = "Revenue Contribution by Country", width = 8,
            status = "info", solidHeader = TRUE,
            fluidRow(
              column(
                4,
                actionButton(
                  "perf_clear_country", "Clear Country Focus",
                  icon = icon("times-circle"),
                  class = "btn-outline-secondary btn-block"
                )
              ),
              column(8, htmlOutput("perf_country_focus_label"))
            ),
            tags$p(
              class = "chart-toolbar-note",
              "Click a segment to focus the entire page. Every country remains available through labels, hover details and the structured list below."
            ),
            uiOutput("perf_country_overflow_note"),
            uiOutput("perf_country_donut_ui")
          ),
          bs4Card(
            title = "Country Business Summary", width = 4,
            status = "secondary", solidHeader = TRUE,
            uiOutput("perf_country_business_summary"),
            uiOutput("perf_country_detail_ui")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Revenue and Profitability Detail", width = 12,
            status = "gray-dark", solidHeader = TRUE, collapsible = TRUE,
            DTOutput("perf_detail_table") %>% withSpinner(color = brand_slate),
            report_action_bar(
              "report_performance",
              "Export the current sales summary immediately, or build a customized Excel/PDF report."
            )
          )
        )
      ),

      ########################################################################
      # 3.4 FINANCIAL STATISTICS & DISTRIBUTION LAB
      ########################################################################
      bs4TabItem(
        tabName = "financial_statistics",
        fluidRow(
          bs4Card(
            title = "Financial Statistics & Distribution Lab",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tags$p(
              "Explore filtered synthetic financial records, fit common probability distributions, inspect quantile behavior, and compare groups. Global year and month filters apply to every source."
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  "financial_stats_source",
                  "Dataset",
                  choices = aurelis_stats_dataset_choices,
                  selected = "revenue"
                )
              ),
              column(
                3,
                selectInput(
                  "financial_stats_measure",
                  "Financial measure",
                  choices = aurelis_stats_numeric_fields(revenue_data),
                  selected = "Revenue"
                )
              ),
              column(
                3,
                selectInput(
                  "financial_stats_distribution",
                  "Distribution",
                  choices = c("Normal", "Student t", "Log-normal", "Exponential", "Gamma", "Poisson", "Empirical"),
                  selected = "Normal"
                )
              ),
              column(
                3,
                sliderInput(
                  "financial_stats_df",
                  "Student t degrees of freedom",
                  min = 2.1,
                  max = 60,
                  value = 5,
                  step = 0.1
                )
              )
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  "financial_stats_group_by",
                  "Compare by",
                  choices = c("All records" = "", setNames(aurelis_stats_group_fields(revenue_data), str_replace_all(aurelis_stats_group_fields(revenue_data), "_", " "))),
                  selected = if ("Country" %in% aurelis_stats_group_fields(revenue_data)) "Country" else ""
                )
              ),
              column(
                5,
                selectizeInput(
                  "financial_stats_groups",
                  "Selected groups (blank = compare available groups)",
                  choices = sort(unique(na.omit(revenue_data$Country))),
                  selected = character(0),
                  multiple = TRUE,
                  options = list(plugins = list("remove_button"), maxOptions = 100, placeholder = "All available groups")
                )
              ),
              column(
                4,
                selectInput(
                  "financial_stats_missing_policy",
                  "Missing financial values",
                  choices = c(
                    "Exclude missing values" = "exclude",
                    "Median-fill for synthetic analysis" = "median"
                  ),
                  selected = "exclude"
                )
              )
            ),
            uiOutput("financial_stats_data_note")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Sample profile",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            uiOutput("financial_stats_summary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Observed values and fitted distribution",
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("financial_stats_distribution_plot", height = "380px")
          ),
          bs4Card(
            title = "Distribution quantile check",
            width = 5,
            status = "secondary",
            solidHeader = TRUE,
            plotlyOutput("financial_stats_qq_plot", height = "380px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Group distributions",
            width = 7,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("financial_stats_group_plot", height = "390px")
          ),
          bs4Card(
            title = "Group comparison tests",
            width = 5,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("financial_stats_comparison")
          )
        )
      ),
      
      ########################################################################
      # 3.5 CUSTOMER PERFORMANCE & RELATIONSHIP VALUE TAB
      ########################################################################
      bs4TabItem(
        tabName = "customer_performance",
        
        fluidRow(
          bs4Card(
            title = NULL, width = 12, status = "white",
            fluidRow(
              column(
                5,
                selectizeInput(
                  "cust_perf_client",
                  "Select Customer:",
                  choices = c(
                    "-- Select a Customer --" = "",
                    setNames(customer_performance_clients, customer_performance_clients)
                  ),
                  selected = "",
                  options = list(
                    placeholder = "Search customer name...",
                    maxOptions = 5000
                  )
                )
              ),
              column(
                3,
                selectInput(
                  "cust_perf_scope",
                  "Analysis Period:",
                  choices = c(
                    "Full Relationship History" = "full",
                    "Use Global Year / Month" = "global"
                  ),
                  selected = "full"
                )
              ),
              column(
                2,
                selectInput(
                  "cust_perf_value_basis",
                  "Trend Value:",
                  choices = c(
                    "Purchase Order Value" = "po",
                    "Recognized Revenue" = "revenue"
                  ),
                  selected = "po"
                )
              ),
              column(
                2, br(),
                downloadButton(
                  "cust_perf_download",
                  "Export Customer File",
                  class = "btn-success btn-block"
                )
              )
            ),
            div(
              class = "delivery-filter-note",
              icon("info-circle"),
              " This page combines order volume, revenue, profitability, delivery execution, payment behavior, buyers, and suppliers into one customer-ready performance summary."
            )
          )
        ),
        
        
        fluidRow(
          bs4Card(
            title = "Customer Relationship Resume & Client-Facing PDF", width = 12,
            status = "primary", solidHeader = TRUE,
            icon = icon("file-alt"),
            fluidRow(
              column(3, textInput("cust_perf_resume_title", "PDF title:", value = "Partnership Performance Review")),
              column(3, selectInput(
                "cust_perf_resume_layout", "PDF layout:",
                choices = c(
                  "Executive one-page style" = "executive",
                  "Relationship story" = "story",
                  "Detailed performance review" = "detailed"
                ),
                selected = "story"
              )),
              column(3, checkboxInput("cust_perf_pdf_show_value", "Show investment / order value", TRUE)),
              column(3, checkboxInput("cust_perf_pdf_show_fulfillment", "Show fulfillment metrics", TRUE))
            ),
            fluidRow(
              column(3, checkboxInput("cust_perf_pdf_show_network", "Show Aurelis support network", TRUE)),
              column(3, checkboxInput("cust_perf_pdf_show_history", "Show annual relationship history", TRUE)),
              column(4, textInput("cust_perf_resume_message", "Client-ready message:", value = "Thank you for the continued trust and collaboration with Aurelis Global Supply.")),
              column(2, br(), downloadButton("cust_perf_resume_pdf", "Download Client PDF", icon = icon("file-pdf"), class = "btn-danger btn-block"))
            ),
            uiOutput("cust_perf_resume_preview")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Customer Service KPI Board", width = 12,
            status = "success", solidHeader = TRUE,
            uiOutput("cust_perf_service_kpis"),
            tags$p(
              class = "metric-definition-note",
              "Industry-aligned order-management measures emphasize reliability, completeness and cycle time. Aurelis shows only metrics supported by the current data; true OTIF / Perfect Order will automatically become appropriate once actual delivery, damage-free and documentation-accuracy fields are available."
            )
          )
        ),
        
        fluidRow(
          column(3, bs4ValueBox(
            value = textOutput("cust_perf_total_value"),
            subtitle = "Lifetime Revenue / PO Value",
            icon = icon("dollar-sign"), color = "primary", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("cust_perf_po_count"),
            subtitle = "Distinct Purchase Orders",
            icon = icon("shopping-cart"), color = "info", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("cust_perf_execution_days"),
            subtitle = "Average Planned Execution Time",
            icon = icon("stopwatch"), color = "warning", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("cust_perf_completion_rate"),
            subtitle = "Quantity Completion Rate",
            icon = icon("check-double"), color = "success", width = 12
          ))
        ),
        
        fluidRow(
          bs4Card(
            title = "Customer-Ready Relationship Highlights", width = 7,
            status = "indigo", solidHeader = TRUE,
            uiOutput("cust_perf_highlights")
          ),
          bs4Card(
            title = "Portfolio Standing", width = 5,
            status = "purple", solidHeader = TRUE,
            plotlyOutput("cust_perf_rank_gauge", height = "300px") %>%
              withSpinner(color = "#17324D")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Monthly Relationship Value Trend", width = 8,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("cust_perf_monthly_trend", height = "360px") %>%
              withSpinner(color = "#2F6EA5")
          ),
          bs4Card(
            title = "Order Fulfillment Status", width = 4,
            status = "success", solidHeader = TRUE,
            fluidRow(
              column(6, actionButton(
                "cust_perf_clear_status", "Clear Status Focus",
                icon = icon("times-circle"), class = "btn-outline-secondary btn-block"
              )),
              column(6, htmlOutput("cust_perf_status_focus_label"))
            ),
            plotlyOutput("cust_perf_status_donut", height = "360px") %>%
              withSpinner(color = "#16A34A")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Annual Collaboration Story — Investment and Orders", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("cust_perf_annual_story", height = "390px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Supplier Contribution to Customer Orders", width = 6,
            status = "teal", solidHeader = TRUE,
            plotlyOutput("cust_perf_supplier_mix", height = "390px") %>%
              withSpinner(color = "#3C7474")
          ),
          bs4Card(
            title = "AURELIS Buyer Relationship Coverage", width = 6,
            status = "orange", solidHeader = TRUE,
            plotlyOutput("cust_perf_buyer_mix", height = "390px") %>%
              withSpinner(color = "#9A6A27")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Customer Order, Delivery and Supplier History", width = 12,
            status = "gray-dark", solidHeader = TRUE,
            DTOutput("cust_perf_order_table") %>% withSpinner(color = "#424242"),
            report_action_bar(
              "report_customer_performance",
              "Export a customer-ready relationship summary and the detailed order history."
            )
          )
        )
      ),
      
      ########################################################################
      # 3.6 REALTIME ACCOUNTS RECEIVABLE TAB
      ########################################################################
      bs4TabItem(
        tabName = "ar_tab",
        
        fluidRow(
          column(3, div(
            class = "kpi-card kpi-coral",
            tags$p("TOTAL OUTSTANDING"),
            tags$h2(textOutput("ar_kpi_outstanding")),
            tags$p(class = "sub-text", "Open customer balance")
          )),
          column(3, div(
            class = "kpi-card kpi-amber",
            tags$p("OVERDUE MORE THAN 30 DAYS"),
            tags$h2(textOutput("ar_kpi_overdue30")),
            tags$p(class = "sub-text", "Collection priority")
          )),
          column(3, div(
            class = "kpi-card kpi-sage",
            tags$p("NOT YET DUE"),
            tags$h2(textOutput("ar_kpi_current")),
            tags$p(class = "sub-text", "Open balance before due date")
          )),
          column(3, div(
            class = "kpi-card kpi-navy",
            tags$p("OPEN INVOICES"),
            tags$h2(textOutput("ar_kpi_count")),
            tags$p(class = "sub-text", "Invoices with remaining balance")
          ))
        ),
        
        div(
          class = "definition-grid",
          metric_definition(
            "Days Until Due",
            "Number of calendar days remaining before an open invoice reaches its due date. It is always zero once the invoice is due or overdue."
          ),
          metric_definition(
            "Days Past Due",
            "Number of calendar days since an open invoice passed its due date. It is zero while the invoice is still current."
          ),
          metric_definition(
            "Payment Timing",
            "A plain-language status such as Due in 8 days, Due today, 12 days overdue, or Paid."
          )
        ),
        
        interaction_tip(
          "Click an aging segment to filter all AR views. Click a customer exposure bar or ledger row to open that client statement, click a staff segment to open Sales & Profitability, and click the monthly trend to drill the global period."
        ),
        
        fluidRow(
          bs4Card(
            title = "Outstanding Balance by Aging Group", width = 5,
            status = "danger", solidHeader = TRUE,
            fluidRow(
              column(
                5,
                actionButton(
                  "ar_clear_aging", "Clear Aging Focus",
                  icon = icon("times-circle"),
                  class = "btn-outline-secondary btn-block"
                )
              ),
              column(7, htmlOutput("ar_aging_focus_label"))
            ),
            plotlyOutput("ar_aging_donut", height = "400px") %>%
              withSpinner(color = brand_coral)
          ),
          bs4Card(
            title = "Invoice and Outstanding Balance Trend", width = 7,
            status = "info", solidHeader = TRUE,
            plotlyOutput("ar_monthly_trend", height = "400px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Outstanding Balance by Responsible Staff and Aging", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("ar_staff_aging", height = "390px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Customer Exposure", width = 12,
            status = "warning", solidHeader = TRUE,
            fluidRow(
              column(
                3,
                selectInput(
                  "ar_top_n", "Customers Displayed:",
                  choices = c("Top 5" = 5, "Top 10" = 10, "Top 15" = 15,
                              "Top 20" = 20, "All" = 999),
                  selected = 10
                )
              ),
              column(
                9,
                div(
                  class = "metric-definition",
                  "Bars show open customer balance for the selected global period and aging focus."
                )
              )
            ),
            uiOutput("ar_exposure_plot_ui")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Complete Accounts Receivable Ledger", width = 12,
            status = "gray-dark", solidHeader = TRUE, collapsible = TRUE,
            DTOutput("ar_full_table") %>% withSpinner(color = brand_slate),
            report_action_bar(
              "report_ar",
              "Export a concise receivables summary or build a detailed aging report."
            )
          )
        )
      ),
      
      ########################################################################
      # 3.7 ACCOUNT STATEMENT PER CLIENT TAB
      ########################################################################
      bs4TabItem(
        tabName = "client_statement",
        
        fluidRow(
          bs4Card(title = NULL, width = 12, status = "white",
                  fluidRow(
                    column(5, selectizeInput(
                      "stmt_client",
                      "Select Client:",
                      choices = c(
                        "-- Select a Client --" = "",
                        setNames(all_clients_ar, all_clients_ar)
                      ),
                      selected = "",
                      options = list(
                        placeholder = "Type a customer name...",
                        maxOptions = 1000
                      )
                    )),
                    column(3, br(), downloadButton("stmt_download", "Export Statement (CSV)", class = "btn-success btn-block")),
                    column(4, br(), tags$p(class = "text-muted", "Select a client to view their full account statement with aging details."))
                  )
          )
        ),
        
        # Client Summary KPIs
        fluidRow(
          column(3, bs4ValueBox(value = textOutput("stmt_total_invoiced"), subtitle = "Total Invoiced", 
                                icon = icon("file-invoice"), color = "primary", width = 12)),
          column(3, bs4ValueBox(value = textOutput("stmt_balance"), subtitle = "Balance Outstanding", 
                                icon = icon("exclamation-circle"), color = "danger", width = 12)),
          column(3, bs4ValueBox(value = textOutput("stmt_paid_pct"), subtitle = "% Paid", 
                                icon = icon("check-circle"), color = "success", width = 12)),
          column(3, bs4ValueBox(value = textOutput("stmt_invoice_count"), subtitle = "Invoice Count", 
                                icon = icon("hashtag"), color = "info", width = 12))
        ),
        
        # Client aging donut + payment timeline
        fluidRow(
          bs4Card(
            title = "Client Outstanding Balance by Aging Group", width = 5,
            status = "danger", solidHeader = TRUE,
            fluidRow(
              column(6, actionButton(
                "stmt_clear_aging", "Clear Aging Focus",
                icon = icon("times-circle"), class = "btn-outline-secondary btn-block"
              )),
              column(6, htmlOutput("stmt_aging_focus_label"))
            ),
            plotlyOutput("stmt_aging_donut", height = "300px") %>% withSpinner(color = "#A65359")
          ),
          bs4Card(title = "Invoice Timeline", width = 7, 
                  status = "primary", solidHeader = TRUE,
                  plotlyOutput("stmt_timeline", height = "300px") %>% withSpinner(color = "#2F6EA5"))
        ),
        
        # Statement Table
        fluidRow(
          bs4Card(title = "Detailed Account Statement", width = 12, 
                  status = "navy", solidHeader = TRUE,
                  DTOutput("stmt_detail_table") %>% withSpinner(color = "#1A237E"),
                  report_action_bar("report_statement", "Export the selected client statement or build a paginated report."))
        )
      ),
      
      ########################################################################
      # 3.8 ORDERS, DELIVERY PLANNING & CUSTOMER HISTORY TAB
      ########################################################################
      bs4TabItem(
        tabName = "order_tracker",
        
        fluidRow(
          bs4Card(
            title = NULL, width = 12, status = "white",
            fluidRow(
              column(3, selectizeInput(
                "tracker_client", "Customer:",
                choices = c(
                  "All Customers" = "all",
                  setNames(all_inventory_clients, all_inventory_clients)
                ),
                selected = "all",
                options = list(placeholder = "Search customer...", maxOptions = 2000)
              )),
              column(3, selectizeInput(
                "tracker_buyer", "Buyer:",
                choices = c(
                  "All Buyers" = "all",
                  setNames(all_inventory_buyers, all_inventory_buyers)
                ),
                selected = "all",
                options = list(placeholder = "Search buyer...", maxOptions = 1000)
              )),
              column(3, selectizeInput(
                "tracker_supplier", "Supplier:",
                choices = c(
                  "All Suppliers" = "all",
                  setNames(all_inventory_suppliers, all_inventory_suppliers)
                ),
                selected = "all",
                options = list(placeholder = "Search supplier...", maxOptions = 2000)
              )),
              column(3, selectInput(
                "tracker_status", "Delivery Status:",
                choices = c(
                  "All Statuses" = "all",
                  setNames(all_inventory_statuses, all_inventory_statuses)
                ),
                selected = "all"
              ))
            ),
            fluidRow(
              column(4, textInput(
                "tracker_search", "Search PO, Part or Description:",
                placeholder = "PO number, part number, memo, customer..."
              )),
              column(3, selectInput(
                "tracker_horizon", "Delivery Horizon:",
                choices = c(
                  "All Dates" = "all",
                  "Overdue" = "overdue",
                  "Next 7 Days" = "7",
                  "Next 14 Days" = "14",
                  "Next 30 Days" = "30",
                  "Next 90 Days" = "90"
                ),
                selected = "all"
              )),
              column(2, selectInput(
                "tracker_timeline_metric", "Timeline Measure:",
                choices = c(
                  "Open Value" = "Open_Balance",
                  "Backordered Units" = "Backordered_Qty",
                  "Order Lines" = "Lines"
                ),
                selected = "Open_Balance"
              )),
              column(
                3, br(),
                actionButton(
                  "tracker_reset", "Reset Delivery Filters",
                  icon = icon("undo"),
                  class = "btn-outline-secondary btn-block"
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(3, div(
            class = "kpi-card kpi-blue",
            tags$p("OPEN PURCHASE ORDERS"),
            tags$h2(textOutput("tracker_open_orders")),
            tags$p(class = "sub-text", "POs with units awaiting receipt")
          )),
          column(3, div(
            class = "kpi-card kpi-coral",
            tags$p("OVERDUE OPEN VALUE"),
            tags$h2(textOutput("tracker_overdue")),
            tags$p(class = "sub-text", "Open balance past delivery date")
          )),
          column(3, div(
            class = "kpi-card kpi-amber",
            tags$p("DUE WITHIN 14 DAYS"),
            tags$h2(textOutput("tracker_due_soon")),
            tags$p(class = "sub-text", "Open delivery lines")
          )),
          column(3, div(
            class = "kpi-card kpi-teal",
            tags$p("TOTAL ORDER VALUE"),
            tags$h2(textOutput("tracker_open_value")),
            tags$p(class = "sub-text", "Value of selected inventory order lines")
          ))
        ),
        
        div(
          class = "definition-grid",
          metric_definition(
            "Open Purchase Order",
            "A PO with at least one product line whose received quantity is below the ordered quantity."
          ),
          metric_definition(
            "Backordered Quantity",
            "Ordered quantity that has not yet been received. It can be scheduled, due soon, or overdue."
          ),
          metric_definition(
            "Total Order Value",
            "The full line value of the selected inventory orders. This differs from open value, which represents only the remaining unreceived or unpaid order balance."
          )
        ),
        
        interaction_tip(
          "Click a status segment to focus all order charts and the schedule table. Click a workload heatmap cell to apply its year and month to the global filters."
        ),
        
        fluidRow(
          bs4Card(
            title = "Delivery Status Mix", width = 4,
            status = "info", solidHeader = TRUE,
            fluidRow(
              column(
                6,
                actionButton(
                  "tracker_clear_status", "Clear Status Focus",
                  icon = icon("times-circle"),
                  class = "btn-outline-secondary btn-block"
                )
              ),
              column(6, htmlOutput("tracker_status_focus_label"))
            ),
            plotlyOutput("tracker_status_donut", height = "360px") %>%
              withSpinner(color = brand_blue)
          ),
          bs4Card(
            title = "Exact-Date Delivery Workload", width = 8,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("tracker_delivery_timeline", height = "360px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Delivery Risk Matrix", width = 7,
            status = "warning", solidHeader = TRUE,
            tags$p(
              class = "chart-toolbar-note",
              "Use the modebar or drag a rectangle to zoom. Mouse-wheel zoom is enabled; double-click resets the view."
            ),
            plotlyOutput("tracker_delivery_risk", height = "480px") %>%
              withSpinner(color = brand_amber)
          ),
          bs4Card(
            title = "Delivery Workload Calendar", width = 5,
            status = "secondary", solidHeader = TRUE,
            plotlyOutput("tracker_delivery_heatmap", height = "430px") %>%
              withSpinner(color = brand_slate)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Open Exposure by Customer and Status", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("tracker_customer_exposure", height = "450px") %>%
              withSpinner(color = brand_blue)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Prioritized Delivery Schedule", width = 12,
            status = "gray-dark", solidHeader = TRUE,
            DTOutput("tracker_priority_table") %>% withSpinner(color = brand_slate),
            div(
              class = "schedule-export-bar",
              tags$span(
                icon("file-export"),
                " Export the prioritized schedule exactly as currently filtered."
              ),
              div(
                class = "schedule-export-actions",
                downloadButton(
                  "tracker_priority_excel", "Download Excel",
                  icon = icon("file-excel"), class = "btn-success"
                ),
                downloadButton(
                  "tracker_priority_pdf", "Download PDF",
                  icon = icon("file-pdf"), class = "btn-danger"
                )
              )
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Detailed Customer Order History", width = 12,
            status = "gray-dark", solidHeader = TRUE, collapsible = TRUE,
            DTOutput("tracker_po_table") %>% withSpinner(color = brand_slate),
            report_action_bar(
              "report_tracker",
              "Export the current delivery summary or build a detailed order-history report."
            )
          )
        )
      ),
      
      ########################################################################
      # 3.9 INVENTORY & REAL-TIME WAREHOUSE TAB
      ########################################################################
      bs4TabItem(
        tabName = "inventory_tab",
        
        div(
          class = if (warehouse_inventory_available) "data-quality-note" else "data-quality-note warning",
          icon(if (warehouse_inventory_available) "database" else "exclamation-triangle"),
          if (warehouse_inventory_available) {
            " Live warehouse quantities are loaded from the configured Access or QuickBooks source."
          } else {
            " Current workbooks contain inbound/open-order quantities but not verified warehouse stock. Warehouse stock cards remain explicitly unavailable until Access or QuickBooks is connected."
          }
        ),
        
        tabsetPanel(
          id = "inventory_view",
          
          tabPanel(
            "Warehouse Availability",
            value = "warehouse",
            
            fluidRow(
              bs4Card(
                title = NULL, width = 12, status = "white",
                fluidRow(
                  column(3, selectizeInput(
                    "wh_warehouse", "Warehouse:",
                    choices = c(
                      "All Warehouses" = "all",
                      setNames(
                        sort(unique(na.omit(warehouse_inventory_data$Warehouse))),
                        sort(unique(na.omit(warehouse_inventory_data$Warehouse)))
                      )
                    ),
                    selected = "all"
                  )),
                  column(3, selectizeInput(
                    "wh_supplier", "Preferred Supplier:",
                    choices = c(
                      "All Suppliers" = "all",
                      setNames(
                        sort(unique(na.omit(warehouse_inventory_data$Supplier))),
                        sort(unique(na.omit(warehouse_inventory_data$Supplier)))
                      )
                    ),
                    selected = "all"
                  )),
                  column(3, selectInput(
                    "wh_status", "Stock Status:",
                    choices = c(
                      "All Stock Statuses" = "all",
                      setNames(
                        sort(unique(na.omit(warehouse_inventory_data$Stock_Status))),
                        sort(unique(na.omit(warehouse_inventory_data$Stock_Status)))
                      )
                    ),
                    selected = "all"
                  )),
                  column(3, textInput(
                    "wh_search", "Item Search:",
                    placeholder = "Item code, description, bin..."
                  ))
                )
              )
            ),
            
            fluidRow(
              column(3, div(
                class = "kpi-card kpi-blue",
                tags$p("TRACKED ITEMS"),
                tags$h2(textOutput("wh_item_count")),
                tags$p(class = "sub-text", "Distinct warehouse item records")
              )),
              column(3, div(
                class = "kpi-card kpi-navy",
                tags$p("QUANTITY ON HAND"),
                tags$h2(textOutput("wh_on_hand")),
                tags$p(class = "sub-text", "Physical quantity reported")
              )),
              column(3, div(
                class = "kpi-card kpi-teal",
                tags$p("AVAILABLE QUANTITY"),
                tags$h2(textOutput("wh_available")),
                tags$p(class = "sub-text", "On hand minus allocated")
              )),
              column(3, div(
                class = "kpi-card kpi-amber",
                tags$p("ITEMS REQUIRING ATTENTION"),
                tags$h2(textOutput("wh_attention")),
                tags$p(class = "sub-text", "Out of stock, negative or reorder")
              ))
            ),
            
            interaction_tip(
              "Click a stock-status segment to filter the warehouse charts and item table. Hover over points to see availability, demand and reorder context."
            ),
            
            fluidRow(
              bs4Card(
                title = "Warehouse Stock Status", width = 4,
                status = "info", solidHeader = TRUE,
                fluidRow(
                  column(
                    6,
                    actionButton(
                      "wh_clear_status", "Clear Status Focus",
                      icon = icon("times-circle"),
                      class = "btn-outline-secondary btn-block"
                    )
                  ),
                  column(6, htmlOutput("wh_status_focus_label"))
                ),
                plotlyOutput("wh_status_donut", height = "350px") %>%
                  withSpinner(color = brand_blue)
              ),
              bs4Card(
                title = "On Hand, Allocated, Available and On Order", width = 8,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("wh_quantity_mix", height = "350px") %>%
                  withSpinner(color = brand_blue)
              )
            ),
            
            fluidRow(
              bs4Card(
                title = "Reorder Priority Matrix", width = 7,
                status = "warning", solidHeader = TRUE,
                plotlyOutput("wh_reorder_matrix", height = "420px") %>%
                  withSpinner(color = brand_amber)
              ),
              bs4Card(
                title = "Inventory Value by Warehouse", width = 5,
                status = "success", solidHeader = TRUE,
                plotlyOutput("wh_value_by_warehouse", height = "420px") %>%
                  withSpinner(color = brand_teal)
              )
            ),
            
            fluidRow(
              bs4Card(
                title = "Real-Time Warehouse Item Detail", width = 12,
                status = "gray-dark", solidHeader = TRUE,
                DTOutput("wh_inventory_table") %>% withSpinner(color = brand_slate),
                downloadButton(
                  "wh_download", "Export Warehouse Detail",
                  icon = icon("file-excel"), class = "btn-success"
                )
              )
            )
          ),
          
          tabPanel(
            "Inbound & Backorders",
            value = "inbound",
            
            fluidRow(
              bs4Card(
                title = NULL, width = 12, status = "white",
                fluidRow(
                  column(3, selectizeInput(
                    "inv_client", "Customer:",
                    choices = c(
                      "All Customers" = "all",
                      setNames(all_inventory_clients, all_inventory_clients)
                    ),
                    selected = "all"
                  )),
                  column(3, selectizeInput(
                    "inv_supplier", "Supplier:",
                    choices = c(
                      "All Suppliers" = "all",
                      setNames(all_inventory_suppliers, all_inventory_suppliers)
                    ),
                    selected = "all"
                  )),
                  column(2, selectInput(
                    "inv_status", "Delivery Status:",
                    choices = c(
                      "All Statuses" = "all",
                      setNames(all_inventory_statuses, all_inventory_statuses)
                    ),
                    selected = "all"
                  )),
                  column(2, selectInput(
                    "inv_delivery_year", "Delivery Year:",
                    choices = c("All Years" = "all", all_delivery_years),
                    selected = "all"
                  )),
                  column(2, selectInput(
                    "inv_delivery_month", "Delivery Month:",
                    choices = c("All Months" = "all", setNames(1:12, month.name)),
                    selected = "all"
                  ))
                ),
                fluidRow(
                  column(3, selectInput(
                    "inv_delivery_week", "Week of Month:",
                    choices = c("All Weeks" = "all"),
                    selected = "all"
                  )),
                  column(6, textInput(
                    "inv_search", "Search Part, PO or Description:",
                    placeholder = "Part number, PO, supplier, customer..."
                  )),
                  column(
                    3, br(),
                    actionButton(
                      "inv_delivery_reset", "Reset Inbound Filters",
                      icon = icon("undo"),
                      class = "btn-outline-secondary btn-block"
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              column(3, div(
                class = "kpi-card kpi-blue",
                tags$p("OPEN INBOUND VALUE"),
                tags$h2(textOutput("inv_open_value")),
                tags$p(class = "sub-text", "Open purchase-order balance")
              )),
              column(3, div(
                class = "kpi-card kpi-amber",
                tags$p("UNITS AWAITING RECEIPT"),
                tags$h2(textOutput("inv_backordered_qty")),
                tags$p(class = "sub-text", "Ordered minus received")
              )),
              column(3, div(
                class = "kpi-card kpi-navy",
                tags$p("PRODUCT LINES"),
                tags$h2(textOutput("inv_item_count")),
                tags$p(class = "sub-text", "Filtered inbound item lines")
              )),
              column(3, div(
                class = "kpi-card kpi-teal",
                tags$p("ACTIVE SUPPLIERS"),
                tags$h2(textOutput("inv_supplier_count")),
                tags$p(class = "sub-text", "Suppliers in selected inbound data")
              ))
            ),
            
            div(
              class = "definition-grid",
              metric_definition(
                "Backordered / Awaiting Receipt",
                "The portion of ordered quantity not yet received. This does not by itself mean late."
              ),
              metric_definition(
                "Overdue",
                "Units are still awaiting receipt and the scheduled delivery date has passed."
              ),
              metric_definition(
                "Open Inbound Value",
                "Open balance recorded on the purchase-order lines in Book6 or the future connected source."
              )
            ),
            
            fluidRow(
              bs4Card(
                title = "Open Inbound Value by Supplier", width = 6,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("inv_supplier_value", height = "390px") %>%
                  withSpinner(color = brand_blue)
              ),
              bs4Card(
                title = "Units Awaiting Receipt by Customer", width = 6,
                status = "warning", solidHeader = TRUE,
                plotlyOutput("inv_client_units", height = "390px") %>%
                  withSpinner(color = brand_amber)
              )
            ),
            
            fluidRow(
              bs4Card(
                title = "Top Parts Awaiting Receipt", width = 7,
                status = "info", solidHeader = TRUE,
                plotlyOutput("inv_part_backorders", height = "420px") %>%
                  withSpinner(color = brand_blue)
              ),
              bs4Card(
                title = "Inbound Timing Profile", width = 5,
                status = "secondary", solidHeader = TRUE,
                fluidRow(
                  column(6, actionButton(
                    "inv_clear_timing", "Clear Timing Focus",
                    icon = icon("times-circle"), class = "btn-outline-secondary btn-block"
                  )),
                  column(6, htmlOutput("inv_timing_focus_label"))
                ),
                plotlyOutput("inv_timing_donut", height = "420px") %>%
                  withSpinner(color = brand_slate)
              )
            ),
            
            fluidRow(
              bs4Card(
                title = "Inbound and Backorder Detail", width = 12,
                status = "gray-dark", solidHeader = TRUE,
                DTOutput("inventory_detail_table") %>% withSpinner(color = brand_slate)
              )
            )
          )
        ),
        
        report_action_bar(
          "report_inventory",
          "Export the summary for the active inventory view or build a detailed warehouse/inbound report."
        )
      ),
      
      ########################################################################
      # 3.10 SUPPLIER INTELLIGENCE & PROCUREMENT TAB
      ########################################################################
      bs4TabItem(
        tabName = "buyer_tool",
        
        fluidRow(
          bs4Card(
            title = "Supplier Selection Workbench", width = 12,
            status = "success", solidHeader = TRUE,
            icon = icon("balance-scale"),
            fluidRow(
              column(
                4,
                textInput(
                  "supplier_compare_search", "Part number / description focus:",
                  placeholder = "Optional: filter to comparable items"
                )
              ),
              column(
                4,
                selectizeInput(
                  "supplier_compare_suppliers", "Suppliers to compare:",
                  choices = setNames(quotation_supplier_choices, quotation_supplier_choices),
                  selected = character(0), multiple = TRUE,
                  options = list(
                    placeholder = "All suppliers unless selected...",
                    plugins = list("remove_button"), maxOptions = 5000
                  )
                )
              ),
              column(
                4,
                selectInput(
                  "supplier_compare_priority", "Decision priority:",
                  choices = c(
                    "Balanced cost, speed and risk" = "balanced",
                    "Lowest historical cost" = "cost",
                    "Fastest planned delivery" = "speed",
                    "Lowest delivery / backorder risk" = "risk"
                  ),
                  selected = "balanced"
                )
              )
            ),
            fluidRow(
              column(
                3,
                div(class = "kpi-card", style = "--kpi-accent:#5F7D67;",
                    tags$p("RECOMMENDED SUPPLIER"), tags$h2(textOutput("supplier_compare_best")),
                    tags$p(class = "sub-text", "Highest weighted decision score"))
              ),
              column(
                3,
                div(class = "kpi-card", style = "--kpi-accent:#2F6EA5;",
                    tags$p("LOWEST MEDIAN COST"), tags$h2(textOutput("supplier_compare_cheapest")),
                    tags$p(class = "sub-text", "Use an item focus for fair comparison"))
              ),
              column(
                3,
                div(class = "kpi-card", style = "--kpi-accent:#3C7474;",
                    tags$p("FASTEST PLANNED LEAD"), tags$h2(textOutput("supplier_compare_fastest")),
                    tags$p(class = "sub-text", "Historical order-to-delivery plan"))
              ),
              column(
                3,
                div(class = "kpi-card", style = "--kpi-accent:#9A6A27;",
                    tags$p("BEST RELIABILITY"), tags$h2(textOutput("supplier_compare_reliable")),
                    tags$p(class = "sub-text", "Lowest overdue and backorder exposure"))
              )
            ),
            interaction_tip(
              "Use a part number or description before comparing cost. The score combines median unit cost, planned lead time, overdue rate, backorder rate and open exposure."
            ),
            fluidRow(
              column(5, plotlyOutput("supplier_compare_score_plot", height = "390px") %>%
                       withSpinner(color = brand_sage)),
              column(7, DTOutput("supplier_compare_table") %>% withSpinner(color = brand_sage))
            )
          )
        ),
        
        fluidRow(
          column(3, div(
            class = "kpi-card kpi-blue",
            tags$p("PURCHASE ORDERS"),
            tags$h2(textOutput("supplier_kpi_po_count")),
            tags$p(class = "sub-text", "Distinct supplier POs in current global period")
          )),
          column(3, div(
            class = "kpi-card kpi-teal",
            tags$p("ACTIVE SUPPLIERS"),
            tags$h2(textOutput("supplier_kpi_active_suppliers")),
            tags$p(class = "sub-text", "Suppliers with order activity")
          )),
          column(3, div(
            class = "kpi-card kpi-amber",
            tags$p("OPEN PROCUREMENT VALUE"),
            tags$h2(textOutput("supplier_kpi_open_value")),
            tags$p(class = "sub-text", "Unreceived / open supplier exposure")
          )),
          column(3, div(
            class = "kpi-card kpi-coral",
            tags$p("BACKORDERED UNITS"),
            tags$h2(textOutput("supplier_kpi_backorders")),
            tags$p(class = "sub-text", "Units still awaiting receipt")
          ))
        ),
        
        fluidRow(
          bs4Card(
            title = "Seller Product & Service Directory", width = 12,
            status = "info", solidHeader = TRUE,
            icon = icon("search"),
            fluidRow(
              column(4, textInput(
                "seller_catalog_search", "Product / service / part search:",
                placeholder = "Part number, pump, inspection, calibration..."
              )),
              column(3, selectizeInput(
                "seller_catalog_supplier", "Seller / supplier:",
                choices = c("All Sellers" = "all", setNames(sort(unique(seller_catalog_data$Supplier)), sort(unique(seller_catalog_data$Supplier)))),
                selected = "all",
                options = list(placeholder = "All sellers", maxOptions = 5000)
              )),
              column(3, selectizeInput(
                "seller_catalog_category", "Category:",
                choices = c("All Categories" = "all", setNames(sort(unique(seller_catalog_data$Category)), sort(unique(seller_catalog_data$Category)))),
                selected = "all"
              )),
              column(2, selectInput(
                "seller_catalog_type", "Type:",
                choices = c("All" = "all", "Products" = "Product", "Services" = "Service"),
                selected = "all"
              ))
            ),
            interaction_tip(
              "Search by product, part number or service. Select a seller in the table or a supplier chart to focus the procurement workspace."
            ),
            DTOutput("seller_catalog_table") %>% withSpinner(color = brand_sky)
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Open Purchase-Order Value by Supplier", width = 6,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("buyer_supplier_value", height = "380px") %>%
              withSpinner(color = "#2F6EA5")
          ),
          bs4Card(
            title = "Supplier Delivery and Exposure Summary", width = 6,
            status = "info", solidHeader = TRUE,
            DTOutput("buyer_supplier_summary") %>% withSpinner(color = "#2F6EA5")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Purchase Order Lookup for Supplier and Client Analysis", width = 12,
            status = "olive", solidHeader = TRUE, collapsible = TRUE,
            fluidRow(
              column(3, textInput(
                "buyer_search_po", "Search PO / Part:",
                placeholder = "PO number or part number"
              )),
              column(3, selectizeInput(
                "buyer_search_client", "Customer:",
                choices = c("All" = "all", setNames(all_inventory_clients, all_inventory_clients)),
                selected = "all",
                options = list(placeholder = "Search customer...", maxOptions = 3000)
              )),
              column(3, selectizeInput(
                "buyer_search_supplier", "Seller / Supplier:",
                choices = c("All" = "all", setNames(all_inventory_suppliers, all_inventory_suppliers)),
                selected = "all",
                options = list(placeholder = "Search supplier...", maxOptions = 3000)
              )),
              column(3, selectizeInput(
                "buyer_search_buyer", "Buyer:",
                choices = c("All" = "all", setNames(all_inventory_buyers, all_inventory_buyers)),
                selected = "all",
                options = list(placeholder = "Search buyer...", maxOptions = 1000)
              ))
            ),
            DTOutput("buyer_po_lookup_table") %>% withSpinner(color = "#827717"),
            report_action_bar(
              "report_buyer",
              "Export the selected historical purchase orders and supplier analysis."
            )
          )
        )
      ),
      
      ########################################################################
      # 3.11 QUOTATION STUDIO TAB
      ########################################################################
      bs4TabItem(
        tabName = "quotation_studio",
        
        div(
          class = "quote-process-strip",
          div(class = "quote-process-step", tags$strong("1 · Customer"), "Identify the customer, RFQ and commercial context."),
          div(class = "quote-process-step", tags$strong("2 · History"), "Review prior orders and customer payment milestones."),
          div(class = "quote-process-step", tags$strong("3 · Vendor Cost"), "Build or clone editable quotation lines."),
          div(class = "quote-process-step", tags$strong("4 · Landed Cost"), "Add known order-specific freight, duty and direct costs."),
          div(class = "quote-process-step", tags$strong("5 · Financing"), "Model supplier payments, bank rate and cash exposure."),
          div(class = "quote-process-step", tags$strong("6 · Pricing Policy"), "Apply contingency, gross margin, discount and tax."),
          div(class = "quote-process-step", tags$strong("7 · Final Calculation"), "Review scenario impact and the complete price bridge."),
          div(class = "quote-process-step", tags$strong("8 · Customer Quote"), "Preview and export the final customer-facing quotation.")
        ),
        
        fluidRow(
          bs4Card(
            title = "1. Quotation Identity and Customer", width = 12,
            status = "primary", solidHeader = TRUE,
            icon = icon("file-signature"),
            div(
              class = "quote-toolbar",
              actionButton("quotation_new_quote", "New Quote", icon = icon("plus-circle"), class = "btn-primary"),
              actionButton("quotation_copy_quote", "Duplicate Number", icon = icon("copy"), class = "btn-outline-primary"),
              actionButton("quotation_save_draft", "Save Draft", icon = icon("save"), class = "btn-success"),
              checkboxInput("quotation_share_draft", "Share with buyers", value = FALSE),
              tags$span(class = "text-muted small", "Database defaults remain editable. A historical order can be cloned in Step 2. Saved drafts are stored in the central multi-user workspace.")
            ),
            fluidRow(
              column(3, textInput("quotation_number", "Quotation number:", value = paste0("Q-", format(Sys.time(), "%Y%m%d-%H%M")))),
              column(3, dateInput("quotation_date", "Quotation date:", value = Sys.Date())),
              column(3, numericInput("quotation_validity_days", "Validity (days):", value = 30, min = 1, max = 365, step = 1)),
              column(3, selectInput(
                "quotation_currency", "Quotation currency:",
                choices = c("USD — US Dollar" = "USD", "EUR — Euro" = "EUR", "GBP — Pound Sterling" = "GBP", "CAD — Canadian Dollar" = "CAD", "XAF — Central African CFA" = "XAF"),
                selected = "USD"
              ))
            ),
            fluidRow(
              column(4, selectizeInput(
                "quotation_customer_lookup", "Customer name lookup:",
                choices = c("Manual / New Customer" = "", setNames(quotation_customer_choices, quotation_customer_choices)),
                selected = "", options = list(placeholder = "Search customer name...", create = TRUE, maxOptions = 5000)
              )),
              column(4, textInput("quotation_customer_name", "Customer / billing name:", value = "")),
              column(2, textInput("quotation_customer_code", "Customer ID:", value = "")),
              column(2, textInput("quotation_rfq_reference", "RFQ / inquiry ref.:", value = ""))
            ),
            fluidRow(
              column(5, selectizeInput(
                "quotation_customer_id_lookup", "Customer ID / billing lookup:",
                choices = c("Select by customer ID" = ""),
                selected = "",
                options = list(placeholder = "Search AGC-xxxxx or company name...", maxOptions = 10000)
              )),
              column(7, uiOutput("quotation_customer_identity_status"))
            ),
            fluidRow(
              column(3, textInput("quotation_customer_contact", "Attention / contact:", value = "")),
              column(3, textInput("quotation_customer_email", "Customer email:", value = "")),
              column(3, selectizeInput(
                "quotation_prepared_by", "Prepared by:",
                choices = sort(unique(na.omit(c(all_buyers, all_staff_rev)))),
                selected = if (length(all_buyers) > 0) all_buyers[[1]] else "",
                options = list(create = TRUE, placeholder = "Buyer / representative")
              )),
              column(3, textInput("quotation_delivery_location", "Delivery location:", value = ""))
            ),
            fluidRow(
              column(5, textAreaInput("quotation_customer_address", "Customer address:", value = "", height = "78px", width = "100%")),
              column(3, textInput("quotation_payment_terms", "Customer payment terms:", value = "NET 30 DAYS")),
              column(2, selectInput("quotation_incoterm", "Incoterm:", choices = c("Not specified", "EXW", "FCA", "FOB", "CFR", "CIF", "CPT", "CIP", "DAP", "DPU", "DDP"), selected = "Not specified")),
              column(2, textInput("quotation_delivery_statement", "Delivery statement:", value = "To be confirmed"))
            ),
            fluidRow(
              column(4, textInput("quotation_company_name", "Seller name on document:", value = "Aurelis Global Supply, Inc.")),
              column(4, textInput("quotation_company_contact", "Seller phone / email:", value = "")),
              column(4, textInput("quotation_company_address", "Seller address:", value = ""))
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "2A. Customer Order History — Clone and Modify", width = 7,
            status = "info", solidHeader = TRUE,
            icon = icon("history"),
            fluidRow(
              column(5, selectizeInput(
                "quotation_history_customer", "Customer history:",
                choices = c("Select customer" = "", setNames(quotation_customer_choices, quotation_customer_choices)),
                selected = "", options = list(placeholder = "Choose customer to browse past orders", maxOptions = 5000)
              )),
              column(4, selectizeInput(
                "quotation_source_po", "Past order:",
                choices = c("Select a customer first" = ""), selected = "",
                options = list(placeholder = "Choose a historical PO", maxOptions = 10000)
              )),
              column(3, br(), actionButton("quotation_import_po", "Clone Into New Quote", icon = icon("clone"), class = "btn-primary btn-block"))
            ),
            uiOutput("quotation_history_order_summary"),
            DTOutput("quotation_history_order_table") %>% withSpinner(color = brand_blue),
            tags$p(class = "metric-definition-note", "Cloning copies the historical order lines into the editable quote table. You can then change quantity, description, supplier, cost, discount or any commercial field without changing the original historical record.")
          ),
          
          bs4Card(
            title = "2B. Customer Payment History, Milestones & Exposure Profile", width = 5,
            status = "warning", solidHeader = TRUE,
            icon = icon("hourglass-half"),
            uiOutput("quotation_customer_milestone"),
            uiOutput("quotation_customer_ar_profile"),
            actionButton("quotation_apply_customer_milestones", "Apply Milestones to Finance Scenario", icon = icon("arrow-down"), class = "btn-warning btn-block"),
            tags$p(class = "metric-definition-note", "Customer payment milestones affect the customer-payment months in the finance engine. They do not change the supplier disbursement schedule, which is entered separately in Step 5.")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "3. Editable Line Items and Vendor Cost", width = 12,
            status = "info", solidHeader = TRUE,
            icon = icon("list-alt"),
            fluidRow(
              column(7, fileInput("quotation_import_csv", "Import a line-item CSV:", accept = c(".csv", "text/csv"))),
              column(5, tags$p(class = "text-muted mt-4", "Use Step 2 to clone a customer's prior order, or import / enter new lines here."))
            ),
            div(
              class = "quote-toolbar",
              actionButton("quotation_add_line", "Add Line", icon = icon("plus"), class = "btn-success"),
              actionButton("quotation_duplicate_line", "Duplicate Selected", icon = icon("copy"), class = "btn-outline-success"),
              actionButton("quotation_delete_line", "Delete Selected", icon = icon("trash"), class = "btn-outline-danger"),
              actionButton("quotation_reset_lines", "Clear Lines", icon = icon("eraser"), class = "btn-outline-secondary"),
              downloadButton("quotation_line_template", "CSV Template", icon = icon("file-csv"), class = "btn-outline-primary")
            ),
            DTOutput("quotation_line_table") %>% withSpinner(color = brand_blue),
            fluidRow(
              column(4, checkboxInput("quotation_use_cost_override", "Use a manual total-item-cost override", value = TRUE)),
              column(4, numericInput("quotation_cost_override", "Manual total item cost:", value = 235105.93, min = 0, step = 0.01)),
              column(4, uiOutput("quotation_line_cost_status"))
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "4. Landed-Cost Inputs — Known Order-Specific Expenses", width = 12,
            status = "secondary", solidHeader = TRUE,
            icon = icon("boxes"),
            tags$p(
              class = "text-muted",
              "Enter expenses that Aurelis expects to incur because of this order. Leave a field at 0 when it does not apply or is already captured elsewhere. The target gross margin is applied after these costs; it should not be used to hide known direct expenses."
            ),
            fluidRow(
              column(3, numericInput("quotation_freight", "Freight / shipping:", value = 0, min = 0, step = 0.01)),
              column(3, numericInput("quotation_insurance", "Cargo insurance:", value = 0, min = 0, step = 0.01)),
              column(3, numericInput("quotation_duty_pct", "Duty / customs — order-specific (% of items):", value = 0, min = 0, step = 0.1)),
              column(3, numericInput("quotation_handling_pct", "Procurement / handling overhead — order-specific (%):", value = 0, min = 0, step = 0.1))
            ),
            fluidRow(
              column(3, numericInput("quotation_qaqc", "Quality assurance / quality control (QA/QC) / inspection:", value = 0, min = 0, step = 0.01)),
              column(3, numericInput("quotation_bank_fees", "Bank / transfer fees (non-interest):", value = 0, min = 0, step = 0.01)),
              column(3, numericInput("quotation_packing", "Packing / warehouse:", value = 0, min = 0, step = 0.01)),
              column(3, numericInput("quotation_other_costs", "Other direct costs:", value = 0, min = 0, step = 0.01))
            ),
            div(class = "quote-help-grid",
                div(class = "quote-help-card", tags$strong("QA/QC"), "Quality Assurance / Quality Control: inspection, testing, certification or verification required for this specific order."),
                div(class = "quote-help-card", tags$strong("Duty / Customs"), "Use for customs duty, import tax or clearance charges attributable to the order. Enter 0 when not applicable."),
                div(class = "quote-help-card", tags$strong("Procurement / Handling"), "Use only for incremental sourcing, procurement or handling overhead intentionally allocated to this order. Enter 0 if your company policy absorbs it elsewhere."),
                div(class = "quote-help-card", tags$strong("Gross Margin Comes Later"), "Gross margin is calculated after item cost, landed costs, financing and contingency. This prevents known costs from being mistaken for profit.")),
            div(class = "quote-math-note", uiOutput("quotation_cost_formula_note"))
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "5. Finance-Charge Scenario Engine", width = 12,
            status = "warning", solidHeader = TRUE,
            icon = icon("university"),
            fluidRow(
              column(4, selectInput(
                "quotation_interest_method", "Interest method:",
                choices = c("Simple monthly interest — matches uploaded workbook" = "simple", "Monthly compound interest" = "compound"), selected = "simple"
              )),
              column(4, selectInput(
                "quotation_finance_base", "Finance-charge base:",
                choices = c("Vendor item cost only — matches workbook" = "items", "Full landed cost before finance" = "landed"), selected = "items"
              )),
              column(4, selectInput(
                "quotation_payment_preset", "Supplier disbursement preset:",
                choices = c(
                  "Uploaded-model example: 20% advance financed" = "template20",
                  "20% advance + 80% at 60 days" = "20_80_60",
                  "100% payment in advance" = "advance100",
                  "100% at 30 days" = "net30", "100% at 60 days" = "net60", "100% at 90 days" = "net90",
                  "Custom schedule" = "custom"
                ), selected = "template20"
              ))
            ),
            tags$h5("Timing and bank-rate scenarios"),
            tags$table(
              class = "quote-input-matrix",
              tags$thead(tags$tr(tags$th("Input"), tags$th(class = "quote-scenario-min", "Minimum"), tags$th(class = "quote-scenario-average", "Average"), tags$th(class = "quote-scenario-max", "Maximum"))),
              tags$tbody(
                tags$tr(tags$td("Vendor lead time (months)"), tags$td(numericInput("quotation_vendor_min", NULL, 3, min = 0, step = .25)), tags$td(numericInput("quotation_vendor_avg", NULL, 3, min = 0, step = .25)), tags$td(numericInput("quotation_vendor_max", NULL, 4, min = 0, step = .25))),
                tags$tr(tags$td("Aurelis procurement, QA/QC, shipping and invoicing (months)"), tags$td(numericInput("quotation_aurelis_min", NULL, 1, min = 0, step = .25)), tags$td(numericInput("quotation_aurelis_avg", NULL, 2, min = 0, step = .25)), tags$td(numericInput("quotation_aurelis_max", NULL, 3, min = 0, step = .25))),
                tags$tr(tags$td("Customer payment-history time after invoice (months)"), tags$td(numericInput("quotation_client_pay_min", NULL, 2, min = 0, step = .25)), tags$td(numericInput("quotation_client_pay_avg", NULL, 3, min = 0, step = .25)), tags$td(numericInput("quotation_client_pay_max", NULL, 4, min = 0, step = .25))),
                tags$tr(tags$td("Monthly bank interest rate (%)"), tags$td(numericInput("quotation_rate_min", NULL, 1.5, min = 0, step = .1)), tags$td(numericInput("quotation_rate_avg", NULL, 2, min = 0, step = .1)), tags$td(numericInput("quotation_rate_max", NULL, 2.5, min = 0, step = .1)))
              )
            ),
            tags$h5(class = "mt-4", "Supplier/vendor cash-disbursement schedule"),
            tags$p(class = "text-muted", "Enter the portion of the selected finance base paid to suppliers at each milestone. Values may total less than 100% when only part of the deal requires bank financing."),
            tags$table(
              class = "quote-input-matrix",
              tags$thead(tags$tr(tags$th("Payment milestone"), tags$th(class = "quote-scenario-min", "Minimum %"), tags$th(class = "quote-scenario-average", "Average %"), tags$th(class = "quote-scenario-max", "Maximum %"))),
              tags$tbody(
                tags$tr(tags$td("Advance payment — month 0"), tags$td(numericInput("quotation_pay_0_min", NULL, 20, min = 0, step = 1)), tags$td(numericInput("quotation_pay_0_avg", NULL, 20, min = 0, step = 1)), tags$td(numericInput("quotation_pay_0_max", NULL, 20, min = 0, step = 1))),
                tags$tr(tags$td("Second payment — 30 days / month 1"), tags$td(numericInput("quotation_pay_1_min", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_1_avg", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_1_max", NULL, 0, min = 0, step = 1))),
                tags$tr(tags$td("Third payment — 60 days / month 2"), tags$td(numericInput("quotation_pay_2_min", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_2_avg", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_2_max", NULL, 0, min = 0, step = 1))),
                tags$tr(tags$td("Fourth payment — 90 days / month 3"), tags$td(numericInput("quotation_pay_3_min", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_3_avg", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_3_max", NULL, 0, min = 0, step = 1))),
                tags$tr(tags$td("Fifth payment — 120 days / month 4"), tags$td(numericInput("quotation_pay_4_min", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_4_avg", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_4_max", NULL, 0, min = 0, step = 1))),
                tags$tr(tags$td("Sixth payment — 150 days / month 5"), tags$td(numericInput("quotation_pay_5_min", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_5_avg", NULL, 0, min = 0, step = 1)), tags$td(numericInput("quotation_pay_5_max", NULL, 0, min = 0, step = 1)))
              )
            ),
            uiOutput("quotation_schedule_status"),
            div(class = "quote-math-note", tags$strong("Finance-charge math: "), "for each supplier payment, the calculator applies the selected monthly bank rate to the financed amount for the remaining months until expected customer payment, then sums all six milestones.")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "6. Commercial Pricing Policy", width = 12,
            status = "success", solidHeader = TRUE,
            icon = icon("percentage"),
            tags$p(class = "text-muted", "Target gross margin is the recommended default. It is applied after modeled order costs, financing and contingency, so the target represents protected commercial margin rather than a substitute for known expenses."),
            fluidRow(
              column(3, selectInput("quotation_selected_scenario", "Official quote scenario:", choices = c("Minimum" = "Min", "Average / expected" = "Average", "Maximum / conservative" = "Max"), selected = "Average")),
              column(3, selectInput("quotation_pricing_method", "Pricing method:", choices = c("Target gross margin — recommended" = "margin", "Cost markup — advanced" = "markup"), selected = "margin")),
              column(3, numericInput("quotation_pricing_rate", "Target margin / markup (%):", value = 30, min = 0, max = 99, step = 0.5)),
              column(3, selectInput("quotation_contingency_profile", "Contingency risk profile:", choices = c("Low — costs confirmed" = "low", "Standard — normal order uncertainty" = "standard", "Elevated — several estimates / longer lead" = "elevated", "High — volatile or complex transaction" = "high", "Manual only" = "manual"), selected = "standard"))
            ),
            fluidRow(
              column(3, numericInput("quotation_contingency_pct", "Contingency reserve (%):", value = 3, min = 0, max = 25, step = 0.5)),
              column(3, br(), actionButton("quotation_apply_contingency", "Apply Recommended Contingency", icon = icon("magic"), class = "btn-outline-success btn-block")),
              column(6, uiOutput("quotation_contingency_recommendation"))
            ),
            fluidRow(
              column(4, selectInput(
                "quotation_discount_mode", "Discount behavior:",
                choices = c(
                  "Presentation discount — protect target price" = "protected",
                  "Real commercial discount — reduce final selling price" = "real"
                ), selected = "protected"
              )),
              column(2, numericInput("quotation_discount_pct", "Discount (%):", value = 0, min = 0, max = 99, step = 0.5)),
              column(2, numericInput("quotation_tax_pct", "Sales tax / VAT (%):", value = 0, min = 0, step = 0.1)),
              column(2, selectInput("quotation_rounding", "Round final subtotal up to:", choices = c("No rounding" = "0", "0.01" = "0.01", "1" = "1", "5" = "5", "10" = "10", "50" = "50", "100" = "100"), selected = "0.01")),
              column(2, uiOutput("quotation_discount_explanation"))
            ),
            fluidRow(
              column(3, div(class = "kpi-card", style = "--kpi-accent:#2F6EA5;", tags$p("ITEM COST"), tags$h2(textOutput("quotation_kpi_item_cost")), tags$p(class = "sub-text", "Editable lines or manual override"))),
              column(3, div(class = "kpi-card", style = "--kpi-accent:#9A6A27;", tags$p("FINANCE CHARGE"), tags$h2(textOutput("quotation_kpi_finance")), tags$p(class = "sub-text", "Selected timing scenario"))),
              column(3, div(class = "kpi-card", style = "--kpi-accent:#5F7D67;", tags$p("QUOTE SUBTOTAL"), tags$h2(textOutput("quotation_kpi_subtotal")), tags$p(class = "sub-text", "After commercial discount, before tax"))),
              column(3, div(class = "kpi-card", style = "--kpi-accent:#17324D;", tags$p("GRAND TOTAL"), tags$h2(textOutput("quotation_kpi_total")), tags$p(class = "sub-text", "Customer-facing total")))
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "7. Final Pricing Calculation & Scenario Review", width = 12,
            status = "gray-dark", solidHeader = TRUE,
            uiOutput("quotation_final_price_summary"),
            DTOutput("quotation_scenario_table") %>% withSpinner(color = brand_navy),
            tags$hr(),
            div(class = "quote-bridge-wrap", plotlyOutput("quotation_cost_bridge", height = "470px") %>% withSpinner(color = brand_navy)),
            uiOutput("quotation_risk_flags")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "8. Final Customer Quotation — Preview and Export", width = 12,
            status = "success", solidHeader = TRUE,
            icon = icon("file-pdf"),
            fluidRow(
              column(4, textAreaInput("quotation_scope_notes", "Scope / commercial note shown on quotation:", value = "Thank you for the opportunity to quote. Prices and delivery remain subject to final supplier confirmation.", height = "90px", width = "100%")),
              column(4, textAreaInput("quotation_terms_notes", "Terms and conditions shown on quotation:", value = "Quotation validity, payment terms, delivery terms, warranty, taxes and exclusions are governed by the fields above and any written notes in this section.", height = "90px", width = "100%")),
              column(4, textAreaInput("quotation_internal_notes", "Internal buyer notes — not printed:", value = "", height = "90px", width = "100%"))
            ),
            div(
              class = "quote-toolbar",
              downloadButton("quotation_download_pdf", "Download Quotation PDF", icon = icon("file-pdf"), class = "btn-danger"),
              downloadButton("quotation_download_excel", "Download Calculation Workbook", icon = icon("file-excel"), class = "btn-success"),
              tags$span(class = "text-muted small", "This preview is the final customer-facing document. Internal cost, financing, contingency and margin are not printed on the customer PDF.")
            ),
            uiOutput("quotation_preview") %>% withSpinner(color = brand_sage)
          )
        )
      ),
      
      ########################################################################
      # 3.12 RELATIONSHIP DIRECTORY TAB
      ########################################################################
      bs4TabItem(
        tabName = "contacts_tab",
        
        fluidRow(
          bs4Card(
            title = NULL, width = 12, status = "white",
            fluidRow(
              column(
                2,
                selectInput(
                  "contact_type", "Relationship Category:",
                  choices = c("Customer", "Buyer", "Seller / Supplier"),
                  selected = "Customer"
                )
              ),
              column(
                3,
                selectizeInput(
                  "contact_entity", "Select Company / Person:",
                  choices = c("All" = "all"), selected = "all",
                  options = list(
                    placeholder = "Search relationship...",
                    maxOptions = 5000
                  )
                )
              ),
              column(
                2,
                selectInput(
                  "contact_country", "Country:",
                  choices = c(
                    "All" = "all",
                    sort(unique(na.omit(relationship_directory$Country)))
                  ),
                  selected = "all"
                )
              ),
              column(
                3,
                textInput(
                  "contact_search", "Search Contact / Role / Entity:",
                  placeholder = "Name, department, email, supplier..."
                )
              ),
              column(
                2, br(),
                downloadButton(
                  "contact_download", "Export Directory",
                  class = "btn-dark btn-block"
                )
              )
            ),
            div(
              class = "directory-note",
              icon("address-card"),
              " Customer contacts come from the Contacts sheet. Buyer and seller records are enriched with operational history. Email and phone fields will populate automatically when the optional Access / QuickBooks DIRECTORY query supplies them."
            )
          )
        ),
        
        fluidRow(
          column(3, bs4ValueBox(
            value = textOutput("contact_total"), subtitle = "Directory Records",
            icon = icon("users"), color = "teal", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("contact_entities"), subtitle = "Distinct Relationships",
            icon = icon("building"), color = "purple", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("contact_order_count"), subtitle = "Purchase Orders in View",
            icon = icon("file-contract"), color = "primary", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("contact_order_value"), subtitle = "Order Value in View",
            icon = icon("dollar-sign"), color = "orange", width = 12
          ))
        ),
        
        fluidRow(
          bs4Card(
            title = "Selected Relationship Profile", width = 5,
            status = "indigo", solidHeader = TRUE,
            uiOutput("contact_profile")
          ),
          bs4Card(
            title = "Monthly Activity History", width = 7,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("contact_activity_trend", height = "340px") %>%
              withSpinner(color = "#2F6EA5")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Buyer–Customer–Seller Relationship Flow", width = 7,
            status = "purple", solidHeader = TRUE,
            fluidRow(
              column(5, selectInput(
                "contact_flow_metric", "Relationship measure:",
                choices = c(
                  "Order value" = "value",
                  "Purchase-order count" = "orders",
                  "Backordered exposure" = "backorders"
                ),
                selected = "value"
              )),
              column(4, selectInput(
                "contact_flow_limit", "Relationships shown:",
                choices = c("Top 15" = 15, "Top 25" = 25, "Top 40" = 40, "Top 60" = 60),
                selected = 25
              )),
              column(3, br(), actionButton(
                "contact_flow_reset", "Reset Network",
                icon = icon("undo"), class = "btn-outline-secondary btn-block"
              ))
            ),
            tags$p(class = "chart-toolbar-note", "Click a node or use the directory filters to focus the network. The Sankey recalculates using the selected value, PO-count or backorder measure."),
            plotlyOutput("contact_relationship_flow", height = "430px") %>%
              withSpinner(color = "#64748B")
          ),
          bs4Card(
            title = "Highest-Value Counterparties", width = 5,
            status = "teal", solidHeader = TRUE,
            plotlyOutput("contact_counterparty_chart", height = "430px") %>%
              withSpinner(color = "#3C7474")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Seller / Supplier Product & Service Directory", width = 7,
            status = "info", solidHeader = TRUE,
            fluidRow(
              column(6, textInput(
                "contact_catalog_search", "Lookup product / service:",
                placeholder = "Search part number, description, category or service"
              )),
              column(6, selectizeInput(
                "contact_catalog_supplier", "Seller / supplier:",
                choices = c("All Sellers" = "all", setNames(sort(unique(seller_catalog_data$Supplier)), sort(unique(seller_catalog_data$Supplier)))),
                selected = "all"
              ))
            ),
            DTOutput("contact_seller_catalog_table") %>% withSpinner(color = brand_sky)
          ),
          bs4Card(
            title = "Register a New Client", width = 5,
            status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            tags$p(class = "metric-definition-note", "This writes only to Aurelis_User_Clients.csv inside the public demo data folder. It never modifies production Excel, Access or QuickBooks data."),
            fluidRow(
              column(6, textInput("new_client_company", "Company / billing name:", value = "")),
              column(6, textInput("new_client_id", "Customer ID (optional):", value = "", placeholder = "Auto-generated if blank"))
            ),
            fluidRow(
              column(4, selectizeInput("new_client_country", "Country:", choices = sort(unique(na.omit(c("United States", relationship_directory$Country, geography_data$Country)))), selected = "United States", options = list(create = TRUE))),
              column(4, textInput("new_client_region", "Region / state / province:", value = "")),
              column(4, textInput("new_client_city", "City / town:", value = ""))
            ),
            fluidRow(
              column(4, textInput("new_client_parent", "Parent company:", value = "Independent Entity")),
              column(4, selectInput("new_client_terms", "Payment terms:", choices = sort(unique(quotation_terms_reference$Terms)), selected = "NET 30 DAYS")),
              column(4, selectizeInput("new_client_rep", "Assigned Aurelis representative:", choices = sort(unique(c(all_staff_rev, all_buyers))), selected = if (length(all_staff_rev) > 0) all_staff_rev[[1]] else ""))
            ),
            fluidRow(
              column(4, numericInput("new_client_pay_min", "Fast payment (months):", value = 0.8, min = 0, step = .1)),
              column(4, numericInput("new_client_pay_avg", "Average payment (months):", value = 1.1, min = 0, step = .1)),
              column(4, numericInput("new_client_pay_max", "Slow payment (months):", value = 1.8, min = 0, step = .1))
            ),
            textInput("new_client_contact", "Primary contact:", value = ""),
            fluidRow(
              column(6, textInput("new_client_email", "Email:", value = "")),
              column(6, textInput("new_client_phone", "Phone:", value = ""))
            ),
            textInput("new_client_address", "Billing / office address:", value = ""),
            fluidRow(
              column(6, textInput("new_client_department", "Department:", value = "Procurement")),
              column(6, textInput("new_client_position", "Position:", value = "Procurement Manager"))
            ),
            actionButton("register_new_client", "Register Client", icon = icon("user-plus"), class = "btn-success btn-block"),
            br(),
            uiOutput("new_client_registration_status")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Complete Order Interaction History", width = 12,
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            DTOutput("contact_history_table") %>% withSpinner(color = "#0288D1")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Contact and Relationship Directory", width = 12,
            status = "gray-dark", solidHeader = TRUE,
            DTOutput("contact_table") %>% withSpinner(color = "#424242"),
            report_action_bar(
              "report_contacts",
              "Export the filtered contact directory and the selected relationship history."
            )
          )
        )
      ),
      
      ########################################################################
      # 3.12 DATA CONNECTIONS & PROCESS TAB
      ########################################################################
      bs4TabItem(
        tabName = "misc_tab",
        
        fluidRow(
          column(3, bs4ValueBox(
            value = textOutput("connection_mode"), subtitle = "Demo Source Mode",
            icon = icon("database"), color = "primary", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("connection_access_config"), subtitle = "Synthetic Dataset",
            icon = icon("table"), color = "info", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("connection_qb_config"), subtitle = "External Accounting",
            icon = icon("book"), color = "warning", width = 12
          )),
          column(3, bs4ValueBox(
            value = textOutput("connection_fallback"), subtitle = "Production Data Access",
            icon = icon("file-excel"), color = "success", width = 12
          ))
        ),
        
        fluidRow(
          bs4Card(
            title = "Synthetic Dataset Refresh", width = 12,
            status = "success", solidHeader = TRUE,
            fluidRow(
              column(3, tags$strong("Automatic interval"), tags$div(textOutput("live_refresh_interval"))),
              column(3, tags$strong("Last successful refresh"), tags$div(textOutput("live_refresh_last"))),
              column(3, tags$strong("Refresh status"), tags$div(textOutput("live_refresh_state"))),
              column(3, actionButton(
                "refresh_live_data_connections", "Reload Demo Dataset",
                icon = icon("sync-alt"), class = "btn-success btn-block"
              ))
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Connection Tests", width = 5,
            status = "indigo", solidHeader = TRUE,
            tags$p(
              "This public build runs only from the included synthetic CSV package. Production databases, accounting systems, shared drives and real Excel files are disabled."
            ),
            fluidRow(
              column(5, actionButton(
                "test_access_connection", "Validate Demo Dataset",
                icon = icon("database"), class = "btn-info btn-block"
              )),
              column(7,
                     div(
                       class = "alert alert-light mb-0",
                       icon("pause-circle"),
                       tags$strong(" External accounting disabled"),
                       tags$span(" — this public build remains fully isolated from production accounting files.")
                     )
              )
            ),
            br(),
            uiOutput("connection_test_status")
          ),
          bs4Card(
            title = "Datasets Loaded by the Dashboard", width = 7,
            status = "primary", solidHeader = TRUE,
            DTOutput("connection_source_table")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Adaptive Import & Schema Diagnostics", width = 12,
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            tags$p(
              "The Signature build normalizes common field-name variations, protects identifiers as character data, parses multiple date formats, cleans currency/percentage characters, and supplies safe defaults for non-critical missing fields."
            ),
            DTOutput("connection_schema_diagnostics"),
            tags$p(
              class = "metric-definition-note",
              "A diagnostic note does not automatically mean the dataset is invalid. It documents schema adaptation so field substitutions are visible instead of silently failing."
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Demo Dataset Contract", width = 12,
            status = "secondary", solidHeader = TRUE, collapsible = TRUE,
            tags$p(
              "The public dashboard is intentionally isolated. The table below documents the synthetic data contract used by the demonstration build."
            ),
            DTOutput("connection_query_template_table"),
            tags$h5("Public-demo isolation"),
            tags$pre(
              class = "connection-code",
              paste(
                "PUBLIC DEMO MODE = SYNTHETIC ONLY",
                "Data folder: ./data/",
                "Production Access / QuickBooks / shared drives: DISABLED",
                "All customers, buyers, suppliers, contacts and financial values are fictional.",
                sep = "
"
              )
            ),
            tags$p(
              tags$strong("External accounting:"),
              " intentionally disabled in the public build. Production connectors are not bundled, referenced or contacted."
            )
          )
        ),
        
        fluidRow(
          bs4Card(title = "Process Capture & Documentation", width = 12, status = "indigo", solidHeader = TRUE,
                  icon = icon("clipboard-list"),
                  div(class = "p-3",
                      tags$h4("Purpose: Capture all operational processes"),
                      tags$p("Use this section to document processes, track misc tasks, and maintain operational notes."),
                      hr(),
                      fluidRow(
                        column(6,
                               tags$h5(icon("list-check"), " Active Process Checklist"),
                               tags$div(class = "p-3", style = "background: #f8f9fa; border-radius: 8px;",
                                        tags$ul(
                                          tags$li("PO Creation -> Approval -> Sending to Supplier"),
                                          tags$li("Invoice Receipt -> QuickBooks Entry -> Payment Follow-up"),
                                          tags$li("Quote Request -> Supplier Comparison -> Quotation Studio -> Client PDF"),
                                          tags$li("AR Aging Review -> Client Contact -> Payment Collection"),
                                          tags$li("New Supplier Onboarding -> Database Update -> First Order")
                                        )
                               )
                        ),
                        column(6,
                               tags$h5(icon("lightbulb"), " Future Enhancements"),
                               tags$div(class = "p-3", style = "background: #f8f9fa; border-radius: 8px;",
                                        tags$ul(
                                          tags$li("External Accounting integration after Admin approval"),
                                          tags$li("Automated email alerts for overdue AR"),
                                          tags$li("Supplier rating system"),
                                          tags$li("Document attachment per PO"),
                                          tags$li("Role-based access control"),
                                          tags$li("Automated report generation & email")
                                        )
                               )
                        )
                      ),
                      hr(),
                      tags$h5(icon("sticky-note"), " Quick Notes"),
                      textAreaInput("misc_notes", NULL, value = "", 
                                    placeholder = "Type operational notes here... (Note: these are session-only, not persisted)",
                                    width = "100%", height = "150px", resize = "vertical"),
                      actionButton("misc_save_note", "Save Note (Session)", class = "btn-indigo", icon = icon("save"))
                  )
          )
        ),
        
        # Data Summary
        fluidRow(
          bs4Card(title = "Data Sources Summary", width = 12, status = "secondary", solidHeader = TRUE,
                  fluidRow(
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Daily PO"), tags$h3(nrow(daily_po)), tags$p("Records"))),
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Revenue"), tags$h3(nrow(revenue_data)), tags$p("Records"))),
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Accounts Receivable"), tags$h3(nrow(ar_data)), tags$p("Records"))),
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Inbound Orders"), tags$h3(nrow(inventory_data)), tags$p("Lines"))),
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Warehouse Items"), tags$h3(nrow(warehouse_inventory_data)), tags$p(
                                    ifelse(warehouse_inventory_available, "Connected records", "Proxy records")
                                  ))),
                    column(2, div(class = "text-center p-3",
                                  tags$h5("Contacts"), tags$h3(nrow(contacts_data)), tags$p("Records")))
                  ),
                  report_action_bar("report_misc", "Create a data-source and process summary report.")
          )
        )
      )

      ,bs4TabItem(
        tabName = "product_intelligence",
        fluidRow(
          column(
            width = 12,
            div(
              class = "prism-masthead",
              div(
                class = "prism-masthead-copy",
                div(class = "prism-masthead-kicker", icon("gem"), "AURELIS PRODUCT INTELLIGENCE"),
                tags$h2("Find the right product before it becomes a quotation."),
                tags$p("A focused recommendation studio built on your live catalog, supplier lead times, cost signals, and inventory position."),
                div(
                  class = "prism-mode-pill",
                  icon("shield-alt"),
                  tags$span("Decision-grade catalog intelligence · synthetic demo data")
                )
              ),
              div(
                class = "prism-masthead-stats",
                div(class = "prism-masthead-stat", tags$span("Catalog records"), tags$strong(textOutput("pi_catalog_count", inline = TRUE))),
                div(class = "prism-masthead-stat", tags$span("Available sellers"), tags$strong(textOutput("pi_supplier_count", inline = TRUE))),
                div(class = "prism-masthead-stat", tags$span("Recommended view"), tags$strong("Live"))
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            bs4Card(
              title = "Recommendation brief", width = 12, status = "primary", solidHeader = TRUE,
              textInput("pi_search", "What are you sourcing?", placeholder = "pump, valve, actuator, service..."),
              sliderInput("pi_budget", "Target unit cost", min = 0, max = 500000, value = 150000, step = 5000, pre = "$", sep = ","),
              selectInput("pi_category", "Category", choices = c("All categories" = "all")),
              selectInput("pi_stock", "Availability", choices = c("Any availability" = "all", "In stock" = "in_stock", "Backorder risk" = "risk")),
              sliderInput("pi_top_n", "Recommendations", min = 5, max = 20, value = 10, step = 1),
              div(class = "pi-brief-note", icon("wand-magic-sparkles"), tags$span("Rankings balance relevance, seller preference, price fit, lead time, and operational availability."))
            ),
            bs4Card(
              title = "Priority recommendation", width = 12, status = "warning", solidHeader = TRUE,
              uiOutput("pi_priority_card")
            )
          ),
          column(
            width = 8,
            bs4Card(
              title = "Recommended products", width = 12, status = "primary", solidHeader = TRUE,
              DTOutput("pi_rank_table")
            ),
            bs4Card(
              title = "Cost and delivery landscape", width = 12, status = "info", solidHeader = TRUE,
              plotlyOutput("pi_landscape_plot", height = "330px")
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            bs4Card(
              title = "Catalog explorer", width = 12, status = "secondary", solidHeader = TRUE,
              DTOutput("pi_catalog_table")
            )
          ),
          column(
            width = 5,
            bs4Card(
              title = "Client inquiry studio", width = 12, status = "success", solidHeader = TRUE,
              textInput("pi_client", "Client", value = "Northstar Energy"),
              textAreaInput("pi_request", "Request", rows = 4, placeholder = "Describe the operating need, delivery expectation, and constraints."),
              actionButton("pi_generate_inquiry", "Generate recommendation brief", icon = icon("file-signature"), class = "btn-primary"),
              br(), br(),
              uiOutput("pi_inquiry_result")
            )
          )
        )
      )
      
    ) # end bs4TabItems
  ), # end body
  
  # Footer
  footer = bs4DashFooter(
    left = tags$div(
      class = "footer-brand",
      tags$img(src = aurelis_logo_header_source, alt = "Aurelis logo"),
      tags$span("Aurelis Global Supply • Operations")
    ),
    right = tags$div(paste0("Data Period: ", min(all_years), " - ", max(all_years)))
  )
)

################################################################################
# SECTION 4: SERVER LOGIC
################################################################################

server <- function(input, output, session) {

  access_state <- reactiveVal(list(
    authenticated = !isTRUE(AURELIS_ACCESS_CONTROL_ENABLED),
    email = if (isTRUE(AURELIS_ACCESS_CONTROL_ENABLED)) "" else "analyst@aurelis.local",
    role = if (isTRUE(AURELIS_ACCESS_CONTROL_ENABLED)) NULL else "Analyst"
  ))

  output$aurelis_access_portal <- renderUI({
    state <- access_state()
    if (!isTRUE(AURELIS_ACCESS_CONTROL_ENABLED) || isTRUE(state$authenticated)) return(NULL)
    div(
      class = "aurelis-access-portal",
      div(
        class = "aurelis-access-portal-card",
        tags$img(src = aurelis_logo_header_source, alt = "Aurelis Global Supply"),
        tags$h1("Enter the Aurelis command layer"),
        tags$p("Sign in with your company identity. The access key maps the identity to the workspace designed for that position."),
        textInput("aurelis_login_email", "Work email", placeholder = "name@company.com"),
        passwordInput("aurelis_login_password", "Password", placeholder = "Company password"),
        textInput("aurelis_login_key", "Access key", placeholder = "Assigned role key"),
        actionButton("aurelis_login_submit", "Open command workspace", icon = icon("arrow-right"), class = "btn-primary"),
        tags$p(class = "aurelis-access-portal-note", textOutput("aurelis_login_message"))
      )
    )
  })

  output$aurelis_login_message <- renderText({
    if (!isTRUE(AURELIS_ACCESS_CONTROL_ENABLED)) return("Full Analyst access is active for this local build.")
    "Identity gateway ready."
  })

  observeEvent(input$aurelis_login_submit, {
    login_value <- function(value) if (is.null(value) || length(value) == 0 || is.na(value[[1]])) "" else as.character(value[[1]])
    email <- tolower(str_squish(login_value(input$aurelis_login_email)))
    password <- login_value(input$aurelis_login_password)
    key <- toupper(str_squish(login_value(input$aurelis_login_key)))
    match <- AURELIS_ACCESS_DIRECTORY[AURELIS_ACCESS_DIRECTORY$email == email & AURELIS_ACCESS_DIRECTORY$access_key == key, , drop = FALSE]
    password_name <- if (nrow(match) == 1) paste0("AURELIS_PASSWORD_", match$role[[1]]) else ""
    expected_password <- if (nzchar(password_name)) Sys.getenv(password_name, unset = "") else ""
    if (nrow(match) == 1 && nzchar(expected_password) && identical(password, expected_password)) {
      access_state(list(authenticated = TRUE, email = email, role = match$role[[1]]))
      updateSelectInput(session, "multi_user_role", selected = match$role[[1]])
    } else {
      showNotification("Identity, access key, or configured company password could not be verified.", type = "error", duration = 5)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$global_year_mobile, {
    value <- input$global_year_mobile
    if (!is.null(value) && length(value) > 0 && !identical(value, input$global_year)) {
      updateSelectInput(session, "global_year", selected = value)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$global_month_mobile, {
    value <- input$global_month_mobile
    if (!is.null(value) && length(value) > 0 && !identical(value, input$global_month)) {
      updateSelectInput(session, "global_month", selected = value)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$global_year, {
    if (!is.null(input$global_year) && length(input$global_year) > 0) {
      updateSelectInput(session, "global_year_mobile", selected = input$global_year)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$global_month, {
    if (!is.null(input$global_month) && length(input$global_month) > 0) {
      updateSelectInput(session, "global_month_mobile", selected = input$global_month)
    }
  }, ignoreInit = TRUE)
  
  
  # --------------------------------------------------------------------------
  # Multi-user session identity and persistent quotation drafts
  # --------------------------------------------------------------------------
  session_token <- tryCatch(session$token, error = function(e) "")
  if (is.null(session_token) || !nzchar(session_token)) session_token <- paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = "")
  aurelis_session_id <- paste0("S-", substr(gsub("[^A-Za-z0-9]", "", session_token), 1, 18))
  
  # FIXED_v2:
  # Workspace ownership is normal per-session state, not a reactiveVal.
  # It is therefore safe during server startup and session shutdown.
  initial_workspace_buyer <- if (length(all_buyers) > 0) {
    all_buyers[[1]]
  } else {
    "Unassigned Buyer"
  }
  
  workspace_session_state <- new.env(parent = emptyenv())
  workspace_session_state$buyer <- initial_workspace_buyer
  workspace_session_state$role <- "Buyer"
  
  current_draft_id <- reactiveVal("")
  multiuser_tick <- reactiveVal(0L)
  
  safe_workspace_buyer <- function() {
    value <- if (
      is.null(input$multi_user_buyer) ||
      length(input$multi_user_buyer) == 0
    ) {
      workspace_session_state$buyer
    } else {
      input$multi_user_buyer
    }
    
    value <- str_squish(as.character(value[[1]]))
    if (is.na(value) || value == "") value <- "Unassigned Buyer"
    
    workspace_session_state$buyer <- value
    value
  }

  safe_workspace_role <- function() {
    allowed <- c("CEO", "Manager", "Buyer", "Analyst")
    state <- access_state()
    value <- if (isTRUE(AURELIS_ACCESS_CONTROL_ENABLED) && isTRUE(state$authenticated)) state$role else if (is.null(input$multi_user_role) || length(input$multi_user_role) == 0) "Analyst" else as.character(input$multi_user_role[[1]])
    if (!value %in% allowed) value <- "Buyer"
    workspace_session_state$role <- value
    value
  }

  role_can_see_executive <- function() !isTRUE(AURELIS_ACCESS_CONTROL_ENABLED) || safe_workspace_role() %in% c("CEO", "Analyst")
  role_can_manage_team <- function() !isTRUE(AURELIS_ACCESS_CONTROL_ENABLED) || safe_workspace_role() %in% c("CEO", "Manager", "Analyst")
  
  # Safe startup: no reactive value is read here.
  aurelis_multiuser_register_session(
    aurelis_session_id,
    initial_workspace_buyer,
    workspace_session_state$role
  )
  
  aurelis_multiuser_log(
    aurelis_session_id,
    initial_workspace_buyer,
    "session_started",
    "Dashboard browser session opened",
    workspace_session_state$role
  )
  
  observeEvent(input$multi_user_buyer, {
    buyer <- safe_workspace_buyer()
    workspace_session_state$buyer <- buyer
    
    aurelis_multiuser_register_session(
      aurelis_session_id,
      buyer,
      safe_workspace_role()
    )
    
    aurelis_multiuser_log(
      aurelis_session_id,
      buyer,
      "workspace_selected",
      paste0("Buyer workspace selected: ", buyer),
      safe_workspace_role()
    )
    
    multiuser_tick(isolate(multiuser_tick()) + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$multi_user_role, {
    role <- safe_workspace_role()
    buyer <- safe_workspace_buyer()
    aurelis_multiuser_register_session(aurelis_session_id, buyer, role)
    aurelis_multiuser_log(aurelis_session_id, buyer, "access_profile_changed", paste0("Demo access profile selected: ", role), role)
    multiuser_tick(isolate(multiuser_tick()) + 1L)
  }, ignoreInit = TRUE)
  
  session$onSessionEnded(function() {
    buyer_at_close <- workspace_session_state$buyer
    
    try(
      aurelis_multiuser_log(
        aurelis_session_id,
        buyer_at_close,
        "session_closed",
        "Dashboard browser session closed",
        workspace_session_state$role
      ),
      silent = TRUE
    )
    
    try(
      aurelis_multiuser_close_session(aurelis_session_id),
      silent = TRUE
    )
  })
  
  multiuser_heartbeat <- reactiveTimer(
    60 * 1000,
    session = session
  )
  
  observe({
    multiuser_heartbeat()
    
    try(
      aurelis_multiuser_touch_session(
        aurelis_session_id,
      workspace_session_state$buyer,
      workspace_session_state$role
      ),
      silent = TRUE
    )
    
    multiuser_tick(isolate(multiuser_tick()) + 1L)
  })
  
  # --------------------------------------------------------------------------
  # Live refresh manager
  # --------------------------------------------------------------------------
  data_version <- reactiveVal(0L)
  refresh_running <- reactiveVal(FALSE)
  refresh_last_success <- reactiveVal(Sys.time())
  refresh_state <- reactiveVal("Initial data loaded")
  app_started_at <- Sys.time()
  
  update_live_filter_choices <- function() {
    try(updateSelectInput(session, "global_year", choices = c("All Years" = "all", all_years), selected = isolate(input$global_year)), silent = TRUE)
    try(updateSelectInput(session, "sales_buyer", choices = c("All Buyers" = "all", all_buyers), selected = isolate(input$sales_buyer)), silent = TRUE)
    try(updateSelectInput(session, "sales_client", choices = c("All Clients" = "all", all_clients_po), selected = isolate(input$sales_client)), silent = TRUE)
    try(updateSelectInput(session, "buyer_act_year", choices = c("All Years" = "all", all_years), selected = isolate(input$buyer_act_year)), silent = TRUE)
    try(updateSelectizeInput(session, "buyer_act_buyer", choices = c("All Buyers" = "all", setNames(all_buyers, all_buyers)), selected = isolate(input$buyer_act_buyer), server = TRUE), silent = TRUE)
    try(updateSelectInput(session, "perf_staff", choices = c("All Representatives" = "all", all_staff_rev), selected = isolate(input$perf_staff)), silent = TRUE)
    try(updateSelectInput(session, "perf_country", choices = c("All Countries" = "all", all_countries), selected = isolate(input$perf_country)), silent = TRUE)
    try(updateSelectizeInput(session, "cust_perf_client", choices = c("-- Select a Customer --" = "", setNames(customer_performance_clients, customer_performance_clients)), selected = isolate(input$cust_perf_client), server = TRUE), silent = TRUE)
    try(updateSelectizeInput(session, "tracker_client", choices = c("All Customers" = "all", setNames(all_inventory_clients, all_inventory_clients)), selected = isolate(input$tracker_client), server = TRUE), silent = TRUE)
    refreshed_quote_customers <- sort(unique(na.omit(c(customer_performance_clients, quotation_customer_reference$Company))))
    refreshed_quote_pos <- sort(unique(na.omit(inventory_data$PO_Number)))
    refreshed_quote_suppliers <- sort(unique(na.omit(inventory_data$Supplier)))
    quote_id_ref <- quotation_customer_reference %>%
      filter(!is.na(Customer_Code), Customer_Code != "", !is.na(Company), Company != "") %>%
      distinct(Customer_Code, .keep_all = TRUE) %>%
      arrange(Customer_Code)
    quote_id_choices <- setNames(
      as.character(quote_id_ref$Customer_Code),
      paste0(quote_id_ref$Customer_Code, " · ", quote_id_ref$Company)
    )
    try(updateSelectizeInput(session, "quotation_customer_lookup", choices = c("Manual / New Customer" = "", setNames(refreshed_quote_customers, refreshed_quote_customers)), selected = isolate(input$quotation_customer_lookup), server = TRUE), silent = TRUE)
    try(updateSelectizeInput(session, "quotation_customer_id_lookup", choices = c("Select by customer ID" = "", quote_id_choices), selected = isolate(input$quotation_customer_id_lookup), server = TRUE), silent = TRUE)
    try(updateSelectizeInput(session, "quotation_history_customer", choices = c("Select customer" = "", setNames(refreshed_quote_customers, refreshed_quote_customers)), selected = isolate(input$quotation_history_customer), server = TRUE), silent = TRUE)
    history_customer <- isolate(input$quotation_history_customer)
    history_pos <- if (!is.null(history_customer) && nzchar(history_customer)) {
      inventory_data %>% filter(Client == history_customer) %>% pull(PO_Number) %>% unique() %>% na.omit() %>% sort()
    } else refreshed_quote_pos
    try(updateSelectizeInput(session, "quotation_source_po", choices = c("Select a historical PO" = "", setNames(history_pos, history_pos)), selected = isolate(input$quotation_source_po), server = TRUE), silent = TRUE)
    try(updateSelectizeInput(session, "supplier_compare_suppliers", choices = setNames(refreshed_quote_suppliers, refreshed_quote_suppliers), selected = isolate(input$supplier_compare_suppliers), server = TRUE), silent = TRUE)
    invisible(TRUE)
  }
  
  perform_live_refresh <- function(trigger="Manual") {
    if (isTRUE(isolate(refresh_running())) || isTRUE(AURELIS_SHARED_REFRESH_STATE$running)) return(invisible(FALSE))
    refresh_running(TRUE)
    AURELIS_SHARED_REFRESH_STATE$running <- TRUE
    refresh_state(paste(trigger,"synthetic data reload in progress"))
    on.exit({
      refresh_running(FALSE)
      AURELIS_SHARED_REFRESH_STATE$running <- FALSE
    }, add=TRUE)
    result <- tryCatch({
      load_aurelis_demo_data(AURELIS_APP_ENV)
      AURELIS_SHARED_REFRESH_STATE$version <- as.integer(AURELIS_SHARED_REFRESH_STATE$version)+1L
      AURELIS_SHARED_REFRESH_STATE$last_refresh <- Sys.time()
      data_version(AURELIS_SHARED_REFRESH_STATE$version)
      refresh_last_success(AURELIS_SHARED_REFRESH_STATE$last_refresh)
      refresh_state(paste0(trigger," synthetic package reload completed"))
      update_live_filter_choices()
      try(aurelis_multiuser_log(aurelis_session_id,workspace_session_state$buyer,"data_refresh",paste0(trigger," demo reload"), workspace_session_state$role),silent=TRUE)
      if (identical(trigger,"Manual")) showNotification("Synthetic public-demo data reloaded. No production source was contacted.",type="message",duration=5)
      TRUE
    },error=function(error) {
      refresh_state(paste0(trigger," synthetic reload failed: ",conditionMessage(error)))
      if (identical(trigger,"Manual")) showNotification(paste("Reload failed:",conditionMessage(error)),type="error",duration=10)
      FALSE
    })
    invisible(result)
  }
  
  output$live_refresh_interval <- renderText({
    data_version()
    paste0(
      "Access/dashboard: ", AURELIS_AUTO_REFRESH_SECONDS,
      " seconds · QuickBooks: ",
      if (isTRUE(AURELIS_QUICKBOOKS_ENABLED)) paste0(round(AURELIS_QB_SYNC_SECONDS / 60, 1), " minutes") else "deferred / Excel fallback"
    )
  })
  output$live_refresh_last <- renderText({
    data_version()
    format(refresh_last_success(), "%Y-%m-%d %H:%M:%S")
  })
  output$live_refresh_state <- renderText({
    data_version()
    refresh_state()
  })
  
  observeEvent(input$refresh_live_data, {
    perform_live_refresh("Manual")
  }, ignoreInit = TRUE)
  
  observeEvent(input$refresh_live_data_connections, {
    perform_live_refresh("Manual")
  }, ignoreInit = TRUE)
  
  auto_refresh_timer <- reactiveTimer(AURELIS_AUTO_REFRESH_SECONDS * 1000, session = session)
  first_auto_cycle <- reactiveVal(TRUE)
  observe({
    auto_refresh_timer()
    if (isTRUE(isolate(first_auto_cycle()))) {
      first_auto_cycle(FALSE)
      return()
    }
    perform_live_refresh("Automatic")
  })
  
  future_page_copy <- list(
    multi_user_workspace = list(
      title = "My Multi-User Workspace",
      subtitle = "Identify your buyer workspace, coordinate concurrent sessions and save or reopen quotation drafts without interfering with another buyer's active work."
    ),
    exec_overview = list(
      title = "Executive Command Overview",
      subtitle = "A unified pulse of revenue, profitability, receivables, customers and purchasing activity."
    ),
    global_network = list(
      title = "Global Relationship & Operations Atlas",
      subtitle = "Navigate customers, suppliers and Aurelis representatives geographically, then drill from continent to country, region, city and individual business relationship."
    ),
    kpi_board = list(
      title = "KPI Command Center",
      subtitle = "Track the management metrics that connect financial performance, customer service, purchasing activity and operational execution."
    ),
    buyer_activity = list(
      title = "Buyer Activity Intelligence",
      subtitle = "Drill from year to month, week and day to understand workload, customer coverage and purchase-order activity by buyer."
    ),
    sale_performance = list(
      title = "Sales & Profitability Intelligence",
      subtitle = "Explore revenue quality, gross profit, country contribution and representative performance."
    ),
    monthly_sales = list(
      title = "Purchase Order Control Center",
      subtitle = "Track PO volume, purchasing value, customer demand and buyer execution from one workspace."
    ),
    customer_performance = list(
      title = "Customer Relationship Intelligence",
      subtitle = "Turn order history, fulfillment, margin and payment behavior into customer-ready insights."
    ),
    ar_tab = list(
      title = "Receivables Risk Radar",
      subtitle = "Prioritize collections with aging focus, customer exposure and payment timing intelligence."
    ),
    client_statement = list(
      title = "Client Account Workspace",
      subtitle = "Review one customer's invoicing, outstanding balances, aging profile and statement history."
    ),
    order_tracker = list(
      title = "Delivery Operations Radar",
      subtitle = "Monitor inbound commitments, overdue risk, delivery timing and priority schedules interactively."
    ),
    inventory_tab = list(
      title = "Inventory & Warehouse Intelligence",
      subtitle = "Combine real-time warehouse readiness with inbound supply and backorder visibility."
    ),
    buyer_tool = list(
      title = "Supplier & Procurement Intelligence",
      subtitle = "Compare sourcing partners using historical cost, delivery timing, reliability and open exposure."
    ),
    quotation_studio = list(
      title = "Quotation Studio & Finance Engine",
      subtitle = "Build auditable customer quotations from editable line items, landed costs, financing scenarios and commercial pricing policy."
    ),
    contacts_tab = list(
      title = "Relationship Network",
      subtitle = "Connect customers, buyers, suppliers and operational histories through one relationship view."
    ),
    misc_tab = list(
      title = "Demo Data & Digital Operations",
      subtitle = "Review the isolated synthetic data package, public-demo refresh controls, process documentation and export behavior."
    ),
    executive_observatory = list(
      title = "Executive Activity Observatory",
      subtitle = "Review past employee actions, live sessions, future commitments and role-level operating signals."
    )
  )
  
  output$obsidian_header_page <- renderText({
    selected_tab <- input$sidebar_tabs
    if (is.null(selected_tab) || !selected_tab %in% names(future_page_copy)) {
      selected_tab <- "exec_overview"
    }
    future_page_copy[[selected_tab]]$title
  })
  
  output$future_page_hero <- renderUI({
    data_version()
    
    selected_tab <- input$sidebar_tabs
    if (is.null(selected_tab) || !selected_tab %in% names(future_page_copy)) {
      selected_tab <- "exec_overview"
    }
    page_copy <- future_page_copy[[selected_tab]]
    
    year_scope <- if (
      is.null(input$global_year) ||
      length(input$global_year) == 0 ||
      identical(input$global_year, "all")
    ) "All years" else as.character(input$global_year[[1]])
    
    month_scope <- if (
      is.null(input$global_month) ||
      length(input$global_month) == 0 ||
      identical(input$global_month, "all")
    ) "All months" else month.name[as.integer(input$global_month[[1]])]
    
    div(
      class = "future-page-hero",
      div(
        class = "prism-masthead",
        div(
          div(class = "prism-masthead-kicker", "Aurelis · Business Operations"),
          tags$h2(page_copy$title),
          tags$p(page_copy$subtitle)
        ),
        div(
          class = "prism-masthead-stats",
          div(class = "prism-masthead-stat", tags$span("Year"), tags$strong(year_scope)),
          div(class = "prism-masthead-stat", tags$span("Month"), tags$strong(month_scope)),
          div(
            class = "prism-masthead-stat",
            tags$span("PO Network"),
            tags$strong(paste0(format(n_distinct(daily_po$PO_Number), big.mark = ","), " POs"))
          )
        )
      )
    )
  })
  
  output$prism_context_label <- renderText({
    data_version()
    
    selected_tab <- input$sidebar_tabs
    if (is.null(selected_tab) || !selected_tab %in% names(future_page_copy)) {
      selected_tab <- "exec_overview"
    }
    
    year_scope <- if (
      is.null(input$global_year) ||
      length(input$global_year) == 0 ||
      identical(input$global_year, "all")
    ) "All years" else as.character(input$global_year[[1]])
    
    month_scope <- if (
      is.null(input$global_month) ||
      length(input$global_month) == 0 ||
      identical(input$global_month, "all")
    ) "All months" else month.name[as.integer(input$global_month[[1]])]
    
    paste0(future_page_copy[[selected_tab]]$title, " · ", year_scope, " · ", month_scope)
  })
  
  output$future_clock <- renderText({
    data_version()
    invalidateLater(1000, session)
    format(Sys.time(), "%A • %H:%M:%S")
  })
  
  navigate_sidebar <- function(tab_name) {
    if (exists("updatebs4TabItems", mode = "function")) {
      try(updatebs4TabItems(session, "sidebar_tabs", selected = tab_name), silent = TRUE)
    } else if (exists("updateTabItems", mode = "function")) {
      try(updateTabItems(session, "sidebar_tabs", selected = tab_name), silent = TRUE)
    }
    invisible(TRUE)
  }

  role_allowed_tab <- function(tab_name) {
    if (!isTRUE(AURELIS_ACCESS_CONTROL_ENABLED)) return(TRUE)
    role <- safe_workspace_role()
    if (role == "CEO") return(TRUE)
    if (tab_name == "executive_observatory") return(FALSE)
    if (role == "Manager") return(tab_name %in% c("exec_overview", "global_network", "multi_user_workspace", "kpi_board", "sale_performance", "quotation_studio", "customer_performance", "client_statement", "monthly_sales", "buyer_activity", "order_tracker", "inventory_tab", "buyer_tool", "ar_tab", "contacts_tab", "product_intelligence", "misc_tab"))
    if (role == "Analyst") return(tab_name %in% c("exec_overview", "global_network", "kpi_board", "sale_performance", "customer_performance", "client_statement", "monthly_sales", "buyer_activity", "order_tracker", "inventory_tab", "buyer_tool", "ar_tab", "contacts_tab", "product_intelligence", "misc_tab"))
    tab_name %in% c("exec_overview", "global_network", "multi_user_workspace", "sale_performance", "quotation_studio", "customer_performance", "monthly_sales", "buyer_activity", "order_tracker", "buyer_tool", "product_intelligence")
  }

  observeEvent(input$sidebar_tabs, {
    selected <- input$sidebar_tabs
    if (!is.null(selected) && !role_allowed_tab(selected)) {
      showNotification("This workspace requires a higher access profile in the Aurelis demo.", type = "warning", duration = 4)
      navigate_sidebar("exec_overview")
    }
  }, ignoreInit = TRUE)
  
  # --------------------------------------------------------------------------
  # Cross-navigation helpers used by interactive charts across the platform.
  # A click can focus a customer, buyer, supplier or period and open the
  # corresponding analytical workspace.
  # --------------------------------------------------------------------------
  open_customer_workspace <- function(customer, tab = "customer_performance") {
    customer <- str_squish(coalesce(as.character(customer), ""))
    if (customer == "") return(invisible(FALSE))
    if (tab == "client_statement") {
      try(updateSelectizeInput(session, "stmt_client", selected = customer), silent = TRUE)
    } else {
      try(updateSelectizeInput(session, "cust_perf_client", selected = customer), silent = TRUE)
    }
    navigate_sidebar(tab)
    invisible(TRUE)
  }
  
  open_buyer_workspace <- function(buyer) {
    buyer <- str_squish(coalesce(as.character(buyer), ""))
    if (buyer == "") return(invisible(FALSE))
    try(updateSelectizeInput(session, "buyer_act_buyer", selected = buyer), silent = TRUE)
    navigate_sidebar("buyer_activity")
    invisible(TRUE)
  }
  
  open_supplier_workspace <- function(supplier) {
    supplier <- str_squish(coalesce(as.character(supplier), ""))
    if (supplier == "") return(invisible(FALSE))
    try(updateSelectizeInput(session, "supplier_compare_suppliers", selected = supplier), silent = TRUE)
    try(updateSelectizeInput(session, "buyer_search_supplier", selected = supplier), silent = TRUE)
    navigate_sidebar("buyer_tool")
    invisible(TRUE)
  }
  
  apply_global_period_from_date <- function(value) {
    parsed <- suppressWarnings(as.Date(value))
    if (is.na(parsed)) return(invisible(FALSE))
    try(updateSelectInput(session, "global_year", selected = as.character(year(parsed))), silent = TRUE)
    try(updateSelectInput(session, "global_month", selected = as.character(month(parsed))), silent = TRUE)
    invisible(TRUE)
  }
  
  output$signature_data_health <- renderText({
    data_version()
    
    expected_files <- c(
      "Aurelis_Daily_PO.csv",
      "Aurelis_Revenue.csv",
      "Aurelis_Accounts_Receivable.csv",
      "Aurelis_Inventory.csv",
      "Aurelis_Warehouse.csv",
      "Aurelis_Contacts.csv",
      "Aurelis_Directory.csv",
      "Aurelis_Customer_Reference.csv",
      "Aurelis_Terms_Reference.csv",
      "Aurelis_Customers.csv",
      "Aurelis_Suppliers.csv",
      "Aurelis_Products.csv",
      "Aurelis_Seller_Catalog.csv",
      "Aurelis_Geography.csv",
      "Aurelis_User_Clients.csv"
    )
    
    available <- sum(file.exists(file.path(AURELIS_DATA_DIR, expected_files)))
    diagnostics <- length(AURELIS_DATA_DIAGNOSTICS$messages)
    
    paste0(
      available, "/", length(expected_files),
      " datasets available · adaptive schema mapping active · ",
      diagnostics, " import note(s) · last load ",
      format(AURELIS_DEMO_LOADED_AT, "%H:%M:%S")
    )
  })
  
  output$signature_record_scope <- renderText({
    data_version()
    
    year_label <- if (
      is.null(input$global_year) ||
      length(input$global_year) == 0 ||
      identical(input$global_year, "all")
    ) "All years" else as.character(input$global_year[[1]])
    
    month_label <- if (
      is.null(input$global_month) ||
      length(input$global_month) == 0 ||
      identical(input$global_month, "all")
    ) "All months" else month.name[as.integer(input$global_month[[1]])]
    
    paste0(
      year_label, " · ", month_label,
      " · ", format(n_distinct(daily_po$PO_Number), big.mark = ","), " POs",
      " · ", format(n_distinct(revenue_data$Customer), big.mark = ","), " customers",
      " · ", format(n_distinct(inventory_data$Supplier), big.mark = ","), " suppliers"
    )
  })
  
  observeEvent(input$signature_search_go, {
    query <- if (
      is.null(input$signature_global_search) ||
      length(input$signature_global_search) == 0
    ) "" else str_squish(as.character(input$signature_global_search[[1]]))
    
    if (is.na(query) || query == "") {
      showNotification(
        "Enter a customer, customer ID, PO, buyer, supplier, part number, product or service.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    query_key <- str_to_lower(query)
    close_palette <- function() {
      try(session$sendCustomMessage("signatureCloseCommand", list()), silent = TRUE)
    }
    
    # 1. Exact customer ID.
    customer_by_id <- quotation_customer_reference %>%
      filter(str_to_lower(as.character(Customer_Code)) == query_key) %>%
      slice(1)
    
    if (nrow(customer_by_id) > 0) {
      company <- as.character(customer_by_id$Company[[1]])
      open_customer_workspace(company)
      close_palette()
      showNotification(
        paste0("Opened customer: ", company, " · ", customer_by_id$Customer_Code[[1]]),
        type = "message",
        duration = 4
      )
      return()
    }
    
    # 2. Exact / partial customer billing name.
    customer_match <- quotation_customer_reference %>%
      mutate(Search_Key = str_to_lower(as.character(Company))) %>%
      filter(Search_Key == query_key | str_detect(Search_Key, fixed(query_key))) %>%
      arrange(desc(Search_Key == query_key)) %>%
      slice(1)
    
    if (nrow(customer_match) > 0) {
      company <- as.character(customer_match$Company[[1]])
      open_customer_workspace(company)
      close_palette()
      showNotification(paste0("Opened customer: ", company), type = "message", duration = 4)
      return()
    }
    
    # 3. Purchase order.
    po_match <- inventory_data %>%
      filter(str_detect(str_to_lower(as.character(PO_Number)), fixed(query_key))) %>%
      distinct(PO_Number) %>%
      slice(1)
    
    if (nrow(po_match) > 0) {
      po_value <- as.character(po_match$PO_Number[[1]])
      updateTextInput(session, "tracker_search", value = po_value)
      navigate_sidebar("order_tracker")
      close_palette()
      showNotification(paste0("Opened purchase order search: ", po_value), type = "message", duration = 4)
      return()
    }
    
    # 4. Buyer.
    buyer_match <- all_buyers[str_to_lower(all_buyers) == query_key]
    if (length(buyer_match) == 0) {
      buyer_match <- all_buyers[str_detect(str_to_lower(all_buyers), fixed(query_key))]
    }
    if (length(buyer_match) > 0) {
      open_buyer_workspace(buyer_match[[1]])
      close_palette()
      showNotification(paste0("Opened buyer workspace: ", buyer_match[[1]]), type = "message", duration = 4)
      return()
    }
    
    # 5. Supplier.
    supplier_pool <- sort(unique(na.omit(inventory_data$Supplier)))
    supplier_match <- supplier_pool[str_to_lower(supplier_pool) == query_key]
    if (length(supplier_match) == 0) {
      supplier_match <- supplier_pool[str_detect(str_to_lower(supplier_pool), fixed(query_key))]
    }
    if (length(supplier_match) > 0) {
      open_supplier_workspace(supplier_match[[1]])
      updateSelectizeInput(session, "seller_catalog_supplier", selected = supplier_match[[1]])
      close_palette()
      showNotification(paste0("Opened supplier: ", supplier_match[[1]]), type = "message", duration = 4)
      return()
    }
    
    # 6. Geographic entity / city / region.
    geography_match <- geography_data %>%
      mutate(
        Search_Key = str_to_lower(paste(
          Entity_Type, Entity_ID, Entity, Continent, Country, Region, City,
          Representative
        ))
      ) %>%
      filter(str_detect(Search_Key, fixed(query_key))) %>%
      arrange(
        desc(str_to_lower(Entity) == query_key),
        desc(str_to_lower(City) == query_key),
        desc(str_to_lower(Country) == query_key)
      ) %>%
      slice(1)
    
    if (nrow(geography_match) > 0) {
      updateSelectInput(
        session, "atlas_continent",
        selected = geography_match$Continent[[1]]
      )
      updateSelectizeInput(
        session, "atlas_country",
        selected = geography_match$Country[[1]]
      )
      updateSelectizeInput(
        session, "atlas_region",
        selected = geography_match$Region[[1]]
      )
      updateSelectizeInput(
        session, "atlas_city",
        selected = geography_match$City[[1]]
      )
      updateSelectizeInput(
        session, "atlas_entity_types",
        selected = geography_match$Entity_Type[[1]]
      )
      updateTextInput(
        session, "atlas_search",
        value = geography_match$Entity[[1]]
      )
      navigate_sidebar("global_network")
      close_palette()
      showNotification(
        paste0(
          "Opened Atlas location: ",
          geography_match$Entity[[1]],
          " · ", geography_match$City[[1]],
          ", ", geography_match$Country[[1]]
        ),
        type = "message",
        duration = 5
      )
      return()
    }
    
    # 7. Part number, product or service capability.
    catalog_search <- seller_catalog_data %>%
      mutate(
        Search_Key = str_to_lower(paste(
          coalesce(as.character(Product_Code), ""),
          coalesce(as.character(Product_Service), ""),
          coalesce(as.character(Category), ""),
          coalesce(as.character(Manufacturer), "")
        ))
      ) %>%
      filter(str_detect(Search_Key, fixed(query_key))) %>%
      slice(1)
    
    if (nrow(catalog_search) > 0) {
      updateTextInput(session, "seller_catalog_search", value = query)
      navigate_sidebar("buyer_tool")
      close_palette()
      showNotification(
        paste0(
          "Opened seller catalog for: ",
          catalog_search$Product_Service[[1]]
        ),
        type = "message",
        duration = 5
      )
      return()
    }
    
    # 8. General part/order description fallback.
    inventory_match <- inventory_data %>%
      mutate(
        Search_Key = str_to_lower(paste(
          coalesce(as.character(Part_Number), ""),
          coalesce(as.character(Order_Description), ""),
          coalesce(as.character(Memo), "")
        ))
      ) %>%
      filter(str_detect(Search_Key, fixed(query_key))) %>%
      slice(1)
    
    if (nrow(inventory_match) > 0) {
      updateTextInput(session, "inv_search", value = query)
      navigate_sidebar("inventory_tab")
      close_palette()
      showNotification("Opened Inventory & Warehouse search.", type = "message", duration = 4)
      return()
    }
    
    showNotification(
      paste0("No direct match found for '", query, "'. Try a shorter keyword."),
      type = "warning",
      duration = 6
    )
  }, ignoreInit = TRUE)
  
  plotly_clicked_value <- function(clicked) {
    if (is.null(clicked) || nrow(clicked) == 0) return(NULL)
    candidates <- list(clicked$customdata, clicked$key, clicked$label)
    for (candidate in candidates) {
      if (!is.null(candidate) && length(candidate) > 0 && !is.na(candidate[[1]])) {
        value <- str_squish(as.character(candidate[[1]]))
        if (value != "") return(value)
      }
    }
    NULL
  }
  
  ##############################################################################
  # 4.1 REACTIVE FILTERED DATA (Global Year + Month filters)
  ##############################################################################
  
  filtered_revenue <- reactive({
    data_version()
    df <- revenue_data
    if (input$global_year != "all") df <- df %>% filter(Year == as.numeric(input$global_year))
    if (input$global_month != "all") df <- df %>% filter(Month == as.numeric(input$global_month))
    df
  })
  
  filtered_po <- reactive({
    data_version()
    df <- daily_po
    if (input$global_year != "all") df <- df %>% filter(Year == as.numeric(input$global_year))
    if (input$global_month != "all") df <- df %>% filter(Month == as.numeric(input$global_month))
    df
  })
  
  filtered_ar <- reactive({
    data_version()
    invalidateLater(60 * 60 * 1000, session)
    today <- Sys.Date()
    
    df <- ar_data %>%
      mutate(
        Days_Outstanding = as.numeric(difftime(today, Due_Date, units = "days")),
        Days_Until_Due = case_when(
          is.na(Due_Date) | Balance_Remaining <= 0 ~ 0,
          Due_Date > today ~ as.numeric(difftime(Due_Date, today, units = "days")),
          TRUE ~ 0
        ),
        Days_Past_Due = case_when(
          is.na(Due_Date) | Balance_Remaining <= 0 ~ 0,
          Due_Date < today ~ as.numeric(difftime(today, Due_Date, units = "days")),
          TRUE ~ 0
        ),
        Payment_Timing = case_when(
          Balance_Remaining <= 0 ~ "Paid",
          is.na(Due_Date) ~ "Due date unavailable",
          Due_Date > today ~ paste0("Due in ", as.integer(Days_Until_Due), " day(s)"),
          Due_Date == today ~ "Due today",
          TRUE ~ paste0(as.integer(Days_Past_Due), " day(s) overdue")
        ),
        Aging_Bucket = case_when(
          Balance_Remaining <= 0 ~ "Paid",
          Days_Past_Due == 0 ~ "Current",
          Days_Past_Due <= 30 ~ "1-30 Days",
          Days_Past_Due <= 60 ~ "31-60 Days",
          Days_Past_Due <= 90 ~ "61-90 Days",
          TRUE ~ "90+ Days"
        ),
        Aging_Bucket = factor(
          Aging_Bucket,
          levels = c("Paid", "Current", "1-30 Days", "31-60 Days", "61-90 Days", "90+ Days")
        )
      )
    
    if (input$global_year != "all") df <- df %>% filter(Year == as.numeric(input$global_year))
    if (input$global_month != "all") df <- df %>% filter(Month == as.numeric(input$global_month))
    df
  })
  
  filtered_inventory <- reactive({
    data_version()
    invalidateLater(60 * 60 * 1000, session)
    today <- Sys.Date()
    
    df <- inventory_data %>%
      mutate(
        Days_To_Delivery = as.numeric(difftime(Delivery_Date, today, units = "days")),
        Order_Status = case_when(
          Line_Type == "Information" ~ "Information",
          Backordered_Qty <= 0 | Received_Qty >= Quantity ~ "Fully Received",
          !is.na(Delivery_Date) & Delivery_Date < today ~ "Overdue",
          !is.na(Delivery_Date) & Delivery_Date <= today + 14 ~ "Due Soon",
          TRUE ~ "Open"
        ),
        Order_Status = factor(
          Order_Status,
          levels = c("Overdue", "Due Soon", "Open", "Fully Received", "Information")
        )
      )
    
    if (input$global_year != "all") df <- df %>% filter(Year == as.numeric(input$global_year))
    if (input$global_month != "all") df <- df %>% filter(Month == as.numeric(input$global_month))
    df
  })

  observeEvent(input$financial_stats_source, {
    source_data <- aurelis_stats_source_frame(input$financial_stats_source)
    measure_fields <- aurelis_stats_numeric_fields(source_data)
    group_fields <- aurelis_stats_group_fields(source_data)
    measure_choices <- stats::setNames(
      measure_fields,
      stringr::str_replace_all(measure_fields, "_", " ")
    )
    group_choices <- c(
      "All records" = "",
      stats::setNames(group_fields, stringr::str_replace_all(group_fields, "_", " "))
    )
    measure_selected <- if (isolate(input$financial_stats_measure) %in% measure_fields) {
      isolate(input$financial_stats_measure)
    } else {
      measure_fields[[1]]
    }

    updateSelectInput(
      session,
      "financial_stats_measure",
      choices = measure_choices,
      selected = measure_selected
    )
    updateSelectInput(
      session,
      "financial_stats_group_by",
      choices = group_choices,
      selected = ""
    )
    updateSelectizeInput(
      session,
      "financial_stats_groups",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  financial_stats_source_data <- reactive({
    data_version()
    source <- input$financial_stats_source
    if (is.null(source) || length(source) == 0 || !source %in% unname(aurelis_stats_dataset_choices)) {
      validate(need(FALSE, "Choose a financial dataset to continue."))
    }
    switch(
      source,
      revenue = filtered_revenue(),
      ar = filtered_ar(),
      orders = filtered_po(),
      inventory = filtered_inventory()
    )
  })

  financial_stats_prepared <- reactive({
    source_data <- financial_stats_source_data()
    measure <- input$financial_stats_measure
    group_by <- input$financial_stats_group_by
    selected_groups <- input$financial_stats_groups
    validate(need(nrow(source_data) > 0, "No records match the current year and month filters."))
    measure_fields <- aurelis_stats_numeric_fields(source_data)
    validate(need(length(measure_fields) > 0, "The selected dataset has no usable numeric measures."))
    if (is.null(measure) || length(measure) != 1 || !measure %in% measure_fields) {
      measure <- measure_fields[[1]]
    }
    group_fields <- aurelis_stats_group_fields(source_data)
    if (is.null(group_by) || length(group_by) != 1 || !group_by %in% group_fields) {
      group_by <- ""
      selected_groups <- character(0)
    }
    if (length(selected_groups) > 0 && nzchar(group_by)) {
      available_groups <- unique(as.character(source_data[[group_by]]))
      selected_groups <- intersect(as.character(selected_groups), available_groups)
    }

    if (!is.null(group_by) && nzchar(group_by) && !length(selected_groups)) {
      group_values <- as.character(source_data[[group_by]])
      group_values[is.na(group_values) | !nzchar(trimws(group_values))] <- "Unspecified"
      selected_groups <- names(utils::head(sort(table(group_values), decreasing = TRUE), 8))
    }

    prepared <- aurelis_stats_prepare(
      source_data,
      measure = measure,
      group_by = group_by,
      groups = selected_groups,
      missing_policy = input$financial_stats_missing_policy
    )
    validate(need(nrow(prepared) > 0, "No usable numeric values remain for this selection."))
    prepared
  })

  output$financial_stats_data_note <- renderUI({
    prepared <- financial_stats_prepared()
    missing_count <- attr(prepared, "missing_count")
    imputed_count <- attr(prepared, "imputed_count")
    excluded_count <- attr(prepared, "excluded_count")
    group_note <- if (!is.null(input$financial_stats_group_by) &&
                      nzchar(input$financial_stats_group_by) &&
                      !length(input$financial_stats_groups)) {
      " With no groups selected, the eight largest groups are used for readability."
    } else {
      ""
    }
    value_note <- if (imputed_count > 0) {
      paste0(
        " Median-filled ", scales::comma(imputed_count),
        " missing value(s); these are synthetic analysis values and do not modify the source data."
      )
    } else if (excluded_count > 0) {
      paste0(" Excluded ", scales::comma(excluded_count), " missing or non-finite value(s) from analysis.")
    } else {
      " No missing or non-finite values were found in the selected measure."
    }
    div(
      class = "financial-stats-note",
      paste0("Values are filtered by the dashboard's global date controls.", value_note, group_note)
    )
  })

  output$financial_stats_summary <- renderUI({
    prepared <- financial_stats_prepared()
    values <- prepared$value
    sample_size <- length(values)
    average <- mean(values)
    standard_deviation <- if (sample_size > 1) stats::sd(values) else NA_real_
    confidence_margin <- if (sample_size > 1) {
      stats::qt(.975, df = sample_size - 1) * standard_deviation / sqrt(sample_size)
    } else {
      NA_real_
    }
    coefficient_variation <- if (is.finite(average) && average != 0 && is.finite(standard_deviation)) {
      100 * standard_deviation / abs(average)
    } else {
      NA_real_
    }
    format_value <- function(value, suffix = "") {
      if (!is.finite(value)) return("Not available")
      paste0(scales::number(value, accuracy = 0.01, big.mark = ","), suffix)
    }
    summary_cards <- list(
      c("Usable observations", scales::comma(sample_size)),
      c("Mean", format_value(average)),
      c("Median", format_value(stats::median(values))),
      c("Standard deviation", format_value(standard_deviation)),
      c("Coefficient of variation", format_value(coefficient_variation, "%")),
      c(
        "95% mean confidence interval",
        if (is.finite(confidence_margin)) {
          paste0(format_value(average - confidence_margin), " to ", format_value(average + confidence_margin))
        } else {
          "Not available"
        }
      )
    )
    div(
      class = "financial-stats-summary",
      lapply(summary_cards, function(card) {
        div(
          class = "financial-stats-summary-card",
          tags$span(card[[1]]),
          tags$strong(card[[2]])
        )
      })
    )
  })

  output$financial_stats_distribution_plot <- renderPlotly({
    prepared <- financial_stats_prepared()
    values <- prepared$value
    validate(need(length(values) >= 2, "At least two usable observations are required to fit a distribution."))
    distribution <- input$financial_stats_distribution
    fit <- tryCatch(
      aurelis_stats_distribution_fit(values, distribution, input$financial_stats_df),
      error = function(error) error
    )
    if (inherits(fit, "error")) {
      validate(need(FALSE, conditionMessage(fit)))
    }

    data <- data.frame(value = values)
    if (isTRUE(fit$discrete)) {
      graph <- ggplot(data, aes(x = value)) +
        geom_histogram(
          aes(y = after_stat(count / sum(count))),
          binwidth = 1,
          boundary = -0.5,
          fill = "#32D1C6",
          color = "#111827",
          alpha = 0.78
        )
      curve_values <- seq(
        max(0, floor(min(values))),
        min(ceiling(max(values)), stats::qpois(.999, lambda = mean(values)) + 1),
        by = 1
      )
    } else {
      graph <- ggplot(data, aes(x = value)) +
        geom_histogram(
          aes(y = after_stat(density)),
          bins = max(10, min(50, round(sqrt(length(values))))),
          fill = "#32D1C6",
          color = "#111827",
          alpha = 0.78
        )
      curve_values <- seq(min(values), max(values), length.out = 250)
    }

    if (!is.null(fit$density) && length(curve_values) > 0) {
      curve <- data.frame(x = curve_values, density = fit$density(curve_values))
      graph <- graph + geom_line(
        data = curve,
        aes(x = x, y = density),
        inherit.aes = FALSE,
        color = "#FFB15C",
        linewidth = 1.1
      )
    }
    graph <- graph +
      labs(
        x = input$financial_stats_measure,
        y = if (isTRUE(fit$discrete)) "Probability" else "Density",
        title = paste(distribution, "distribution"),
        subtitle = paste(scales::comma(length(values)), "usable records")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    plotly::config(plotly::ggplotly(graph, tooltip = c("x", "y")), responsive = TRUE)
  })

  output$financial_stats_qq_plot <- renderPlotly({
    prepared <- financial_stats_prepared()
    values <- prepared$value
    validate(need(length(values) >= 2, "At least two usable observations are required for a quantile check."))
    fit <- tryCatch(
      aurelis_stats_distribution_fit(values, input$financial_stats_distribution, input$financial_stats_df),
      error = function(error) error
    )
    if (inherits(fit, "error")) {
      validate(need(FALSE, conditionMessage(fit)))
    }
    validate(need(
      !is.null(fit$quantile),
      "The empirical distribution has no theoretical quantiles to compare."
    ))
    probabilities <- stats::ppoints(length(values))
    qq_data <- data.frame(
      theoretical = fit$quantile(probabilities),
      observed = sort(values)
    )
    graph <- ggplot(qq_data, aes(x = theoretical, y = observed)) +
      geom_point(color = "#32D1C6", alpha = 0.68, size = 1.5) +
      geom_abline(intercept = 0, slope = 1, color = "#FFB15C", linewidth = 0.9) +
      labs(
        x = "Theoretical quantiles",
        y = "Observed quantiles",
        subtitle = input$financial_stats_distribution
      ) +
      theme_minimal(base_size = 11)
    plotly::config(plotly::ggplotly(graph, tooltip = c("x", "y")), responsive = TRUE)
  })

  output$financial_stats_group_plot <- renderPlotly({
    prepared <- financial_stats_prepared()
    validate(need(
      !is.null(input$financial_stats_group_by) &&
        nzchar(input$financial_stats_group_by) &&
        dplyr::n_distinct(prepared$group) >= 2,
      "Choose a comparison field and at least two groups."
    ))
    graph <- ggplot(prepared, aes(x = group, y = value, fill = group)) +
      geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = NULL, y = input$financial_stats_measure) +
      theme_minimal(base_size = 11)
    plotly::config(plotly::ggplotly(graph, tooltip = c("x", "y")), responsive = TRUE)
  })

  output$financial_stats_comparison <- renderUI({
    prepared <- financial_stats_prepared()
    validate(need(
      !is.null(input$financial_stats_group_by) &&
        nzchar(input$financial_stats_group_by),
      "Choose a comparison field to run group tests."
    ))
    groups <- split(prepared$value, droplevels(prepared$group))
    groups <- groups[lengths(groups) > 0]
    validate(need(length(groups) >= 2, "At least two non-empty groups are required for comparison."))
    format_p <- function(value) {
      if (!is.finite(value)) "Not available" else format.pval(value, digits = 3, eps = 0.001)
    }
    result_block <- function(label, result) {
      if (inherits(result, "error")) {
        div(class = "financial-stats-note", paste0(label, ": unavailable — ", conditionMessage(result)))
      } else {
        div(class = "financial-stats-summary-card", tags$span(label), tags$strong(format_p(result$p.value)))
      }
    }
    if (length(groups) == 2) {
      pair_data <- prepared[prepared$group %in% names(groups), , drop = FALSE]
      parametric <- tryCatch(stats::t.test(value ~ group, data = pair_data), error = function(error) error)
      rank_test <- tryCatch(
        stats::wilcox.test(value ~ group, data = pair_data, exact = FALSE),
        error = function(error) error
      )
      tagList(
        tags$p("Two-group comparison. Welch's t-test does not assume equal group variances; Wilcoxon is rank-based."),
        result_block("Welch t-test p-value", parametric),
        result_block("Wilcoxon rank-sum p-value", rank_test)
      )
    } else {
      parametric <- tryCatch(
        stats::oneway.test(value ~ group, data = prepared, var.equal = FALSE),
        error = function(error) error
      )
      rank_test <- tryCatch(
        stats::kruskal.test(value ~ group, data = prepared),
        error = function(error) error
      )
      tagList(
        tags$p("Multiple-group comparison. Welch's one-way test allows unequal variances; Kruskal-Wallis is rank-based."),
        result_block("Welch one-way test p-value", parametric),
        result_block("Kruskal-Wallis p-value", rank_test)
      )
    }
  })
  
  ##############################################################################
  # 4.2 GLOBAL NETWORK ATLAS - Geographic Intelligence
  ##############################################################################
  
  atlas_selected_key <- reactiveVal("")
  atlas_breakdown_reset_version <- reactiveVal(0L)
  
  atlas_input_text <- function(value, fallback = "") {
    if (is.null(value) || length(value) == 0) return(fallback)
    value <- as.character(value[[1]])
    if (is.na(value)) fallback else str_squish(value)
  }
  
  atlas_entity_colors <- c(
    Customer = "#FF7A45",
    Supplier = "#FFC857",
    Representative = "#32D1C6"
  )
  
  atlas_metric_title <- function(metric) {
    switch(
      metric,
      activity = "Business activity value",
      count = "Order / transaction count",
      exposure = "Open exposure",
      profit = "Gross profit",
      "Business activity value"
    )
  }
  
  atlas_network_data <- reactive({
    data_version()
    
    customer_revenue <- filtered_revenue() %>%
      filter(!is.na(Customer), Customer != "") %>%
      group_by(Entity = Customer) %>%
      summarise(
        Activity_Value = sum(pmax(Revenue, 0), na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Transaction_Count = n(),
        .groups = "drop"
      )
    
    customer_po <- filtered_po() %>%
      filter(!is.na(Client), Client != "") %>%
      group_by(Entity = Client) %>%
      summarise(
        PO_Count = n_distinct(PO_Number),
        PO_Value = sum(pmax(Total_PO, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    customer_ar <- filtered_ar() %>%
      filter(!is.na(Customer), Customer != "") %>%
      group_by(Entity = Customer) %>%
      summarise(
        Open_Exposure = sum(pmax(Balance_Remaining, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    customer_metrics <- customer_revenue %>%
      full_join(customer_po, by = "Entity") %>%
      full_join(customer_ar, by = "Entity") %>%
      mutate(
        Entity_Type = "Customer",
        Record_Count = pmax(coalesce(PO_Count, 0), coalesce(Transaction_Count, 0)),
        Activity_Value = coalesce(Activity_Value, 0),
        Gross_Profit = coalesce(Gross_Profit, 0),
        Open_Exposure = coalesce(Open_Exposure, 0),
        PO_Count = coalesce(PO_Count, 0),
        PO_Value = coalesce(PO_Value, 0)
      ) %>%
      select(
        Entity_Type, Entity, Activity_Value, Gross_Profit,
        Open_Exposure, Record_Count, PO_Count, PO_Value
      )
    
    supplier_metrics <- filtered_inventory() %>%
      filter(
        Line_Type != "Information",
        !is.na(Supplier), Supplier != ""
      ) %>%
      group_by(Entity = Supplier) %>%
      summarise(
        Activity_Value = sum(pmax(Amount, 0), na.rm = TRUE),
        Gross_Profit = 0,
        Open_Exposure = sum(pmax(Open_Balance, 0), na.rm = TRUE),
        Record_Count = n_distinct(PO_Number),
        PO_Count = n_distinct(PO_Number),
        PO_Value = sum(pmax(Amount, 0), na.rm = TRUE),
        Backordered_Units = sum(pmax(Backordered_Qty, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Entity_Type = "Supplier") %>%
      select(
        Entity_Type, Entity, Activity_Value, Gross_Profit,
        Open_Exposure, Record_Count, PO_Count, PO_Value, Backordered_Units
      )
    
    rep_revenue <- filtered_revenue() %>%
      filter(!is.na(Staff), Staff != "") %>%
      group_by(Entity = Staff) %>%
      summarise(
        Activity_Value = sum(pmax(Revenue, 0), na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Record_Count = n(),
        Customer_Count = n_distinct(Customer),
        .groups = "drop"
      )
    
    rep_ar <- filtered_ar() %>%
      filter(!is.na(Staff), Staff != "") %>%
      group_by(Entity = Staff) %>%
      summarise(
        Open_Exposure = sum(pmax(Balance_Remaining, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    rep_metrics <- rep_revenue %>%
      full_join(rep_ar, by = "Entity") %>%
      mutate(
        Entity_Type = "Representative",
        Activity_Value = coalesce(Activity_Value, 0),
        Gross_Profit = coalesce(Gross_Profit, 0),
        Record_Count = coalesce(Record_Count, 0),
        Open_Exposure = coalesce(Open_Exposure, 0),
        PO_Count = 0,
        PO_Value = 0
      ) %>%
      select(
        Entity_Type, Entity, Activity_Value, Gross_Profit,
        Open_Exposure, Record_Count, PO_Count, PO_Value, Customer_Count
      )
    
    metric_rows <- bind_rows(customer_metrics, supplier_metrics, rep_metrics)
    
    geography_data %>%
      left_join(metric_rows, by = c("Entity_Type", "Entity")) %>%
      mutate(
        Activity_Value = coalesce(Activity_Value, 0),
        Gross_Profit = coalesce(Gross_Profit, 0),
        Open_Exposure = coalesce(Open_Exposure, 0),
        Record_Count = coalesce(Record_Count, 0),
        PO_Count = coalesce(PO_Count, 0),
        PO_Value = coalesce(PO_Value, 0),
        Backordered_Units = coalesce(Backordered_Units, 0),
        Customer_Count = coalesce(Customer_Count, 0),
        Entity_Key = paste(Entity_Type, Entity, sep = "||"),
        Search_Key = str_to_lower(paste(
          Entity_Type, Entity_ID, Entity, Continent, Country, Region, City,
          Representative
        ))
      )
  })
  
  observe({
    data_version()
    continent <- atlas_input_text(input$atlas_continent, "all")
    
    countries <- geography_data
    if (continent != "all") {
      countries <- countries %>% filter(Continent == continent)
    }
    country_values <- sort(unique(na.omit(countries$Country)))
    selected <- isolate(atlas_input_text(input$atlas_country, "all"))
    if (!selected %in% country_values) selected <- "all"
    
    updateSelectizeInput(
      session, "atlas_country",
      choices = c("All Countries" = "all", setNames(country_values, country_values)),
      selected = selected,
      server = TRUE
    )
  })
  
  observe({
    data_version()
    continent <- atlas_input_text(input$atlas_continent, "all")
    country <- atlas_input_text(input$atlas_country, "all")
    
    regions <- geography_data
    if (continent != "all") regions <- regions %>% filter(Continent == continent)
    if (country != "all") regions <- regions %>% filter(Country == country)
    
    region_values <- sort(unique(na.omit(regions$Region[regions$Region != ""])))
    selected <- isolate(atlas_input_text(input$atlas_region, "all"))
    if (!selected %in% region_values) selected <- "all"
    
    updateSelectizeInput(
      session, "atlas_region",
      choices = c("All Regions" = "all", setNames(region_values, region_values)),
      selected = selected,
      server = TRUE
    )
  })
  
  observe({
    data_version()
    continent <- atlas_input_text(input$atlas_continent, "all")
    country <- atlas_input_text(input$atlas_country, "all")
    region <- atlas_input_text(input$atlas_region, "all")
    
    cities <- geography_data
    if (continent != "all") cities <- cities %>% filter(Continent == continent)
    if (country != "all") cities <- cities %>% filter(Country == country)
    if (region != "all") cities <- cities %>% filter(Region == region)
    
    city_values <- sort(unique(na.omit(cities$City[cities$City != ""])))
    selected <- isolate(atlas_input_text(input$atlas_city, "all"))
    if (!selected %in% city_values) selected <- "all"
    
    updateSelectizeInput(
      session, "atlas_city",
      choices = c("All Cities" = "all", setNames(city_values, city_values)),
      selected = selected,
      server = TRUE
    )
  })
  
  atlas_filtered_data <- reactive({
    df <- atlas_network_data()
    
    entity_types <- input$atlas_entity_types
    if (!is.null(entity_types) && length(entity_types) > 0) {
      df <- df %>% filter(Entity_Type %in% entity_types)
    }
    
    continent <- atlas_input_text(input$atlas_continent, "all")
    country <- atlas_input_text(input$atlas_country, "all")
    region <- atlas_input_text(input$atlas_region, "all")
    city <- atlas_input_text(input$atlas_city, "all")
    search <- str_to_lower(atlas_input_text(input$atlas_search, ""))
    
    if (continent != "all") df <- df %>% filter(Continent == continent)
    if (country != "all") df <- df %>% filter(Country == country)
    if (region != "all") df <- df %>% filter(Region == region)
    if (city != "all") df <- df %>% filter(City == city)
    
    if (search != "") {
      df <- df %>% filter(str_detect(Search_Key, fixed(search)))
    }
    
    metric <- atlas_input_text(input$atlas_metric, "activity")
    df <- df %>%
      mutate(
        Metric_Value = case_when(
          metric == "count" ~ as.numeric(Record_Count),
          metric == "exposure" ~ as.numeric(Open_Exposure),
          metric == "profit" ~ as.numeric(Gross_Profit),
          TRUE ~ as.numeric(Activity_Value)
        ),
        Metric_Value = coalesce(Metric_Value, 0)
      )
    
    df
  })
  
  atlas_selected_row <- reactive({
    key <- atlas_selected_key()
    if (is.null(key) || key == "") return(atlas_network_data()[0, , drop = FALSE])
    
    atlas_network_data() %>%
      filter(Entity_Key == key) %>%
      slice(1)
  })
  
  output$atlas_kpi_locations <- renderText({
    format(nrow(atlas_filtered_data()), big.mark = ",")
  })
  
  output$atlas_kpi_countries <- renderText({
    format(n_distinct(atlas_filtered_data()$Country), big.mark = ",")
  })
  
  output$atlas_kpi_activity <- renderText({
    dollar(sum(atlas_filtered_data()$Activity_Value, na.rm = TRUE), accuracy = 1)
  })
  
  output$atlas_kpi_exposure <- renderText({
    dollar(sum(atlas_filtered_data()$Open_Exposure, na.rm = TRUE), accuracy = 1)
  })
  
  output$atlas_world_map <- renderPlotly({
    df <- atlas_filtered_data()
    validate(need(nrow(df) > 0, "No geographic relationships match the current Atlas filters."))
    
    magnitude <- sqrt(pmax(abs(df$Metric_Value), 0))
    if (all(!is.finite(magnitude)) || max(magnitude, na.rm = TRUE) <= 0) {
      marker_size <- rep(10, nrow(df))
    } else {
      marker_size <- scales::rescale(
        magnitude,
        to = c(7, 31),
        from = range(magnitude, na.rm = TRUE)
      )
    }
    
    # Small deterministic displacement preserves city context when many
    # relationships share an office / industrial hub.
    row_index <- seq_len(nrow(df))
    df <- df %>%
      mutate(
        Map_Latitude = Latitude + ((row_index %% 7) - 3) * .055,
        Map_Longitude = Longitude + ((row_index %% 9) - 4) * .065,
        Marker_Size = marker_size,
        Hover_Text = paste0(
          "<b>", Entity, "</b>",
          "<br>", Entity_Type,
          "<br>", City, ifelse(Region == "", "", paste0(", ", Region)), ", ", Country,
          "<br>", atlas_metric_title(atlas_input_text(input$atlas_metric, "activity")),
          ": ", ifelse(
            atlas_input_text(input$atlas_metric, "activity") == "count",
            format(round(Metric_Value), big.mark = ","),
            dollar(Metric_Value, accuracy = 1)
          ),
          "<br>Open exposure: ", dollar(Open_Exposure, accuracy = 1),
          "<br>Records: ", format(round(Record_Count), big.mark = ",")
        )
      )
    
    selected <- atlas_selected_key()
    df$Marker_Line_Width <- ifelse(df$Entity_Key == selected, 3.5, .7)
    df$Marker_Line_Color <- ifelse(df$Entity_Key == selected, "#FFD76B", "rgba(255,255,255,.75)")
    
    chart <- plot_ly(
      df,
      source = "atlas_world",
      type = "scattergeo",
      mode = "markers",
      lon = ~Map_Longitude,
      lat = ~Map_Latitude,
      color = ~Entity_Type,
      colors = atlas_entity_colors,
      key = ~Entity_Key,
      customdata = ~Entity_Key,
      text = ~Hover_Text,
      hovertemplate = "%{text}<extra></extra>",
      marker = list(
        size = df$Marker_Size,
        opacity = .78,
        line = list(
          width = df$Marker_Line_Width,
          color = df$Marker_Line_Color
        )
      )
    ) %>%
      layout(
        margin = list(l = 0, r = 0, t = 10, b = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        legend = list(
          orientation = "h",
          x = .02,
          y = .02,
          bgcolor = "rgba(7,10,15,.54)",
          font = list(color = "#DDE7F2")
        ),
        geo = list(
          projection = list(type = atlas_input_text(input$atlas_projection, "natural earth")),
          showland = TRUE,
          landcolor = "#192437",
          showocean = TRUE,
          oceancolor = "#0C1524",
          showlakes = TRUE,
          lakecolor = "#0C1524",
          showcountries = TRUE,
          countrycolor = "rgba(160,183,209,.28)",
          coastlinecolor = "rgba(160,183,209,.22)",
          showframe = FALSE,
          bgcolor = "rgba(0,0,0,0)"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d")
      )
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "atlas_world", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "atlas_world", priority = "event")
      selected <- plotly_clicked_value(clicked)
      if (is.null(selected) || !str_detect(selected, "\\|\\|")) return()
      atlas_selected_key(selected)
    },
    ignoreInit = TRUE
  )
  
  output$atlas_selected_entity <- renderUI({
    row <- atlas_selected_row()
    
    if (nrow(row) == 0) {
      return(
        div(
          class = "atlas-entity-summary",
          div(class = "atlas-type", "Atlas Selection"),
          tags$h3("Select a relationship on the map"),
          div(class = "atlas-place", "Click a customer, supplier or representative to reveal business metrics and connected records."),
          div(
            class = "atlas-mini-grid",
            div(class = "atlas-mini-stat", tags$span("Navigation"), tags$strong("Continent → country → region → entity")),
            div(class = "atlas-mini-stat", tags$span("Workspace"), tags$strong("One-click cross-navigation"))
          )
        )
      )
    }
    
    div(
      class = "atlas-entity-summary",
      div(class = "atlas-type", row$Entity_Type[[1]]),
      tags$h3(row$Entity[[1]]),
      div(
        class = "atlas-place",
        paste(
          c(row$City[[1]], row$Region[[1]], row$Country[[1]])[
            c(row$City[[1]], row$Region[[1]], row$Country[[1]]) != ""
          ],
          collapse = " · "
        )
      ),
      div(
        class = "atlas-mini-grid",
        div(
          class = "atlas-mini-stat",
          tags$span("Activity Value"),
          tags$strong(dollar(row$Activity_Value[[1]], accuracy = 1))
        ),
        div(
          class = "atlas-mini-stat",
          tags$span("Gross Profit"),
          tags$strong(dollar(row$Gross_Profit[[1]], accuracy = 1))
        ),
        div(
          class = "atlas-mini-stat",
          tags$span("Open Exposure"),
          tags$strong(dollar(row$Open_Exposure[[1]], accuracy = 1))
        ),
        div(
          class = "atlas-mini-stat",
          tags$span("Records"),
          tags$strong(format(round(row$Record_Count[[1]]), big.mark = ","))
        )
      )
    )
  })
  
  atlas_selected_history <- reactive({
    row <- atlas_selected_row()
    if (nrow(row) == 0) return(tibble())
    
    entity <- row$Entity[[1]]
    entity_type <- row$Entity_Type[[1]]
    
    if (entity_type == "Customer") {
      df <- revenue_data %>%
        filter(Customer == entity) %>%
        mutate(Period = floor_date(Date, "month")) %>%
        group_by(Period) %>%
        summarise(
          Value = sum(Revenue, na.rm = TRUE),
          Secondary = sum(Gross_Profit, na.rm = TRUE),
          .groups = "drop"
        )
    } else if (entity_type == "Supplier") {
      df <- inventory_data %>%
        filter(Line_Type != "Information", Supplier == entity) %>%
        mutate(Period = floor_date(Order_Date, "month")) %>%
        group_by(Period) %>%
        summarise(
          Value = sum(Amount, na.rm = TRUE),
          Secondary = sum(Open_Balance, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      df <- revenue_data %>%
        filter(Staff == entity) %>%
        mutate(Period = floor_date(Date, "month")) %>%
        group_by(Period) %>%
        summarise(
          Value = sum(Revenue, na.rm = TRUE),
          Secondary = sum(Gross_Profit, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    if (input$global_year != "all") {
      df <- df %>% filter(year(Period) == as.numeric(input$global_year))
    }
    if (input$global_month != "all") {
      df <- df %>% filter(month(Period) == as.numeric(input$global_month))
    }
    
    df %>% arrange(Period)
  })
  
  output$atlas_selected_timeline <- renderPlotly({
    df <- atlas_selected_history()
    validate(need(nrow(df) > 0, "No timeline records are available for this selection and period."))
    
    chart <- plot_ly(df, source = "atlas_timeline") %>%
      add_lines(
        x = ~Period,
        y = ~Value,
        mode = "lines+markers",
        name = "Activity",
        line = list(color = "#FF7A45", width = 3),
        marker = list(size = 7),
        hovertemplate = "%{x|%b %Y}<br>Activity: $%{y:,.0f}<extra></extra>"
      ) %>%
      add_lines(
        x = ~Period,
        y = ~Secondary,
        mode = "lines",
        name = "Profit / exposure",
        line = list(color = "#FFC857", width = 2, dash = "dot"),
        hovertemplate = "%{x|%b %Y}<br>Secondary: $%{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        margin = list(l = 55, r = 20, t = 15, b = 45),
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    chart
  })
  
  output$atlas_selected_contacts <- renderDT({
    row <- atlas_selected_row()
    if (nrow(row) == 0) {
      return(datatable(
        tibble(Information = "Select a map relationship to display contacts."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    entity <- row$Entity[[1]]
    entity_type <- row$Entity_Type[[1]]
    
    df <- if (entity_type %in% c("Customer", "Supplier")) {
      relationship_directory %>%
        filter(
          Entity == entity |
            str_to_lower(Entity) == str_to_lower(entity)
        )
    } else {
      relationship_directory %>%
        filter(
          AURELIS_Rep == entity |
            Contact_Name == entity
        )
    }
    
    if (nrow(df) == 0) {
      df <- tibble(
        Contact_Name = row$Representative[[1]],
        Position = ifelse(entity_type == "Representative", "Aurelis Representative", ""),
        Email = "",
        Phone = ""
      )
    }
    
    df <- df %>%
      select(any_of(c("Contact_Name", "Position", "Department", "Email", "Phone"))) %>%
      slice_head(n = 8)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE, pageLength = 8)
    )
  })
  
  output$atlas_location_table <- renderDT({
    df <- atlas_filtered_data() %>%
      arrange(Continent, Country, Region, City, Entity_Type, Entity) %>%
      transmute(
        Type = Entity_Type,
        `Entity ID` = Entity_ID,
        Entity,
        Continent,
        Country,
        Region,
        City,
        `Activity Value` = Activity_Value,
        `Open Exposure` = Open_Exposure,
        Records = Record_Count,
        Entity_Key
      )
    
    display_df <- df %>% select(-Entity_Key)
    
    datatable(
      display_df,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ) %>%
      formatCurrency(c("Activity Value", "Open Exposure"), "$", digits = 0)
  })
  
  observeEvent(input$atlas_location_table_rows_selected, {
    idx <- input$atlas_location_table_rows_selected
    if (length(idx) != 1) return()
    
    df <- atlas_filtered_data() %>%
      arrange(Continent, Country, Region, City, Entity_Type, Entity)
    
    if (idx < 1 || idx > nrow(df)) return()
    atlas_selected_key(df$Entity_Key[[idx]])
  }, ignoreInit = TRUE)
  
  atlas_geo_hierarchy <- reactive({
    df <- atlas_filtered_data()
    validate(need(nrow(df) > 0, "No geographic business hierarchy matches the active Atlas filters."))
    
    metric <- atlas_input_text(input$atlas_metric, "activity")
    
    continents <- df %>%
      group_by(Continent) %>%
      summarise(Value = sum(Metric_Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        ID = paste0("CONT::", Continent),
        Parent = "",
        Label = Continent,
        Level = "Continent"
      )
    
    countries <- df %>%
      group_by(Continent, Country) %>%
      summarise(Value = sum(Metric_Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        ID = paste0("COUNTRY::", Continent, "::", Country),
        Parent = paste0("CONT::", Continent),
        Label = Country,
        Level = "Country"
      )
    
    cities <- df %>%
      group_by(Continent, Country, City) %>%
      summarise(Value = sum(Metric_Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        ID = paste0("CITY::", Continent, "::", Country, "::", City),
        Parent = paste0("COUNTRY::", Continent, "::", Country),
        Label = ifelse(City == "", "Unspecified city", City),
        Level = "City"
      )
    
    bind_rows(
      continents %>% select(ID, Parent, Label, Level, Value),
      countries %>% select(ID, Parent, Label, Level, Value),
      cities %>% select(ID, Parent, Label, Level, Value)
    ) %>%
      mutate(
        Value = pmax(coalesce(Value, 0), 0),
        Custom = paste(Level, Label, sep = "||")
      )
  })
  
  observeEvent(input$atlas_breakdown_reset, {
    # Restore the hierarchy to its initial world view. The metric, projection
    # and relationship-type selection remain intact; only geographic drill
    # filters created by the hierarchy are cleared.
    updateSelectInput(session, "atlas_continent", selected = "all")
    updateSelectizeInput(session, "atlas_country", selected = "all")
    updateSelectizeInput(session, "atlas_region", selected = "all")
    updateSelectizeInput(session, "atlas_city", selected = "all")
    updateTextInput(session, "atlas_search", value = "")
    atlas_selected_key("")
    atlas_breakdown_reset_version(atlas_breakdown_reset_version() + 1L)
  }, ignoreInit = TRUE)
  
  output$atlas_country_breakdown <- renderPlotly({
    atlas_breakdown_reset_version()
    df <- atlas_geo_hierarchy()
    validate(need(sum(df$Value, na.rm = TRUE) > 0, "The selected Atlas metric has no positive values for this scope."))
    
    chart <- plot_ly(
      df,
      source = "atlas_sunburst",
      type = "sunburst",
      ids = ~ID,
      labels = ~Label,
      parents = ~Parent,
      values = ~Value,
      customdata = ~Custom,
      branchvalues = "total",
      maxdepth = 3,
      marker = list(
        colors = rep(c("#FF7A45", "#FFC857", "#F0544F", "#32D1C6", "#63D99A", "#8A72F5"), length.out = nrow(df))
      ),
      hovertemplate = "<b>%{label}</b><br>Value: %{value:,.0f}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 10, r = 10, t = 10, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "atlas_sunburst", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "atlas_sunburst", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      
      custom <- if (!is.null(clicked$customdata) && length(clicked$customdata) > 0) {
        as.character(clicked$customdata[[1]])
      } else {
        ""
      }
      
      if (custom == "" || !str_detect(custom, "\\|\\|")) return()
      
      parts <- str_split(custom, fixed("||"), simplify = TRUE)
      level <- parts[1]
      value <- parts[2]
      
      if (level == "Continent") {
        updateSelectInput(session, "atlas_continent", selected = value)
      } else if (level == "Country") {
        updateSelectizeInput(session, "atlas_country", selected = value)
      } else if (level == "City") {
        updateSelectizeInput(session, "atlas_city", selected = value)
      }
    },
    ignoreInit = TRUE
  )
  
  atlas_relationship_data <- reactive({
    row <- atlas_selected_row()
    if (nrow(row) == 0) return(tibble())
    
    entity <- row$Entity[[1]]
    entity_type <- row$Entity_Type[[1]]
    
    if (entity_type == "Customer") {
      suppliers <- filtered_inventory() %>%
        filter(Line_Type != "Information", Client == entity, Supplier != "") %>%
        group_by(Related = Supplier) %>%
        summarise(Value = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Supplier")
      
      buyers <- filtered_inventory() %>%
        filter(Line_Type != "Information", Client == entity, Buyer != "") %>%
        group_by(Related = Buyer) %>%
        summarise(Value = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Buyer")
      
      bind_rows(suppliers, buyers)
    } else if (entity_type == "Supplier") {
      customers <- filtered_inventory() %>%
        filter(Line_Type != "Information", Supplier == entity, Client != "") %>%
        group_by(Related = Client) %>%
        summarise(Value = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Customer")
      
      buyers <- filtered_inventory() %>%
        filter(Line_Type != "Information", Supplier == entity, Buyer != "") %>%
        group_by(Related = Buyer) %>%
        summarise(Value = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Buyer")
      
      bind_rows(customers, buyers)
    } else {
      customers <- filtered_revenue() %>%
        filter(Staff == entity, Customer != "") %>%
        group_by(Related = Customer) %>%
        summarise(Value = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Customer")
      
      countries <- filtered_revenue() %>%
        filter(Staff == entity, Country != "") %>%
        group_by(Related = Country) %>%
        summarise(Value = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
        mutate(Related_Type = "Country")
      
      bind_rows(customers, countries)
    }
  })
  
  output$atlas_relationship_network <- renderPlotly({
    row <- atlas_selected_row()
    validate(need(nrow(row) > 0, "Select a map relationship to explore its connected business network."))
    
    df <- atlas_relationship_data() %>%
      filter(is.finite(Value), Value > 0) %>%
      arrange(desc(Value)) %>%
      slice_head(n = 20) %>%
      arrange(Value)
    
    validate(need(nrow(df) > 0, "No connected business relationships are available in the active period."))
    
    df$Related <- factor(df$Related, levels = df$Related)
    
    chart <- plot_ly(
      df,
      source = "atlas_relationship",
      y = ~Related,
      x = ~Value,
      key = ~paste(Related_Type, Related, sep = "||"),
      customdata = ~paste(Related_Type, Related, sep = "||"),
      color = ~Related_Type,
      colors = c(
        Customer = "#FF7A45",
        Supplier = "#FFC857",
        Buyer = "#8A72F5",
        Country = "#32D1C6"
      ),
      type = "bar",
      orientation = "h",
      hovertemplate = "<b>%{y}</b><br>%{fullData.name}: $%{x:,.0f}<br>Click to navigate<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Connected activity value", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 190, r = 30, t = 20, b = 55),
        legend = list(orientation = "h", y = 1.12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "atlas_relationship", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "atlas_relationship", priority = "event")
      selected <- plotly_clicked_value(clicked)
      if (is.null(selected) || !str_detect(selected, "\\|\\|")) return()
      
      parts <- str_split(selected, fixed("||"), simplify = TRUE)
      related_type <- parts[1]
      related <- parts[2]
      
      if (related_type == "Customer") {
        matching_key <- atlas_network_data() %>%
          filter(Entity_Type == "Customer", Entity == related) %>%
          pull(Entity_Key)
        if (length(matching_key) > 0) atlas_selected_key(matching_key[[1]])
        open_customer_workspace(related)
      } else if (related_type == "Supplier") {
        matching_key <- atlas_network_data() %>%
          filter(Entity_Type == "Supplier", Entity == related) %>%
          pull(Entity_Key)
        if (length(matching_key) > 0) atlas_selected_key(matching_key[[1]])
        open_supplier_workspace(related)
      } else if (related_type == "Buyer") {
        open_buyer_workspace(related)
      } else if (related_type == "Country") {
        updateSelectizeInput(session, "atlas_country", selected = related)
      }
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$atlas_reset, {
    updateSelectizeInput(
      session, "atlas_entity_types",
      selected = c("Customer", "Supplier", "Representative")
    )
    updateSelectInput(session, "atlas_continent", selected = "all")
    updateSelectizeInput(session, "atlas_country", selected = "all")
    updateSelectizeInput(session, "atlas_region", selected = "all")
    updateSelectizeInput(session, "atlas_city", selected = "all")
    updateTextInput(session, "atlas_search", value = "")
    updateSelectInput(session, "atlas_metric", selected = "activity")
    updateSelectInput(session, "atlas_projection", selected = "natural earth")
    atlas_selected_key("")
  }, ignoreInit = TRUE)
  
  observeEvent(input$atlas_focus_selected, {
    row <- atlas_selected_row()
    if (nrow(row) == 0) {
      showNotification("Select a map relationship first.", type = "warning", duration = 4)
      return()
    }
    
    updateSelectInput(session, "atlas_continent", selected = row$Continent[[1]])
    updateSelectizeInput(session, "atlas_country", selected = row$Country[[1]])
    updateSelectizeInput(session, "atlas_region", selected = row$Region[[1]])
    updateSelectizeInput(session, "atlas_city", selected = row$City[[1]])
    updateSelectizeInput(session, "atlas_entity_types", selected = row$Entity_Type[[1]])
    updateTextInput(session, "atlas_search", value = row$Entity[[1]])
  }, ignoreInit = TRUE)
  
  observeEvent(input$atlas_open_workspace, {
    row <- atlas_selected_row()
    if (nrow(row) == 0) {
      showNotification("Select a customer, supplier or representative first.", type = "warning", duration = 4)
      return()
    }
    
    entity <- row$Entity[[1]]
    entity_type <- row$Entity_Type[[1]]
    
    if (entity_type == "Customer") {
      open_customer_workspace(entity)
    } else if (entity_type == "Supplier") {
      open_supplier_workspace(entity)
    } else {
      try(updateSelectInput(session, "perf_staff", selected = entity), silent = TRUE)
      navigate_sidebar("sale_performance")
    }
  }, ignoreInit = TRUE)
  
  ##############################################################################
  # 4.3 MULTI-USER WORKSPACE - Server Logic
  ##############################################################################
  
  multiuser_drafts <- reactive({
    multiuser_tick()
    buyer <- safe_workspace_buyer()
    aurelis_multiuser_list_drafts(buyer, include_shared = isTRUE(input$multi_user_include_shared))
  })
  
  observe({
    df <- multiuser_drafts()
    choices <- if (nrow(df) > 0) {
      labels <- paste0(
        df$quote_number, " · ", df$customer, " · ", df$owner,
        ifelse(df$is_shared == 1, " · shared", ""), " · v", df$version,
        " · ", df$saved_at
      )
      setNames(df$draft_id, labels)
    } else character(0)
    selected <- isolate(input$multi_user_draft_id)
    if (is.null(selected) || !selected %in% df$draft_id) selected <- ""
    updateSelectizeInput(session, "multi_user_draft_id", choices = c("Select a draft" = "", choices), selected = selected, server = TRUE)
  })
  
  output$multi_user_session_status <- renderUI({
    buyer <- safe_workspace_buyer()
    role <- safe_workspace_role()
    div(
      class = "multiuser-session-id",
      tags$strong("Session"), tags$br(), aurelis_session_id,
      tags$br(), tags$span(paste0("Owner: ", buyer)),
      tags$br(), tags$span(paste0("Profile: ", role))
    )
  })
  
  output$multi_user_active_count <- renderText({
    multiuser_tick()
    format(nrow(aurelis_multiuser_active_sessions(10)), big.mark = ",")
  })
  
  output$multi_user_draft_count <- renderText({
    format(nrow(multiuser_drafts()), big.mark = ",")
  })
  
  output$multi_user_refresh_status <- renderText({
    data_version()
    shared <- AURELIS_SHARED_REFRESH_STATE
    if (isTRUE(shared$running)) return("Refresh running")
    if (is.na(shared$last_refresh)) return("Initial data")
    paste0("v", shared$version, " · ", format(shared$last_refresh, "%H:%M:%S"))
  })
  
  output$multi_user_session_table <- renderDT({
    multiuser_tick()
    df <- aurelis_multiuser_active_sessions(10) %>%
      transmute(Buyer = buyer, Role = role, Started = started_at, `Last Seen` = last_seen)
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "compact")
  })
  
  output$multi_user_draft_table <- renderDT({
    df <- multiuser_drafts() %>%
      transmute(
        Draft = draft_id, Owner = owner, `Quotation No.` = quote_number,
        Customer = customer, Shared = ifelse(is_shared == 1, "Yes", "No"),
        Version = version, `Saved At` = saved_at
      )
    datatable(df, rownames = FALSE, selection = "none", options = list(pageLength = 8, dom = "tip", scrollX = TRUE), class = "compact")
  })
  
  output$multi_user_activity_table <- renderDT({
    multiuser_tick()
    df <- aurelis_multiuser_recent_activity(60) %>%
      transmute(Buyer = buyer, Role = role, Activity = event_type, Detail = detail, Time = event_at)
    datatable(df, rownames = FALSE, options = list(pageLength = 12, dom = "tip", scrollX = TRUE), class = "compact")
  })

  executive_activity_data <- reactive({
    multiuser_tick()
    activity <- .aurelis_demo_state$activity
    audit_path <- file.path(AURELIS_DATA_DIR, "aurelis_activity_log.csv")
    if (file.exists(audit_path)) {
      persisted <- tryCatch(read.csv(audit_path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.data.frame(persisted) && nrow(persisted) > 0) activity <- bind_rows(persisted, activity)
    }
    operational_history <- inventory_data %>%
      filter(Line_Type != "Information", !is.na(Buyer), Buyer != "") %>%
      transmute(
        session_id = "operational-history",
        buyer = Buyer,
        role = "Buyer",
        event_type = "purchase_order_activity",
        detail = paste0("PO ", PO_Number, " · ", Client, " · ", Item_Description),
        event_at = as.character(Order_Date)
      ) %>%
      arrange(desc(event_at)) %>%
      slice_head(n = 500)
    activity <- bind_rows(activity, operational_history)
    if (nrow(activity) == 0) {
      activity <- tibble(session_id = character(), buyer = character(), role = character(), event_type = character(), detail = character(), event_at = character())
    } else {
      activity <- activity %>% distinct(session_id, buyer, role, event_type, detail, event_at, .keep_all = TRUE)
    }
    activity
  })

  output$access_profile_badge <- renderText({
    role <- safe_workspace_role()
    if (identical(role, "CEO")) "CEO EXECUTIVE ACCESS · full observatory" else paste0(role, " ACCESS · restricted operating view")
  })

  output$exec_live_sessions <- renderText({
    multiuser_tick()
    format(nrow(aurelis_multiuser_active_sessions(10)), big.mark = ",")
  })

  output$exec_logged_events <- renderText({
    if (!role_can_see_executive()) return("Restricted")
    format(nrow(executive_activity_data()), big.mark = ",")
  })

  output$exec_future_commitments <- renderText({
    if (!role_can_see_executive()) return("Restricted")
    format(sum(!is.na(inventory_data$Delivery_Date) & inventory_data$Delivery_Date >= Sys.Date()), big.mark = ",")
  })

  output$exec_past_activity <- renderDT({
    validate(need(role_can_see_executive(), "CEO access required for the employee activity observatory."))
    df <- executive_activity_data() %>%
      arrange(desc(event_at)) %>%
      transmute(Employee = buyer, Role = role, Action = event_type, Detail = detail, Time = event_at)
    datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE), class = "compact")
  })

  output$exec_live_activity <- renderDT({
    validate(need(role_can_see_executive(), "CEO access required for live employee monitoring."))
    df <- aurelis_multiuser_active_sessions(10) %>%
      transmute(Employee = buyer, Role = role, `Session started` = started_at, `Last seen` = last_seen, Status = "Live")
    datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE), class = "compact")
  })

  output$exec_future_activity <- renderDT({
    validate(need(role_can_see_executive(), "CEO access required for future commitment monitoring."))
    df <- inventory_data %>%
      filter(Line_Type != "Information", !is.na(Delivery_Date), Delivery_Date >= Sys.Date()) %>%
      arrange(Delivery_Date) %>%
      transmute(
        Employee = Buyer,
        Client,
        Commitment = Item_Description,
        `Due date` = Delivery_Date,
        `Days out` = as.integer(Delivery_Date - Sys.Date()),
        Supplier,
        Status = Order_Status
      ) %>%
      slice_head(n = 100)
    datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE), class = "compact")
  })

  output$exec_activity_by_role <- renderPlotly({
    validate(need(role_can_see_executive(), "CEO access required for role analytics."))
    df <- executive_activity_data() %>%
      count(role, buyer, name = "Events") %>%
      group_by(role) %>%
      summarise(Events = sum(Events), Employees = n_distinct(buyer), .groups = "drop") %>%
      arrange(Events)
    validate(need(nrow(df) > 0, "No employee activity has been logged yet."))
    plot_ly(df, x = ~Events, y = ~reorder(role, Events), type = "bar", orientation = "h", text = ~paste(Employees, "employee(s)"), hoverinfo = "text+x", marker = list(color = "#27D5B2")) %>%
      layout(xaxis = list(title = "Logged actions"), yaxis = list(title = "Access role"), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$exec_access_governance <- renderUI({
    role <- safe_workspace_role()
    if (identical(role, "CEO")) {
      return(tagList(
        tags$h4(icon("crown"), " Executive command access"),
        tags$p("You can inspect session activity, historical events, role patterns, future delivery commitments, and shared quotation behavior."),
        tags$ul(
          tags$li(tags$b("CEO:"), " full observatory and cross-team visibility"),
          tags$li(tags$b("Manager:"), " team operations and shared workspaces"),
          tags$li(tags$b("Buyer:"), " assigned clients, buyers, suppliers, and quotations"),
          tags$li(tags$b("Analyst:"), " read-only intelligence and reporting")
        ),
        tags$p(class = "metric-definition-note", "This local demo profile is not authentication. Production enforcement must use company identity, server-side authorization, and persistent audit storage.")
      ))
    }
    tagList(
      tags$h4(icon("lock"), " Restricted observatory"),
      tags$p(paste0("Current profile: ", role, ". CEO access is required to view employee-level activity.")),
      tags$p(class = "metric-definition-note", "Your role can still use its permitted operating modules; the observatory intentionally withholds cross-team employee telemetry.")
    )
  })
  
  quotation_draft_text_ids <- c(
    "quotation_number", "quotation_customer_name", "quotation_customer_code", "quotation_rfq_reference",
    "quotation_customer_contact", "quotation_customer_email", "quotation_delivery_location", "quotation_payment_terms",
    "quotation_delivery_statement", "quotation_company_name", "quotation_company_contact", "quotation_company_address"
  )
  quotation_draft_textarea_ids <- c("quotation_customer_address", "quotation_scope_notes", "quotation_terms_notes", "quotation_internal_notes")
  quotation_draft_numeric_ids <- c(
    "quotation_validity_days", "quotation_cost_override", "quotation_freight", "quotation_insurance", "quotation_duty_pct",
    "quotation_handling_pct", "quotation_qaqc", "quotation_bank_fees", "quotation_packing", "quotation_other_costs",
    "quotation_vendor_min", "quotation_vendor_avg", "quotation_vendor_max", "quotation_aurelis_min", "quotation_aurelis_avg", "quotation_aurelis_max",
    "quotation_client_pay_min", "quotation_client_pay_avg", "quotation_client_pay_max", "quotation_rate_min", "quotation_rate_avg", "quotation_rate_max",
    paste0("quotation_pay_", rep(0:5, each = 3), "_", rep(c("min", "avg", "max"), times = 6)),
    "quotation_pricing_rate", "quotation_contingency_pct", "quotation_discount_pct", "quotation_tax_pct"
  )
  quotation_draft_select_ids <- c(
    "quotation_currency", "quotation_prepared_by", "quotation_incoterm",
    "quotation_interest_method", "quotation_finance_base", "quotation_selected_scenario",
    "quotation_pricing_method", "quotation_contingency_profile", "quotation_discount_mode", "quotation_rounding"
  )
  quotation_draft_checkbox_ids <- c("quotation_use_cost_override", "quotation_share_draft")
  
  quotation_snapshot <- reactive({
    fields <- unique(c(quotation_draft_text_ids, quotation_draft_textarea_ids, quotation_draft_numeric_ids, quotation_draft_select_ids, quotation_draft_checkbox_ids))
    input_values <- setNames(lapply(fields, function(id) isolate(input[[id]])), fields)
    input_values$quotation_date <- as.character(isolate(input$quotation_date))
    list(
      schema_version = 1,
      saved_at = as.character(Sys.time()),
      owner = safe_workspace_buyer(),
      inputs = input_values,
      lines = quotation_lines()
    )
  })
  
  apply_quotation_snapshot <- function(snapshot) {
    values <- snapshot$inputs
    updateSelectizeInput(session, "quotation_customer_lookup", selected = "")
    updateSelectInput(session, "quotation_payment_preset", selected = "custom")
    for (id in quotation_draft_text_ids) if (!is.null(values[[id]])) updateTextInput(session, id, value = as.character(values[[id]]))
    for (id in quotation_draft_textarea_ids) if (!is.null(values[[id]])) updateTextAreaInput(session, id, value = as.character(values[[id]]))
    for (id in quotation_draft_numeric_ids) if (!is.null(values[[id]])) updateNumericInput(session, id, value = suppressWarnings(as.numeric(values[[id]])))
    for (id in quotation_draft_select_ids) {
      if (!is.null(values[[id]])) {
        if (id %in% c("quotation_customer_lookup", "quotation_prepared_by")) updateSelectizeInput(session, id, selected = as.character(values[[id]]))
        else updateSelectInput(session, id, selected = as.character(values[[id]]))
      }
    }
    for (id in quotation_draft_checkbox_ids) if (!is.null(values[[id]])) updateCheckboxInput(session, id, value = isTRUE(values[[id]]))
    if (!is.null(values$quotation_date)) updateDateInput(session, "quotation_date", value = as.Date(values$quotation_date))
    if (!is.null(snapshot$lines)) {
      lines <- as.data.frame(snapshot$lines, stringsAsFactors = FALSE)
      required <- c("Line", "Part_Number", "Description", "Quantity", "Unit", "Supplier", "Vendor_Unit_Cost", "Supplier_Discount_Pct")
      for (nm in setdiff(required, names(lines))) lines[[nm]] <- if (nm %in% c("Line", "Quantity", "Vendor_Unit_Cost", "Supplier_Discount_Pct")) 0 else ""
      lines <- lines[, required, drop = FALSE]
      lines$Line <- seq_len(nrow(lines))
      lines$Quantity <- suppressWarnings(as.numeric(lines$Quantity))
      lines$Vendor_Unit_Cost <- suppressWarnings(as.numeric(lines$Vendor_Unit_Cost))
      lines$Supplier_Discount_Pct <- suppressWarnings(as.numeric(lines$Supplier_Discount_Pct))
      quotation_lines(as_tibble(lines))
    }
    invisible(TRUE)
  }
  
  observeEvent(input$quotation_save_draft, {
    req(requireNamespace("jsonlite", quietly = TRUE))
    snapshot <- quotation_snapshot()
    payload <- jsonlite::toJSON(snapshot, auto_unbox = TRUE, dataframe = "rows", null = "null", na = "null", digits = NA)
    existing <- current_draft_id()
    if (existing != "") {
      old <- aurelis_multiuser_get_draft(existing)
      if (is.null(old) || !identical(as.character(old$owner[[1]]), safe_workspace_buyer())) existing <- ""
    }
    result <- aurelis_multiuser_save_draft(
      owner = safe_workspace_buyer(), quote_number = input$quotation_number,
      customer = input$quotation_customer_name, payload_json = payload,
      is_shared = isTRUE(input$quotation_share_draft), draft_id = existing
    )
    current_draft_id(result$draft_id)
    aurelis_multiuser_log(aurelis_session_id, safe_workspace_buyer(), "quotation_saved", paste0(input$quotation_number, " · ", input$quotation_customer_name, " · draft ", result$draft_id, " v", result$version), safe_workspace_role())
    multiuser_tick(isolate(multiuser_tick()) + 1L)
    showNotification(paste0("Quotation draft saved as ", result$draft_id, " (version ", result$version, ")."), type = "message", duration = 5)
  }, ignoreInit = TRUE)
  
  observeEvent(input$multi_user_load_draft, {
    req(input$multi_user_draft_id != "")
    row <- aurelis_multiuser_get_draft(input$multi_user_draft_id)
    if (is.null(row)) {
      showNotification("That draft no longer exists.", type = "warning")
      return()
    }
    snapshot <- jsonlite::fromJSON(row$payload_json[[1]], simplifyDataFrame = TRUE)
    apply_quotation_snapshot(snapshot)
    current_draft_id(row$draft_id[[1]])
    aurelis_multiuser_log(aurelis_session_id, safe_workspace_buyer(), "quotation_opened", paste0(row$quote_number[[1]], " · draft ", row$draft_id[[1]]), safe_workspace_role())
    multiuser_tick(isolate(multiuser_tick()) + 1L)
    if (exists("updatebs4TabItems", mode = "function")) try(updatebs4TabItems(session, "sidebar_tabs", selected = "quotation_studio"), silent = TRUE)
    showNotification(paste0("Draft ", row$draft_id[[1]], " loaded into Quotation Studio."), type = "message", duration = 5)
  }, ignoreInit = TRUE)
  
  observeEvent(input$multi_user_delete_draft, {
    req(input$multi_user_draft_id != "")
    deleted <- aurelis_multiuser_delete_draft(input$multi_user_draft_id, safe_workspace_buyer())
    if (isTRUE(deleted)) {
      aurelis_multiuser_log(aurelis_session_id, safe_workspace_buyer(), "quotation_deleted", paste0("Draft ", input$multi_user_draft_id), safe_workspace_role())
      if (identical(current_draft_id(), input$multi_user_draft_id)) current_draft_id("")
      multiuser_tick(isolate(multiuser_tick()) + 1L)
      showNotification("Your quotation draft was deleted.", type = "message")
    } else {
      showNotification("Only the draft owner can delete this quotation draft.", type = "warning")
    }
  }, ignoreInit = TRUE)
  
  ##############################################################################
  # 4.3 KPI COMMAND CENTER - Server Logic
  ##############################################################################
  
  kpi_board_inventory <- reactive({
    filtered_inventory() %>% filter(Line_Type != "Information")
  })
  
  output$kpi_board_revenue <- renderText({
    dollar(sum(filtered_revenue()$Revenue, na.rm = TRUE), accuracy = 1)
  })
  
  output$kpi_board_margin <- renderText({
    revenue <- sum(filtered_revenue()$Revenue, na.rm = TRUE)
    profit <- sum(filtered_revenue()$Gross_Profit, na.rm = TRUE)
    if (revenue <= 0) "Not available" else percent(profit / revenue, accuracy = 0.1)
  })
  
  output$kpi_board_po_value <- renderText({
    dollar(sum(filtered_po()$Total_PO, na.rm = TRUE), accuracy = 1)
  })
  
  output$kpi_board_ar <- renderText({
    dollar(sum(pmax(filtered_ar()$Balance_Remaining, 0), na.rm = TRUE), accuracy = 1)
  })
  
  output$kpi_board_completion <- renderText({
    df <- kpi_board_inventory()
    ordered <- sum(df$Quantity, na.rm = TRUE)
    received <- sum(df$Received_Qty, na.rm = TRUE)
    if (ordered <= 0) "Not available" else percent(pmin(received / ordered, 1), accuracy = 0.1)
  })
  
  output$kpi_board_backorder <- renderText({
    df <- kpi_board_inventory()
    ordered <- sum(df$Quantity, na.rm = TRUE)
    backordered <- sum(pmax(df$Backordered_Qty, 0), na.rm = TRUE)
    if (ordered <= 0) "Not available" else percent(backordered / ordered, accuracy = 0.1)
  })
  
  output$kpi_board_customers <- renderText({
    customers <- unique(na.omit(c(filtered_revenue()$Customer, filtered_po()$Client, filtered_inventory()$Client)))
    format(length(customers), big.mark = ",")
  })
  
  output$kpi_board_buyers <- renderText({
    format(n_distinct(filtered_po()$Buyer[!is.na(filtered_po()$Buyer) & filtered_po()$Buyer != ""]), big.mark = ",")
  })
  
  output$kpi_board_trend <- renderPlotly({
    rev <- filtered_revenue() %>%
      mutate(Period = floor_date(Date, "month")) %>%
      group_by(Period) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), Gross_Profit = sum(Gross_Profit, na.rm = TRUE), .groups = "drop")
    po <- filtered_po() %>%
      mutate(Period = floor_date(Date, "month")) %>%
      group_by(Period) %>%
      summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), .groups = "drop")
    df <- full_join(rev, po, by = "Period") %>% arrange(Period) %>%
      mutate(across(c(Revenue, Gross_Profit, PO_Value, PO_Count), ~ replace_na(.x, 0)))
    validate(need(nrow(df) > 0, "No KPI trend data is available for the selected global period."))
    
    plot_ly(df) %>%
      add_bars(x = ~Period, y = ~Revenue, name = "Revenue", marker = list(color = brand_blue), hovertemplate = "%{x|%b %Y}<br>Revenue: $%{y:,.0f}<extra></extra>") %>%
      add_lines(x = ~Period, y = ~PO_Value, name = "PO Value", yaxis = "y2", line = list(color = brand_amber, width = 3), mode = "lines+markers", hovertemplate = "%{x|%b %Y}<br>PO value: $%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Revenue", tickformat = "$,.0f"),
        yaxis2 = list(title = "PO Value", overlaying = "y", side = "right", tickformat = "$,.0f"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(t = 55, b = 55, l = 70, r = 85),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$kpi_board_health <- renderDT({
    inv <- kpi_board_inventory()
    ordered <- sum(inv$Quantity, na.rm = TRUE)
    received <- sum(inv$Received_Qty, na.rm = TRUE)
    backordered <- sum(pmax(inv$Backordered_Qty, 0), na.rm = TRUE)
    rev <- sum(filtered_revenue()$Revenue, na.rm = TRUE)
    gp <- sum(filtered_revenue()$Gross_Profit, na.rm = TRUE)
    metrics <- tibble(
      KPI = c("Gross margin", "Quantity fulfillment", "Backorder rate", "Open AR", "PO count"),
      Current = c(
        if (rev > 0) percent(gp / rev, accuracy = .1) else "N/A",
        if (ordered > 0) percent(pmin(received / ordered, 1), accuracy = .1) else "N/A",
        if (ordered > 0) percent(backordered / ordered, accuracy = .1) else "N/A",
        dollar(sum(pmax(filtered_ar()$Balance_Remaining, 0), na.rm = TRUE), accuracy = 1),
        format(n_distinct(filtered_po()$PO_Number), big.mark = ",")
      ),
      Meaning = c(
        "Gross profit as a share of recognized revenue",
        "Received quantity divided by ordered quantity",
        "Backordered quantity divided by ordered quantity",
        "Customer balance still outstanding",
        "Distinct purchase orders in the selected period"
      )
    )
    datatable(metrics, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "compact")
  })

  # Future command deck: convert the existing filtered metrics into decisions.
  future_operating_metrics <- reactive({
    inv <- filtered_inventory() %>% filter(Line_Type != "Information")
    revenue <- sum(filtered_revenue()$Revenue, na.rm = TRUE)
    profit <- sum(filtered_revenue()$Gross_Profit, na.rm = TRUE)
    ordered <- sum(inv$Quantity, na.rm = TRUE)
    received <- sum(inv$Received_Qty, na.rm = TRUE)
    overdue <- sum(as.character(inv$Order_Status) == "Overdue", na.rm = TRUE)
    backordered <- sum(pmax(inv$Backordered_Qty, 0), na.rm = TRUE)
    open_ar <- sum(pmax(filtered_ar()$Balance_Remaining, 0), na.rm = TRUE)
    tibble(
      revenue = revenue,
      profit = profit,
      margin = if (revenue > 0) profit / revenue else NA_real_,
      fulfillment = if (ordered > 0) received / ordered else NA_real_,
      overdue = overdue,
      backordered = backordered,
      open_ar = open_ar
    )
  })

  output$future_now_signal <- renderText({
    m <- future_operating_metrics()
    if (is.finite(m$margin) && m$margin >= .25) "Operating posture: strong" else if (is.finite(m$margin)) "Operating posture: watch" else "Operating posture: awaiting signal"
  })

  output$future_now_detail <- renderText({
    m <- future_operating_metrics()
    paste0("", dollar(m$revenue, accuracy = 1), " revenue · ", if (is.finite(m$margin)) percent(m$margin, accuracy = .1) else "N/A", " gross margin")
  })

  output$future_risk_signal <- renderText({
    m <- future_operating_metrics()
    if (m$overdue > 0 || m$backordered > 0) paste(format(m$overdue + m$backordered, big.mark = ","), "exceptions") else "No critical exceptions"
  })

  output$future_risk_detail <- renderText({
    m <- future_operating_metrics()
    paste(format(m$overdue, big.mark = ","), "overdue lines ·", format(m$backordered, big.mark = ","), "backordered units ·", dollar(m$open_ar, accuracy = 1), "open AR")
  })

  output$future_next_move <- renderText({
    m <- future_operating_metrics()
    if (m$overdue > 0 || m$backordered > 0) "Stabilize delivery flow" else if (is.finite(m$open_ar) && m$open_ar > 0) "Prioritize cash conversion" else "Build the next quote"
  })

  output$future_next_detail <- renderText({
    m <- future_operating_metrics()
    if (m$overdue > 0 || m$backordered > 0) "Open the delivery control tower and resolve exceptions." else if (m$open_ar > 0) "Review customer balances and aging before expansion." else "Use Product Intelligence to shape the next opportunity."
  })

  future_open_tab <- function(tab_name) {
    if (exists("updatebs4TabItems", mode = "function")) {
      try(updatebs4TabItems(session, "sidebar_tabs", selected = tab_name), silent = TRUE)
    } else if (exists("updateTabItems", mode = "function")) {
      try(updateTabItems(session, "sidebar_tabs", selected = tab_name), silent = TRUE)
    }
  }

  observeEvent(input$future_open_next, {
    m <- future_operating_metrics()
    future_open_tab(if (m$overdue > 0 || m$backordered > 0) "order_tracker" else if (m$open_ar > 0) "ar_tab" else "product_intelligence")
  }, ignoreInit = TRUE)

  observeEvent(input$future_open_risk, {
    m <- future_operating_metrics()
    future_open_tab(if (m$overdue > 0 || m$backordered > 0) "order_tracker" else "ar_tab")
  }, ignoreInit = TRUE)
  
  output$kpi_board_customer_rank <- renderPlotly({
    df <- filtered_revenue() %>%
      filter(!is.na(Customer), Customer != "") %>%
      group_by(Customer) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), Gross_Profit = sum(Gross_Profit, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Revenue)) %>% slice_head(n = 12) %>% arrange(Revenue)
    validate(need(nrow(df) > 0, "No customer contribution data is available."))
    df$Customer <- factor(df$Customer, levels = df$Customer)
    
    chart <- plot_ly(
      df, source = "kpi_customer_rank",
      y = ~Customer, x = ~Revenue,
      key = ~as.character(Customer), customdata = ~as.character(Customer),
      type = "bar", orientation = "h",
      marker = list(color = brand_sky),
      text = ~dollar(Revenue, accuracy = 1), textposition = "auto",
      hovertemplate = "<b>%{y}</b><br>Revenue: $%{x:,.2f}<br>Click for customer performance<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Revenue", tickformat = "$,.0f"), yaxis = list(title = "", automargin = TRUE), margin = list(l = 190, r = 25, t = 20, b = 50), showlegend = FALSE, plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "kpi_customer_rank", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "kpi_customer_rank", priority = "event"))
    if (!is.null(selected)) open_customer_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$kpi_board_buyer_rank <- renderPlotly({
    df <- filtered_po() %>%
      filter(!is.na(Buyer), Buyer != "") %>%
      group_by(Buyer) %>%
      summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), Customers = n_distinct(Client), .groups = "drop") %>%
      arrange(desc(PO_Value)) %>% slice_head(n = 12) %>% arrange(PO_Value)
    validate(need(nrow(df) > 0, "No buyer activity is available."))
    df$Buyer <- factor(df$Buyer, levels = df$Buyer)
    
    chart <- plot_ly(
      df, source = "kpi_buyer_rank",
      y = ~Buyer, x = ~PO_Value,
      key = ~as.character(Buyer), customdata = ~as.character(Buyer),
      type = "bar", orientation = "h", marker = list(color = brand_amber),
      text = ~paste0(PO_Count, " POs"), textposition = "auto",
      hovertemplate = "<b>%{y}</b><br>PO value: $%{x:,.2f}<br>%{text}<br>Click for buyer activity<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "PO Value", tickformat = "$,.0f"), yaxis = list(title = "", automargin = TRUE), margin = list(l = 135, r = 25, t = 20, b = 50), showlegend = FALSE, plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "kpi_buyer_rank", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "kpi_buyer_rank", priority = "event"))
    if (!is.null(selected)) open_buyer_workspace(selected)
  }, ignoreInit = TRUE)
  
  ##############################################################################
  # 4.3 BUYER ACTIVITY INTELLIGENCE - Server Logic
  ##############################################################################
  
  buyer_activity_base <- reactive({
    data_version()
    daily_po %>%
      filter(!is.na(Date), Date <= Sys.Date()) %>%
      mutate(
        Week_Of_Month = pmin(ceiling(day(Date) / 7), 5),
        Day_Of_Month = day(Date)
      )
  })
  
  observeEvent(input$buyer_act_year, {
    df <- buyer_activity_base()
    if (!is.null(input$buyer_act_year) && input$buyer_act_year != "all") df <- df %>% filter(Year == as.numeric(input$buyer_act_year))
    months <- sort(unique(na.omit(df$Month)))
    choices <- c("All Months" = "all", setNames(months, month.name[months]))
    selected <- isolate(input$buyer_act_month)
    if (is.null(selected) || (!identical(selected, "all") && !as.numeric(selected) %in% months)) selected <- "all"
    updateSelectInput(session, "buyer_act_month", choices = choices, selected = selected)
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$buyer_act_year, input$buyer_act_month), {
    df <- buyer_activity_base()
    if (!is.null(input$buyer_act_year) && input$buyer_act_year != "all") df <- df %>% filter(Year == as.numeric(input$buyer_act_year))
    if (!is.null(input$buyer_act_month) && input$buyer_act_month != "all") df <- df %>% filter(Month == as.numeric(input$buyer_act_month))
    weeks <- sort(unique(na.omit(df$Week_Of_Month)))
    choices <- c("All Weeks" = "all", setNames(weeks, paste("Week", weeks)))
    selected <- isolate(input$buyer_act_week)
    if (is.null(selected) || (!identical(selected, "all") && !as.numeric(selected) %in% weeks)) selected <- "all"
    updateSelectInput(session, "buyer_act_week", choices = choices, selected = selected)
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$buyer_act_year, input$buyer_act_month, input$buyer_act_week), {
    df <- buyer_activity_base()
    if (!is.null(input$buyer_act_year) && input$buyer_act_year != "all") df <- df %>% filter(Year == as.numeric(input$buyer_act_year))
    if (!is.null(input$buyer_act_month) && input$buyer_act_month != "all") df <- df %>% filter(Month == as.numeric(input$buyer_act_month))
    if (!is.null(input$buyer_act_week) && input$buyer_act_week != "all") df <- df %>% filter(Week_Of_Month == as.numeric(input$buyer_act_week))
    days <- sort(unique(na.omit(df$Day_Of_Month)))
    choices <- c("All Days" = "all", setNames(days, paste0("Day ", days)))
    selected <- isolate(input$buyer_act_day)
    if (is.null(selected) || (!identical(selected, "all") && !as.numeric(selected) %in% days)) selected <- "all"
    updateSelectInput(session, "buyer_act_day", choices = choices, selected = selected)
  }, ignoreInit = FALSE)
  
  observeEvent(input$buyer_act_reset, {
    updateSelectInput(session, "buyer_act_year", selected = "all")
    updateSelectInput(session, "buyer_act_month", selected = "all")
    updateSelectInput(session, "buyer_act_week", selected = "all")
    updateSelectInput(session, "buyer_act_day", selected = "all")
    updateSelectizeInput(session, "buyer_act_buyer", selected = "all")
  })
  
  buyer_activity_filtered <- reactive({
    df <- buyer_activity_base()
    if (!is.null(input$buyer_act_year) && input$buyer_act_year != "all") df <- df %>% filter(Year == as.numeric(input$buyer_act_year))
    if (!is.null(input$buyer_act_month) && input$buyer_act_month != "all") df <- df %>% filter(Month == as.numeric(input$buyer_act_month))
    if (!is.null(input$buyer_act_week) && input$buyer_act_week != "all") df <- df %>% filter(Week_Of_Month == as.numeric(input$buyer_act_week))
    if (!is.null(input$buyer_act_day) && input$buyer_act_day != "all") df <- df %>% filter(Day_Of_Month == as.numeric(input$buyer_act_day))
    if (!is.null(input$buyer_act_buyer) && input$buyer_act_buyer != "all") df <- df %>% filter(Buyer == input$buyer_act_buyer)
    df
  })
  
  output$buyer_act_drill_path <- renderUI({
    labels <- c(
      if (is.null(input$buyer_act_year) || input$buyer_act_year == "all") "All years" else paste("Year", input$buyer_act_year),
      if (is.null(input$buyer_act_month) || input$buyer_act_month == "all") "All months" else month.name[as.numeric(input$buyer_act_month)],
      if (is.null(input$buyer_act_week) || input$buyer_act_week == "all") "All weeks" else paste("Week", input$buyer_act_week),
      if (is.null(input$buyer_act_day) || input$buyer_act_day == "all") "All days" else paste("Day", input$buyer_act_day),
      if (is.null(input$buyer_act_buyer) || input$buyer_act_buyer == "all") "All buyers" else input$buyer_act_buyer
    )
    div(class = "buyer-drill-path", lapply(labels, function(x) tags$span(class = "buyer-drill-chip", x)))
  })
  
  output$buyer_act_kpi_pos <- renderText(format(n_distinct(buyer_activity_filtered()$PO_Number), big.mark = ","))
  output$buyer_act_kpi_value <- renderText(dollar(sum(buyer_activity_filtered()$Total_PO, na.rm = TRUE), accuracy = 1))
  output$buyer_act_kpi_customers <- renderText(format(n_distinct(buyer_activity_filtered()$Client), big.mark = ","))
  output$buyer_act_kpi_avg <- renderText({
    df <- buyer_activity_filtered()
    if (nrow(df) == 0) return("$0")
    by_po <- df %>% group_by(PO_Number) %>% summarise(Value = max(Total_PO, na.rm = TRUE), .groups = "drop")
    dollar(mean(by_po$Value, na.rm = TRUE), accuracy = 1)
  })
  output$buyer_act_kpi_buyers <- renderText(format(n_distinct(buyer_activity_filtered()$Buyer), big.mark = ","))
  output$buyer_act_kpi_days <- renderText(format(n_distinct(buyer_activity_filtered()$Date), big.mark = ","))
  
  output$buyer_act_daily_trend <- renderPlotly({
    df <- buyer_activity_filtered() %>%
      group_by(Date) %>%
      summarise(PO_Count = n_distinct(PO_Number), PO_Value = sum(Total_PO, na.rm = TRUE), .groups = "drop") %>%
      arrange(Date)
    validate(need(nrow(df) > 0, "No buyer activity matches the selected drill-down."))
    
    chart <- plot_ly(df, source = "buyer_activity_date") %>%
      add_bars(x = ~Date, y = ~PO_Count, key = ~as.character(Date), customdata = ~as.character(Date),
               name = "PO Count", marker = list(color = brand_blue),
               hovertemplate = "%{x|%b %d, %Y}<br>POs: %{y}<extra></extra>") %>%
      add_lines(x = ~Date, y = ~PO_Value, key = ~as.character(Date), customdata = ~as.character(Date),
                name = "PO Value", yaxis = "y2", line = list(color = brand_amber, width = 3),
                mode = "lines+markers", hovertemplate = "%{x|%b %d, %Y}<br>Value: $%{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Activity date"), yaxis = list(title = "PO count"),
             yaxis2 = list(title = "PO value", overlaying = "y", side = "right", tickformat = "$,.0f"),
             legend = list(orientation = "h", x = 0, y = 1.12),
             margin = list(t = 50, l = 60, r = 85, b = 55),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "buyer_activity_date", priority = "event"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "buyer_activity_date", priority = "event")
    if (is.null(clicked) || nrow(clicked) == 0) return()
    value <- if (!is.null(clicked$customdata)) clicked$customdata[[1]] else clicked$x[[1]]
    apply_global_period_from_date(value)
  }, ignoreInit = TRUE)
  
  output$buyer_act_leaderboard <- renderPlotly({
    df <- buyer_activity_filtered() %>%
      filter(!is.na(Buyer), Buyer != "") %>%
      group_by(Buyer) %>%
      summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), Customers = n_distinct(Client), .groups = "drop") %>%
      arrange(desc(PO_Value)) %>% slice_head(n = 15) %>% arrange(PO_Value)
    validate(need(nrow(df) > 0, "No buyer records match the selected drill-down."))
    df$Buyer <- factor(df$Buyer, levels = df$Buyer)
    
    chart <- plot_ly(
      df, source = "buyer_activity_buyer",
      y = ~Buyer, x = ~PO_Value, key = ~as.character(Buyer), customdata = ~as.character(Buyer),
      type = "bar", orientation = "h", marker = list(color = brand_amber),
      text = ~paste0(PO_Count, " POs"), textposition = "auto",
      hovertemplate = "<b>%{y}</b><br>PO value: $%{x:,.2f}<br>%{text}<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "PO Value", tickformat = "$,.0f"), yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 135, r = 25, t = 20, b = 50), showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "buyer_activity_buyer", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "buyer_activity_buyer", priority = "event"))
    if (!is.null(selected)) updateSelectizeInput(session, "buyer_act_buyer", selected = selected)
  }, ignoreInit = TRUE)
  
  output$buyer_act_customer_mix <- renderPlotly({
    df <- buyer_activity_filtered() %>%
      filter(!is.na(Client), Client != "") %>%
      group_by(Client) %>%
      summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), .groups = "drop") %>%
      arrange(desc(PO_Value)) %>% slice_head(n = 12) %>% arrange(PO_Value)
    validate(need(nrow(df) > 0, "No customer activity matches the selected drill-down."))
    df$Client <- factor(df$Client, levels = df$Client)
    
    chart <- plot_ly(
      df, source = "buyer_activity_customer",
      y = ~Client, x = ~PO_Value, key = ~as.character(Client), customdata = ~as.character(Client),
      type = "bar", orientation = "h", marker = list(color = brand_sky),
      text = ~paste0(PO_Count, " POs"), textposition = "auto",
      hovertemplate = "<b>%{y}</b><br>PO value: $%{x:,.2f}<br>%{text}<br>Click for customer performance<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "PO Value", tickformat = "$,.0f"), yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 190, r = 25, t = 20, b = 50), showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "buyer_activity_customer", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "buyer_activity_customer", priority = "event"))
    if (!is.null(selected)) open_customer_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$buyer_act_table <- renderDT({
    df <- buyer_activity_filtered() %>%
      arrange(desc(Date), Buyer, PO_Number) %>%
      transmute(Date, Buyer, Customer = Client, PO_Number, PO_Value = Total_PO, Year, Month = Month_Name, Week = paste("Week", Week_Of_Month), Day = Day_Of_Month)
    datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)) %>%
      formatCurrency("PO_Value", currency = "$", digits = 2)
  })
  
  ##############################################################################
  # 4.2 EXECUTIVE OVERVIEW - Server Logic
  ##############################################################################
  
  # KPIs
  output$kpi_total_revenue <- renderText({
    data_version()
    paste0("$", format(round(sum(filtered_revenue()$Revenue, na.rm = TRUE), 0), big.mark = ","))
  })
  output$kpi_total_profit <- renderText({
    data_version()
    paste0("$", format(round(sum(filtered_revenue()$Gross_Profit, na.rm = TRUE), 0), big.mark = ","))
  })
  output$kpi_total_ar <- renderText({
    data_version()
    paste0("$", format(round(sum(filtered_ar()$Balance_Remaining, na.rm = TRUE), 0), big.mark = ","))
  })
  output$kpi_total_po <- renderText({
    data_version()
    format(nrow(filtered_po()), big.mark = ",")
  })
  
  # Revenue & Profit Monthly Trend
  output$exec_revenue_trend <- renderPlotly({
    data_version()
    req(nrow(filtered_revenue()) > 0)
    
    monthly <- filtered_revenue() %>%
      group_by(Year, Month, Month_Abbr) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Profit = sum(Gross_Profit, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Year, Month) %>%
      mutate(
        Period = paste(Month_Abbr, Year),
        Hover_Revenue = paste0("Revenue: $", format(round(Revenue), big.mark = ",")),
        Hover_Profit = paste0("Gross profit: $", format(round(Profit), big.mark = ","))
      )
    
    monthly$Period <- factor(monthly$Period, levels = monthly$Period)
    
    plot_ly(monthly, source = "exec_period") %>%
      add_trace(
        x = ~Period, y = ~Revenue,
        type = "scatter", mode = "lines",
        name = "Revenue",
        line = list(color = "#FF7A45", width = 3, shape = "spline"),
        fill = "tozeroy",
        fillcolor = "rgba(255,122,69,0.25)",
        text = ~Hover_Revenue,
        hovertemplate = "<b>%{x}</b><br>%{text}<extra></extra>"
      ) %>%
      add_trace(
        x = ~Period, y = ~Profit,
        type = "scatter", mode = "lines+markers",
        name = "Gross Profit",
        yaxis = "y2",
        line = list(color = "#FFC857", width = 3, shape = "spline"),
        marker = list(
          color = "#FFC857", size = 7,
          line = list(color = "#111827", width = 1)
        ),
        text = ~Hover_Profit,
        hovertemplate = "<b>%{x}</b><br>%{text}<extra></extra>"
      ) %>%
      layout(
        yaxis = list(
          title = "Revenue ($)", tickformat = "$,.0f",
          gridcolor = "rgba(255,255,255,.065)"
        ),
        yaxis2 = list(
          title = "Profit ($)", overlaying = "y", side = "right",
          tickformat = "$,.0f", showgrid = FALSE
        ),
        xaxis = list(
          title = "", tickangle = -45,
          gridcolor = "rgba(255,255,255,.035)"
        ),
        legend = list(orientation = "h", y = 1.10),
        margin = list(b = 80, l = 70, r = 70, t = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#CBD6E3")
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  # Profit Margin Distribution
  output$exec_profit_donut <- renderPlotly({
    data_version()
    req(nrow(filtered_revenue()) > 0)
    staff_profit <- filtered_revenue() %>%
      filter(!is.na(Staff) & Staff != "") %>%
      group_by(Staff) %>%
      summarise(Profit = sum(Gross_Profit, na.rm = TRUE), .groups = "drop") %>%
      filter(Profit > 0) %>%
      arrange(desc(Profit))
    
    chart <- plot_ly(
      staff_profit,
      source = "exec_rep_profit",
      labels = ~Staff,
      values = ~Profit,
      key = ~Staff,
      customdata = ~Staff,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      marker = list(colors = rep(pal_executive, length.out = nrow(staff_profit))),
      hovertemplate = "<b>%{label}</b><br>Gross profit: $%{value:,.2f}<br>%{percent}<extra></extra>"
    ) %>%
      layout(showlegend = FALSE,
             annotations = list(text = "Profit<br>by Rep", x = 0.5, y = 0.5, 
                                font = list(size = 14), showarrow = FALSE),
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)') %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  # Country Treemap
  output$exec_country_treemap <- renderPlotly({
    data_version()
    req(nrow(filtered_revenue()) > 0)
    
    country_rev <- filtered_revenue() %>%
      filter(!is.na(Country) & Country != "") %>%
      group_by(Country) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Customers = n_distinct(Customer),
        .groups = "drop"
      ) %>%
      filter(Revenue > 0)
    
    chart <- plot_geo(
      country_rev,
      source = "exec_country",
      locationmode = "country names"
    ) %>%
      add_trace(
        type = "choropleth",
        locations = ~Country,
        z = ~Revenue,
        key = ~Country,
        customdata = ~Country,
        colorscale = list(
          c(0.00, "#332A22"),
          c(0.22, "#8B452F"),
          c(0.48, "#F0544F"),
          c(0.72, "#FF7A45"),
          c(1.00, "#FFC857")
        ),
        marker = list(
          line = list(color = "rgba(9,13,22,.85)", width = .7)
        ),
        text = ~paste0(
          "<b>", Country, "</b>",
          "<br>Revenue: $", format(round(Revenue), big.mark = ","),
          "<br>Gross profit: $", format(round(Gross_Profit), big.mark = ","),
          "<br>Customers: ", Customers,
          "<br>Click for profitability"
        ),
        hovertemplate = "%{text}<extra></extra>",
        colorbar = list(
          title = "Revenue",
          tickprefix = "$",
          thickness = 12,
          len = .72,
          bgcolor = "rgba(0,0,0,0)",
          tickfont = list(color = "#B8C5D4"),
          titlefont = list(color = "#B8C5D4")
        )
      ) %>%
      layout(
        margin = list(t = 0, b = 0, l = 0, r = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        geo = list(
          projection = list(type = "natural earth"),
          bgcolor = "rgba(0,0,0,0)",
          showland = TRUE,
          landcolor = "#17212C",
          showocean = TRUE,
          oceancolor = "#0D141D",
          showlakes = TRUE,
          lakecolor = "#0D141D",
          showcountries = TRUE,
          countrycolor = "rgba(181,197,214,.18)",
          coastlinecolor = "rgba(181,197,214,.16)",
          showframe = FALSE
        )
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE
      )
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "exec_country", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "exec_country", priority = "event"))
    if (is.null(selected)) return()
    updateSelectInput(session, "perf_country", selected = selected)
    navigate_sidebar("sale_performance")
  }, ignoreInit = TRUE)
  
  # Top 10 Clients Bar - Now using mapped Client_Name
  output$exec_top_clients <- renderPlotly({
    data_version()
    req(nrow(filtered_revenue()) > 0)
    top_clients <- filtered_revenue() %>%
      filter(!is.na(Customer) & Customer != "") %>%
      group_by(Customer) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Revenue)) %>%
      head(10)
    
    top_clients <- top_clients %>% arrange(Revenue)
    top_clients$Customer <- factor(top_clients$Customer, levels = top_clients$Customer)
    
    plot_ly(
      top_clients,
      source = "exec_customer",
      y = ~Customer,
      x = ~Revenue,
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~Revenue,
        colorscale = list(
          c(0.00, "#3A2D24"),
          c(0.45, "#C45638"),
          c(0.75, "#FF7A45"),
          c(1.00, "#FFC857")
        )
      ),
      hovertemplate = "<b>%{y}</b><br>Revenue: $%{x:,.0f}<br>Click for customer performance<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Revenue ($)", tickformat = "$,.0f"),
             yaxis = list(title = ""),
             margin = list(l = 200, r = 20),
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Year-over-Year Comparison
  output$exec_yoy_comparison <- renderPlotly({
    data_version()
    yoy <- revenue_data %>%
      group_by(Year, Month, Month_Abbr) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(Year, Month)
    
    yoy$Month_Abbr <- factor(yoy$Month_Abbr, levels = month.abb)
    
    plot_ly(yoy, x = ~Month_Abbr, y = ~Revenue, color = ~factor(Year), type = "scatter", mode = "lines+markers",
            colors = pal_executive) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Revenue ($)", tickformat = "$,.0f"),
             legend = list(orientation = "h", y = 1.1),
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "exec_customer"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "exec_customer")
    if (is.null(clicked) || is.null(clicked$y)) return()
    customer_name <- as.character(clicked$y[[1]])
    updateSelectizeInput(
      session, "cust_perf_client",
      selected = customer_name,
      server = TRUE
    )
    navigate_sidebar("customer_performance")
    showNotification(
      paste0("Customer Performance opened for ", customer_name, "."),
      type = "message",
      duration = 4
    )
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "exec_rep_profit", priority = "event"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "exec_rep_profit", priority = "event")
    representative <- plotly_clicked_value(clicked)
    if (is.null(representative)) return()
    updateSelectInput(session, "perf_staff", selected = representative)
    navigate_sidebar("sale_performance")
    showNotification(
      paste0("Sales & Profitability focused on ", representative, "."),
      type = "message",
      duration = 4
    )
  })
  
  ##############################################################################
  # 4.3 MONTHLY SALES PERFORMANCE - Server Logic
  ##############################################################################
  
  sales_po <- reactive({
    data_version()
    specific_year <- if (is.null(input$sales_year_specific)) "global" else input$sales_year_specific
    
    if (specific_year == "global") {
      df <- filtered_po()
    } else {
      df <- daily_po %>% filter(Year == as.numeric(specific_year))
      if (!is.null(input$global_month) && input$global_month != "all") {
        df <- df %>% filter(Month == as.numeric(input$global_month))
      }
    }
    
    if (!is.null(input$sales_buyer) && input$sales_buyer != "all") {
      df <- df %>% filter(Buyer == input$sales_buyer)
    }
    if (!is.null(input$sales_client) && input$sales_client != "all") {
      df <- df %>% filter(Client == input$sales_client)
    }
    df
  })
  
  observeEvent(input$sales_reset, {
    updateSelectInput(session, "sales_buyer", selected = "all")
    updateSelectInput(session, "sales_client", selected = "all")
    updateSelectInput(session, "sales_year_specific", selected = "global")
  })
  
  sales_po_summary <- reactive({
    data_version()
    df <- sales_po()
    valid_po <- df %>% filter(!is.na(PO_Number), PO_Number != "")
    po_count <- n_distinct(valid_po$PO_Number)
    total_value <- sum(valid_po$Total_PO, na.rm = TRUE)
    
    tibble(
      PO_Count = po_count,
      Total_Value = total_value,
      Average_Value = if (po_count > 0) total_value / po_count else 0,
      Customers = n_distinct(valid_po$Client[!is.na(valid_po$Client) & valid_po$Client != ""])
    )
  })
  
  output$sales_kpi_po_count <- renderText({
    data_version()
    format(sales_po_summary()$PO_Count, big.mark = ",")
  })
  
  output$sales_kpi_total_value <- renderText({
    data_version()
    dollar(sales_po_summary()$Total_Value, accuracy = 1)
  })
  
  output$sales_kpi_avg_value <- renderText({
    data_version()
    dollar(sales_po_summary()$Average_Value, accuracy = 1)
  })
  
  output$sales_kpi_customers <- renderText({
    data_version()
    format(sales_po_summary()$Customers, big.mark = ",")
  })
  
  output$sales_heatmap <- renderPlotly({
    data_version()
    req(nrow(sales_po()) > 0)
    heat_data <- sales_po() %>%
      group_by(Buyer, Month_Name) %>%
      summarise(Count = n(), .groups = "drop")
    
    heat_data$Month_Name <- factor(heat_data$Month_Name, levels = month.name)
    
    heat_matrix <- heat_data %>%
      pivot_wider(names_from = Month_Name, values_from = Count, values_fill = 0)
    
    buyers <- heat_matrix$Buyer
    months_present <- intersect(month.name, names(heat_matrix))
    mat <- as.matrix(heat_matrix[, months_present])
    
    chart <- plot_ly(source = "po_heatmap", x = months_present, y = buyers, z = mat, type = "heatmap",
                     colorscale = list(c(0, "#E3F2FD"), c(0.5, "#42A5F5"), c(1, "#0D47A1")),
                     hovertemplate = "Buyer: %{y}<br>Month: %{x}<br>PO Count: %{z}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             margin = list(l = 100),
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
    
    register_plotly_click(chart)
  })
  
  output$sales_buyer_stack <- renderPlotly({
    data_version()
    req(nrow(sales_po()) > 0)
    
    stack_data <- sales_po() %>%
      group_by(Buyer, Month_Abbr, Month) %>%
      summarise(Total = sum(Total_PO, na.rm = TRUE), .groups = "drop") %>%
      arrange(Month)
    
    stack_data$Month_Abbr <- factor(stack_data$Month_Abbr, levels = month.abb)
    
    chart <- plot_ly(
      stack_data,
      source = "sales_buyer_stack",
      x = ~Month_Abbr,
      y = ~Total,
      color = ~Buyer,
      key = ~Buyer,
      customdata = ~Buyer,
      type = "bar",
      colors = pal_sales,
      hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>PO value: $%{y:,.0f}<br>Click to focus buyer<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "PO Value ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = -0.2),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "sales_buyer_stack", priority = "event"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "sales_buyer_stack", priority = "event")
    if (is.null(clicked) || nrow(clicked) == 0) return()
    buyer <- if (!is.null(clicked$customdata)) as.character(clicked$customdata[[1]]) else ""
    if (buyer == "") return()
    updateSelectInput(session, "sales_buyer", selected = buyer)
  }, ignoreInit = TRUE)
  
  output$sales_monthly_line <- renderPlotly({
    data_version()
    req(nrow(sales_po()) > 0)
    monthly <- sales_po() %>%
      group_by(Year, Month, Month_Abbr) %>%
      summarise(Total = sum(Total_PO, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(Year, Month) %>%
      mutate(Period = paste(Month_Abbr, Year))
    
    monthly$Period <- factor(monthly$Period, levels = monthly$Period)
    
    plot_ly(monthly, x = ~Period, y = ~Total, type = "scatter", mode = "lines+markers",
            line = list(color = "#5F7D67", width = 3),
            marker = list(size = 8, color = "#5F7D67"),
            hovertemplate = "%{x}<br>$%{y:,.0f}<br>%{text} orders",
            text = ~Count) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "PO Value ($)", tickformat = "$,.0f"),
             margin = list(b = 80),
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$sales_buyer_summary_table <- renderDT({
    data_version()
    req(nrow(sales_po()) > 0)
    summary_tbl <- sales_po() %>%
      group_by(Buyer) %>%
      summarise(
        `Total Orders` = n_distinct(PO_Number),
        `Total Value` = sum(Total_PO, na.rm = TRUE),
        `Avg Order Value` = mean(Total_PO, na.rm = TRUE),
        `Max Order` = max(Total_PO, na.rm = TRUE),
        `Clients Served` = n_distinct(Client)
      ) %>%
      arrange(desc(`Total Value`)) %>%
      mutate(
        `Total Value` = paste0("$", format(round(`Total Value`, 0), big.mark = ",")),
        `Avg Order Value` = paste0("$", format(round(`Avg Order Value`, 0), big.mark = ",")),
        `Max Order` = paste0("$", format(round(`Max Order`, 0), big.mark = ","))
      )
    
    datatable(summary_tbl, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  })
  
  output$sales_po_detail_table <- renderDT({
    data_version()
    req(nrow(sales_po()) > 0)
    detail <- sales_po() %>%
      select(Date, PO_Number, Client, Buyer, Total_PO, Month_Name, Year) %>%
      arrange(desc(Date)) %>%
      mutate(Total_PO = paste0("$", format(round(Total_PO, 2), big.mark = ",")))
    
    datatable(detail, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE,
              filter = "top")
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "po_heatmap"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "po_heatmap")
    if (is.null(clicked) || is.null(clicked$x) || is.null(clicked$y)) return()
    
    clicked_month_label <- as.character(clicked$x[[1]])
    selected_month <- dplyr::coalesce(
      match(clicked_month_label, month.name),
      match(clicked_month_label, month.abb)
    )
    selected_buyer <- as.character(clicked$y[[1]])
    
    if (!is.na(selected_month)) {
      updateSelectInput(session, "global_month", selected = as.character(selected_month))
    }
    updateSelectInput(session, "sales_buyer", selected = selected_buyer)
    
    showNotification(
      paste0(
        "Purchase Orders focused on ", selected_buyer,
        ifelse(is.na(selected_month), "", paste0(" in ", month.name[selected_month])),
        "."
      ),
      type = "message",
      duration = 4
    )
  })
  
  ##############################################################################
  # 4.4 SALES, REVENUE & PROFITABILITY - Server Logic
  ##############################################################################
  
  perf_country_focus <- reactiveVal(NULL)
  
  perf_data_base <- reactive({
    data_version()
    df <- filtered_revenue()
    if (!is.null(input$perf_staff) && input$perf_staff != "all") {
      df <- df %>% filter(Staff == input$perf_staff)
    }
    if (!is.null(input$perf_country) && input$perf_country != "all") {
      df <- df %>% filter(Country == input$perf_country)
    }
    if (!is.null(input$perf_parent) && input$perf_parent != "all") {
      df <- df %>% filter(Parent_Company == input$perf_parent)
    }
    df
  })
  
  perf_data <- reactive({
    data_version()
    df <- perf_data_base()
    focused_country <- perf_country_focus()
    if (!is.null(focused_country) && focused_country != "") {
      df <- df %>% filter(Country == focused_country)
    }
    df
  })
  
  observeEvent(input$perf_reset, {
    updateSelectInput(session, "perf_staff", selected = "all")
    updateSelectInput(session, "perf_country", selected = "all")
    updateSelectInput(session, "perf_parent", selected = "all")
    updateNumericInput(session, "perf_margin_target", value = 30)
    perf_country_focus(NULL)
  })
  
  observeEvent(input$perf_clear_country, {
    perf_country_focus(NULL)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "perf_country", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "perf_country", priority = "event")
      selected <- plotly_clicked_value(clicked)
      if (is.null(selected) || selected == "") return()
      perf_country_focus(if (identical(perf_country_focus(), selected)) NULL else selected)
    },
    ignoreInit = TRUE
  )
  
  output$perf_country_focus_label <- renderUI({
    data_version()
    focused <- perf_country_focus()
    if (is.null(focused)) {
      tags$span(class = "status-chip", "All countries")
    } else {
      tags$span(class = "status-chip", paste("Focused:", focused))
    }
  })
  
  output$perf_kpi_rev <- renderText({
    data_version()
    paste0("$", format(round(sum(perf_data()$Revenue, na.rm = TRUE), 0), big.mark = ","))
  })
  
  output$perf_kpi_profit <- renderText({
    data_version()
    paste0("$", format(round(sum(perf_data()$Gross_Profit, na.rm = TRUE), 0), big.mark = ","))
  })
  
  output$perf_kpi_margin <- renderText({
    data_version()
    revenue <- sum(perf_data()$Revenue, na.rm = TRUE)
    profit <- sum(perf_data()$Gross_Profit, na.rm = TRUE)
    if (revenue > 0) paste0(round(profit / revenue * 100, 1), "%") else "N/A"
  })
  
  output$perf_kpi_deals <- renderText({
    data_version()
    format(nrow(perf_data()), big.mark = ",")
  })
  
  output$perf_staff_bar <- renderPlotly({
    data_version()
    df <- perf_data() %>%
      filter(!is.na(Staff), Staff != "") %>%
      group_by(Staff) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Weighted_Margin = safe_divide(Gross_Profit, Revenue) * 100,
        Transactions = n(),
        .groups = "drop"
      ) %>%
      arrange(Revenue)
    
    validate(need(nrow(df) > 0, "No representative data matches the selected filters."))
    df$Staff <- factor(df$Staff, levels = df$Staff)
    
    chart <- plot_ly(df, source = "perf_staff") %>%
      add_bars(
        y = ~Staff, x = ~Revenue,
        key = ~as.character(Staff), customdata = ~as.character(Staff),
        name = "Revenue", orientation = "h",
        marker = list(color = brand_blue),
        text = ~paste0("Transactions: ", Transactions, "<br>Weighted margin: ", round(Weighted_Margin, 1), "%"),
        hovertemplate = "<b>%{y}</b><br>Revenue: $%{x:,.2f}<br>%{text}<br>Click to focus representative<extra></extra>"
      ) %>%
      add_bars(
        y = ~Staff, x = ~Gross_Profit,
        key = ~as.character(Staff), customdata = ~as.character(Staff),
        name = "Gross Profit", orientation = "h",
        marker = list(color = brand_teal),
        hovertemplate = "<b>%{y}</b><br>Gross profit: $%{x:,.2f}<br>Click to focus representative<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Amount ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 150, r = 25, t = 35, b = 55),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "perf_staff", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "perf_staff", priority = "event"))
    if (is.null(selected)) return()
    updateSelectInput(session, "perf_staff", selected = selected)
  }, ignoreInit = TRUE)
  
  output$perf_country_donut_ui <- renderUI({
    data_version()
    country_count <- perf_data_base() %>%
      filter(!is.na(Country), Country != "", Revenue > 0) %>%
      summarise(Count = n_distinct(Country)) %>%
      pull(Count)
    
    country_count <- ifelse(length(country_count) == 0 || is.na(country_count), 0, country_count)
    plot_height <- max(520, min(780, 450 + country_count * 9))
    
    plotlyOutput("perf_country_donut", height = paste0(plot_height, "px")) %>%
      withSpinner(color = brand_teal)
  })
  
  output$perf_country_detail_ui <- renderUI({
    data_version()
    country_count <- perf_data_base() %>%
      filter(!is.na(Country), Country != "", Revenue > 0) %>%
      summarise(Count = n_distinct(Country)) %>%
      pull(Count)
    
    country_count <- ifelse(length(country_count) == 0 || is.na(country_count), 0, country_count)
    panel_height <- max(420, min(680, 350 + country_count * 9))
    
    div(
      class = "country-detail-shell",
      style = paste0("min-height:", panel_height, "px;"),
      DTOutput("perf_country_detail") %>% withSpinner(color = brand_slate)
    )
  })
  
  output$perf_country_overflow_note <- renderUI({
    data_version()
    df <- perf_data_base() %>%
      filter(!is.na(Country), Country != "") %>%
      group_by(Country) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
      filter(Revenue > 0) %>%
      arrange(desc(Revenue)) %>%
      mutate(Share = Revenue / sum(Revenue))
    
    small_df <- df %>% filter(Share < 0.02)
    if (nrow(small_df) == 0) return(NULL)
    
    tags$div(
      class = "country-overflow-note",
      tags$strong("Additional countries included:"),
      tags$span(
        paste0(
          small_df$Country,
          " (",
          percent(small_df$Share, accuracy = 0.01),
          ")",
          collapse = " • "
        )
      )
    )
  })
  
  output$perf_country_business_summary <- renderUI({
    data_version()
    df <- perf_data()
    revenue <- sum(df$Revenue, na.rm = TRUE)
    profit <- sum(df$Gross_Profit, na.rm = TRUE)
    margin <- safe_divide(profit, revenue) * 100
    focus <- perf_country_focus()
    
    div(
      tags$div(
        class = "status-chip",
        if (is.null(focus)) "All visible countries" else paste("Selected country:", focus)
      ),
      div(
        class = "country-business-summary",
        div(class = "country-business-metric", tags$span("Revenue"), tags$strong(dollar(revenue, accuracy = 1))),
        div(class = "country-business-metric", tags$span("Gross Profit"), tags$strong(dollar(profit, accuracy = 1))),
        div(class = "country-business-metric", tags$span("Weighted Margin"), tags$strong(paste0(round(margin, 1), "%"))),
        div(class = "country-business-metric", tags$span("Customers"), tags$strong(format(n_distinct(df$Customer), big.mark = ",")))
      )
    )
  })
  
  output$perf_country_donut <- renderPlotly({
    data_version()
    df <- perf_data_base() %>%
      filter(!is.na(Country), Country != "") %>%
      group_by(Country) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Customers = n_distinct(Customer),
        .groups = "drop"
      ) %>%
      filter(Revenue > 0) %>%
      arrange(desc(Revenue)) %>%
      mutate(Share = Revenue / sum(Revenue))
    
    validate(need(nrow(df) > 0, "No country data matches the selected filters."))
    
    selected <- perf_country_focus()
    selected <- if (
      is.null(selected) ||
      length(selected) == 0 ||
      is.na(selected[[1]]) ||
      !nzchar(str_squish(as.character(selected[[1]])))
    ) {
      NULL
    } else {
      str_squish(as.character(selected[[1]]))
    }
    
    selected_match <- if (is.null(selected)) {
      rep(FALSE, nrow(df))
    } else {
      df$Country == selected
    }
    
    pull_values <- ifelse(selected_match, 0.09, 0)
    
    df <- df %>%
      mutate(
        Slice_Label = if_else(
          Share >= 0.02 | selected_match,
          paste0(Country, "<br>", percent(Share, accuracy = .1)),
          ""
        ),
        Hover_Text = paste0(
          "Gross profit: $", format(round(Gross_Profit, 2), big.mark = ","),
          "<br>Customers: ", Customers,
          "<br>Revenue share: ", percent(Share, accuracy = .1)
        )
      )
    
    chart <- plot_ly(
      df,
      source = "perf_country",
      labels = ~Country,
      values = ~Revenue,
      key = ~Country,
      customdata = ~Country,
      type = "pie",
      hole = 0.57,
      sort = FALSE,
      pull = pull_values,
      text = ~Slice_Label,
      textinfo = "text",
      textposition = "auto",
      insidetextorientation = "horizontal",
      hovertext = ~Hover_Text,
      marker = list(
        colors = rep(pal_performance, length.out = nrow(df)),
        line = list(color = "#0B1119", width = 1.2)
      ),
      hovertemplate = "<b>%{label}</b><br>Revenue: $%{value:,.2f}<br>%{hovertext}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        uniformtext = list(minsize = 10, mode = "hide"),
        legend = list(orientation = "v", x = 1.01, y = .98, yanchor = "top"),
        margin = list(t = 20, b = 25, l = 25, r = 155),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        displayModeBar = TRUE,
        toImageButtonOptions = list(format = "png", filename = "Aurelis_Revenue_by_Country", scale = 2)
      )
    
    register_plotly_click(chart)
  })
  
  output$perf_country_detail <- renderDT({
    data_version()
    df <- perf_data() %>%
      group_by(Country) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        `Weighted Margin %` = round(safe_divide(Gross_Profit, Revenue) * 100, 1),
        Customers = n_distinct(Customer),
        Representatives = n_distinct(Staff),
        Transactions = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Revenue))
    
    datatable(
      df,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatCurrency(c("Revenue", "Gross_Profit"), currency = "$", digits = 2) %>%
      formatRound("Weighted Margin %", digits = 1)
  })
  
  output$perf_rev_cost_trend <- renderPlotly({
    data_version()
    df <- perf_data() %>%
      group_by(Year, Month, Month_Abbr) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Cost = sum(Cost, na.rm = TRUE),
        Gross_Profit = sum(Gross_Profit, na.rm = TRUE),
        Transactions = n(),
        .groups = "drop"
      ) %>%
      arrange(Year, Month) %>%
      mutate(Period = paste(Month_Abbr, Year))
    
    validate(need(nrow(df) > 0, "No monthly sales data matches the selected filters."))
    df$Period <- factor(df$Period, levels = unique(df$Period))
    
    plot_ly(df) %>%
      add_bars(
        x = ~Period, y = ~Revenue,
        name = "Revenue", marker = list(color = brand_blue),
        hovertemplate = "%{x}<br>Revenue: $%{y:,.2f}<extra></extra>"
      ) %>%
      add_bars(
        x = ~Period, y = ~Cost,
        name = "Cost", marker = list(color = brand_slate),
        hovertemplate = "%{x}<br>Cost: $%{y:,.2f}<extra></extra>"
      ) %>%
      add_lines(
        x = ~Period, y = ~Gross_Profit,
        name = "Gross Profit",
        line = list(color = brand_teal, width = 3),
        mode = "lines+markers",
        text = ~paste0("Transactions: ", Transactions),
        hovertemplate = "%{x}<br>Gross profit: $%{y:,.2f}<br>%{text}<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "Amount ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.13),
        margin = list(b = 85, t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$perf_margin_gauge <- renderPlotly({
    data_version()
    revenue <- sum(perf_data()$Revenue, na.rm = TRUE)
    profit <- sum(perf_data()$Gross_Profit, na.rm = TRUE)
    margin_pct <- if (revenue > 0) round(profit / revenue * 100, 1) else 0
    target <- if (is.null(input$perf_margin_target)) 30 else input$perf_margin_target
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = margin_pct,
      number = list(suffix = "%", valueformat = ".1f"),
      delta = list(
        reference = target,
        valueformat = ".1f",
        suffix = " pts",
        increasing = list(color = brand_sage),
        decreasing = list(color = brand_coral)
      ),
      title = list(text = "Gross Profit ÷ Revenue"),
      gauge = list(
        axis = list(range = list(0, 100), ticksuffix = "%"),
        bar = list(color = brand_blue, thickness = 0.30),
        bgcolor = "#F1F5F9",
        borderwidth = 0,
        steps = list(
          list(range = c(0, max(target * 0.67, 1)), color = "#F7EDEF"),
          list(range = c(max(target * 0.67, 1), target), color = "#F8F2E7"),
          list(range = c(target, 100), color = "#EAF3EE")
        ),
        threshold = list(
          line = list(color = brand_navy, width = 4),
          thickness = 0.75,
          value = target
        )
      )
    ) %>%
      layout(
        margin = list(t = 75, b = 15, l = 30, r = 30),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$perf_margin_explanation <- renderUI({
    data_version()
    revenue <- sum(perf_data()$Revenue, na.rm = TRUE)
    profit <- sum(perf_data()$Gross_Profit, na.rm = TRUE)
    margin_pct <- if (revenue > 0) profit / revenue * 100 else 0
    target <- if (is.null(input$perf_margin_target)) 30 else input$perf_margin_target
    difference <- margin_pct - target
    
    HTML(paste0(
      "<strong>", round(margin_pct, 1), "%</strong> means approximately <strong>$",
      format(round(margin_pct, 2), nsmall = 2),
      "</strong> of gross profit per $100 of revenue. ",
      "The result is <strong>", abs(round(difference, 1)), " percentage point(s) ",
      ifelse(difference >= 0, "above", "below"),
      "</strong> the selected ", round(target, 1), "% target."
    ))
  })
  
  output$perf_detail_table <- renderDT({
    data_version()
    df <- perf_data() %>%
      select(Date, Customer, Entity, Staff, Country, Revenue, Cost, Gross_Profit, Margin_Pct) %>%
      arrange(desc(Date)) %>%
      mutate(
        Margin_Pct = ifelse(
          is.na(Margin_Pct), NA_character_, paste0(round(Margin_Pct, 1), "%")
        )
      )
    
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(c("Revenue", "Cost", "Gross_Profit"), currency = "$", digits = 2)
  })
  
  output$perf_download <- downloadHandler(
    filename = function() {
      paste0("Aurelis_Sales_Profitability_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        perf_data() %>%
          select(Date, Customer, Entity, Staff, Country, Revenue, Cost, Gross_Profit, Margin_Pct),
        file, row.names = FALSE, na = ""
      )
    }
  )
  
  ##############################################################################
  # 4.5 CUSTOMER PERFORMANCE & RELATIONSHIP VALUE - Server Logic
  ##############################################################################
  
  cust_perf_status_focus <- reactiveVal(NULL)
  
  customer_perf_po <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    df <- if (input$cust_perf_scope == "global") filtered_po() else daily_po
    df %>% filter(Client == input$cust_perf_client)
  })
  
  customer_perf_revenue <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    df <- if (input$cust_perf_scope == "global") filtered_revenue() else revenue_data
    df %>% filter(Customer == input$cust_perf_client)
  })
  
  customer_perf_inventory <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    df <- if (input$cust_perf_scope == "global") filtered_inventory() else inventory_data
    df %>%
      filter(Client == input$cust_perf_client) %>%
      mutate(
        Planned_Execution_Days = as.numeric(
          difftime(Delivery_Date, Order_Date, units = "days")
        )
      )
  })
  
  customer_perf_inventory_view <- reactive({
    data_version()
    df <- customer_perf_inventory()
    focus <- cust_perf_status_focus()
    if (!is.null(focus) && focus != "") {
      df <- df %>% filter(as.character(Order_Status) == focus)
    }
    df
  })
  
  observeEvent(
    list(input$cust_perf_client, input$cust_perf_scope),
    cust_perf_status_focus(NULL),
    ignoreInit = TRUE
  )
  
  observeEvent(input$cust_perf_clear_status, cust_perf_status_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "cust_perf_status", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "cust_perf_status", priority = "event")
      )
      if (is.null(selected)) return()
      cust_perf_status_focus(if (identical(cust_perf_status_focus(), selected)) NULL else selected)
    },
    ignoreInit = TRUE
  )
  
  output$cust_perf_status_focus_label <- renderUI({
    data_version()
    focus <- cust_perf_status_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All fulfillment statuses" else paste("Focused:", focus)
    )
  })
  
  customer_perf_ar <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    df <- if (input$cust_perf_scope == "global") filtered_ar() else ar_data
    df %>% filter(Customer == input$cust_perf_client)
  })
  
  customer_perf_history <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    df <- relationship_order_history
    
    if (input$cust_perf_scope == "global") {
      if (!is.null(input$global_year) && input$global_year != "all") {
        df <- df %>% filter(Year == as.numeric(input$global_year))
      }
      if (!is.null(input$global_month) && input$global_month != "all") {
        df <- df %>% filter(Month == as.numeric(input$global_month))
      }
    }
    
    df %>%
      filter(Client == input$cust_perf_client) %>%
      mutate(
        Planned_Execution_Days = as.numeric(
          difftime(Delivery_Date, Order_Date, units = "days")
        )
      )
  })
  
  customer_perf_summary <- reactive({
    data_version()
    req(input$cust_perf_client != "")
    
    po <- customer_perf_po()
    rev <- customer_perf_revenue()
    inv <- customer_perf_inventory() %>% filter(Line_Type != "Information")
    ar <- customer_perf_ar()
    
    ordered_quantity <- sum(inv$Quantity, na.rm = TRUE)
    received_quantity <- sum(inv$Received_Qty, na.rm = TRUE)
    backordered_quantity <- sum(inv$Backordered_Qty, na.rm = TRUE)
    invoiced_ar <- sum(ar$Invoice_Amount, na.rm = TRUE)
    outstanding_ar <- sum(pmax(ar$Balance_Remaining, 0), na.rm = TRUE)
    
    lead_days <- inv$Planned_Execution_Days
    lead_days <- lead_days[is.finite(lead_days) & lead_days >= 0]
    
    relationship_dates <- c(po$Date, rev$Date, inv$Order_Date)
    relationship_dates <- relationship_dates[!is.na(relationship_dates)]
    
    tibble(
      Customer = input$cust_perf_client,
      PO_Count = n_distinct(po$PO_Number[!is.na(po$PO_Number)]),
      PO_Value = sum(po$Total_PO, na.rm = TRUE),
      Average_PO = if (nrow(po) > 0) mean(po$Total_PO, na.rm = TRUE) else 0,
      Revenue = sum(rev$Revenue, na.rm = TRUE),
      Gross_Profit = sum(rev$Gross_Profit, na.rm = TRUE),
      Gross_Margin = ifelse(
        sum(rev$Revenue, na.rm = TRUE) > 0,
        sum(rev$Gross_Profit, na.rm = TRUE) / sum(rev$Revenue, na.rm = TRUE),
        NA_real_
      ),
      Invoice_Count = nrow(rev),
      Average_Execution_Days = if (length(lead_days) > 0) mean(lead_days) else NA_real_,
      Ordered_Quantity = ordered_quantity,
      Received_Quantity = received_quantity,
      Backordered_Quantity = backordered_quantity,
      Completion_Rate = ifelse(
        ordered_quantity > 0,
        pmin(received_quantity / ordered_quantity, 1),
        NA_real_
      ),
      Backorder_Rate = ifelse(
        ordered_quantity > 0,
        pmax(backordered_quantity / ordered_quantity, 0),
        NA_real_
      ),
      Buyer_Count = n_distinct(c(po$Buyer, inv$Buyer), na.rm = TRUE),
      Supplier_Count = n_distinct(inv$Supplier[!is.na(inv$Supplier) & inv$Supplier != ""]),
      Outstanding_AR = outstanding_ar,
      Paid_Rate = ifelse(
        invoiced_ar > 0,
        pmax(0, pmin(1, 1 - outstanding_ar / invoiced_ar)),
        NA_real_
      ),
      Relationship_Start = if (
        length(relationship_dates) > 0
      ) min(relationship_dates) else as.Date(NA),
      Relationship_End = if (
        length(relationship_dates) > 0
      ) max(relationship_dates) else as.Date(NA)
    )
  })
  
  
  customer_perf_service_metrics <- reactive({
    summary <- customer_perf_summary()
    inv <- customer_perf_inventory() %>% filter(Line_Type != "Information")
    po <- customer_perf_po()
    
    order_level <- inv %>%
      group_by(PO_Number) %>%
      summarise(
        Ordered = sum(Quantity, na.rm = TRUE),
        Received = sum(Received_Qty, na.rm = TRUE),
        Backordered = sum(pmax(Backordered_Qty, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Complete = Ordered > 0 & Received >= Ordered,
        Backorder_Free = Backordered <= 0
      )
    
    relationship_start <- summary$Relationship_Start[[1]]
    relationship_end <- summary$Relationship_End[[1]]
    relationship_months <- if (!is.na(relationship_start) && !is.na(relationship_end)) {
      max(1, as.numeric(interval(relationship_start, relationship_end) / months(1)))
    } else NA_real_
    
    activity_dates <- unique(na.omit(c(po$Date, customer_perf_revenue()$Date, inv$Order_Date)))
    active_years <- if (length(activity_dates) > 0) n_distinct(year(activity_dates)) else 0
    active_months <- if (length(activity_dates) > 0) n_distinct(format(activity_dates, "%Y-%m")) else 0
    
    customer_po_full <- daily_po %>%
      filter(Client == input$cust_perf_client, !is.na(Date))
    current_12_start <- Sys.Date() - 365
    prior_12_start <- Sys.Date() - 730
    current_12_value <- sum(customer_po_full$Total_PO[customer_po_full$Date > current_12_start & customer_po_full$Date <= Sys.Date()], na.rm = TRUE)
    prior_12_value <- sum(customer_po_full$Total_PO[customer_po_full$Date > prior_12_start & customer_po_full$Date <= current_12_start], na.rm = TRUE)
    ltm_growth <- if (prior_12_value > 0) current_12_value / prior_12_value - 1 else NA_real_
    
    tibble(
      Quantity_Fulfillment = summary$Completion_Rate[[1]],
      Backorder_Free_PO_Rate = if (nrow(order_level) > 0) mean(order_level$Backorder_Free, na.rm = TRUE) else NA_real_,
      Complete_PO_Rate = if (nrow(order_level) > 0) mean(order_level$Complete, na.rm = TRUE) else NA_real_,
      Planned_Cycle_Days = summary$Average_Execution_Days[[1]],
      Relationship_Months = relationship_months,
      Active_Years = active_years,
      Active_Months = active_months,
      Average_PO_Value = summary$Average_PO[[1]],
      Last_12_Month_PO_Value = current_12_value,
      Prior_12_Month_PO_Value = prior_12_value,
      Last_12_Month_Growth = ltm_growth
    )
  })
  
  output$cust_perf_service_kpis <- renderUI({
    m <- customer_perf_service_metrics()
    summary <- customer_perf_summary()
    value_or_na <- function(x, formatter) if (is.na(x) || !is.finite(x)) "Not available" else formatter(x)
    div(
      class = "kpi-health-grid",
      div(class = "kpi-health-card", tags$strong("Quantity fulfillment"), tags$div(class = "customer-milestone-value", value_or_na(m$Quantity_Fulfillment[[1]], function(x) percent(x, accuracy = .1))), tags$small("Received quantity ÷ ordered quantity. This is an order-completeness measure supported by current data.")),
      div(class = "kpi-health-card", tags$strong("Backorder-free PO rate"), tags$div(class = "customer-milestone-value", value_or_na(m$Backorder_Free_PO_Rate[[1]], function(x) percent(x, accuracy = .1))), tags$small("Share of recorded POs with no currently backordered quantity.")),
      div(class = "kpi-health-card", tags$strong("Planned order cycle time"), tags$div(class = "customer-milestone-value", value_or_na(m$Planned_Cycle_Days[[1]], function(x) paste0(round(x, 1), " days"))), tags$small("Average planned delivery date minus order date. It is not labeled actual cycle time until actual delivery timestamps are available.")),
      div(class = "kpi-health-card", tags$strong("Relationship continuity"), tags$div(class = "customer-milestone-value", paste0(m$Active_Years[[1]], " active year(s)")), tags$small(paste0(m$Active_Months[[1]], " active month(s) recorded across the relationship."))),
      div(class = "kpi-health-card", tags$strong("Average purchase order"), tags$div(class = "customer-milestone-value", dollar(m$Average_PO_Value[[1]], accuracy = 1)), tags$small("Average recorded PO value for this customer.")),
      div(class = "kpi-health-card", tags$strong("Last 12-month PO value"), tags$div(class = "customer-milestone-value", dollar(m$Last_12_Month_PO_Value[[1]], accuracy = 1)), tags$small(if (is.na(m$Last_12_Month_Growth[[1]])) "Prior 12-month comparison is not available." else paste0("Change versus the prior 12 months: ", percent(m$Last_12_Month_Growth[[1]], accuracy = .1), ".")))
    )
  })
  
  output$cust_perf_annual_story <- renderPlotly({
    df <- customer_perf_po() %>%
      filter(!is.na(Date)) %>%
      mutate(Year = year(Date)) %>%
      group_by(Year) %>%
      summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), Customers = n_distinct(Client), .groups = "drop") %>%
      arrange(Year)
    validate(need(nrow(df) > 0, "No annual purchase-order history is available for this customer."))
    plot_ly(df) %>%
      add_bars(x = ~Year, y = ~PO_Value, name = "PO Value", marker = list(color = brand_blue), text = ~dollar(PO_Value, accuracy = 1), textposition = "auto", hovertemplate = "Year %{x}<br>PO value: $%{y:,.2f}<extra></extra>") %>%
      add_lines(x = ~Year, y = ~PO_Count, name = "PO Count", yaxis = "y2", line = list(color = brand_amber, width = 3), mode = "lines+markers", hovertemplate = "Year %{x}<br>PO count: %{y}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Purchase-order value", tickformat = "$,.0f"),
        yaxis2 = list(title = "Purchase orders", overlaying = "y", side = "right", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(t = 55, b = 55, l = 80, r = 80),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$cust_perf_resume_preview <- renderUI({
    if (
      is.null(input$cust_perf_client) ||
      length(input$cust_perf_client) == 0 ||
      !nzchar(str_squish(as.character(input$cust_perf_client[[1]])))
    ) {
      return(
        div(
          class = "customer-resume-preview customer-resume-empty",
          tags$h3("Customer Relationship Resume"),
          tags$p(
            "Select a customer above to populate relationship dates, purchase-order history, fulfillment, support-network and recent-momentum metrics."
          )
        )
      )
    }
    
    summary <- customer_perf_summary()
    m <- customer_perf_service_metrics()
    start_text <- if (is.na(summary$Relationship_Start[[1]])) "First available record" else format(summary$Relationship_Start[[1]], "%B %Y")
    fulfillment_text <- if (is.na(m$Quantity_Fulfillment[[1]])) "Fulfillment data not available" else paste0(percent(m$Quantity_Fulfillment[[1]], accuracy = .1), " quantity fulfillment")
    backorder_text <- if (is.na(m$Backorder_Free_PO_Rate[[1]])) "Backorder-free rate not available" else paste0(percent(m$Backorder_Free_PO_Rate[[1]], accuracy = .1), " backorder-free POs")
    growth_text <- if (is.na(m$Last_12_Month_Growth[[1]])) "Latest 12-month comparison unavailable" else paste0(percent(m$Last_12_Month_Growth[[1]], accuracy = .1), " change in PO value versus the prior 12 months")
    
    div(
      class = "customer-resume-preview",
      tags$h3(paste0(input$cust_perf_resume_title, " — ", input$cust_perf_client)),
      tags$p(coalesce(input$cust_perf_resume_message, "")),
      div(
        class = "customer-resume-grid",
        div(class = "customer-resume-card", tags$strong("Relationship since"), start_text),
        div(class = "customer-resume-card", tags$strong("Purchase orders"), format(summary$PO_Count[[1]], big.mark = ",")),
        div(class = "customer-resume-card", tags$strong("Recorded PO value"), dollar(summary$PO_Value[[1]], accuracy = 1)),
        div(class = "customer-resume-card", tags$strong("Service continuity"), paste0(m$Active_Years[[1]], " active year(s)")),
        div(class = "customer-resume-card", tags$strong("Fulfillment"), fulfillment_text),
        div(class = "customer-resume-card", tags$strong("Backorder-free performance"), backorder_text),
        div(class = "customer-resume-card", tags$strong("Aurelis support network"), paste0(summary$Buyer_Count[[1]], " buyer(s) · ", summary$Supplier_Count[[1]], " supplier(s)")),
        div(class = "customer-resume-card", tags$strong("Recent relationship momentum"), growth_text)
      ),
      tags$p(class = "metric-definition-note", "The client-facing resume uses factual collaboration and service metrics. Internal gross margin, supplier cost, financing charges and collections-risk details are excluded from the client PDF by default.")
    )
  })
  
  customer_resume_chart_uri <- function(plot_object, width = 1100, height = 520) {
    if (!requireNamespace("base64enc", quietly = TRUE)) return("")
    file <- tempfile(fileext = ".png")
    ggplot2::ggsave(file, plot = plot_object, width = width / 120, height = height / 120, dpi = 120, bg = "white")
    on.exit(unlink(file), add = TRUE)
    base64enc::dataURI(file = file, mime = "image/png")
  }
  
  customer_resume_pdf_html <- function() {
    summary <- customer_perf_summary()
    m <- customer_perf_service_metrics()
    po <- customer_perf_po()
    client <- input$cust_perf_client
    logo_uri <- report_logo_data_uri()
    esc <- htmltools::htmlEscape
    
    annual <- po %>% filter(!is.na(Date)) %>% mutate(Year = year(Date)) %>% group_by(Year) %>% summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), .groups = "drop")
    monthly <- po %>% filter(!is.na(Date)) %>% mutate(Period = floor_date(Date, "month")) %>% group_by(Period) %>% summarise(PO_Value = sum(Total_PO, na.rm = TRUE), PO_Count = n_distinct(PO_Number), .groups = "drop")
    
    annual_uri <- ""
    monthly_uri <- ""
    if (nrow(annual) > 0 && isTRUE(input$cust_perf_pdf_show_history)) {
      p1 <- ggplot(annual, aes(x = factor(Year), y = PO_Value)) + geom_col(fill = brand_blue, width = .65) + geom_text(aes(label = dollar(PO_Value, accuracy = 1)), vjust = -0.3, size = 3.2, color = brand_ink) + scale_y_continuous(labels = dollar_format(accuracy = 1), expand = expansion(mult = c(0, .15))) + labs(x = NULL, y = "Purchase-order value", title = "Annual Collaboration Value") + theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title = element_text(color = brand_navy, face = "bold"))
      annual_uri <- customer_resume_chart_uri(p1)
    }
    if (nrow(monthly) > 1 && identical(input$cust_perf_resume_layout, "detailed")) {
      p2 <- ggplot(monthly, aes(x = Period, y = PO_Value)) + geom_line(color = brand_teal, linewidth = 1.1) + geom_point(color = brand_teal, size = 2) + scale_y_continuous(labels = dollar_format(accuracy = 1)) + labs(x = NULL, y = "Purchase-order value", title = "Monthly Relationship Activity") + theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title = element_text(color = brand_navy, face = "bold"))
      monthly_uri <- customer_resume_chart_uri(p2)
    }
    
    start_text <- if (is.na(summary$Relationship_Start[[1]])) "First available record" else format(summary$Relationship_Start[[1]], "%B %Y")
    completion_text <- if (is.na(m$Quantity_Fulfillment[[1]])) "N/A" else percent(m$Quantity_Fulfillment[[1]], accuracy = .1)
    backorder_free_text <- if (is.na(m$Backorder_Free_PO_Rate[[1]])) "N/A" else percent(m$Backorder_Free_PO_Rate[[1]], accuracy = .1)
    cycle_text <- if (is.na(m$Planned_Cycle_Days[[1]])) "N/A" else paste0(round(m$Planned_Cycle_Days[[1]], 1), " days")
    
    value_section <- if (isTRUE(input$cust_perf_pdf_show_value)) paste0(
      "<section><h2>Relationship value</h2><div class='metrics'>",
      "<div><span>Recorded PO value</span><strong>", esc(dollar(summary$PO_Value[[1]], accuracy = 1)), "</strong></div>",
      "<div><span>Purchase orders</span><strong>", esc(format(summary$PO_Count[[1]], big.mark = ",")), "</strong></div>",
      "<div><span>Average PO value</span><strong>", esc(dollar(summary$Average_PO[[1]], accuracy = 1)), "</strong></div>",
      "<div><span>Relationship since</span><strong>", esc(start_text), "</strong></div></div></section>"
    ) else ""
    
    fulfillment_section <- if (isTRUE(input$cust_perf_pdf_show_fulfillment)) paste0(
      "<section><h2>Service performance</h2><div class='metrics'>",
      "<div><span>Quantity fulfillment</span><strong>", esc(completion_text), "</strong></div>",
      "<div><span>Backorder-free POs</span><strong>", esc(backorder_free_text), "</strong></div>",
      "<div><span>Planned order cycle</span><strong>", esc(cycle_text), "</strong></div>",
      "<div><span>Active years</span><strong>", esc(paste0(m$Active_Years[[1]], " year(s)")), "</strong></div></div>",
      "<p class='footnote'>Quantity fulfillment and backorder-free rate are calculated from current order records. Planned order cycle is based on order date to planned delivery date and is not represented as actual OTIF.</p></section>"
    ) else ""
    
    network_section <- if (isTRUE(input$cust_perf_pdf_show_network)) paste0(
      "<section><h2>Aurelis relationship coverage</h2><div class='network'>",
      "<strong>", esc(summary$Buyer_Count[[1]]), " Aurelis buyer(s)</strong><span>coordinating the relationship</span>",
      "<strong>", esc(summary$Supplier_Count[[1]]), " supplier(s)</strong><span>represented across recorded order history</span>",
      "</div></section>"
    ) else ""
    
    chart_section <- paste0(
      if (annual_uri != "") paste0("<section><img class='chart' src='", annual_uri, "'></section>") else "",
      if (monthly_uri != "") paste0("<section><img class='chart' src='", monthly_uri, "'></section>") else ""
    )
    
    paste0(
      "<!doctype html><html><head><meta charset='utf-8'><style>",
      "@page{size:A4 portrait;margin:14mm;}body{font-family:Arial,sans-serif;color:#1F2937;margin:0;}header{display:flex;justify-content:space-between;align-items:flex-start;border-bottom:4px solid #17324D;padding-bottom:14px;margin-bottom:22px;}header img{max-width:210px;max-height:70px;}h1{color:#17324D;margin:0;font-size:28px;}h2{color:#17324D;font-size:17px;margin:20px 0 10px;border-bottom:1px solid #D7E1EA;padding-bottom:6px;}.sub{color:#64748B;margin-top:5px}.message{background:#EEF4F8;border-left:5px solid #2F6EA5;padding:14px 16px;border-radius:8px;margin:18px 0;}.metrics{display:grid;grid-template-columns:1fr 1fr;gap:10px}.metrics div{border:1px solid #D7E1EA;border-radius:8px;padding:12px;background:#F8FBFD}.metrics span{display:block;color:#64748B;font-size:11px;text-transform:uppercase;letter-spacing:.4px}.metrics strong{display:block;color:#17324D;font-size:18px;margin-top:4px}.network{display:grid;grid-template-columns:1fr 2fr;gap:7px 14px;border:1px solid #D7E1EA;border-radius:8px;padding:14px}.chart{width:100%;max-height:335px;object-fit:contain}.footnote{font-size:10px;color:#64748B;line-height:1.4}.footer{margin-top:24px;border-top:1px solid #D7E1EA;padding-top:10px;color:#64748B;font-size:10px;text-align:center;}section{break-inside:avoid;}",
      "</style></head><body><header>",
      if (logo_uri != "") paste0("<img src='", logo_uri, "' alt='Aurelis logo'>") else "<strong>AURELIS</strong>",
      "<div style='text-align:right'><h1>", esc(coalesce(input$cust_perf_resume_title, "Partnership Performance Review")), "</h1><div class='sub'>", esc(client), "</div><div class='sub'>", esc(format(Sys.Date(), "%B %d, %Y")), "</div></div></header>",
      "<div class='message'>", esc(coalesce(input$cust_perf_resume_message, "")), "</div>",
      value_section, fulfillment_section, network_section, chart_section,
      "<div class='footer'>Aurelis Global Supply, Inc. · Customer relationship summary based on recorded Aurelis operational data.</div>",
      "</body></html>"
    )
  }
  
  output$cust_perf_resume_pdf <- downloadHandler(
    filename = function() paste0("Aurelis_", gsub("[^A-Za-z0-9]+", "_", input$cust_perf_client), "_Relationship_Resume_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(input$cust_perf_client != "")
      if (!requireNamespace("pagedown", quietly = TRUE)) stop("Install pagedown first: install.packages('pagedown')", call. = FALSE)
      html_file <- tempfile(fileext = ".html")
      writeLines(customer_resume_pdf_html(), html_file, useBytes = TRUE)
      pagedown::chrome_print(input = html_file, output = file, wait = 2)
    }
  )
  
  output$cust_perf_total_value <- renderText({
    data_version()
    summary <- customer_perf_summary()
    paste0(
      "Rev ", dollar(summary$Revenue, accuracy = 1),
      " | PO ", dollar(summary$PO_Value, accuracy = 1)
    )
  })
  
  output$cust_perf_po_count <- renderText({
    data_version()
    format(customer_perf_summary()$PO_Count, big.mark = ",")
  })
  
  output$cust_perf_execution_days <- renderText({
    data_version()
    days <- customer_perf_summary()$Average_Execution_Days
    if (is.na(days)) "Not available" else paste0(round(days, 1), " days")
  })
  
  output$cust_perf_completion_rate <- renderText({
    data_version()
    rate <- customer_perf_summary()$Completion_Rate
    if (is.na(rate)) "Not available" else percent(rate, accuracy = 0.1)
  })
  
  output$cust_perf_highlights <- renderUI({
    data_version()
    summary <- customer_perf_summary()
    service <- customer_perf_service_metrics()
    portfolio_row <- customer_portfolio %>%
      filter(Customer == input$cust_perf_client) %>%
      slice_head(n = 1)
    
    percentile_value <- if (nrow(portfolio_row) > 0) {
      round(portfolio_row$Relationship_Score[[1]] * 100)
    } else {
      NA_real_
    }
    top_group <- if (is.na(percentile_value)) {
      "relationship portfolio profile"
    } else {
      paste0("top ", max(1, 100 - percentile_value), "% relationship profile")
    }
    
    start_text <- if (is.na(summary$Relationship_Start[[1]])) {
      "the first available record"
    } else {
      format(summary$Relationship_Start[[1]], "%B %Y")
    }
    
    fulfillment_text <- if (is.na(service$Quantity_Fulfillment[[1]])) {
      "quantity-fulfillment data is not yet available"
    } else {
      paste0(percent(service$Quantity_Fulfillment[[1]], accuracy = 0.1), " recorded quantity fulfillment")
    }
    backorder_text <- if (is.na(service$Backorder_Free_PO_Rate[[1]])) {
      "backorder-free PO rate is not yet available"
    } else {
      paste0(percent(service$Backorder_Free_PO_Rate[[1]], accuracy = 0.1), " of recorded POs currently backorder-free")
    }
    cycle_text <- if (is.na(service$Planned_Cycle_Days[[1]])) {
      "planned order-cycle timing is not available"
    } else {
      paste0(round(service$Planned_Cycle_Days[[1]], 1), " average planned order-cycle days")
    }
    momentum_text <- if (is.na(service$Last_12_Month_Growth[[1]])) {
      paste0("last-12-month recorded PO value is ", dollar(service$Last_12_Month_PO_Value[[1]], accuracy = 1))
    } else {
      paste0(
        "last-12-month recorded PO value is ", dollar(service$Last_12_Month_PO_Value[[1]], accuracy = 1),
        " (", percent(service$Last_12_Month_Growth[[1]], accuracy = 0.1), " versus the prior 12 months)"
      )
    }
    
    div(
      class = "profile-panel",
      tags$h3(input$cust_perf_client),
      tags$span(class = "source-badge", top_group),
      tags$ul(
        class = "highlight-list",
        tags$li(
          paste0(
            "Relationship recorded since ", start_text, ", covering ",
            format(summary$PO_Count[[1]], big.mark = ","), " purchase orders and ",
            service$Active_Years[[1]], " active year(s)."
          )
        ),
        tags$li(
          paste0(
            "Recorded collaboration value: ", dollar(summary$PO_Value[[1]], accuracy = 1),
            " in purchase orders, with an average PO of ", dollar(service$Average_PO_Value[[1]], accuracy = 1), "."
          )
        ),
        tags$li(paste0("Service record: ", fulfillment_text, "; ", backorder_text, "; ", cycle_text, ".")),
        tags$li(paste0("Relationship momentum: ", momentum_text, ".")),
        tags$li(
          paste0(
            "Relationship coverage includes ", summary$Buyer_Count[[1]],
            " Aurelis buyer(s) and ", summary$Supplier_Count[[1]],
            " supplier(s), providing broad sourcing and service support."
          )
        )
      ),
      tags$p(
        class = "metric-definition-note",
        "Customer-ready highlights intentionally exclude internal gross margin, financing and collections data. Metrics are factual summaries of the records currently available."
      )
    )
  })
  
  output$cust_perf_rank_gauge <- renderPlotly({
    data_version()
    req(input$cust_perf_client != "")
    portfolio_row <- customer_portfolio %>%
      filter(Customer == input$cust_perf_client) %>%
      slice_head(n = 1)
    
    score <- if (nrow(portfolio_row) > 0) {
      candidate_score <- round(portfolio_row$Relationship_Score[[1]] * 100, 1)
      if (is.finite(candidate_score)) candidate_score else 0
    } else {
      0
    }
    
    rank_text <- if (nrow(portfolio_row) > 0) {
      paste0(
        "Revenue rank #", portfolio_row$Revenue_Rank[[1]],
        " • PO-value rank #", portfolio_row$PO_Value_Rank[[1]]
      )
    } else {
      "Customer not ranked in the full portfolio"
    }
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = score,
      number = list(suffix = "th percentile"),
      title = list(text = paste0("Relationship value<br><span style='font-size:0.8em'>", rank_text, "</span>")),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#17324D"),
        steps = list(
          list(range = c(0, 50), color = "#F3E5F5"),
          list(range = c(50, 75), color = "#CE93D8"),
          list(range = c(75, 100), color = "#64748B")
        )
      )
    ) %>%
      layout(
        margin = list(t = 80, b = 20, l = 30, r = 30),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$cust_perf_monthly_trend <- renderPlotly({
    data_version()
    req(input$cust_perf_client != "")
    
    if (input$cust_perf_value_basis == "revenue") {
      df <- customer_perf_revenue() %>%
        mutate(Period_Date = floor_date(Date, "month")) %>%
        group_by(Period_Date) %>%
        summarise(
          Value = sum(Revenue, na.rm = TRUE),
          Profit = sum(Gross_Profit, na.rm = TRUE),
          Records = n(),
          .groups = "drop"
        ) %>%
        arrange(Period_Date)
      value_label <- "Recognized Revenue"
    } else {
      df <- customer_perf_po() %>%
        mutate(Period_Date = floor_date(Date, "month")) %>%
        group_by(Period_Date) %>%
        summarise(
          Value = sum(Total_PO, na.rm = TRUE),
          Profit = NA_real_,
          Records = n_distinct(PO_Number),
          .groups = "drop"
        ) %>%
        arrange(Period_Date)
      value_label <- "Purchase Order Value"
    }
    
    validate(need(nrow(df) > 0, "No monthly relationship data is available."))
    
    plot_ly(
      df,
      x = ~Period_Date,
      y = ~Value,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#2F6EA5", width = 3),
      marker = list(size = 8),
      text = ~paste0(
        value_label, ": ", dollar(Value, accuracy = 0.01),
        "<br>Records: ", Records
      ),
      hovertemplate = "%{x|%B %Y}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = value_label, tickformat = "$,.0f"),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$cust_perf_status_donut <- renderPlotly({
    data_version()
    df <- customer_perf_inventory() %>%
      filter(Line_Type != "Information") %>%
      count(Order_Status, name = "Lines") %>%
      filter(Lines > 0)
    
    validate(need(nrow(df) > 0, "No delivery-status data is available."))
    
    status_colors <- c(
      "Overdue" = brand_coral,
      "Due Soon" = brand_amber,
      "Open" = brand_blue,
      "Fully Received" = brand_sage
    )
    
    focus <- cust_perf_status_focus()
    pulls <- if (is.null(focus)) rep(0, nrow(df)) else ifelse(as.character(df$Order_Status) == focus, .09, 0)
    
    chart <- plot_ly(
      df,
      source = "cust_perf_status",
      labels = ~Order_Status,
      values = ~Lines,
      key = ~as.character(Order_Status),
      customdata = ~as.character(Order_Status),
      type = "pie",
      hole = 0.58,
      sort = FALSE,
      pull = pulls,
      marker = list(colors = status_colors[as.character(df$Order_Status)]),
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value} lines<br>Click to focus related charts and history<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$cust_perf_supplier_mix <- renderPlotly({
    data_version()
    df <- customer_perf_inventory_view() %>%
      filter(Line_Type != "Information", !is.na(Supplier), Supplier != "") %>%
      group_by(Supplier) %>%
      summarise(
        Order_Value = sum(Amount, na.rm = TRUE),
        PO_Count = n_distinct(PO_Number),
        Backordered = sum(Backordered_Qty, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Order_Value)) %>%
      slice_head(n = 15) %>%
      arrange(Order_Value)
    
    validate(need(nrow(df) > 0, "No supplier history is available for this customer."))
    df$Supplier <- factor(df$Supplier, levels = df$Supplier)
    
    chart <- plot_ly(
      df, source = "cust_perf_supplier",
      y = ~Supplier, x = ~Order_Value,
      key = ~as.character(Supplier), customdata = ~as.character(Supplier),
      type = "bar", orientation = "h",
      marker = list(
        color = ~Backordered,
        colorscale = list(c(0, "#E0F2F1"), c(1, "#3C7474")),
        showscale = TRUE, colorbar = list(title = "Backorder")
      ),
      text = ~paste0("POs: ", PO_Count, "<br>Backordered: ", Backordered),
      hovertemplate = "<b>%{y}</b><br>$%{x:,.2f}<br>%{text}<br>Click for supplier intelligence<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Order Value ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 210, r = 55),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "cust_perf_supplier", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "cust_perf_supplier", priority = "event"))
    if (!is.null(selected)) open_supplier_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$cust_perf_buyer_mix <- renderPlotly({
    data_version()
    po_df <- customer_perf_po() %>%
      filter(!is.na(Buyer), Buyer != "") %>%
      group_by(Buyer) %>%
      summarise(
        PO_Value = sum(Total_PO, na.rm = TRUE),
        PO_Count = n_distinct(PO_Number),
        First_Order = min(Date, na.rm = TRUE),
        Last_Order = max(Date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(PO_Value)
    
    validate(need(nrow(po_df) > 0, "No buyer assignment history is available."))
    po_df$Buyer <- factor(po_df$Buyer, levels = po_df$Buyer)
    
    chart <- plot_ly(
      po_df, source = "cust_perf_buyer",
      y = ~Buyer, x = ~PO_Value,
      key = ~as.character(Buyer), customdata = ~as.character(Buyer),
      type = "bar", orientation = "h",
      marker = list(
        color = ~PO_Count,
        colorscale = list(c(0, "#FFF3E0"), c(1, "#9A6A27")),
        showscale = TRUE, colorbar = list(title = "POs")
      ),
      text = ~paste0(
        "POs: ", PO_Count,
        "<br>Active: ", format(First_Order, "%b %Y"),
        " to ", format(Last_Order, "%b %Y")
      ),
      hovertemplate = "<b>%{y}</b><br>$%{x:,.2f}<br>%{text}<br>Click for buyer activity<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Managed PO Value ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 150, r = 55),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "cust_perf_buyer", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "cust_perf_buyer", priority = "event"))
    if (!is.null(selected)) open_buyer_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$cust_perf_order_table <- renderDT({
    data_version()
    req(input$cust_perf_client != "")
    df <- customer_perf_history()
    focus <- cust_perf_status_focus()
    if (!is.null(focus) && focus != "") {
      df <- df %>% filter(as.character(Order_Status) == focus)
    }
    df <- df %>%
      arrange(desc(Order_Date), PO_Number) %>%
      select(
        Order_Status,
        History_Source,
        Order_Date,
        Delivery_Date,
        Planned_Execution_Days,
        PO_Number,
        Buyer,
        Supplier,
        Part_Number,
        `Order Description` = Order_Description,
        Quantity,
        Received_Qty,
        Backordered_Qty,
        Unit_Price,
        Line_Amount = Amount,
        Open_Balance
      )
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(
        c("Unit_Price", "Line_Amount", "Open_Balance"),
        currency = "$", digits = 2
      ) %>%
      formatStyle(
        "Order_Status",
        target = "row",
        backgroundColor = styleEqual(
          c("Overdue", "Due Soon", "Open", "Fully Received"),
          c("#FFEBEE", "#FFF3E0", "#E3F2FD", "#E8F5E9")
        )
      )
  })
  
  output$cust_perf_download <- downloadHandler(
    filename = function() {
      paste0(
        "Customer_Performance_",
        gsub("[^A-Za-z0-9]+", "_", input$cust_perf_client),
        "_", Sys.Date(), ".xlsx"
      )
    },
    content = function(file) {
      req(input$cust_perf_client != "")
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
      }
      
      wb <- openxlsx::createWorkbook(creator = "Aurelis Dashboard")
      openxlsx::addWorksheet(wb, "Relationship Summary")
      openxlsx::writeDataTable(
        wb, "Relationship Summary", customer_perf_summary(),
        tableStyle = "TableStyleMedium2"
      )
      openxlsx::setColWidths(wb, "Relationship Summary", cols = 1:20, widths = "auto")
      
      datasets <- list(
        "Complete PO History" = customer_perf_history(),
        "Purchase Orders" = customer_perf_po(),
        "Revenue" = customer_perf_revenue(),
        "Delivery and Suppliers" = customer_perf_inventory(),
        "Accounts Receivable" = customer_perf_ar()
      )
      
      for (sheet_name in names(datasets)) {
        sheet_data <- datasets[[sheet_name]]
        openxlsx::addWorksheet(wb, sheet_name)
        if (nrow(sheet_data) > 0) {
          openxlsx::writeDataTable(
            wb, sheet_name, sheet_data,
            tableStyle = "TableStyleMedium2", withFilter = TRUE
          )
          openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2)
          openxlsx::setColWidths(
            wb, sheet_name, cols = seq_len(ncol(sheet_data)), widths = "auto"
          )
        } else {
          openxlsx::writeData(wb, sheet_name, "No matching records.")
        }
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  ##############################################################################
  # 4.6 ACCOUNTS RECEIVABLE - Server Logic
  ##############################################################################
  
  ar_aging_focus <- reactiveVal(NULL)
  
  ar_view_data <- reactive({
    data_version()
    df <- filtered_ar()
    focus <- ar_aging_focus()
    if (!is.null(focus) && nzchar(focus)) {
      df <- df %>% filter(as.character(Aging_Bucket) == focus)
    }
    df
  })
  
  observeEvent(input$ar_clear_aging, ar_aging_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "ar_aging", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "ar_aging", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      
      selected <- NULL
      for (field in c("customdata", "label", "key")) {
        candidate <- clicked[[field]]
        if (!is.null(candidate) && length(candidate) > 0 && !is.na(candidate[[1]])) {
          selected <- as.character(candidate[[1]])
          break
        }
      }
      if (is.null(selected) || !nzchar(selected)) return()
      
      new_focus <- if (identical(ar_aging_focus(), selected)) NULL else selected
      ar_aging_focus(new_focus)
      showNotification(
        if (is.null(new_focus)) "Accounts Receivable aging focus cleared."
        else paste0("Accounts Receivable focused on ", new_focus, "."),
        type = "message",
        duration = 3
      )
    },
    ignoreInit = TRUE
  )
  
  output$ar_aging_focus_label <- renderUI({
    data_version()
    focus <- ar_aging_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All aging groups" else paste("Focused:", focus)
    )
  })
  
  output$ar_kpi_outstanding <- renderText({
    data_version()
    df <- ar_view_data()
    total <- sum(df$Balance_Remaining[df$Balance_Remaining > 0], na.rm = TRUE)
    paste0("$", format(round(total, 0), big.mark = ","))
  })
  
  output$ar_kpi_overdue30 <- renderText({
    data_version()
    df <- ar_view_data()
    total <- sum(
      df$Balance_Remaining[
        df$Days_Past_Due > 30 & df$Balance_Remaining > 0
      ],
      na.rm = TRUE
    )
    paste0("$", format(round(total, 0), big.mark = ","))
  })
  
  output$ar_kpi_current <- renderText({
    data_version()
    df <- ar_view_data()
    total <- sum(
      df$Balance_Remaining[
        df$Days_Until_Due > 0 & df$Balance_Remaining > 0
      ],
      na.rm = TRUE
    )
    paste0("$", format(round(total, 0), big.mark = ","))
  })
  
  output$ar_kpi_count <- renderText({
    data_version()
    format(sum(ar_view_data()$Balance_Remaining > 0, na.rm = TRUE), big.mark = ",")
  })
  
  output$ar_aging_donut <- renderPlotly({
    data_version()
    df <- filtered_ar() %>%
      filter(Balance_Remaining > 0) %>%
      group_by(Aging_Bucket) %>%
      summarise(
        Amount = sum(Balance_Remaining, na.rm = TRUE),
        Invoices = n(),
        Customers = n_distinct(Customer),
        .groups = "drop"
      ) %>%
      filter(Amount > 0)
    
    validate(need(nrow(df) > 0, "No open receivables match the selected filters."))
    
    colors_aging <- c(
      "Current" = brand_sage,
      "1-30 Days" = "#7B946F",
      "31-60 Days" = brand_amber,
      "61-90 Days" = "#B46A49",
      "90+ Days" = brand_coral
    )
    
    focus <- ar_aging_focus()
    pulls <- if (is.null(focus)) rep(0, nrow(df)) else ifelse(as.character(df$Aging_Bucket) == focus, .08, 0)
    
    chart <- plot_ly(
      df,
      source = "ar_aging",
      labels = ~Aging_Bucket,
      values = ~Amount,
      key = ~as.character(Aging_Bucket),
      customdata = ~as.character(Aging_Bucket),
      type = "pie",
      hole = .58,
      sort = FALSE,
      pull = pulls,
      textinfo = "label+percent",
      marker = list(colors = colors_aging[as.character(df$Aging_Bucket)]),
      text = ~paste0("Invoices: ", Invoices, "<br>Customers: ", Customers),
      hovertemplate = "<b>%{label}</b><br>Outstanding: $%{value:,.2f}<br>%{percent}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$ar_exposure_plot_ui <- renderUI({
    data_version()
    available <- ar_view_data() %>%
      filter(Balance_Remaining > 0, !is.na(Customer), Customer != "") %>%
      summarise(Customer_Count = n_distinct(Customer)) %>%
      pull(Customer_Count)
    requested <- if (is.null(input$ar_top_n)) 10 else as.numeric(input$ar_top_n)
    displayed <- min(requested, available)
    plot_height <- max(430, min(1500, 150 + displayed * 34))
    plotlyOutput("ar_exposure_bar", height = paste0(plot_height, "px")) %>%
      withSpinner(color = brand_amber)
  })
  
  output$ar_exposure_bar <- renderPlotly({
    data_version()
    n_top <- if (is.null(input$ar_top_n)) 10 else as.numeric(input$ar_top_n)
    df <- ar_view_data() %>%
      filter(Balance_Remaining > 0, !is.na(Customer), Customer != "") %>%
      group_by(Customer) %>%
      summarise(
        Balance = sum(Balance_Remaining, na.rm = TRUE),
        Oldest_Days_Past_Due = max(Days_Past_Due, na.rm = TRUE),
        Open_Invoices = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Balance))
    
    if (n_top < nrow(df)) df <- slice_head(df, n = n_top)
    df <- arrange(df, Balance)
    validate(need(nrow(df) > 0, "No customer exposure matches the selected aging focus."))
    df$Customer <- factor(df$Customer, levels = df$Customer)
    
    chart <- plot_ly(
      df,
      source = "ar_customer_exposure",
      y = ~Customer,
      x = ~Balance,
      key = ~as.character(Customer),
      customdata = ~as.character(Customer),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~Oldest_Days_Past_Due,
        colorscale = list(c(0, "#DCE9F2"), c(.55, "#C89A58"), c(1, "#A65359")),
        showscale = TRUE,
        colorbar = list(title = "Oldest<br>days late")
      ),
      text = ~paste0(
        "Open invoices: ", Open_Invoices,
        "<br>Oldest days past due: ", Oldest_Days_Past_Due
      ),
      hovertemplate = "<b>%{y}</b><br>Outstanding: $%{x:,.2f}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Outstanding Balance ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 235, r = 80, t = 25, b = 55),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "ar_customer_exposure", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "ar_customer_exposure", priority = "event")
      )
      if (is.null(selected)) return()
      open_customer_workspace(selected, tab = "client_statement")
    },
    ignoreInit = TRUE
  )
  
  output$ar_staff_aging <- renderPlotly({
    data_version()
    df <- ar_view_data() %>%
      filter(Balance_Remaining > 0, !is.na(Staff), Staff != "") %>%
      group_by(Staff, Aging_Bucket) %>%
      summarise(Amount = sum(Balance_Remaining, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No staff-level receivables match the selected aging focus."))
    
    colors_aging <- c(
      "Current" = brand_sage,
      "1-30 Days" = "#7B946F",
      "31-60 Days" = brand_amber,
      "61-90 Days" = "#B46A49",
      "90+ Days" = brand_coral
    )
    
    chart <- plot_ly(
      df,
      source = "ar_staff",
      x = ~Staff, y = ~Amount,
      key = ~Staff,
      customdata = ~Staff,
      color = ~Aging_Bucket,
      type = "bar",
      colors = colors_aging,
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: $%{y:,.2f}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", automargin = TRUE),
        yaxis = list(title = "Outstanding Balance ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.14),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "ar_staff", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "ar_staff", priority = "event")
      )
      if (is.null(selected)) return()
      updateSelectInput(session, "perf_staff", selected = selected)
      navigate_sidebar("sale_performance")
    },
    ignoreInit = TRUE
  )
  
  output$ar_monthly_trend <- renderPlotly({
    data_version()
    df <- ar_view_data() %>%
      group_by(Year, Month, Month_Name) %>%
      summarise(
        Invoiced = sum(Invoice_Amount, na.rm = TRUE),
        Outstanding = sum(Balance_Remaining, na.rm = TRUE),
        Open_Invoices = sum(Balance_Remaining > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Year, Month) %>%
      mutate(
        Period = paste(substr(Month_Name, 1, 3), Year),
        Period_Date = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month)))
      )
    
    validate(need(nrow(df) > 0, "No monthly receivables data matches the selected focus."))
    df$Period <- factor(df$Period, levels = unique(df$Period))
    
    chart <- plot_ly(df, source = "ar_monthly") %>%
      add_bars(
        x = ~Period, y = ~Invoiced,
        key = ~as.character(Period_Date),
        customdata = ~as.character(Period_Date),
        name = "Invoiced", marker = list(color = brand_sky),
        hovertemplate = "%{x}<br>Invoiced: $%{y:,.2f}<extra></extra>"
      ) %>%
      add_lines(
        x = ~Period, y = ~Outstanding,
        key = ~as.character(Period_Date),
        customdata = ~as.character(Period_Date),
        name = "Outstanding",
        line = list(color = brand_coral, width = 3),
        mode = "lines+markers",
        text = ~paste0("Open invoices: ", Open_Invoices),
        hovertemplate = "%{x}<br>Outstanding: $%{y:,.2f}<br>%{text}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "Amount ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.12),
        margin = list(b = 80),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "ar_monthly", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "ar_monthly", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      period_value <- if (!is.null(clicked$customdata)) clicked$customdata[[1]] else clicked$key[[1]]
      apply_global_period_from_date(period_value)
    },
    ignoreInit = TRUE
  )
  
  output$ar_full_table <- renderDT({
    data_version()
    df <- ar_view_data() %>%
      select(
        Customer,
        Staff,
        `PO Number` = PO_Number,
        `Invoice Reference` = Invoice_Ref,
        `Invoice Date` = Txn_Date,
        `Due Date` = Due_Date,
        `Invoice Amount` = Invoice_Amount,
        `Balance Remaining` = Balance_Remaining,
        `Payment Timing` = Payment_Timing,
        `Days Until Due` = Days_Until_Due,
        `Days Past Due` = Days_Past_Due,
        `Aging Group` = Aging_Bucket
      ) %>%
      arrange(desc(`Days Past Due`), desc(`Balance Remaining`))
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top",
      selection = "single"
    ) %>%
      formatCurrency(c("Invoice Amount", "Balance Remaining"), currency = "$", digits = 2) %>%
      formatStyle(
        "Days Past Due",
        backgroundColor = styleInterval(
          c(0, 30, 60, 90),
          c("#FFFFFF", "#F8F3E8", "#F6E9E1", "#F3DEDE", "#EED0D2")
        )
      )
  })
  
  
  observeEvent(input$ar_full_table_rows_selected, {
    idx <- input$ar_full_table_rows_selected
    if (length(idx) != 1) return()
    
    df <- ar_view_data() %>%
      select(
        Customer,
        Staff,
        `PO Number` = PO_Number,
        `Invoice Reference` = Invoice_Ref,
        `Invoice Date` = Txn_Date,
        `Due Date` = Due_Date,
        `Invoice Amount` = Invoice_Amount,
        `Balance Remaining` = Balance_Remaining,
        `Payment Timing` = Payment_Timing,
        `Days Until Due` = Days_Until_Due,
        `Days Past Due` = Days_Past_Due,
        `Aging Group` = Aging_Bucket
      ) %>%
      arrange(desc(`Days Past Due`), desc(`Balance Remaining`))
    
    if (idx < 1 || idx > nrow(df)) return()
    open_customer_workspace(df$Customer[[idx]], tab = "client_statement")
  }, ignoreInit = TRUE)
  
  ##############################################################################
  # 4.7 CLIENT STATEMENT - Server Logic (Now with real client names)
  ##############################################################################
  
  stmt_aging_focus <- reactiveVal(NULL)
  
  stmt_data <- reactive({
    data_version()
    req(input$stmt_client != "")
    today <- Sys.Date()
    
    ar_data %>%
      filter(Customer == input$stmt_client) %>%
      mutate(
        Days_Until_Due = case_when(
          is.na(Due_Date) | Balance_Remaining <= 0 ~ 0,
          Due_Date > today ~ as.numeric(difftime(Due_Date, today, units = "days")),
          TRUE ~ 0
        ),
        Days_Past_Due = case_when(
          is.na(Due_Date) | Balance_Remaining <= 0 ~ 0,
          Due_Date < today ~ as.numeric(difftime(today, Due_Date, units = "days")),
          TRUE ~ 0
        ),
        Payment_Timing = case_when(
          Balance_Remaining <= 0 ~ "Paid",
          is.na(Due_Date) ~ "Due date unavailable",
          Due_Date > today ~ paste0("Due in ", as.integer(Days_Until_Due), " day(s)"),
          Due_Date == today ~ "Due today",
          TRUE ~ paste0(as.integer(Days_Past_Due), " day(s) overdue")
        )
      )
  })
  
  stmt_view_data <- reactive({
    data_version()
    df <- stmt_data()
    focus <- stmt_aging_focus()
    if (!is.null(focus) && focus != "") {
      df <- df %>% filter(as.character(Aging_Bucket) == focus)
    }
    df
  })
  
  observeEvent(input$stmt_client, stmt_aging_focus(NULL), ignoreInit = TRUE)
  observeEvent(input$stmt_clear_aging, stmt_aging_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "stmt_aging", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "stmt_aging", priority = "event")
      )
      if (is.null(selected)) return()
      stmt_aging_focus(if (identical(stmt_aging_focus(), selected)) NULL else selected)
    },
    ignoreInit = TRUE
  )
  
  output$stmt_aging_focus_label <- renderUI({
    data_version()
    focus <- stmt_aging_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All aging groups" else paste("Focused:", focus)
    )
  })
  
  output$stmt_total_invoiced <- renderText({
    data_version()
    req(input$stmt_client != "")
    paste0("$", format(round(sum(stmt_data()$Invoice_Amount, na.rm = TRUE), 0), big.mark = ","))
  })
  output$stmt_balance <- renderText({
    data_version()
    req(input$stmt_client != "")
    paste0("$", format(round(sum(stmt_data()$Balance_Remaining, na.rm = TRUE), 0), big.mark = ","))
  })
  output$stmt_paid_pct <- renderText({
    data_version()
    req(input$stmt_client != "")
    inv <- sum(stmt_data()$Invoice_Amount, na.rm = TRUE)
    bal <- sum(stmt_data()$Balance_Remaining, na.rm = TRUE)
    if (inv > 0) paste0(round((1 - bal/inv) * 100, 1), "%") else "N/A"
  })
  output$stmt_invoice_count <- renderText({
    data_version()
    req(input$stmt_client != "")
    format(nrow(stmt_data()), big.mark = ",")
  })
  
  output$stmt_aging_donut <- renderPlotly({
    data_version()
    req(input$stmt_client != "", nrow(stmt_data()) > 0)
    aging <- stmt_data() %>%
      filter(Balance_Remaining > 0) %>%
      group_by(Aging_Bucket) %>%
      summarise(Amount = sum(Balance_Remaining, na.rm = TRUE), Invoices = n(), .groups = "drop")
    
    if (nrow(aging) == 0) {
      plot_ly() %>% layout(title = "All invoices paid")
    } else {
      colors_aging <- c(
        "Current" = brand_sage, "1-30 Days" = "#7B946F",
        "31-60 Days" = brand_amber, "61-90 Days" = "#B46A49", "90+ Days" = brand_coral
      )
      focus <- stmt_aging_focus()
      pulls <- if (is.null(focus)) rep(0, nrow(aging)) else ifelse(as.character(aging$Aging_Bucket) == focus, .09, 0)
      
      chart <- plot_ly(
        aging,
        source = "stmt_aging",
        labels = ~Aging_Bucket,
        values = ~Amount,
        key = ~as.character(Aging_Bucket),
        customdata = ~as.character(Aging_Bucket),
        type = "pie",
        hole = 0.55,
        sort = FALSE,
        pull = pulls,
        textinfo = "label+percent",
        marker = list(colors = colors_aging[as.character(aging$Aging_Bucket)]),
        text = ~paste0("Invoices: ", Invoices),
        hovertemplate = "<b>%{label}</b><br>Balance: $%{value:,.2f}<br>%{text}<br>Click to focus the timeline and statement<extra></extra>"
      ) %>%
        layout(
          showlegend = TRUE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displaylogo = FALSE, responsive = TRUE)
      
      register_plotly_click(chart)
    }
  })
  
  output$stmt_timeline <- renderPlotly({
    data_version()
    req(input$stmt_client != "", nrow(stmt_view_data()) > 0)
    
    timeline <- stmt_view_data() %>%
      arrange(Txn_Date) %>%
      mutate(Cumulative = cumsum(Invoice_Amount))
    
    chart <- plot_ly(timeline, source = "stmt_timeline") %>%
      add_bars(
        x = ~Txn_Date, y = ~Invoice_Amount,
        key = ~as.character(Txn_Date), customdata = ~as.character(Txn_Date),
        name = "Invoice Amount", marker = list(color = "#2F6EA5"),
        hovertemplate = "%{x|%b %d, %Y}<br>Invoice: $%{y:,.2f}<br>Click to drill period<extra></extra>"
      ) %>%
      add_lines(
        x = ~Txn_Date, y = ~Cumulative,
        key = ~as.character(Txn_Date), customdata = ~as.character(Txn_Date),
        name = "Cumulative", yaxis = "y2",
        line = list(color = "#9A6A27", width = 2)
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Invoice ($)", tickformat = "$,.0f"),
        yaxis2 = list(title = "Cumulative ($)", overlaying = "y", side = "right", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.1),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "stmt_timeline", priority = "event"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "stmt_timeline", priority = "event")
    if (is.null(clicked) || nrow(clicked) == 0) return()
    value <- if (!is.null(clicked$customdata)) clicked$customdata[[1]] else clicked$x[[1]]
    apply_global_period_from_date(value)
  }, ignoreInit = TRUE)
  
  output$stmt_detail_table <- renderDT({
    data_version()
    req(input$stmt_client != "", nrow(stmt_view_data()) > 0)
    df <- stmt_view_data() %>%
      select(
        `Invoice Date` = Txn_Date,
        `Due Date` = Due_Date,
        `Invoice Reference` = Invoice_Ref,
        `PO Number` = PO_Number,
        Terms,
        `Invoice Amount` = Invoice_Amount,
        `Balance Remaining` = Balance_Remaining,
        `Payment Timing` = Payment_Timing,
        `Days Until Due` = Days_Until_Due,
        `Days Past Due` = Days_Past_Due,
        `Aging Group` = Aging_Bucket
      ) %>%
      arrange(desc(`Invoice Date`))
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(c("Invoice Amount", "Balance Remaining"), currency = "$", digits = 2)
  })
  
  output$stmt_download <- downloadHandler(
    filename = function() { paste0("Statement_", gsub("[^A-Za-z0-9]", "_", input$stmt_client), "_", Sys.Date(), ".csv") },
    content = function(file) {
      req(input$stmt_client != "")
      write.csv(stmt_view_data() %>% select(Txn_Date, Due_Date, Invoice_Ref, PO_Number, Terms, 
                                            Invoice_Amount, Balance_Remaining, Payment_Timing,
                                            Days_Until_Due, Days_Past_Due, Aging_Bucket),
                file, row.names = FALSE)
    }
  )
  
  ##############################################################################
  # 4.8 ORDERS, DELIVERY PLANNING & CUSTOMER HISTORY - Server Logic
  ##############################################################################
  
  tracker_status_focus <- reactiveVal(NULL)
  
  tracker_data_base <- reactive({
    data_version()
    df <- filtered_inventory()
    
    if (!is.null(input$tracker_client) && input$tracker_client != "all") {
      df <- df %>% filter(Client == input$tracker_client)
    }
    if (!is.null(input$tracker_buyer) && input$tracker_buyer != "all") {
      df <- df %>% filter(Buyer == input$tracker_buyer)
    }
    if (!is.null(input$tracker_supplier) && input$tracker_supplier != "all") {
      df <- df %>% filter(Supplier == input$tracker_supplier)
    }
    if (!is.null(input$tracker_status) && input$tracker_status != "all") {
      df <- df %>% filter(as.character(Order_Status) == input$tracker_status)
    }
    
    horizon <- if (is.null(input$tracker_horizon)) "all" else input$tracker_horizon
    if (horizon == "overdue") {
      df <- df %>% filter(Backordered_Qty > 0, !is.na(Delivery_Date), Delivery_Date < Sys.Date())
    } else if (horizon %in% c("7", "14", "30", "90")) {
      horizon_days <- as.numeric(horizon)
      df <- df %>% filter(
        Backordered_Qty > 0,
        !is.na(Delivery_Date),
        Delivery_Date >= Sys.Date(),
        Delivery_Date <= Sys.Date() + horizon_days
      )
    }
    
    search_term <- str_to_lower(str_squish(if (is.null(input$tracker_search)) "" else input$tracker_search))
    if (search_term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$PO_Number, ""),
        coalesce(df$Part_Number, ""),
        coalesce(df$Memo, ""),
        coalesce(df$Order_Description, ""),
        coalesce(df$Client, ""),
        coalesce(df$Buyer, ""),
        coalesce(df$Supplier, "")
      ))
      df <- df[str_detect(searchable, fixed(search_term)), , drop = FALSE]
    }
    
    df
  })
  
  tracker_data <- reactive({
    data_version()
    df <- tracker_data_base()
    focus <- tracker_status_focus()
    if (!is.null(focus) && nzchar(focus)) {
      df <- df %>% filter(as.character(Order_Status) == focus)
    }
    df
  })
  
  observeEvent(input$tracker_reset, {
    updateSelectizeInput(session, "tracker_client", selected = "all")
    updateSelectizeInput(session, "tracker_buyer", selected = "all")
    updateSelectizeInput(session, "tracker_supplier", selected = "all")
    updateSelectInput(session, "tracker_status", selected = "all")
    updateSelectInput(session, "tracker_horizon", selected = "all")
    updateSelectInput(session, "tracker_timeline_metric", selected = "Open_Balance")
    updateTextInput(session, "tracker_search", value = "")
    tracker_status_focus(NULL)
  })
  
  observeEvent(input$tracker_clear_status, tracker_status_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "tracker_status", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "tracker_status", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      
      selected <- NULL
      for (field in c("customdata", "label", "key")) {
        candidate <- clicked[[field]]
        if (!is.null(candidate) && length(candidate) > 0 && !is.na(candidate[[1]])) {
          selected <- as.character(candidate[[1]])
          break
        }
      }
      if (is.null(selected) || !nzchar(selected)) return()
      
      new_focus <- if (identical(tracker_status_focus(), selected)) NULL else selected
      tracker_status_focus(new_focus)
      showNotification(
        if (is.null(new_focus)) "Delivery status focus cleared."
        else paste0("Orders & Delivery focused on ", new_focus, "."),
        type = "message",
        duration = 3
      )
    },
    ignoreInit = TRUE
  )
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "tracker_heatmap"), {
    clicked <- safe_plotly_event_data("plotly_click", source = "tracker_heatmap")
    if (is.null(clicked) || is.null(clicked$x) || is.null(clicked$y)) return()
    
    clicked_month <- match(as.character(clicked$x[[1]]), month.abb)
    clicked_year <- suppressWarnings(as.numeric(as.character(clicked$y[[1]])))
    
    if (!is.na(clicked_month) && !is.na(clicked_year)) {
      updateSelectInput(session, "global_year", selected = as.character(clicked_year))
      updateSelectInput(session, "global_month", selected = as.character(clicked_month))
      showNotification(
        paste0("Global period changed to ", month.name[clicked_month], " ", clicked_year, "."),
        type = "message",
        duration = 4
      )
    }
  })
  
  output$tracker_status_focus_label <- renderUI({
    data_version()
    focus <- tracker_status_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All statuses" else paste("Focused:", focus)
    )
  })
  
  output$tracker_open_orders <- renderText({
    data_version()
    df <- tracker_data()
    format(n_distinct(df$PO_Number[df$Backordered_Qty > 0]), big.mark = ",")
  })
  
  output$tracker_overdue <- renderText({
    data_version()
    value <- tracker_data() %>%
      filter(
        Backordered_Qty > 0,
        !is.na(Delivery_Date),
        Delivery_Date < Sys.Date()
      ) %>%
      summarise(Value = sum(Open_Balance, na.rm = TRUE)) %>%
      pull(Value)
    paste0("$", format(round(coalesce(value, 0), 0), big.mark = ","))
  })
  
  output$tracker_due_soon <- renderText({
    data_version()
    lines_due <- tracker_data() %>%
      filter(
        Backordered_Qty > 0,
        !is.na(Delivery_Date),
        Delivery_Date >= Sys.Date(),
        Delivery_Date <= Sys.Date() + 14
      ) %>%
      nrow()
    format(lines_due, big.mark = ",")
  })
  
  output$tracker_open_value <- renderText({
    data_version()
    total_value <- tracker_data() %>%
      filter(Line_Type != "Information") %>%
      summarise(Value = sum(Amount, na.rm = TRUE)) %>%
      pull(Value)
    
    dollar(coalesce(total_value, 0), accuracy = 1)
  })
  
  output$tracker_status_donut <- renderPlotly({
    data_version()
    df <- tracker_data_base() %>%
      filter(Line_Type != "Information") %>%
      count(Order_Status, name = "Lines") %>%
      filter(Lines > 0)
    
    validate(need(nrow(df) > 0, "No order records match the selected filters."))
    
    status_colors <- c(
      "Overdue" = brand_coral,
      "Due Soon" = brand_amber,
      "Open" = brand_blue,
      "Fully Received" = brand_sage,
      "Information" = brand_slate
    )
    
    focus <- tracker_status_focus()
    pulls <- if (is.null(focus)) rep(0, nrow(df)) else ifelse(as.character(df$Order_Status) == focus, .08, 0)
    
    chart <- plot_ly(
      df,
      source = "tracker_status",
      labels = ~Order_Status,
      values = ~Lines,
      key = ~as.character(Order_Status),
      customdata = ~as.character(Order_Status),
      type = "pie",
      hole = .58,
      sort = FALSE,
      pull = pulls,
      textinfo = "label+percent",
      marker = list(colors = status_colors[as.character(df$Order_Status)]),
      hovertemplate = "<b>%{label}</b><br>%{value} order lines<br>%{percent}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$tracker_delivery_timeline <- renderPlotly({
    data_version()
    metric <- if (is.null(input$tracker_timeline_metric)) {
      "Open_Balance"
    } else {
      input$tracker_timeline_metric
    }
    
    df <- tracker_data() %>%
      filter(
        Line_Type != "Information",
        !is.na(Delivery_Date),
        Backordered_Qty > 0
      ) %>%
      group_by(Delivery_Date, Order_Status) %>%
      summarise(
        Open_Balance = sum(Open_Balance, na.rm = TRUE),
        Backordered_Qty = sum(Backordered_Qty, na.rm = TRUE),
        Lines = n(),
        Customers = n_distinct(Client),
        Suppliers = n_distinct(Supplier),
        .groups = "drop"
      ) %>%
      arrange(Delivery_Date)
    
    validate(need(nrow(df) > 0, "No scheduled open deliveries match the selected filters."))
    
    metric_labels <- c(
      Open_Balance = "Open Value ($)",
      Backordered_Qty = "Backordered Units",
      Lines = "Order Lines"
    )
    metric_value <- df[[metric]]
    is_currency <- metric == "Open_Balance"
    
    status_colors <- c(
      "Overdue" = brand_coral,
      "Due Soon" = brand_amber,
      "Open" = brand_blue,
      "Fully Received" = brand_sage,
      "Information" = brand_slate
    )
    
    plot_ly(
      df,
      x = ~Delivery_Date,
      y = metric_value,
      color = ~Order_Status,
      colors = status_colors,
      type = "bar",
      text = ~paste0(
        "Customers: ", Customers,
        "<br>Suppliers: ", Suppliers,
        "<br>Order lines: ", Lines,
        "<br>Backordered units: ", format(Backordered_Qty, big.mark = ","),
        "<br>Open value: $", format(round(Open_Balance, 2), big.mark = ",", nsmall = 2)
      ),
      hovertemplate = "<b>%{x|%b %d, %Y}</b><br>%{fullData.name}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(
          title = "Scheduled Delivery Date",
          type = "date",
          rangeslider = list(visible = TRUE, thickness = .07)
        ),
        yaxis = list(
          title = metric_labels[[metric]],
          tickformat = if (is_currency) "$,.0f" else ",.0f"
        ),
        legend = list(orientation = "h", y = 1.14),
        shapes = list(list(
          type = "line",
          x0 = Sys.Date(), x1 = Sys.Date(),
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = brand_navy, width = 2, dash = "dash")
        )),
        margin = list(t = 55, b = 80),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$tracker_delivery_risk <- renderPlotly({
    data_version()
    df <- tracker_data() %>%
      filter(
        Line_Type == "Product",
        Backordered_Qty > 0,
        !is.na(Delivery_Date)
      ) %>%
      mutate(
        Risk_Age = pmax(-Days_To_Delivery, 0),
        Bubble_Size = pmax(Backordered_Qty, 1),
        Hover = paste0(
          "<b>", Client, "</b>",
          "<br>PO: ", PO_Number,
          "<br>Supplier: ", coalesce(Supplier, "Unknown"),
          "<br>Part: ", coalesce(Part_Number, "Not provided"),
          "<br>Scheduled: ", format(Delivery_Date, "%b %d, %Y"),
          "<br>Days to delivery: ", Days_To_Delivery,
          "<br>Units awaiting receipt: ", format(Backordered_Qty, big.mark = ","),
          "<br>Open value: $", format(round(Open_Balance, 2), big.mark = ",", nsmall = 2)
        )
      )
    
    validate(need(nrow(df) > 0, "No open product lines are available for the risk matrix."))
    
    status_colors <- c(
      "Overdue" = brand_coral,
      "Due Soon" = brand_amber,
      "Open" = brand_blue,
      "Fully Received" = brand_sage
    )
    
    plot_ly(
      df,
      x = ~Days_To_Delivery,
      y = ~Open_Balance,
      size = ~Bubble_Size,
      color = ~Order_Status,
      colors = status_colors,
      type = "scatter",
      mode = "markers",
      text = ~Hover,
      hovertemplate = "%{text}<extra></extra>",
      marker = list(opacity = .78, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        xaxis = list(
          title = "Days Until Scheduled Delivery (negative = overdue)",
          zeroline = TRUE,
          zerolinecolor = brand_navy,
          zerolinewidth = 2
        ),
        yaxis = list(title = "Open Value ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.12),
        margin = list(t = 40, b = 65),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        displayModeBar = TRUE,
        scrollZoom = TRUE,
        doubleClick = "reset",
        toImageButtonOptions = list(
          format = "png", filename = "Aurelis_Delivery_Risk_Matrix", scale = 2
        )
      )
  })
  
  output$tracker_delivery_heatmap <- renderPlotly({
    data_version()
    df <- tracker_data_base() %>%
      filter(
        Line_Type != "Information",
        !is.na(Delivery_Year),
        !is.na(Delivery_Month)
      ) %>%
      group_by(Delivery_Year, Delivery_Month) %>%
      summarise(
        Workload = sum(Backordered_Qty, na.rm = TRUE),
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Lines = n(),
        .groups = "drop"
      )
    
    validate(need(nrow(df) > 0, "No delivery calendar data matches the selected filters."))
    
    years <- sort(unique(df$Delivery_Year))
    grid <- df %>%
      complete(
        Delivery_Year = years,
        Delivery_Month = 1:12,
        fill = list(Workload = 0, Open_Value = 0, Lines = 0)
      ) %>%
      mutate(
        Month_Label = factor(month.abb[Delivery_Month], levels = month.abb),
        Hover = paste0(
          month.name[Delivery_Month], " ", Delivery_Year,
          "<br>Units awaiting receipt: ", format(Workload, big.mark = ","),
          "<br>Open value: $", format(round(Open_Value, 2), big.mark = ",", nsmall = 2),
          "<br>Order lines: ", Lines
        )
      )
    
    value_matrix <- grid %>%
      select(Delivery_Year, Month_Label, Workload) %>%
      pivot_wider(names_from = Month_Label, values_from = Workload, values_fill = 0) %>%
      arrange(Delivery_Year)
    
    hover_matrix <- grid %>%
      select(Delivery_Year, Month_Label, Hover) %>%
      pivot_wider(names_from = Month_Label, values_from = Hover, values_fill = "") %>%
      arrange(Delivery_Year)
    
    chart <- plot_ly(
      source = "tracker_heatmap",
      x = names(value_matrix)[-1],
      y = as.character(value_matrix$Delivery_Year),
      z = as.matrix(value_matrix[, -1, drop = FALSE]),
      text = as.matrix(hover_matrix[, -1, drop = FALSE]),
      type = "heatmap",
      colorscale = list(c(0, "#F3F7FA"), c(.5, "#8FB8D7"), c(1, "#2F6EA5")),
      hovertemplate = "%{text}<extra></extra>",
      colorbar = list(title = "Units")
    ) %>%
      layout(
        xaxis = list(title = "Delivery Month"),
        yaxis = list(title = "Delivery Year"),
        margin = list(l = 60, r = 30, t = 25, b = 55),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$tracker_customer_exposure <- renderPlotly({
    data_version()
    df <- tracker_data() %>%
      filter(Line_Type != "Information", Backordered_Qty > 0, !is.na(Client), Client != "") %>%
      group_by(Client, Order_Status) %>%
      summarise(
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Units = sum(Backordered_Qty, na.rm = TRUE),
        POs = n_distinct(PO_Number),
        .groups = "drop"
      ) %>%
      group_by(Client) %>%
      mutate(Customer_Total = sum(Open_Value, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(Customer_Total)) %>%
      filter(Client %in% unique(Client)[seq_len(min(18, n_distinct(Client)))])
    
    validate(need(nrow(df) > 0, "No open customer exposure matches the selected filters."))
    
    status_colors <- c(
      "Overdue" = brand_coral,
      "Due Soon" = brand_amber,
      "Open" = brand_blue,
      "Fully Received" = brand_sage
    )
    
    chart <- plot_ly(
      df, source = "tracker_customer",
      x = ~Client, y = ~Open_Value,
      key = ~Client, customdata = ~Client,
      color = ~Order_Status, colors = status_colors,
      type = "bar",
      text = ~paste0("Units awaiting receipt: ", format(Units, big.mark = ","), "<br>Distinct POs: ", POs),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: $%{y:,.2f}<br>%{text}<br>Click for customer performance<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -35, automargin = TRUE),
        yaxis = list(title = "Open Exposure ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", y = 1.13),
        margin = list(l = 75, r = 25, t = 45, b = 145),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "tracker_customer", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "tracker_customer", priority = "event"))
    if (is.null(selected)) return()
    updateSelectizeInput(session, "tracker_client", selected = selected)
    open_customer_workspace(selected)
  }, ignoreInit = TRUE)
  
  tracker_priority_data <- reactive({
    data_version()
    tracker_data() %>%
      filter(
        Line_Type != "Information",
        Backordered_Qty > 0
      ) %>%
      mutate(
        Delivery_Priority = case_when(
          is.na(Delivery_Date) ~ "Date Missing",
          Delivery_Date < Sys.Date() ~ "1 - Overdue",
          Delivery_Date == Sys.Date() ~ "2 - Due Today",
          Delivery_Date <= Sys.Date() + 7 ~ "3 - Due in 7 Days",
          Delivery_Date <= Sys.Date() + 14 ~ "4 - Due in 14 Days",
          TRUE ~ "5 - Scheduled"
        )
      ) %>%
      arrange(Delivery_Priority, Delivery_Date, desc(Open_Balance)) %>%
      select(
        Delivery_Priority,
        Delivery_Date,
        Days_To_Delivery,
        Customer = Client,
        Buyer,
        Supplier,
        PO_Number,
        Part_Number,
        `Order Description` = Order_Description,
        Backordered_Qty,
        Open_Balance
      )
  })
  
  output$tracker_priority_table <- renderDT({
    data_version()
    datatable(
      tracker_priority_data(),
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency("Open_Balance", currency = "$", digits = 2) %>%
      formatStyle(
        "Delivery_Priority",
        target = "row",
        backgroundColor = styleEqual(
          c(
            "1 - Overdue", "2 - Due Today", "3 - Due in 7 Days",
            "4 - Due in 14 Days", "5 - Scheduled", "Date Missing"
          ),
          c("#F4DCDD", "#F6E5DD", "#F7EDD9", "#F3F0E5", "#EAF2F8", "#EEF1F4")
        )
      )
  })
  
  
  output$tracker_priority_excel <- downloadHandler(
    filename = function() paste0("Aurelis_Prioritized_Delivery_Schedule_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
      }
      
      df <- tracker_priority_data()
      wb <- openxlsx::createWorkbook(creator = "Aurelis Global Supply")
      openxlsx::addWorksheet(wb, "Priority Schedule")
      
      logo_path <- c(file.path("www", "aurelis_logo.png"), file.path("www", "aurelis_logo.jpg"))
      logo_path <- logo_path[file.exists(logo_path)][1]
      if (length(logo_path) > 0 && !is.na(logo_path)) {
        try(openxlsx::insertImage(wb, "Priority Schedule", logo_path, startRow = 1, startCol = 1, width = 2.0, height = .7), silent = TRUE)
      }
      
      openxlsx::writeData(wb, "Priority Schedule", "Prioritized Delivery Schedule", startRow = 1, startCol = 4)
      openxlsx::writeData(wb, "Priority Schedule", paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M")), startRow = 2, startCol = 4)
      openxlsx::writeDataTable(wb, "Priority Schedule", df, startRow = 5, tableStyle = "TableStyleMedium2", withFilter = TRUE)
      openxlsx::freezePane(wb, "Priority Schedule", firstActiveRow = 6)
      if (ncol(df) > 0) openxlsx::setColWidths(wb, "Priority Schedule", cols = seq_len(ncol(df)), widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$tracker_priority_pdf <- downloadHandler(
    filename = function() paste0("Aurelis_Prioritized_Delivery_Schedule_", Sys.Date(), ".pdf"),
    content = function(file) {
      if (!requireNamespace("pagedown", quietly = TRUE)) {
        stop("Install pagedown first: install.packages('pagedown')", call. = FALSE)
      }
      
      df <- tracker_priority_data()
      summary <- tibble(
        Metric = c("Priority Lines", "Distinct POs", "Customers", "Units Awaiting Receipt", "Open Value"),
        Value = c(
          format(nrow(df), big.mark = ","),
          format(n_distinct(df$PO_Number), big.mark = ","),
          format(n_distinct(df$Customer), big.mark = ","),
          format(sum(df$Backordered_Qty, na.rm = TRUE), big.mark = ","),
          dollar(sum(df$Open_Balance, na.rm = TRUE), accuracy = .01)
        )
      )
      
      filters <- tibble(
        Filter = c("Customer", "Buyer", "Supplier", "Status", "Delivery Horizon", "Interactive Status Focus"),
        Selection = c(
          selected_or_all(input$tracker_client),
          selected_or_all(input$tracker_buyer),
          selected_or_all(input$tracker_supplier),
          selected_or_all(input$tracker_status),
          selected_or_all(input$tracker_horizon),
          selected_or_all(tracker_status_focus(), c("", NA))
        )
      )
      
      report_html <- build_aurelis_pdf_html(
        title = "Prioritized Delivery Schedule",
        page_name = "Orders & Delivery",
        filters = filters,
        summary = summary,
        detail = df,
        notes = "Priorities are ordered by delivery urgency, scheduled date and open value.",
        content_mode = "summary_details",
        rows_per_page = 24,
        orientation = "landscape",
        page_specification = "all",
        paper_size = "A4"
      )
      
      temporary_html <- tempfile(fileext = ".html")
      temporary_pdf <- tempfile(fileext = ".pdf")
      writeLines(report_html, temporary_html, useBytes = TRUE)
      pagedown::chrome_print(input = temporary_html, output = temporary_pdf, wait = 1)
      if (!file.copy(temporary_pdf, file, overwrite = TRUE)) {
        stop("The prioritized schedule PDF could not be created.", call. = FALSE)
      }
    }
  )
  
  output$tracker_po_table <- renderDT({
    data_version()
    df <- tracker_data() %>%
      arrange(
        factor(
          Order_Status,
          levels = c("Overdue", "Due Soon", "Open", "Fully Received", "Information")
        ),
        Delivery_Date,
        PO_Number
      ) %>%
      select(
        Order_Status,
        Line_Type,
        Buyer,
        Customer = Client,
        PO_Number,
        Supplier,
        Part_Number,
        `Order Description` = Order_Description,
        Order_Date,
        Delivery_Date,
        Quantity,
        Unit_Price,
        `Line Amount` = Amount,
        Received_Qty,
        Backordered_Qty,
        Open_Balance,
        Days_To_Delivery
      )
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(
        c("Unit_Price", "Line Amount", "Open_Balance"),
        currency = "$",
        digits = 2
      )
  })
  
  output$tracker_download <- downloadHandler(
    filename = function() paste0("Aurelis_Orders_Delivery_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(tracker_data(), file, row.names = FALSE, na = "")
    }
  )
  
  ##############################################################################
  # 4.9 INVENTORY, WAREHOUSE & INBOUND - Server Logic
  ##############################################################################
  
  # ---- 4.9A Real-time warehouse availability --------------------------------
  
  wh_status_focus <- reactiveVal(NULL)
  
  warehouse_filtered_base <- reactive({
    data_version()
    df <- warehouse_inventory_data
    
    if (!is.null(input$wh_warehouse) && input$wh_warehouse != "all") {
      df <- df %>% filter(Warehouse == input$wh_warehouse)
    }
    if (!is.null(input$wh_supplier) && input$wh_supplier != "all") {
      df <- df %>% filter(Supplier == input$wh_supplier)
    }
    if (!is.null(input$wh_status) && input$wh_status != "all") {
      df <- df %>% filter(Stock_Status == input$wh_status)
    }
    
    term <- str_to_lower(str_squish(if (is.null(input$wh_search)) "" else input$wh_search))
    if (term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$Item_Code, ""),
        coalesce(df$Item_Description, ""),
        coalesce(df$Bin_Location, ""),
        coalesce(df$Supplier, ""),
        coalesce(df$Warehouse, "")
      ))
      df <- df[str_detect(searchable, fixed(term)), , drop = FALSE]
    }
    
    df
  })
  
  warehouse_filtered <- reactive({
    data_version()
    df <- warehouse_filtered_base()
    focus <- wh_status_focus()
    if (!is.null(focus) && nzchar(focus)) {
      df <- df %>% filter(Stock_Status == focus)
    }
    df
  })
  
  observeEvent(input$wh_clear_status, wh_status_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "wh_status", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "wh_status", priority = "event")
      )
      if (is.null(selected)) return()
      wh_status_focus(if (identical(wh_status_focus(), selected)) NULL else selected)
    },
    ignoreInit = TRUE
  )
  
  output$wh_status_focus_label <- renderUI({
    data_version()
    focus <- wh_status_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All stock statuses" else paste("Focused:", focus)
    )
  })
  
  output$wh_item_count <- renderText({
    data_version()
    format(n_distinct(warehouse_filtered()$Item_Code), big.mark = ",")
  })
  
  output$wh_on_hand <- renderText({
    data_version()
    if (!warehouse_inventory_available) return("Pending")
    format(round(sum(warehouse_filtered()$On_Hand, na.rm = TRUE), 0), big.mark = ",")
  })
  
  output$wh_available <- renderText({
    data_version()
    if (!warehouse_inventory_available) return("Pending")
    format(round(sum(warehouse_filtered()$Available, na.rm = TRUE), 0), big.mark = ",")
  })
  
  output$wh_attention <- renderText({
    data_version()
    attention_statuses <- c(
      "Negative Availability", "Reorder Required", "Out of Stock",
      "Data Unavailable", "Warehouse data pending"
    )
    format(
      sum(warehouse_filtered()$Stock_Status %in% attention_statuses, na.rm = TRUE),
      big.mark = ","
    )
  })
  
  output$wh_status_donut <- renderPlotly({
    data_version()
    df <- warehouse_filtered_base() %>%
      count(Stock_Status, name = "Items") %>%
      filter(Items > 0)
    
    validate(need(nrow(df) > 0, "No warehouse items match the selected filters."))
    
    stock_colors <- c(
      "Available" = brand_sage,
      "Reorder Required" = brand_amber,
      "Out of Stock" = brand_coral,
      "Negative Availability" = "#8E3E49",
      "Data Unavailable" = brand_slate,
      "Warehouse data pending" = brand_slate
    )
    
    focus <- wh_status_focus()
    pulls <- if (is.null(focus)) rep(0, nrow(df)) else ifelse(df$Stock_Status == focus, .08, 0)
    
    chart <- plot_ly(
      df,
      source = "wh_status",
      labels = ~Stock_Status,
      values = ~Items,
      key = ~Stock_Status,
      customdata = ~Stock_Status,
      type = "pie",
      hole = .58,
      sort = FALSE,
      pull = pulls,
      textinfo = "label+percent",
      marker = list(colors = stock_colors[df$Stock_Status]),
      hovertemplate = "<b>%{label}</b><br>%{value} items<br>%{percent}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$wh_quantity_mix <- renderPlotly({
    data_version()
    df <- warehouse_filtered() %>%
      filter(!is.na(Item_Code), Item_Code != "") %>%
      mutate(
        Attention = case_when(
          Stock_Status %in% c("Out of Stock", "Negative Availability", "Reorder Required") ~ 1,
          TRUE ~ 0
        )
      ) %>%
      arrange(desc(Attention), Available) %>%
      slice_head(n = 20) %>%
      select(Item_Code, On_Hand, Allocated, Available, On_Order) %>%
      pivot_longer(
        cols = c(On_Hand, Allocated, Available, On_Order),
        names_to = "Quantity_Type",
        values_to = "Quantity"
      ) %>%
      filter(!is.na(Quantity))
    
    validate(need(
      nrow(df) > 0,
      "Connected warehouse quantities are not available yet. Configure the Access or QuickBooks warehouse query."
    ))
    
    quantity_colors <- c(
      On_Hand = brand_navy,
      Allocated = brand_amber,
      Available = brand_teal,
      On_Order = brand_sky
    )
    
    plot_ly(
      df,
      x = ~Item_Code,
      y = ~Quantity,
      color = ~Quantity_Type,
      colors = quantity_colors,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -40, automargin = TRUE),
        yaxis = list(title = "Quantity", tickformat = ",.0f"),
        legend = list(orientation = "h", y = 1.13),
        margin = list(b = 115, t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$wh_reorder_matrix <- renderPlotly({
    data_version()
    df <- warehouse_filtered() %>%
      filter(
        is.finite(Available),
        is.finite(Reorder_Point)
      ) %>%
      mutate(
        Gap_To_Reorder = Available - Reorder_Point,
        Demand_Size = if_else(
          is.finite(Monthly_Demand) & Monthly_Demand > 0,
          Monthly_Demand,
          pmax(On_Order, 1)
        ),
        Hover = paste0(
          "<b>", Item_Code, "</b>",
          "<br>", coalesce(Item_Description, ""),
          "<br>Available: ", format(Available, big.mark = ","),
          "<br>Reorder point: ", format(Reorder_Point, big.mark = ","),
          "<br>Gap: ", format(Gap_To_Reorder, big.mark = ","),
          "<br>Monthly demand: ", ifelse(
            is.finite(Monthly_Demand),
            format(Monthly_Demand, big.mark = ","),
            "Not available"
          ),
          "<br>On order: ", format(On_Order, big.mark = ",")
        )
      )
    
    validate(need(
      nrow(df) > 0,
      "Reorder-point data will appear after the warehouse source is connected."
    ))
    
    stock_colors <- c(
      "Available" = brand_sage,
      "Reorder Required" = brand_amber,
      "Out of Stock" = brand_coral,
      "Negative Availability" = "#8E3E49",
      "Data Unavailable" = brand_slate
    )
    
    plot_ly(
      df,
      x = ~Gap_To_Reorder,
      y = ~Available,
      size = ~Demand_Size,
      color = ~Stock_Status,
      colors = stock_colors,
      type = "scatter",
      mode = "markers",
      text = ~Hover,
      hovertemplate = "%{text}<extra></extra>",
      marker = list(opacity = .78, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        xaxis = list(
          title = "Available Quantity minus Reorder Point",
          zeroline = TRUE,
          zerolinecolor = brand_navy,
          zerolinewidth = 2
        ),
        yaxis = list(title = "Available Quantity", zeroline = TRUE),
        legend = list(orientation = "h", y = 1.13),
        margin = list(t = 40, b = 65),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$wh_value_by_warehouse <- renderPlotly({
    data_version()
    
    df <- warehouse_filtered() %>%
      mutate(
        Inventory_Value_Calc = case_when(
          is.finite(Inventory_Value) ~ Inventory_Value,
          is.finite(On_Hand) & is.finite(Unit_Cost) ~ pmax(On_Hand, 0) * pmax(Unit_Cost, 0),
          TRUE ~ NA_real_
        ),
        Available_Value_Calc = case_when(
          is.finite(Available) & is.finite(Unit_Cost) ~ pmax(Available, 0) * pmax(Unit_Cost, 0),
          TRUE ~ NA_real_
        )
      ) %>%
      filter(is.finite(Inventory_Value_Calc)) %>%
      group_by(Warehouse) %>%
      summarise(
        Inventory_Value = sum(Inventory_Value_Calc, na.rm = TRUE),
        Available_Value = sum(Available_Value_Calc, na.rm = TRUE),
        Items = n_distinct(Item_Code),
        .groups = "drop"
      ) %>%
      filter(Inventory_Value > 0) %>%
      mutate(
        Hover_Text = paste0(
          "Available value: $", format(round(Available_Value, 2), big.mark = ","),
          "<br>Items: ", Items
        )
      )
    
    validate(need(
      nrow(df) > 0,
      "Warehouse inventory value requires on-hand quantity and unit-cost data."
    ))
    
    chart <- plot_ly(
      df,
      source = "wh_warehouse_value",
      labels = ~Warehouse,
      values = ~Inventory_Value,
      key = ~Warehouse,
      customdata = ~Warehouse,
      type = "pie",
      hole = .50,
      textinfo = "label+percent",
      hovertext = ~Hover_Text,
      marker = list(colors = rep(pal_executive, length.out = nrow(df))),
      hovertemplate = "<b>%{label}</b><br>Inventory value: $%{value:,.2f}<br>%{hovertext}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "wh_warehouse_value", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "wh_warehouse_value", priority = "event")
      )
      if (is.null(selected)) return()
      current <- if (is.null(input$wh_warehouse)) "all" else input$wh_warehouse
      updateSelectizeInput(
        session, "wh_warehouse",
        selected = if (identical(current, selected)) "all" else selected
      )
    },
    ignoreInit = TRUE
  )
  
  output$wh_inventory_table <- renderDT({
    data_version()
    df <- warehouse_filtered() %>%
      arrange(
        factor(
          Stock_Status,
          levels = c(
            "Negative Availability", "Out of Stock", "Reorder Required",
            "Available", "Data Unavailable", "Warehouse data pending"
          )
        ),
        Available
      ) %>%
      select(
        Stock_Status,
        Item_Code,
        Item_Description,
        Warehouse,
        Bin_Location,
        Supplier,
        On_Hand,
        Allocated,
        Available,
        On_Order,
        Reorder_Point,
        Monthly_Demand,
        Months_Of_Cover,
        Unit_Cost,
        Inventory_Value,
        Last_Updated,
        Data_Source
      )
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(c("Unit_Cost", "Inventory_Value"), currency = "$", digits = 2) %>%
      formatRound("Months_Of_Cover", digits = 1) %>%
      formatStyle(
        "Stock_Status",
        target = "row",
        backgroundColor = styleEqual(
          c(
            "Negative Availability", "Out of Stock", "Reorder Required",
            "Available", "Data Unavailable", "Warehouse data pending"
          ),
          c("#EED0D2", "#F3DEDE", "#F8F0DD", "#EAF3EE", "#EEF1F4", "#EEF1F4")
        )
      )
  })
  
  output$wh_download <- downloadHandler(
    filename = function() paste0("Aurelis_Warehouse_Inventory_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
      }
      
      wb <- openxlsx::createWorkbook(creator = "Aurelis Dashboard")
      openxlsx::addWorksheet(wb, "Warehouse Detail")
      openxlsx::writeDataTable(
        wb, "Warehouse Detail", warehouse_filtered(),
        tableStyle = "TableStyleMedium2"
      )
      openxlsx::addWorksheet(wb, "Definitions")
      openxlsx::writeDataTable(
        wb, "Definitions",
        tibble(
          Metric = c("On Hand", "Allocated", "Available", "On Order", "Reorder Point"),
          Definition = c(
            "Physical quantity reported by the warehouse source.",
            "Quantity reserved or committed.",
            "On hand minus allocated unless directly provided by the source.",
            "Quantity ordered from suppliers and not yet received.",
            "Available quantity level at or below which replenishment is required."
          )
        ),
        tableStyle = "TableStyleMedium2"
      )
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- 4.9B Inbound purchase orders and backorders ---------------------------
  
  inv_timing_focus <- reactiveVal(NULL)
  
  observeEvent(input$inv_delivery_month, {
    if (is.null(input$inv_delivery_month) || input$inv_delivery_month == "all") {
      updateSelectInput(
        session, "inv_delivery_week",
        choices = c("All Weeks" = "all"),
        selected = "all"
      )
    } else {
      updateSelectInput(
        session, "inv_delivery_week",
        choices = c(
          "All Weeks" = "all",
          "Week 1 (Days 1-7)" = "1",
          "Week 2 (Days 8-14)" = "2",
          "Week 3 (Days 15-21)" = "3",
          "Week 4 (Days 22-28)" = "4",
          "Week 5 (Days 29-End)" = "5"
        ),
        selected = "all"
      )
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$inv_delivery_reset, {
    updateSelectizeInput(session, "inv_client", selected = "all")
    updateSelectizeInput(session, "inv_supplier", selected = "all")
    updateSelectInput(session, "inv_status", selected = "all")
    updateSelectInput(session, "inv_delivery_year", selected = "all")
    updateSelectInput(session, "inv_delivery_month", selected = "all")
    updateSelectInput(
      session, "inv_delivery_week",
      choices = c("All Weeks" = "all"),
      selected = "all"
    )
    updateTextInput(session, "inv_search", value = "")
    inv_timing_focus(NULL)
  })
  
  inventory_filtered_base <- reactive({
    data_version()
    df <- filtered_inventory()
    
    if (!is.null(input$inv_client) && input$inv_client != "all") {
      df <- df %>% filter(Client == input$inv_client)
    }
    if (!is.null(input$inv_supplier) && input$inv_supplier != "all") {
      df <- df %>% filter(Supplier == input$inv_supplier)
    }
    if (!is.null(input$inv_status) && input$inv_status != "all") {
      df <- df %>% filter(as.character(Order_Status) == input$inv_status)
    }
    if (!is.null(input$inv_delivery_year) && input$inv_delivery_year != "all") {
      df <- df %>% filter(Delivery_Year == as.numeric(input$inv_delivery_year))
    }
    if (!is.null(input$inv_delivery_month) && input$inv_delivery_month != "all") {
      df <- df %>% filter(Delivery_Month == as.numeric(input$inv_delivery_month))
    }
    if (!is.null(input$inv_delivery_week) && input$inv_delivery_week != "all") {
      df <- df %>% filter(Delivery_Week == as.numeric(input$inv_delivery_week))
    }
    
    term <- str_to_lower(str_squish(if (is.null(input$inv_search)) "" else input$inv_search))
    if (term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$Part_Number, ""),
        coalesce(df$PO_Number, ""),
        coalesce(df$Memo, ""),
        coalesce(df$Order_Description, ""),
        coalesce(df$Supplier, ""),
        coalesce(df$Client, ""),
        coalesce(df$Buyer, "")
      ))
      df <- df[str_detect(searchable, fixed(term)), , drop = FALSE]
    }
    
    df
  })
  
  inventory_filtered <- reactive({
    data_version()
    df <- inventory_filtered_base() %>%
      mutate(
        Inbound_Timing = case_when(
          Line_Type == "Information" | Backordered_Qty <= 0 ~ "Not Awaiting Receipt",
          is.na(Delivery_Date) ~ "Delivery Date Missing",
          Delivery_Date < Sys.Date() ~ "Overdue",
          Delivery_Date == Sys.Date() ~ "Due Today",
          Delivery_Date <= Sys.Date() + 14 ~ "Due Within 14 Days",
          TRUE ~ "Scheduled Later"
        )
      )
    
    focus <- inv_timing_focus()
    if (!is.null(focus) && focus != "") {
      df <- df %>% filter(Inbound_Timing == focus)
    }
    df
  })
  
  observeEvent(input$inv_clear_timing, inv_timing_focus(NULL))
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "inv_timing", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "inv_timing", priority = "event")
      )
      if (is.null(selected)) return()
      inv_timing_focus(if (identical(inv_timing_focus(), selected)) NULL else selected)
    },
    ignoreInit = TRUE
  )
  
  output$inv_timing_focus_label <- renderUI({
    data_version()
    focus <- inv_timing_focus()
    tags$span(
      class = "status-chip",
      if (is.null(focus)) "All inbound timing" else paste("Focused:", focus)
    )
  })
  
  output$inv_open_value <- renderText({
    data_version()
    paste0(
      "$",
      format(
        round(sum(inventory_filtered()$Open_Balance, na.rm = TRUE), 2),
        big.mark = ",",
        nsmall = 2
      )
    )
  })
  
  output$inv_backordered_qty <- renderText({
    data_version()
    format(round(sum(inventory_filtered()$Backordered_Qty, na.rm = TRUE), 0), big.mark = ",")
  })
  
  output$inv_item_count <- renderText({
    data_version()
    format(sum(inventory_filtered()$Line_Type == "Product", na.rm = TRUE), big.mark = ",")
  })
  
  output$inv_supplier_count <- renderText({
    data_version()
    format(
      n_distinct(inventory_filtered()$Supplier[
        !is.na(inventory_filtered()$Supplier) &
          inventory_filtered()$Supplier != ""
      ]),
      big.mark = ","
    )
  })
  
  output$inv_supplier_value <- renderPlotly({
    data_version()
    df <- inventory_filtered() %>%
      filter(!is.na(Supplier), Supplier != "", Open_Balance > 0) %>%
      group_by(Supplier) %>%
      summarise(
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Units_Awaiting = sum(Backordered_Qty, na.rm = TRUE),
        Open_POs = n_distinct(PO_Number),
        .groups = "drop"
      ) %>%
      arrange(desc(Open_Value)) %>% slice_head(n = 15) %>% arrange(Open_Value)
    
    validate(need(nrow(df) > 0, "No supplier inbound value matches the selected filters."))
    df$Supplier <- factor(df$Supplier, levels = df$Supplier)
    
    chart <- plot_ly(
      df, source = "inv_supplier",
      y = ~Supplier, x = ~Open_Value,
      key = ~as.character(Supplier), customdata = ~as.character(Supplier),
      type = "bar", orientation = "h",
      marker = list(color = ~Units_Awaiting, colorscale = list(c(0, "#DDEAF3"), c(1, "#2F6EA5")), showscale = TRUE, colorbar = list(title = "Units")),
      text = ~paste0("Units awaiting receipt: ", format(Units_Awaiting, big.mark = ","), "<br>Open POs: ", Open_POs),
      hovertemplate = "<b>%{y}</b><br>Open value: $%{x:,.2f}<br>%{text}<br>Click for supplier intelligence<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Open Inbound Value ($)", tickformat = "$,.0f"), yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 195, r = 75, t = 25, b = 55), plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "inv_supplier", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "inv_supplier", priority = "event"))
    if (is.null(selected)) return()
    updateSelectizeInput(session, "inv_supplier", selected = selected)
    open_supplier_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$inv_client_units <- renderPlotly({
    data_version()
    df <- inventory_filtered() %>%
      filter(!is.na(Client), Client != "", Backordered_Qty > 0) %>%
      group_by(Client) %>%
      summarise(
        Units_Awaiting = sum(Backordered_Qty, na.rm = TRUE),
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        POs = n_distinct(PO_Number),
        .groups = "drop"
      ) %>%
      arrange(desc(Units_Awaiting)) %>% slice_head(n = 15) %>% arrange(Units_Awaiting)
    
    validate(need(nrow(df) > 0, "No customer quantities awaiting receipt match the filters."))
    df$Client <- factor(df$Client, levels = df$Client)
    
    chart <- plot_ly(
      df, source = "inv_customer",
      y = ~Client, x = ~Units_Awaiting,
      key = ~as.character(Client), customdata = ~as.character(Client),
      type = "bar", orientation = "h",
      marker = list(color = ~Open_Value, colorscale = list(c(0, "#F6EDD9"), c(1, "#9A6A27")), showscale = TRUE, colorbar = list(title = "Open $")),
      text = ~paste0("Open value: $", format(round(Open_Value, 2), big.mark = ",", nsmall = 2), "<br>Distinct POs: ", POs),
      hovertemplate = "<b>%{y}</b><br>Units awaiting receipt: %{x:,.0f}<br>%{text}<br>Click for customer performance<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Units Awaiting Receipt", tickformat = ",.0f"), yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 215, r = 75, t = 25, b = 55), plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "inv_customer", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "inv_customer", priority = "event"))
    if (is.null(selected)) return()
    updateSelectizeInput(session, "inv_client", selected = selected)
    open_customer_workspace(selected)
  }, ignoreInit = TRUE)
  
  output$inv_part_backorders <- renderPlotly({
    data_version()
    df <- inventory_filtered() %>%
      filter(Line_Type == "Product", !is.na(Part_Number), Part_Number != "", Backordered_Qty > 0) %>%
      group_by(Part_Number, Order_Description) %>%
      summarise(
        Units_Awaiting = sum(Backordered_Qty, na.rm = TRUE),
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Suppliers = n_distinct(Supplier),
        Customers = n_distinct(Client),
        .groups = "drop"
      ) %>%
      arrange(desc(Units_Awaiting)) %>% slice_head(n = 18) %>% arrange(Units_Awaiting)
    
    validate(need(nrow(df) > 0, "No part-level backorders match the selected filters."))
    df$Part_Number <- factor(df$Part_Number, levels = df$Part_Number)
    
    chart <- plot_ly(
      df, source = "inv_part",
      y = ~Part_Number, x = ~Units_Awaiting,
      key = ~as.character(Part_Number), customdata = ~as.character(Part_Number),
      type = "bar", orientation = "h", marker = list(color = brand_blue),
      text = ~paste0(coalesce(Order_Description, ""), "<br>Open value: $", format(round(Open_Value, 2), big.mark = ",", nsmall = 2),
                     "<br>Suppliers: ", Suppliers, "<br>Customers: ", Customers),
      hovertemplate = "<b>%{y}</b><br>Units awaiting receipt: %{x:,.0f}<br>%{text}<br>Click to filter this part<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Units Awaiting Receipt", tickformat = ",.0f"), yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 160, r = 30, t = 25, b = 55), plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(safe_plotly_event_data("plotly_click", source = "inv_part", priority = "event"), {
    selected <- plotly_clicked_value(safe_plotly_event_data("plotly_click", source = "inv_part", priority = "event"))
    if (is.null(selected)) return()
    updateTextInput(session, "inv_search", value = selected)
  }, ignoreInit = TRUE)
  
  output$inv_timing_donut <- renderPlotly({
    data_version()
    df <- inventory_filtered_base() %>%
      filter(Line_Type != "Information", Backordered_Qty > 0) %>%
      mutate(
        Timing = case_when(
          is.na(Delivery_Date) ~ "Delivery Date Missing",
          Delivery_Date < Sys.Date() ~ "Overdue",
          Delivery_Date == Sys.Date() ~ "Due Today",
          Delivery_Date <= Sys.Date() + 14 ~ "Due Within 14 Days",
          TRUE ~ "Scheduled Later"
        )
      ) %>%
      group_by(Timing) %>%
      summarise(
        Units = sum(Backordered_Qty, na.rm = TRUE),
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Lines = n(),
        .groups = "drop"
      ) %>%
      filter(Units > 0)
    
    validate(need(nrow(df) > 0, "No inbound timing data matches the selected filters."))
    
    timing_colors <- c(
      "Overdue" = brand_coral,
      "Due Today" = "#B46A49",
      "Due Within 14 Days" = brand_amber,
      "Scheduled Later" = brand_blue,
      "Delivery Date Missing" = brand_slate
    )
    
    focus <- inv_timing_focus()
    pulls <- if (is.null(focus)) rep(0, nrow(df)) else ifelse(df$Timing == focus, .09, 0)
    
    chart <- plot_ly(
      df,
      source = "inv_timing",
      labels = ~Timing,
      values = ~Units,
      key = ~Timing,
      customdata = ~Timing,
      type = "pie",
      hole = .55,
      sort = FALSE,
      pull = pulls,
      textinfo = "label+percent",
      marker = list(colors = timing_colors[df$Timing]),
      text = ~paste0(
        "Open value: $", format(round(Open_Value, 2), big.mark = ","),
        "<br>Order lines: ", Lines
      ),
      hovertemplate = "<b>%{label}</b><br>%{value:,.0f} units<br>%{percent}<br>%{text}<br>Click to focus inbound analytics<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  output$inventory_detail_table <- renderDT({
    data_version()
    df <- inventory_filtered() %>%
      arrange(
        factor(
          Order_Status,
          levels = c("Overdue", "Due Soon", "Open", "Fully Received", "Information")
        ),
        Delivery_Date
      ) %>%
      select(
        Order_Status,
        Line_Type,
        Buyer,
        Customer = Client,
        PO_Number,
        Supplier,
        Part_Number,
        `Order Description` = Order_Description,
        Delivery_Location,
        Order_Date,
        Delivery_Date,
        Quantity,
        Unit_Price,
        Received_Qty,
        Backordered_Qty,
        `Line Amount` = Amount,
        Open_Balance,
        Days_To_Delivery
      )
    
    datatable(
      df,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(
        c("Unit_Price", "Line Amount", "Open_Balance"),
        currency = "$",
        digits = 2
      ) %>%
      formatStyle(
        "Order_Status",
        target = "row",
        backgroundColor = styleEqual(
          c("Overdue", "Due Soon", "Open", "Fully Received", "Information"),
          c("#F3DEDE", "#F8F0DD", "#EAF2F8", "#EAF3EE", "#EEF1F4")
        )
      )
  })
  
  ##############################################################################
  # 4.10 BUYER / SUPPLIER TOOL - Server Logic
  ##############################################################################
  
  supplier_period_data <- reactive({
    data_version()
    filtered_inventory() %>%
      filter(Line_Type != "Information", !is.na(Supplier), Supplier != "")
  })
  
  output$supplier_kpi_po_count <- renderText({
    format(n_distinct(supplier_period_data()$PO_Number), big.mark = ",")
  })
  
  output$supplier_kpi_active_suppliers <- renderText({
    format(n_distinct(supplier_period_data()$Supplier), big.mark = ",")
  })
  
  output$supplier_kpi_open_value <- renderText({
    dollar(sum(pmax(supplier_period_data()$Open_Balance, 0), na.rm = TRUE), accuracy = 1)
  })
  
  output$supplier_kpi_backorders <- renderText({
    format(sum(pmax(supplier_period_data()$Backordered_Qty, 0), na.rm = TRUE), big.mark = ",")
  })
  
  seller_catalog_filtered <- reactive({
    data_version()
    df <- seller_catalog_data
    
    search_term <- str_to_lower(str_squish(coalesce(input$seller_catalog_search, "")))
    if (search_term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$Product_Code, ""),
        coalesce(df$Product_Service, ""),
        coalesce(df$Category, ""),
        coalesce(df$Supplier, ""),
        coalesce(df$Manufacturer, "")
      ))
      df <- df[str_detect(searchable, fixed(search_term)), , drop = FALSE]
    }
    
    if (!is.null(input$seller_catalog_supplier) && input$seller_catalog_supplier != "all") {
      df <- df %>% filter(Supplier == input$seller_catalog_supplier)
    }
    if (!is.null(input$seller_catalog_category) && input$seller_catalog_category != "all") {
      df <- df %>% filter(Category == input$seller_catalog_category)
    }
    if (!is.null(input$seller_catalog_type) && input$seller_catalog_type != "all") {
      df <- df %>% filter(Product_Service_Type == input$seller_catalog_type)
    }
    
    df %>%
      arrange(Product_Service_Type, Category, Product_Service, Preferred_Rank, Supplier)
  })
  
  output$seller_catalog_table <- renderDT({
    df <- seller_catalog_filtered() %>%
      transmute(
        Type = Product_Service_Type,
        Category,
        `Product / Service Code` = Product_Code,
        `Product / Service` = Product_Service,
        Seller = Supplier,
        Country,
        Manufacturer,
        `Preferred Rank` = Preferred_Rank,
        `Typical Lead Days` = Typical_Lead_Days,
        `Last Quoted Cost` = Last_Quoted_Unit_Cost,
        Currency
      )
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
    ) %>%
      formatCurrency("Last Quoted Cost", currency = "$", digits = 2)
  })
  
  observeEvent(input$seller_catalog_table_rows_selected, {
    idx <- input$seller_catalog_table_rows_selected
    if (length(idx) != 1) return()
    df <- seller_catalog_filtered()
    if (idx < 1 || idx > nrow(df)) return()
    
    selected_supplier <- df$Supplier[[idx]]
    updateSelectizeInput(session, "seller_catalog_supplier", selected = selected_supplier)
    updateSelectizeInput(session, "supplier_compare_suppliers", selected = selected_supplier)
    updateSelectizeInput(session, "buyer_search_supplier", selected = selected_supplier)
  }, ignoreInit = TRUE)
  
  output$buyer_supplier_value <- renderPlotly({
    data_version()
    
    df <- supplier_period_data() %>%
      filter(Open_Balance > 0) %>%
      group_by(Supplier) %>%
      summarise(
        Open_Value = sum(Open_Balance, na.rm = TRUE),
        Backordered = sum(Backordered_Qty, na.rm = TRUE),
        PO_Count = n_distinct(PO_Number),
        .groups = "drop"
      ) %>%
      arrange(desc(Open_Value)) %>%
      slice_head(n = 15) %>%
      arrange(Open_Value)
    
    validate(need(nrow(df) > 0, "No supplier open-order data is available."))
    df$Supplier <- factor(df$Supplier, levels = df$Supplier)
    
    chart <- plot_ly(
      df,
      source = "supplier_open_value",
      y = ~Supplier,
      x = ~Open_Value,
      key = ~as.character(Supplier),
      customdata = ~as.character(Supplier),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~Backordered,
        colorscale = list(c(0, "#E3F2FD"), c(1, "#0D47A1"))
      ),
      text = ~paste0(PO_Count, " POs · ", format(Backordered, big.mark = ","), " units"),
      hovertemplate = "<b>%{y}</b><br>Open value: $%{x:,.2f}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Open Value ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 190, r = 25),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "supplier_open_value", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "supplier_open_value", priority = "event")
      )

      if (is.null(selected)) return()
      updateSelectizeInput(session, "seller_catalog_supplier", selected = selected)
      updateSelectizeInput(session, "supplier_compare_suppliers", selected = selected)
      updateSelectizeInput(session, "buyer_search_supplier", selected = selected)
    },
    ignoreInit = TRUE
  )

      ##############################################################################
      # 4.10 PRODUCT INTELLIGENCE - Catalog Ranking and Inquiry Studio
      ##############################################################################

      pi_input_value <- function(value, fallback) {
        if (is.null(value) || length(value) == 0 || all(is.na(value))) fallback else value[[1]]
      }

      pi_product_master <- reactive({
        data_version()

        catalog <- seller_catalog_data %>%
          filter(tolower(Product_Service_Type) %in% c("product", "products", "item", "")) %>%
          mutate(
            Product_Code = as.character(Product_Code),
            Product_Service = as.character(Product_Service),
            Category = as.character(Category),
            Supplier = as.character(Supplier),
            Manufacturer = as.character(Manufacturer),
            Preferred_Rank = suppressWarnings(as.numeric(Preferred_Rank)),
            Typical_Lead_Days = suppressWarnings(as.numeric(Typical_Lead_Days)),
            Last_Quoted_Unit_Cost = suppressWarnings(as.numeric(Last_Quoted_Unit_Cost))
          )

        stock <- warehouse_inventory_data %>%
          transmute(
            Product_Code = as.character(Item_Code),
            On_Hand = suppressWarnings(as.numeric(On_Hand)),
            Available = suppressWarnings(as.numeric(Available)),
            On_Order = suppressWarnings(as.numeric(On_Order))
          ) %>%
          filter(!is.na(Product_Code), Product_Code != "") %>%
          group_by(Product_Code) %>%
          summarise(
            On_Hand = sum(On_Hand, na.rm = TRUE),
            Available = sum(Available, na.rm = TRUE),
            On_Order = sum(On_Order, na.rm = TRUE),
            .groups = "drop"
          )

            order_stock <- inventory_data %>%
              transmute(
                Product_Code = as.character(Part_Number),
                On_Hand = pmax(suppressWarnings(as.numeric(Received_Qty)), 0),
                Available = pmax(suppressWarnings(as.numeric(Quantity)) - suppressWarnings(as.numeric(Received_Qty)) - suppressWarnings(as.numeric(Backordered_Qty)), 0),
                On_Order = pmax(suppressWarnings(as.numeric(Backordered_Qty)), 0)
              ) %>%
              filter(!is.na(Product_Code), Product_Code != "") %>%
              group_by(Product_Code) %>%
              summarise(across(c(On_Hand, Available, On_Order), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

            if (nrow(stock) == 0 || all(stock$On_Hand == 0 & stock$Available == 0 & stock$On_Order == 0)) {
              stock <- order_stock
            } else if (nrow(order_stock) > 0) {
              stock <- full_join(stock, order_stock, by = "Product_Code", suffix = c("", ".orders")) %>%
                mutate(
                  On_Hand = coalesce(On_Hand, 0) + coalesce(On_Hand.orders, 0),
                  Available = coalesce(Available, 0) + coalesce(Available.orders, 0),
                  On_Order = coalesce(On_Order, 0) + coalesce(On_Order.orders, 0)
                ) %>%
                select(Product_Code, On_Hand, Available, On_Order)
            }

        catalog %>%
          left_join(stock, by = "Product_Code") %>%
          mutate(
            On_Hand = coalesce(On_Hand, 0),
            Available = coalesce(Available, 0),
            On_Order = coalesce(On_Order, 0),
            Preferred_Rank = coalesce(Preferred_Rank, 999),
            Typical_Lead_Days = coalesce(Typical_Lead_Days, 0),
            Last_Quoted_Unit_Cost = coalesce(Last_Quoted_Unit_Cost, 0),
            Availability = case_when(
              Available > 0 ~ "In stock",
              On_Order > 0 ~ "Inbound",
              TRUE ~ "Backorder risk"
            )
          ) %>%
          distinct(Product_Code, Supplier, .keep_all = TRUE)
      })

      observe({
        categories <- pi_product_master() %>%
          pull(Category) %>%
          unique() %>%
          sort() %>%
          na.omit()
        updateSelectInput(session, "pi_category", choices = c("All categories" = "all", categories))
      })

      pi_filtered_products <- reactive({
        df <- pi_product_master()
        query <- str_to_lower(str_squish(pi_input_value(input$pi_search, "")))
        category <- pi_input_value(input$pi_category, "all")
        availability <- pi_input_value(input$pi_stock, "all")

        if (query != "") {
          searchable <- str_to_lower(paste(df$Product_Code, df$Product_Service, df$Category, df$Supplier, df$Manufacturer))
          df <- df[str_detect(searchable, fixed(query)), , drop = FALSE]
        }
        if (category != "all") df <- df %>% filter(Category == category)
        if (availability == "in_stock") df <- df %>% filter(Available > 0)
        if (availability == "risk") df <- df %>% filter(Available <= 0)
        df
      })

      pi_ranked_products <- reactive({
        df <- pi_filtered_products()
        budget <- suppressWarnings(as.numeric(pi_input_value(input$pi_budget, 0)))
        query <- str_to_lower(str_squish(pi_input_value(input$pi_search, "")))

        if (nrow(df) == 0) return(df)

        relevance <- if (query == "") 70 else {
          searchable <- str_to_lower(paste(df$Product_Code, df$Product_Service, df$Category, df$Manufacturer))
          ifelse(str_detect(searchable, fixed(query)), 100, 35)
        }
        price_fit <- if (budget > 0 && any(df$Last_Quoted_Unit_Cost > 0)) {
          pmax(0, 100 - abs(df$Last_Quoted_Unit_Cost - budget) / budget * 100)
        } else rep(60, nrow(df))
        lead_fit <- if (any(df$Typical_Lead_Days > 0)) pmax(0, 100 - df$Typical_Lead_Days * 2) else rep(60, nrow(df))
        stock_fit <- ifelse(df$Available > 0, 100, ifelse(df$On_Order > 0, 55, 20))
        preference_fit <- pmax(0, 100 - pmin(df$Preferred_Rank, 10) * 7)

        df %>%
          mutate(
            Relevance = relevance,
            Price_Fit = price_fit,
            Lead_Fit = lead_fit,
            Stock_Fit = stock_fit,
            Preference_Fit = preference_fit,
            Match_Score = round(
              Relevance * 0.30 + Price_Fit * 0.22 + Lead_Fit * 0.18 +
                Stock_Fit * 0.18 + Preference_Fit * 0.12,
              1
            )
          ) %>%
          arrange(desc(Match_Score), Preferred_Rank, Typical_Lead_Days) %>%
          slice_head(n = as.integer(pi_input_value(input$pi_top_n, 10)))
      })

      output$pi_catalog_count <- renderText({ format(nrow(pi_product_master()), big.mark = ",") })
      output$pi_supplier_count <- renderText({ format(n_distinct(pi_product_master()$Supplier), big.mark = ",") })

      output$pi_rank_table <- renderDT({
        df <- pi_ranked_products() %>%
          transmute(
            Code = Product_Code,
            Product = Product_Service,
            Category,
            Seller = Supplier,
            Availability,
            `Lead days` = Typical_Lead_Days,
            `Quoted cost` = Last_Quoted_Unit_Cost,
            `Match score` = Match_Score
          )
        datatable(df, rownames = FALSE, selection = "single", options = list(pageLength = 8, scrollX = TRUE)) %>%
          formatCurrency("Quoted cost", currency = "$", digits = 0) %>%
          formatStyle("Match score", color = styleInterval(c(50, 75), c("#B45309", "#2563EB", "#047857")), fontWeight = "700")
      })

      output$pi_catalog_table <- renderDT({
        df <- pi_filtered_products() %>%
          transmute(Code = Product_Code, Product = Product_Service, Category, Seller = Supplier, Manufacturer, Availability, On_Hand, Available, On_Order)
        datatable(df, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
      })

      output$pi_landscape_plot <- renderPlotly({
        df <- pi_ranked_products()
        validate(need(nrow(df) > 0, "No catalog records match this brief."))
        plot_ly(
          df,
          x = ~Last_Quoted_Unit_Cost,
          y = ~Typical_Lead_Days,
          type = "scatter",
          mode = "markers",
          text = ~paste(Product_Service, "<br>Score:", Match_Score, "<br>", Availability),
          hoverinfo = "text",
          marker = list(size = 13, color = ~Match_Score, colorscale = "Viridis", showscale = TRUE)
        ) %>%
          layout(
            xaxis = list(title = "Quoted unit cost (USD)"),
            yaxis = list(title = "Typical lead days"),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            margin = list(l = 55, r = 20, t = 20, b = 45)
          )
      })

      output$pi_priority_card <- renderUI({
        df <- pi_ranked_products()
        if (nrow(df) == 0) return(tags$p("No product matches this brief yet."))
        product <- df[1, ]
        tagList(
          div(class = "pi-priority-score", tags$span("TOP MATCH"), tags$strong(paste0(product$Match_Score, "/100"))),
          tags$h4(product$Product_Service),
          tags$p(paste(product$Category, "·", product$Supplier), class = "small-tag"),
          tags$p(tags$b("Why it leads: "), paste(product$Availability, "with", product$Typical_Lead_Days, "day typical lead time and a", product$Preferred_Rank, "preference rank.")),
          tags$p(tags$b("Quoted cost: "), dollar_format()(product$Last_Quoted_Unit_Cost))
        )
      })

      pi_inquiry_recommendation <- eventReactive(input$pi_generate_inquiry, {
        req(str_squish(input$pi_request))
        pi_ranked_products()
      })

      output$pi_inquiry_result <- renderUI({
        if (is.null(input$pi_generate_inquiry) || input$pi_generate_inquiry == 0) return(tags$p("Generate a brief to turn the current recommendation into a client-ready starting point."))
        df <- pi_inquiry_recommendation()
        if (nrow(df) == 0) return(tags$p("No recommendations were found for this request."))
        div(
          class = "pi-inquiry-result",
          tags$strong(paste("Brief prepared for", input$pi_client)),
          tags$p(paste("Lead recommendation:", df$Product_Service[[1]])),
          tags$p(paste("Top", min(3, nrow(df)), "matches are ready for quotation review."))
        )
      })
  
  output$buyer_supplier_summary <- renderDT({
    data_version()
    df <- filtered_inventory() %>%
      filter(!is.na(Supplier), Supplier != "") %>%
      group_by(Supplier) %>%
      summarise(
        `PO Count` = n_distinct(PO_Number),
        `Line Count` = n(),
        `Backordered Units` = sum(Backordered_Qty, na.rm = TRUE),
        `Open Value` = sum(Open_Balance, na.rm = TRUE),
        `Overdue Lines` = sum(as.character(Order_Status) == "Overdue", na.rm = TRUE),
        `Average Days to Delivery` = round(mean(Days_To_Delivery, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Open Value`))
    
    datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency("Open Value", currency = "$", digits = 2)
  })
  
  # --------------------------------------------------------------------------
  # Supplier selection workbench
  # --------------------------------------------------------------------------
  
  supplier_compare_data <- reactive({
    data_version()
    df <- inventory_data %>%
      filter(Line_Type != "Information", !is.na(Supplier), Supplier != "") %>%
      mutate(
        Planned_Lead_Days = as.numeric(difftime(Delivery_Date, Order_Date, units = "days")),
        Search_Text = str_to_lower(paste(
          coalesce(Part_Number, ""),
          coalesce(Order_Description, ""),
          coalesce(Memo, "")
        ))
      )
    
    search_term <- str_to_lower(str_squish(if (is.null(input$supplier_compare_search)) "" else input$supplier_compare_search))
    if (search_term != "") {
      df <- df[str_detect(df$Search_Text, fixed(search_term)), , drop = FALSE]
    }
    
    chosen <- input$supplier_compare_suppliers
    if (!is.null(chosen) && length(chosen) > 0) {
      df <- df %>% filter(Supplier %in% chosen)
    }
    
    summary <- df %>%
      group_by(Supplier) %>%
      summarise(
        `PO Count` = n_distinct(PO_Number),
        `Comparable Lines` = n(),
        `Median Unit Cost` = median(Unit_Price[is.finite(Unit_Price) & Unit_Price >= 0], na.rm = TRUE),
        `Average Unit Cost` = mean(Unit_Price[is.finite(Unit_Price) & Unit_Price >= 0], na.rm = TRUE),
        `Average Planned Lead Days` = mean(Planned_Lead_Days[is.finite(Planned_Lead_Days) & Planned_Lead_Days >= 0], na.rm = TRUE),
        `Overdue Rate` = mean(as.character(Order_Status) == "Overdue", na.rm = TRUE),
        `Backorder Rate` = safe_divide(sum(Backordered_Qty, na.rm = TRUE), sum(Quantity, na.rm = TRUE), 0),
        `Open Exposure` = sum(Open_Balance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(`Median Unit Cost`, `Average Unit Cost`, `Average Planned Lead Days`), ~ if_else(is.nan(.x), NA_real_, .x)),
        `Overdue Rate` = if_else(is.nan(`Overdue Rate`), 0, `Overdue Rate`),
        `Backorder Rate` = if_else(is.nan(`Backorder Rate`), 0, `Backorder Rate`)
      )
    
    if (nrow(summary) == 0) return(summary)
    
    score_low_good <- function(x, neutral = 50) {
      x <- suppressWarnings(as.numeric(x))
      finite <- is.finite(x)
      if (sum(finite) <= 1 || diff(range(x[finite], na.rm = TRUE)) == 0) return(rep(neutral, length(x)))
      out <- rep(neutral, length(x))
      out[finite] <- 100 * (max(x[finite]) - x[finite]) / diff(range(x[finite]))
      out
    }
    
    summary <- summary %>%
      mutate(
        Cost_Score = score_low_good(`Median Unit Cost`),
        Speed_Score = score_low_good(`Average Planned Lead Days`),
        Reliability_Score = pmax(0, pmin(100, 100 * (1 - .55 * `Overdue Rate` - .45 * `Backorder Rate`))),
        Exposure_Score = score_low_good(`Open Exposure`)
      )
    
    weights <- switch(
      input$supplier_compare_priority,
      cost = c(cost = .62, speed = .13, reliability = .20, exposure = .05),
      speed = c(cost = .18, speed = .55, reliability = .22, exposure = .05),
      risk = c(cost = .15, speed = .15, reliability = .55, exposure = .15),
      c(cost = .35, speed = .23, reliability = .32, exposure = .10)
    )
    
    summary %>%
      mutate(
        `Decision Score` = round(
          weights[["cost"]] * Cost_Score +
            weights[["speed"]] * Speed_Score +
            weights[["reliability"]] * Reliability_Score +
            weights[["exposure"]] * Exposure_Score,
          1
        )
      ) %>%
      arrange(desc(`Decision Score`), `Median Unit Cost`)
  })
  
  output$supplier_compare_best <- renderText({
    df <- supplier_compare_data()
    if (nrow(df) == 0) "No match" else df$Supplier[[1]]
  })
  
  output$supplier_compare_cheapest <- renderText({
    df <- supplier_compare_data() %>% filter(is.finite(`Median Unit Cost`)) %>% arrange(`Median Unit Cost`)
    if (nrow(df) == 0) "N/A" else df$Supplier[[1]]
  })
  
  output$supplier_compare_fastest <- renderText({
    df <- supplier_compare_data() %>% filter(is.finite(`Average Planned Lead Days`)) %>% arrange(`Average Planned Lead Days`)
    if (nrow(df) == 0) "N/A" else df$Supplier[[1]]
  })
  
  output$supplier_compare_reliable <- renderText({
    df <- supplier_compare_data() %>% arrange(desc(Reliability_Score), `Backorder Rate`)
    if (nrow(df) == 0) "N/A" else df$Supplier[[1]]
  })
  
  output$supplier_compare_score_plot <- renderPlotly({
    df <- supplier_compare_data() %>% slice_head(n = 12) %>% arrange(`Decision Score`)
    validate(need(nrow(df) > 0, "No supplier history matches the comparison filters."))
    
    chart <- plot_ly(
      df,
      source = "supplier_score",
      x = ~`Decision Score`,
      y = ~reorder(Supplier, `Decision Score`),
      key = ~Supplier,
      type = "bar",
      orientation = "h",
      marker = list(color = brand_sage),
      text = ~paste0(`Decision Score`, "/100"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>Decision score: %{x:.1f}<br>",
        "Median cost: $%{customdata[0]:,.2f}<br>",
        "Lead: %{customdata[1]:.1f} days<extra></extra>"
      ),
      customdata = ~cbind(`Median Unit Cost`, `Average Planned Lead Days`)
    ) %>%
      layout(
        xaxis = list(title = "Weighted score", range = c(0, 110)),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 160, r = 35),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "supplier_score", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "supplier_score", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      
      supplier <- if (!is.null(clicked$key) && length(clicked$key) > 0) {
        as.character(clicked$key[[1]])
      } else if (!is.null(clicked$y) && length(clicked$y) > 0) {
        as.character(clicked$y[[1]])
      } else {
        ""
      }
      
      if (is.na(supplier) || supplier == "") return()
      updateSelectizeInput(session, "supplier_compare_suppliers", selected = supplier)
      updateSelectizeInput(session, "seller_catalog_supplier", selected = supplier)
    },
    ignoreInit = TRUE
  )
  
  output$supplier_compare_table <- renderDT({
    df <- supplier_compare_data() %>%
      select(
        Supplier, `Decision Score`, `PO Count`, `Comparable Lines`,
        `Median Unit Cost`, `Average Unit Cost`, `Average Planned Lead Days`,
        `Overdue Rate`, `Backorder Rate`, `Open Exposure`
      )
    datatable(
      df, rownames = FALSE, filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, order = list(list(1, "desc")))
    ) %>%
      formatCurrency(c("Median Unit Cost", "Average Unit Cost", "Open Exposure"), currency = "$", digits = 2) %>%
      formatPercentage(c("Overdue Rate", "Backorder Rate"), digits = 1) %>%
      formatRound(c("Decision Score", "Average Planned Lead Days"), digits = 1)
  })
  
  # --------------------------------------------------------------------------
  # Quotation Studio
  # --------------------------------------------------------------------------
  
  quotation_safe_num <- function(value, default = 0) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 0 || is.na(value[[1]]) || !is.finite(value[[1]])) default else value[[1]]
  }
  
  quotation_currency_symbol <- function(code) {
    switch(
      as.character(code),
      USD = "$", EUR = "€", GBP = "£", CAD = "C$", XAF = "FCFA ",
      paste0(as.character(code), " ")
    )
  }
  
  quotation_money <- function(value, currency = input$quotation_currency, digits = 2) {
    value <- quotation_safe_num(value, 0)
    paste0(
      quotation_currency_symbol(currency),
      format(round(value, digits), big.mark = ",", nsmall = digits, scientific = FALSE)
    )
  }
  
  quotation_new_line_frame <- function(n = 1L) {
    tibble(
      Line = seq_len(max(1L, as.integer(n))),
      Part_Number = rep("", max(1L, as.integer(n))),
      Description = rep("", max(1L, as.integer(n))),
      Quantity = rep(1, max(1L, as.integer(n))),
      Unit = rep("EA", max(1L, as.integer(n))),
      Supplier = rep("", max(1L, as.integer(n))),
      Vendor_Unit_Cost = rep(0, max(1L, as.integer(n))),
      Supplier_Discount_Pct = rep(0, max(1L, as.integer(n)))
    )
  }
  
  quotation_lines <- reactiveVal(quotation_new_line_frame())
  
  quotation_line_calculated <- reactive({
    df <- quotation_lines()
    if (nrow(df) == 0) df <- quotation_new_line_frame()
    df %>%
      mutate(
        Line = row_number(),
        Quantity = suppressWarnings(as.numeric(Quantity)),
        Quantity = pmax(replace_na(Quantity, 0), 0),
        Vendor_Unit_Cost = suppressWarnings(as.numeric(Vendor_Unit_Cost)),
        Vendor_Unit_Cost = pmax(replace_na(Vendor_Unit_Cost, 0), 0),
        Supplier_Discount_Pct = suppressWarnings(as.numeric(Supplier_Discount_Pct)),
        Supplier_Discount_Pct = pmax(pmin(replace_na(Supplier_Discount_Pct, 0), 100), 0),
        Net_Unit_Cost = Vendor_Unit_Cost * (1 - Supplier_Discount_Pct / 100),
        Line_Cost = Quantity * Net_Unit_Cost
      )
  })
  
  output$quotation_line_table <- renderDT({
    df <- quotation_line_calculated() %>%
      rename(
        `Part / Reference` = Part_Number,
        `Vendor Unit Cost` = Vendor_Unit_Cost,
        `Supplier Discount %` = Supplier_Discount_Pct,
        `Net Unit Cost` = Net_Unit_Cost,
        `Line Cost` = Line_Cost
      )
    
    currency <- quotation_currency_symbol(input$quotation_currency)
    datatable(
      df,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 8, 9))),
      selection = "single",
      options = list(
        pageLength = 12, scrollX = TRUE, dom = "tip",
        columnDefs = list(list(className = "dt-right", targets = c(3, 6, 7, 8, 9)))
      )
    ) %>%
      formatCurrency(c("Vendor Unit Cost", "Net Unit Cost", "Line Cost"), currency = currency, digits = 2) %>%
      formatRound(c("Quantity", "Supplier Discount %"), digits = 2)
  }, server = FALSE)
  
  observeEvent(input$quotation_line_table_cell_edit, {
    info <- input$quotation_line_table_cell_edit
    display_names <- names(quotation_line_calculated())
    column_index <- quotation_safe_num(info$col, -1) + 1L
    if (column_index < 1 || column_index > length(display_names)) return()
    column_name <- display_names[[column_index]]
    editable_columns <- c(
      "Part_Number", "Description", "Quantity", "Unit", "Supplier",
      "Vendor_Unit_Cost", "Supplier_Discount_Pct"
    )
    if (!column_name %in% editable_columns) return()
    
    df <- quotation_lines()
    row_index <- quotation_safe_num(info$row, 0)
    if (row_index < 1 || row_index > nrow(df)) return()
    value <- info$value
    if (column_name %in% c("Quantity", "Vendor_Unit_Cost", "Supplier_Discount_Pct")) {
      value <- quotation_safe_num(value, 0)
    } else {
      value <- as.character(value)
    }
    df[[column_name]][row_index] <- value
    quotation_lines(df)
  })
  
  observeEvent(input$quotation_add_line, {
    df <- quotation_lines()
    quotation_lines(bind_rows(df, quotation_new_line_frame(1)) %>% mutate(Line = row_number()))
  })
  
  observeEvent(input$quotation_duplicate_line, {
    df <- quotation_lines()
    selected <- input$quotation_line_table_rows_selected
    if (is.null(selected) || length(selected) == 0 || selected[[1]] > nrow(df)) {
      showNotification("Select one quotation line to duplicate.", type = "warning")
      return()
    }
    quotation_lines(bind_rows(df, df[selected[[1]], , drop = FALSE]) %>% mutate(Line = row_number()))
  })
  
  observeEvent(input$quotation_delete_line, {
    df <- quotation_lines()
    selected <- input$quotation_line_table_rows_selected
    if (is.null(selected) || length(selected) == 0 || selected[[1]] > nrow(df)) {
      showNotification("Select one quotation line to delete.", type = "warning")
      return()
    }
    df <- df[-selected[[1]], , drop = FALSE]
    if (nrow(df) == 0) df <- quotation_new_line_frame()
    quotation_lines(df %>% mutate(Line = row_number()))
  })
  
  observeEvent(input$quotation_reset_lines, {
    quotation_lines(quotation_new_line_frame())
  })
  
  output$quotation_line_template <- downloadHandler(
    filename = function() paste0("Aurelis_Quotation_Line_Template_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(
        quotation_new_line_frame(3) %>%
          mutate(
            Part_Number = c("ITEM-001", "ITEM-002", "SERVICE-001"),
            Description = c("Product description", "Product description", "Service / charge"),
            Quantity = c(1, 1, 1),
            Supplier = c("Supplier name", "Supplier name", ""),
            Vendor_Unit_Cost = c(0, 0, 0)
          ) %>%
          select(-Line),
        file, row.names = FALSE, na = ""
      )
    }
  )
  
  observeEvent(input$quotation_import_csv, {
    file_info <- input$quotation_import_csv
    if (is.null(file_info) || !file.exists(file_info$datapath)) return()
    imported <- tryCatch(read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
    if (inherits(imported, "error")) {
      showNotification(paste("CSV import failed:", conditionMessage(imported)), type = "error", duration = 8)
      return()
    }
    
    if (nrow(imported) == 0) {
      showNotification("The selected CSV contains no data rows.", type = "warning")
      return()
    }
    
    normalized_names <- str_to_lower(str_replace_all(names(imported), "[^a-zA-Z0-9]+", "_"))
    names(imported) <- normalized_names
    pull_first <- function(candidates, default = "") {
      found <- intersect(candidates, names(imported))
      if (length(found) == 0) rep(default, nrow(imported)) else imported[[found[[1]]]]
    }
    
    df <- tibble(
      Line = seq_len(max(1, nrow(imported))),
      Part_Number = as.character(pull_first(c("part_number", "part_reference", "item", "sku"))),
      Description = as.character(pull_first(c("description", "item_description", "memo"))),
      Quantity = suppressWarnings(as.numeric(pull_first(c("quantity", "qty"), 1))),
      Unit = as.character(pull_first(c("unit", "uom"), "EA")),
      Supplier = as.character(pull_first(c("supplier", "vendor", "seller"))),
      Vendor_Unit_Cost = suppressWarnings(as.numeric(pull_first(c("vendor_unit_cost", "unit_cost", "cost"), 0))),
      Supplier_Discount_Pct = suppressWarnings(as.numeric(pull_first(c("supplier_discount_pct", "discount_pct", "discount"), 0)))
    ) %>%
      mutate(
        Quantity = replace_na(Quantity, 1),
        Vendor_Unit_Cost = replace_na(Vendor_Unit_Cost, 0),
        Supplier_Discount_Pct = replace_na(Supplier_Discount_Pct, 0)
      )
    
    if (nrow(df) == 0) df <- quotation_new_line_frame()
    quotation_lines(df)
    updateCheckboxInput(session, "quotation_use_cost_override", value = FALSE)
    showNotification(paste(nrow(df), "quotation line(s) imported."), type = "message")
  })
  
  observeEvent(input$quotation_import_po, {
    req(input$quotation_source_po)
    po_rows <- inventory_data %>%
      filter(PO_Number == input$quotation_source_po, Line_Type != "Information") %>%
      transmute(
        Line = row_number(),
        Part_Number = coalesce(Part_Number, ""),
        Description = coalesce(Order_Description, Memo, ""),
        Quantity = if_else(is.na(Quantity) | Quantity <= 0, 1, Quantity),
        Unit = "EA",
        Supplier = coalesce(Supplier, ""),
        Vendor_Unit_Cost = coalesce(Unit_Price, 0),
        Supplier_Discount_Pct = 0
      )
    
    if (nrow(po_rows) == 0) {
      showNotification("No priced product or service lines were found for that PO.", type = "warning")
      return()
    }
    quotation_lines(po_rows)
    updateCheckboxInput(session, "quotation_use_cost_override", value = FALSE)
    
    po_header <- inventory_data %>% filter(PO_Number == input$quotation_source_po) %>% slice(1)
    if (nrow(po_header) > 0) {
      customer <- coalesce(po_header$Client[[1]], "")
      buyer <- coalesce(po_header$Buyer[[1]], "")
      location <- coalesce(po_header$Delivery_Location[[1]], "")
      updateSelectizeInput(session, "quotation_customer_lookup", selected = customer)
      customer_id <- coalesce(as.character(po_header$Client_ID[[1]]), "")
      if (customer_id != "") updateSelectizeInput(session, "quotation_customer_id_lookup", selected = customer_id)
      updateTextInput(session, "quotation_customer_name", value = customer)
      updateTextInput(session, "quotation_customer_code", value = customer_id)
      updateSelectizeInput(session, "quotation_prepared_by", selected = buyer)
      updateTextInput(session, "quotation_delivery_location", value = location)
      updateTextInput(session, "quotation_rfq_reference", value = paste0("Historical PO ", input$quotation_source_po))
    }
    showNotification(paste(nrow(po_rows), "line(s) loaded from", input$quotation_source_po), type = "message")
  })
  
  quotation_input_text <- function(value, fallback = "") {
    if (is.null(value) || length(value) == 0) return(fallback)
    value <- as.character(value[[1]])
    if (is.na(value)) fallback else value
  }
  
  quotation_normalize_name <- function(value) {
    value <- quotation_input_text(value, "")
    str_to_upper(str_squish(str_replace_all(value, "[^A-Za-z0-9]+", " ")))
  }
  
  
  quotation_customer_reference_row <- reactive({
    lookup_id <- quotation_input_text(input$quotation_customer_id_lookup, "")
    typed_id <- quotation_input_text(input$quotation_customer_code, "")
    customer_id <- str_squish(if (lookup_id != "") lookup_id else typed_id)
    
    typed_customer <- quotation_input_text(input$quotation_customer_name, "")
    lookup_customer <- quotation_input_text(input$quotation_customer_lookup, "")
    customer <- str_squish(if (typed_customer != "") typed_customer else lookup_customer)
    
    if (customer_id != "") {
      match_by_id <- quotation_customer_reference %>%
        filter(as.character(Customer_Code) == customer_id) %>%
        slice(1)
      if (nrow(match_by_id) > 0) return(match_by_id)
    }
    
    if (customer == "") return(quotation_customer_reference[0, , drop = FALSE])
    
    quotation_customer_reference %>%
      mutate(Key = quotation_normalize_name(Company)) %>%
      filter(Key == quotation_normalize_name(customer)) %>%
      slice(1)
  })
  
  quotation_customer_ar_rows <- reactive({
    lookup_id <- quotation_input_text(input$quotation_customer_id_lookup, "")
    typed_id <- quotation_input_text(input$quotation_customer_code, "")
    customer_id <- str_squish(if (lookup_id != "") lookup_id else typed_id)
    
    typed_customer <- quotation_input_text(input$quotation_customer_name, "")
    lookup_customer <- quotation_input_text(input$quotation_customer_lookup, "")
    customer <- str_squish(if (typed_customer != "") typed_customer else lookup_customer)
    
    if (customer_id != "" && "Customer_ID" %in% names(ar_data)) {
      matched <- ar_data %>% filter(as.character(Customer_ID) == customer_id)
      if (nrow(matched) > 0) return(matched)
    }
    
    if (customer == "") return(ar_data[0, , drop = FALSE])
    ar_data %>% filter(quotation_normalize_name(Customer) == quotation_normalize_name(customer))
  })
  
  quotation_customer_milestone_data <- reactive({
    ref <- quotation_customer_reference_row()
    ar <- quotation_customer_ar_rows()
    latest_terms <- if (nrow(ar) > 0) {
      terms <- ar %>% filter(!is.na(Terms), Terms != "") %>% count(Terms, sort = TRUE)
      if (nrow(terms) > 0) terms$Terms[[1]] else NA_character_
    } else NA_character_
    
    if (nrow(ref) > 0) {
      tibble(
        Min = quotation_safe_num(ref$Payment_Min_Months[[1]], NA_real_),
        Average = quotation_safe_num(ref$Payment_Average_Months[[1]], NA_real_),
        Max = quotation_safe_num(ref$Payment_Max_Months[[1]], NA_real_),
        Terms = coalesce(ref$Terms[[1]], latest_terms, "Not available"),
        Source = "Registered customer payment-history profile"
      )
    } else {
      tibble(
        Min = NA_real_, Average = NA_real_, Max = NA_real_,
        Terms = coalesce(latest_terms, input$quotation_payment_terms, "Not available"),
        Source = "No historical min/average/max payment milestone is registered for this customer"
      )
    }
  })
  
  output$quotation_customer_milestone <- renderUI({
    data <- quotation_customer_milestone_data()
    fmt_months <- function(x) if (is.na(x) || !is.finite(x)) "Not registered" else paste0(round(x, 2), " months")
    div(
      tags$p(tags$strong("Payment terms: "), coalesce(data$Terms[[1]], "Not available")),
      div(
        class = "customer-milestone-grid",
        div(class = "customer-milestone-card", tags$strong("Fast / minimum payment habit"), tags$div(class = "customer-milestone-value", fmt_months(data$Min[[1]]))),
        div(class = "customer-milestone-card", tags$strong("Expected / average payment habit"), tags$div(class = "customer-milestone-value", fmt_months(data$Average[[1]]))),
        div(class = "customer-milestone-card", tags$strong("Slow / maximum payment habit"), tags$div(class = "customer-milestone-value", fmt_months(data$Max[[1]])))
      ),
      tags$p(class = "metric-definition-note", paste0("Source: ", data$Source[[1]], ". These milestones represent expected time after invoicing before Aurelis receives customer cash."))
    )
  })
  
  output$quotation_customer_ar_profile <- renderUI({
    ar <- quotation_customer_ar_rows()
    if (nrow(ar) == 0) return(div(class = "quote-status-pill warn", tags$strong("Accounts receivable profile"), tags$br(), tags$small("No matching AR records were found for this customer.")))
    open <- ar %>% filter(Balance_Remaining > 0)
    overdue <- open %>% filter(Days_Past_Due > 0)
    outstanding <- sum(pmax(open$Balance_Remaining, 0), na.rm = TRUE)
    overdue_value <- sum(pmax(overdue$Balance_Remaining, 0), na.rm = TRUE)
    overdue_share <- if (outstanding > 0) overdue_value / outstanding else 0
    avg_overdue <- if (nrow(overdue) > 0) mean(overdue$Days_Past_Due, na.rm = TRUE) else 0
    div(
      class = paste("quote-status-pill", if (overdue_share > .25) "warn" else "good"),
      tags$strong("Current AR context"), tags$br(),
      tags$small(paste0(nrow(open), " open invoice(s) · ", quotation_money(outstanding), " outstanding · ", percent(overdue_share, accuracy = .1), " of open AR currently overdue", if (nrow(overdue) > 0) paste0(" · average overdue age ", round(avg_overdue, 1), " days") else ".")),
      tags$br(), tags$small("This AR view is context only. It does not pretend to measure actual historical payment speed because the current AR source does not contain the exact payment date for closed invoices.")
    )
  })
  
  observeEvent(input$quotation_apply_customer_milestones, {
    data <- quotation_customer_milestone_data()
    if (any(is.na(c(data$Min[[1]], data$Average[[1]], data$Max[[1]])))) {
      showNotification("No complete customer payment milestone is registered. Enter the minimum, average and maximum customer payment months manually in the Finance-Charge Scenario Engine.", type = "warning", duration = 8)
      return()
    }
    updateNumericInput(session, "quotation_client_pay_min", value = data$Min[[1]])
    updateNumericInput(session, "quotation_client_pay_avg", value = data$Average[[1]])
    updateNumericInput(session, "quotation_client_pay_max", value = data$Max[[1]])
    showNotification("Customer payment milestones were applied to the finance scenarios.", type = "message")
  })
  
  observeEvent(input$quotation_history_customer, {
    selected <- str_squish(coalesce(input$quotation_history_customer, ""))
    if (selected == "") {
      updateSelectizeInput(session, "quotation_source_po", choices = c("Select a customer first" = ""), selected = "")
      return()
    }
    po_summary <- relationship_order_history %>%
      filter(Client == selected, !is.na(PO_Number), PO_Number != "") %>%
      group_by(PO_Number) %>%
      summarise(Order_Date = min(Order_Date, na.rm = TRUE), Value = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Order_Date), desc(PO_Number))
    po_choices <- if (nrow(po_summary) > 0) {
      labels <- paste0(po_summary$PO_Number, " · ", format(po_summary$Order_Date, "%Y-%m-%d"), " · ", dollar(po_summary$Value, accuracy = 1))
      setNames(po_summary$PO_Number, labels)
    } else character(0)
    updateSelectizeInput(session, "quotation_source_po", choices = c("Select historical PO" = "", po_choices), selected = "", server = TRUE)
    
    if (!identical(isolate(input$quotation_customer_lookup), selected)) updateSelectizeInput(session, "quotation_customer_lookup", selected = selected)
    if (!identical(isolate(input$quotation_customer_name), selected)) updateTextInput(session, "quotation_customer_name", value = selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$quotation_customer_lookup, {
    selected <- str_squish(coalesce(input$quotation_customer_lookup, ""))
    if (selected != "" && !identical(isolate(input$quotation_history_customer), selected)) {
      updateSelectizeInput(session, "quotation_history_customer", selected = selected)
    }
  }, ignoreInit = TRUE, priority = -5)
  
  quotation_history_selected_rows <- reactive({
    po <- str_squish(coalesce(input$quotation_source_po, ""))
    if (po == "") return(inventory_data[0, , drop = FALSE])
    inventory_data %>% filter(PO_Number == po, Line_Type != "Information")
  })
  
  output$quotation_history_order_summary <- renderUI({
    rows <- quotation_history_selected_rows()
    if (nrow(rows) == 0) return(div(class = "quote-status-pill warn", tags$small("Choose a historical PO to preview its lines before cloning.")))
    item_value <- sum(rows$Amount, na.rm = TRUE)
    customer <- first(na.omit(rows$Client), default = "")
    buyer <- first(na.omit(rows$Buyer), default = "")
    supplier_count <- n_distinct(rows$Supplier[!is.na(rows$Supplier) & rows$Supplier != ""])
    div(class = "quote-status-strip",
        div(class = "quote-status-pill good", tags$strong(input$quotation_source_po), tags$br(), tags$small(paste0(customer, " · buyer: ", buyer))),
        div(class = "quote-status-pill", tags$strong(quotation_money(item_value)), tags$br(), tags$small(paste0(nrow(rows), " line(s) · ", supplier_count, " supplier(s)"))))
  })
  
  output$quotation_history_order_table <- renderDT({
    rows <- quotation_history_selected_rows() %>%
      transmute(Part = Part_Number, Description = coalesce(Order_Description, Memo), Quantity, Supplier, Unit_Cost = Unit_Price, Line_Amount = Amount, Delivery_Date)
    datatable(rows, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 8)) %>%
      formatCurrency(c("Unit_Cost", "Line_Amount"), currency = quotation_currency_symbol(input$quotation_currency), digits = 2)
  })
  
  observe({
    data_version()
    ref <- quotation_customer_reference %>%
      filter(!is.na(Customer_Code), Customer_Code != "", !is.na(Company), Company != "") %>%
      distinct(Customer_Code, .keep_all = TRUE) %>%
      arrange(Customer_Code)
    
    choices <- setNames(
      as.character(ref$Customer_Code),
      paste0(ref$Customer_Code, " · ", ref$Company)
    )
    
    selected <- isolate(input$quotation_customer_id_lookup)
    if (is.null(selected) || !as.character(selected) %in% as.character(ref$Customer_Code)) selected <- ""
    
    updateSelectizeInput(
      session, "quotation_customer_id_lookup",
      choices = c("Select by customer ID" = "", choices),
      selected = selected,
      server = TRUE
    )
  })
  
  output$quotation_customer_identity_status <- renderUI({
    ref <- quotation_customer_reference_row()
    if (nrow(ref) == 0) {
      return(div(
        class = "quote-status-pill",
        tags$strong("Customer identity"),
        tags$br(),
        tags$small("Select a customer by billing name or customer ID. Both lookups drive the same payment-history profile.")
      ))
    }
    
    code <- as.character(ref$Customer_Code[[1]])
    company <- coalesce(ref$Company[[1]], "")
    order_count <- inventory_data %>%
      filter(as.character(Client_ID) == code | quotation_normalize_name(Client) == quotation_normalize_name(company)) %>%
      summarise(n = n_distinct(PO_Number)) %>% pull(n)
    invoice_count <- quotation_customer_ar_rows() %>% summarise(n = n()) %>% pull(n)
    
    div(
      class = "quote-status-pill good",
      tags$strong(paste0(code, " · ", company)),
      tags$br(),
      tags$small(paste0(
        coalesce(ref$Terms[[1]], "Terms not registered"),
        " · ", format(order_count, big.mark = ","), " historical PO(s)",
        " · ", format(invoice_count, big.mark = ","), " invoice record(s)"
      ))
    )
  })
  
  observeEvent(input$quotation_customer_id_lookup, {
    selected_id <- str_squish(quotation_input_text(input$quotation_customer_id_lookup, ""))
    if (selected_id == "") return()
    
    ref <- quotation_customer_reference %>%
      filter(as.character(Customer_Code) == selected_id) %>%
      slice(1)
    if (nrow(ref) == 0) return()
    
    company <- coalesce(ref$Company[[1]], "")
    updateTextInput(session, "quotation_customer_code", value = selected_id)
    updateTextInput(session, "quotation_customer_name", value = company)
    updateSelectizeInput(session, "quotation_customer_lookup", selected = company)
    updateSelectizeInput(session, "quotation_history_customer", selected = company)
    
    updateTextInput(session, "quotation_payment_terms", value = coalesce(ref$Terms[[1]], ""))
    updateNumericInput(session, "quotation_client_pay_min", value = quotation_safe_num(ref$Payment_Min_Months[[1]], 1))
    updateNumericInput(session, "quotation_client_pay_avg", value = quotation_safe_num(ref$Payment_Average_Months[[1]], 1.5))
    updateNumericInput(session, "quotation_client_pay_max", value = quotation_safe_num(ref$Payment_Max_Months[[1]], 3))
    
    directory_row <- relationship_directory %>%
      filter(quotation_normalize_name(Entity) == quotation_normalize_name(company)) %>%
      arrange(desc(!is.na(Email)), desc(!is.na(Address)), desc(!is.na(Contact_Name))) %>%
      slice(1)
    
    if (nrow(directory_row) > 0) {
      updateTextInput(session, "quotation_customer_contact", value = coalesce(directory_row$Contact_Name[[1]], ""))
      updateTextInput(session, "quotation_customer_email", value = coalesce(directory_row$Email[[1]], ""))
      updateTextAreaInput(session, "quotation_customer_address", value = coalesce(directory_row$Address[[1]], ""))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$quotation_customer_lookup, {
    selected <- str_squish(if (is.null(input$quotation_customer_lookup)) "" else input$quotation_customer_lookup)
    if (selected == "") return()
    updateTextInput(session, "quotation_customer_name", value = selected)
    
    reference_row <- quotation_customer_reference %>%
      mutate(Key = quotation_normalize_name(Company)) %>%
      filter(Key == quotation_normalize_name(selected)) %>%
      slice(1)
    
    latest_ar <- ar_data %>%
      filter(quotation_normalize_name(Customer) == quotation_normalize_name(selected)) %>%
      arrange(desc(Txn_Date)) %>%
      slice(1)
    
    directory_row <- relationship_directory %>%
      filter(quotation_normalize_name(Entity) == quotation_normalize_name(selected)) %>%
      arrange(desc(!is.na(Email)), desc(!is.na(Address)), desc(!is.na(Contact_Name))) %>%
      slice(1)
    
    if (nrow(reference_row) > 0) {
      customer_code <- as.character(reference_row$Customer_Code[[1]])
      updateTextInput(
        session, "quotation_customer_code",
        value = if (is.na(customer_code)) "" else customer_code
      )
      if (!is.na(customer_code) && customer_code != "") {
        updateSelectizeInput(session, "quotation_customer_id_lookup", selected = customer_code)
      }
      updateTextInput(session, "quotation_payment_terms", value = coalesce(reference_row$Terms[[1]], ""))
      updateNumericInput(session, "quotation_client_pay_min", value = quotation_safe_num(reference_row$Payment_Min_Months[[1]], 1))
      updateNumericInput(session, "quotation_client_pay_avg", value = quotation_safe_num(reference_row$Payment_Average_Months[[1]], 1.5))
      updateNumericInput(session, "quotation_client_pay_max", value = quotation_safe_num(reference_row$Payment_Max_Months[[1]], 3))
    } else if (nrow(latest_ar) > 0 && !is.na(latest_ar$Terms[[1]])) {
      updateTextInput(session, "quotation_payment_terms", value = latest_ar$Terms[[1]])
    }
    
    if (nrow(directory_row) > 0) {
      updateTextInput(session, "quotation_customer_contact", value = coalesce(directory_row$Contact_Name[[1]], ""))
      updateTextInput(session, "quotation_customer_email", value = coalesce(directory_row$Email[[1]], ""))
      updateTextAreaInput(session, "quotation_customer_address", value = coalesce(directory_row$Address[[1]], ""))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$quotation_payment_preset, {
    preset <- input$quotation_payment_preset
    if (is.null(preset) || preset == "custom") return()
    schedule <- switch(
      preset,
      template20 = c(20, 0, 0, 0, 0, 0),
      `20_80_60` = c(20, 0, 80, 0, 0, 0),
      advance100 = c(100, 0, 0, 0, 0, 0),
      net30 = c(0, 100, 0, 0, 0, 0),
      net60 = c(0, 0, 100, 0, 0, 0),
      net90 = c(0, 0, 0, 100, 0, 0),
      c(20, 0, 0, 0, 0, 0)
    )
    for (scenario in c("min", "avg", "max")) {
      for (month_index in 0:5) {
        updateNumericInput(
          session,
          paste0("quotation_pay_", month_index, "_", scenario),
          value = schedule[[month_index + 1]]
        )
      }
    }
  }, ignoreInit = TRUE)
  
  quotation_item_cost <- reactive({
    if (isTRUE(input$quotation_use_cost_override)) {
      return(max(quotation_safe_num(input$quotation_cost_override, 0), 0))
    }
    sum(quotation_line_calculated()$Line_Cost, na.rm = TRUE)
  })
  
  quotation_cost_components <- reactive({
    item_cost <- quotation_item_cost()
    duty <- item_cost * max(quotation_safe_num(input$quotation_duty_pct, 0), 0) / 100
    handling <- item_cost * max(quotation_safe_num(input$quotation_handling_pct, 0), 0) / 100
    fixed <- sum(c(
      quotation_safe_num(input$quotation_freight, 0),
      quotation_safe_num(input$quotation_insurance, 0),
      quotation_safe_num(input$quotation_qaqc, 0),
      quotation_safe_num(input$quotation_bank_fees, 0),
      quotation_safe_num(input$quotation_packing, 0),
      quotation_safe_num(input$quotation_other_costs, 0)
    ))
    tibble(
      Item_Cost = item_cost,
      Duty = duty,
      Handling = handling,
      Fixed_Additional_Costs = fixed,
      Pre_Finance_Landed_Cost = item_cost + duty + handling + fixed
    )
  })
  
  output$quotation_line_cost_status <- renderUI({
    cost <- quotation_item_cost()
    source <- if (isTRUE(input$quotation_use_cost_override)) "Manual override" else paste(nrow(quotation_line_calculated()), "editable line(s)")
    div(class = "quote-status-pill good", tags$strong(quotation_money(cost)), tags$br(), tags$small(source))
  })
  
  output$quotation_cost_formula_note <- renderUI({
    cst <- quotation_cost_components()
    HTML(paste0(
      "<strong>Pre-finance landed cost = item cost + freight + insurance + duty + handling + QA/QC + bank fees + packing/warehouse + other direct costs.</strong><br>",
      "Current pre-finance landed cost: ", quotation_money(cst$Pre_Finance_Landed_Cost[[1]])
    ))
  })
  
  quotation_schedule_for <- function(scenario_suffix) {
    vapply(0:5, function(month_index) {
      quotation_safe_num(input[[paste0("quotation_pay_", month_index, "_", scenario_suffix)]], 0)
    }, numeric(1))
  }
  
  
  quotation_selected_timing_months <- reactive({
    scenario <- coalesce(input$quotation_selected_scenario, "Average")
    if (identical(scenario, "Min")) {
      quotation_safe_num(input$quotation_vendor_min, 0) + quotation_safe_num(input$quotation_aurelis_min, 0) + quotation_safe_num(input$quotation_client_pay_min, 0)
    } else if (identical(scenario, "Max")) {
      quotation_safe_num(input$quotation_vendor_max, 0) + quotation_safe_num(input$quotation_aurelis_max, 0) + quotation_safe_num(input$quotation_client_pay_max, 0)
    } else {
      quotation_safe_num(input$quotation_vendor_avg, 0) + quotation_safe_num(input$quotation_aurelis_avg, 0) + quotation_safe_num(input$quotation_client_pay_avg, 0)
    }
  })
  
  quotation_recommended_contingency <- reactive({
    profile <- coalesce(input$quotation_contingency_profile, "standard")
    base <- switch(profile, low = 1.5, standard = 3, elevated = 5, high = 7.5, manual = quotation_safe_num(input$quotation_contingency_pct, 3), 3)
    timing <- quotation_selected_timing_months()
    timing_adjustment <- if (timing >= 12) 2 else if (timing >= 9) 1 else if (timing >= 6) .5 else 0
    min(12.5, max(0, base + timing_adjustment))
  })
  
  output$quotation_contingency_recommendation <- renderUI({
    recommended <- quotation_recommended_contingency()
    profile <- coalesce(input$quotation_contingency_profile, "standard")
    if (identical(profile, "manual")) {
      return(div(class = "quote-status-pill", tags$strong("Manual contingency"), tags$br(), tags$small("The dashboard will use the percentage you enter. No automatic recommendation is applied.")))
    }
    div(
      class = "quote-status-pill good",
      tags$strong(paste0("Recommended planning reserve: ", round(recommended, 1), "%")), tags$br(),
      tags$small(paste0("Based on the selected ", profile, " risk profile and a ", round(quotation_selected_timing_months(), 1), "-month selected cash cycle. This is a management planning heuristic, not an accounting rule; replace it when a known risk amount is available."))
    )
  })
  
  observeEvent(input$quotation_apply_contingency, {
    updateNumericInput(session, "quotation_contingency_pct", value = quotation_recommended_contingency())
    showNotification("Recommended contingency was applied to the pricing policy.", type = "message")
  })
  
  output$quotation_discount_explanation <- renderUI({
    mode <- coalesce(input$quotation_discount_mode, "protected")
    pct <- quotation_safe_num(input$quotation_discount_pct, 0)
    text <- if (mode == "real") {
      paste0("Real discount: ", round(pct, 2), "% reduces the actual selling subtotal and therefore reduces effective margin.")
    } else {
      paste0("Presentation discount: ", round(pct, 2), "% is shown to the customer while the target selling subtotal remains protected by increasing the list price first.")
    }
    tags$small(class = "text-muted", text)
  })
  
  quotation_results <- reactive({
    cst <- quotation_cost_components()
    item_cost <- cst$Item_Cost[[1]]
    pre_finance <- cst$Pre_Finance_Landed_Cost[[1]]
    finance_base <- if (identical(input$quotation_finance_base, "landed")) pre_finance else item_cost
    interest_method <- if (identical(input$quotation_interest_method, "compound")) "compound" else "simple"
    contingency_rate <- max(quotation_safe_num(input$quotation_contingency_pct, 0), 0) / 100
    pricing_rate <- max(min(quotation_safe_num(input$quotation_pricing_rate, 0), 99.99), 0) / 100
    discount_rate <- max(min(quotation_safe_num(input$quotation_discount_pct, 0), 99.99), 0) / 100
    discount_mode <- if (identical(input$quotation_discount_mode, "real")) "real" else "protected"
    tax_rate <- max(quotation_safe_num(input$quotation_tax_pct, 0), 0) / 100
    rounding <- max(quotation_safe_num(input$quotation_rounding, 0), 0)
    round_up <- function(value) if (rounding > 0) ceiling(value / rounding) * rounding else value
    
    scenario_map <- list(
      Min = list(suffix = "min", vendor = input$quotation_vendor_min, aurelis = input$quotation_aurelis_min, client = input$quotation_client_pay_min, rate = input$quotation_rate_min),
      Average = list(suffix = "avg", vendor = input$quotation_vendor_avg, aurelis = input$quotation_aurelis_avg, client = input$quotation_client_pay_avg, rate = input$quotation_rate_avg),
      Max = list(suffix = "max", vendor = input$quotation_vendor_max, aurelis = input$quotation_aurelis_max, client = input$quotation_client_pay_max, rate = input$quotation_rate_max)
    )
    
    bind_rows(lapply(names(scenario_map), function(scenario_name) {
      scenario <- scenario_map[[scenario_name]]
      vendor_months <- max(quotation_safe_num(scenario$vendor, 0), 0)
      aurelis_months <- max(quotation_safe_num(scenario$aurelis, 0), 0)
      client_months <- max(quotation_safe_num(scenario$client, 0), 0)
      monthly_rate <- max(quotation_safe_num(scenario$rate, 0), 0) / 100
      total_months <- vendor_months + aurelis_months + client_months
      schedule_pct <- pmax(quotation_schedule_for(scenario$suffix), 0)
      schedule_fraction <- schedule_pct / 100
      months_financed <- pmax(total_months - 0:5, 0)
      financed_amounts <- finance_base * schedule_fraction
      
      finance_charge <- if (interest_method == "compound") {
        sum(financed_amounts * ((1 + monthly_rate)^months_financed - 1), na.rm = TRUE)
      } else {
        sum(financed_amounts * monthly_rate * months_financed, na.rm = TRUE)
      }
      
      landed_cost <- pre_finance + finance_charge
      contingency <- landed_cost * contingency_rate
      protected_cost <- landed_cost + contingency
      target_price <- if (identical(input$quotation_pricing_method, "markup")) {
        protected_cost * (1 + pricing_rate)
      } else {
        protected_cost / max(1 - pricing_rate, .0001)
      }
      
      if (discount_mode == "real") {
        # A real commercial discount reduces the amount Aurelis will actually
        # charge. Rounding is applied after the discount, so effective margin
        # transparently shows the commercial concession.
        list_subtotal <- round_up(target_price)
        requested_discount <- list_subtotal * discount_rate
        quote_subtotal <- round_up(max(list_subtotal - requested_discount, 0))
        displayed_discount <- max(list_subtotal - quote_subtotal, 0)
      } else {
        # Presentation discount: preserve the target price by grossing up the
        # displayed list price before showing the discount.
        quote_subtotal <- round_up(target_price)
        list_subtotal <- if (discount_rate > 0) quote_subtotal / max(1 - discount_rate, .0001) else quote_subtotal
        displayed_discount <- max(list_subtotal - quote_subtotal, 0)
      }
      
      actual_discount_pct <- safe_divide(displayed_discount, list_subtotal, 0)
      tax <- quote_subtotal * tax_rate
      grand_total <- quote_subtotal + tax
      profit_after_reserve <- quote_subtotal - protected_cost
      effective_margin <- safe_divide(profit_after_reserve, quote_subtotal, 0)
      target_margin_gap <- if (identical(input$quotation_pricing_method, "margin")) effective_margin - pricing_rate else NA_real_
      
      tibble(
        Scenario = scenario_name,
        Item_Cost = item_cost,
        Pre_Finance_Landed_Cost = pre_finance,
        Finance_Base = finance_base,
        Financed_Share_Pct = sum(schedule_pct),
        Vendor_Lead_Months = vendor_months,
        Aurelis_Lead_Months = aurelis_months,
        Client_Payment_Months = client_months,
        Total_Months = total_months,
        Monthly_Rate_Pct = monthly_rate * 100,
        Finance_Charge = finance_charge,
        Finance_Charge_Pct = safe_divide(finance_charge, item_cost, 0),
        Landed_Cost = landed_cost,
        Contingency = contingency,
        Protected_Cost = protected_cost,
        Target_Price_Before_Discount = target_price,
        Discount_Mode = if (discount_mode == "real") "Real commercial discount" else "Presentation discount",
        List_Subtotal = list_subtotal,
        Displayed_Discount = displayed_discount,
        Actual_Discount_Pct = actual_discount_pct,
        Quote_Subtotal = quote_subtotal,
        Tax = tax,
        Grand_Total = grand_total,
        Profit_After_Reserve = profit_after_reserve,
        Effective_Margin = effective_margin,
        Target_Margin_Gap = target_margin_gap
      )
    }))
  })
  
  quotation_selected_result <- reactive({
    selected <- if (is.null(input$quotation_selected_scenario)) "Average" else input$quotation_selected_scenario
    row <- quotation_results() %>% filter(Scenario == selected)
    if (nrow(row) == 0) row <- quotation_results() %>% filter(Scenario == "Average")
    row %>% slice(1)
  })
  
  output$quotation_schedule_status <- renderUI({
    results <- quotation_results()
    div(
      class = "quote-status-strip",
      lapply(seq_len(nrow(results)), function(i) {
        share <- results$Financed_Share_Pct[[i]]
        cls <- if (share > 100.0001) "danger" else if (share < 99.9999) "warn" else "good"
        explanation <- if (share > 100.0001) {
          "Schedule exceeds 100%; review duplicate or overlapping supplier payments."
        } else if (share < 99.9999) {
          paste0(round(share, 1), "% is financed; the remaining ", round(100 - share, 1), "% is assumed to be paid without bank financing or after customer cash is received.")
        } else {
          "The full selected finance base is included in the financing model."
        }
        div(
          class = paste("quote-status-pill", cls),
          tags$strong(paste0(results$Scenario[[i]], " schedule: ", round(share, 1), "%")),
          tags$br(), tags$small(explanation)
        )
      })
    )
  })
  
  output$quotation_kpi_item_cost <- renderText(quotation_money(quotation_item_cost()))
  output$quotation_kpi_finance <- renderText(quotation_money(quotation_selected_result()$Finance_Charge[[1]]))
  output$quotation_kpi_subtotal <- renderText(quotation_money(quotation_selected_result()$Quote_Subtotal[[1]]))
  output$quotation_kpi_total <- renderText(quotation_money(quotation_selected_result()$Grand_Total[[1]]))
  
  output$quotation_scenario_table <- renderDT({
    df <- quotation_results() %>%
      transmute(
        Scenario,
        `Total Months` = Total_Months,
        `Monthly Rate %` = Monthly_Rate_Pct,
        `Financed Share %` = Financed_Share_Pct,
        `Finance Charge` = Finance_Charge,
        `Finance Charge % of Items` = Finance_Charge_Pct,
        `Landed Cost` = Landed_Cost,
        `Contingency` = Contingency,
        `Protected Cost` = Protected_Cost,
        `Discount Mode` = Discount_Mode,
        `Discount` = Displayed_Discount,
        `Quote Subtotal` = Quote_Subtotal,
        Tax,
        `Grand Total` = Grand_Total,
        `Effective Margin` = Effective_Margin
      )
    currency <- quotation_currency_symbol(input$quotation_currency)
    datatable(
      df, rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE, paging = FALSE,
                     columnDefs = list(list(className = "dt-nowrap", targets = "_all")))
    ) %>%
      formatCurrency(c("Finance Charge", "Landed Cost", "Contingency", "Protected Cost", "Discount", "Quote Subtotal", "Tax", "Grand Total"), currency = currency, digits = 2) %>%
      formatPercentage(c("Finance Charge % of Items", "Effective Margin"), digits = 2) %>%
      formatRound(c("Total Months", "Monthly Rate %", "Financed Share %"), digits = 2)
  })
  
  output$quotation_final_price_summary <- renderUI({
    row <- quotation_selected_result()
    discount_label <- if (identical(input$quotation_discount_mode, "real")) "Real commercial discount" else "Presentation discount"
    div(
      class = "quote-status-strip",
      div(class = "quote-status-pill good", tags$strong(paste0(row$Scenario[[1]], " scenario selected")), tags$br(), tags$small(paste0(round(row$Total_Months[[1]], 1), " total cash-cycle months · ", round(row$Monthly_Rate_Pct[[1]], 2), "% monthly bank rate"))),
      div(class = "quote-status-pill", tags$strong(paste0("Protected cost: ", quotation_money(row$Protected_Cost[[1]]))), tags$br(), tags$small(paste0("Includes landed cost, finance charge and ", round(quotation_safe_num(input$quotation_contingency_pct, 0), 1), "% contingency."))),
      div(class = paste("quote-status-pill", if (identical(input$quotation_discount_mode, "real") && row$Effective_Margin[[1]] < quotation_safe_num(input$quotation_pricing_rate, 0)/100) "warn" else "good"), tags$strong(paste0("Final subtotal: ", quotation_money(row$Quote_Subtotal[[1]]))), tags$br(), tags$small(paste0(discount_label, " · effective margin ", percent(row$Effective_Margin[[1]], accuracy = .1)))),
      div(class = "quote-status-pill good", tags$strong(paste0("Customer grand total: ", quotation_money(row$Grand_Total[[1]]))), tags$br(), tags$small("This is the final amount displayed in Step 8 after tax / VAT."))
    )
  })
  
  output$quotation_cost_bridge <- renderPlotly({
    # The expected / Average scenario is deliberately shown as a waterfall.
    # This makes each incremental cost and commercial component visible in the
    # exact order in which it builds the expected customer price.
    row <- quotation_results() %>%
      filter(Scenario == "Average") %>%
      slice(1)
    
    validate(need(nrow(row) > 0, "Average quotation scenario is not available."))
    
    item_cost <- row$Item_Cost[[1]]
    landed_other <- row$Pre_Finance_Landed_Cost[[1]] - item_cost
    finance <- row$Finance_Charge[[1]]
    contingency <- row$Contingency[[1]]
    profit_reserve <- row$Profit_After_Reserve[[1]]
    tax <- row$Tax[[1]]
    
    bridge <- tibble(
      Component = c(
        "Item cost",
        "Other landed costs",
        "Finance charge",
        "Contingency",
        "Profit / commercial reserve",
        "Quote subtotal",
        "Tax / VAT",
        "Grand total"
      ),
      Measure = c(
        "absolute",
        "relative",
        "relative",
        "relative",
        "relative",
        "total",
        "relative",
        "total"
      ),
      Value = c(
        item_cost,
        landed_other,
        finance,
        contingency,
        profit_reserve,
        0,
        tax,
        0
      ),
      Display_Value = c(
        item_cost,
        landed_other,
        finance,
        contingency,
        profit_reserve,
        row$Quote_Subtotal[[1]],
        tax,
        row$Grand_Total[[1]]
      )
    ) %>%
      mutate(
        Label = vapply(Display_Value, quotation_money, character(1))
      )
    
    chart <- plot_ly(
      bridge,
      source = "quotation_average_waterfall",
      type = "waterfall",
      x = ~Component,
      y = ~Value,
      measure = ~Measure,
      text = ~Label,
      textposition = "outside",
      customdata = ~Display_Value,
      connector = list(
        line = list(
          color = "rgba(145, 163, 181, 0.38)",
          width = 1
        )
      ),
      
      # Positive / added cost components
      increasing = list(
        marker = list(
          color = "#3F8F95",
          line = list(
            color = "#65AEB1",
            width = 1
          )
        )
      ),
      
      # Negative components, if a scenario contains one
      decreasing = list(
        marker = list(
          color = "#A85F68",
          line = list(
            color = "#C67A82",
            width = 1
          )
        )
      ),
      
      # Quote subtotal / grand total
      totals = list(
        marker = list(
          color = "#7068A6",
          line = list(
            color = "#9189C2",
            width = 1
          )
        )
      ),
      hovertemplate = "<b>%{x}</b><br>%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = "Average Scenario Waterfall — Expected Customer Price Build",
          x = 0.01,
          xanchor = "left"
        ),
        xaxis = list(
          title = "",
          tickangle = -22,
          automargin = TRUE
        ),
        yaxis = list(
          title = paste0("Cumulative value (", input$quotation_currency, ")"),
          tickformat = ",.0f",
          automargin = TRUE
        ),
        margin = list(l = 85, r = 35, t = 70, b = 115),
        showlegend = FALSE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    chart
  })
  
  output$quotation_risk_flags <- renderUI({
    result <- quotation_selected_result()
    flags <- list()
    if (quotation_item_cost() <= 0) flags <- c(flags, list(c("danger", "No item cost is available. Enter line costs or enable the cost override.")))
    if (str_squish(coalesce(input$quotation_customer_name, "")) == "") flags <- c(flags, list(c("warn", "Customer name is blank.")))
    if (result$Financed_Share_Pct[[1]] > 100.0001) flags <- c(flags, list(c("danger", "Supplier disbursement percentages exceed 100%.")))
    if (result$Total_Months[[1]] >= 9) flags <- c(flags, list(c("warn", paste0("Long cash cycle: ", round(result$Total_Months[[1]], 1), " months from PO to expected payment."))))
    if (result$Finance_Charge_Pct[[1]] >= .05) flags <- c(flags, list(c("warn", paste0("Finance charge represents ", percent(result$Finance_Charge_Pct[[1]], accuracy = .1), " of item cost."))))
    if (identical(input$quotation_discount_mode, "real") && quotation_safe_num(input$quotation_discount_pct, 0) > 0) {
      flags <- c(flags, list(c("warn", paste0("A real commercial discount is active. The customer subtotal is genuinely reduced by the discount, so effective margin is now ", percent(result$Effective_Margin[[1]], accuracy = .1), "."))))
    }
    if (identical(input$quotation_pricing_method, "margin") && result$Effective_Margin[[1]] + 1e-9 < quotation_safe_num(input$quotation_pricing_rate, 0) / 100) {
      flags <- c(flags, list(c("warn", paste0("Effective margin is below the ", round(quotation_safe_num(input$quotation_pricing_rate, 0), 1), "% target. Review real discount, rounding or cost assumptions."))))
    } else if (result$Effective_Margin[[1]] < .10) {
      flags <- c(flags, list(c("warn", "Effective margin after contingency is below 10%.")))
    }
    if (length(flags) == 0) flags <- list(c("good", "The selected scenario passes the current pricing, financing and schedule checks."))
    div(class = "quote-status-strip", lapply(flags, function(flag) div(class = paste("quote-status-pill", flag[[1]]), icon("shield-alt"), tags$span(paste0(" ", flag[[2]])))))
  })
  
  quotation_final_lines <- reactive({
    lines <- quotation_line_calculated()
    selected <- quotation_selected_result()
    subtotal <- selected$Quote_Subtotal[[1]]
    item_cost <- sum(lines$Line_Cost, na.rm = TRUE)
    if (nrow(lines) == 0) lines <- quotation_new_line_frame()
    shares <- if (item_cost > 0) lines$Line_Cost / item_cost else rep(1 / nrow(lines), nrow(lines))
    final_total <- subtotal * shares
    qty <- ifelse(lines$Quantity > 0, lines$Quantity, 1)
    lines %>%
      mutate(
        Sell_Unit_Price = final_total / qty,
        Sell_Line_Total = final_total
      )
  })
  
  quotation_preview_tag <- reactive({
    lines <- quotation_final_lines()
    selected <- quotation_selected_result()
    validity_date <- as.Date(input$quotation_date) + quotation_safe_num(input$quotation_validity_days, 30)
    discount_pct <- quotation_safe_num(input$quotation_discount_pct, 0)
    
    line_rows <- lapply(seq_len(nrow(lines)), function(i) {
      tags$tr(
        tags$td(i),
        tags$td(tags$strong(coalesce(lines$Part_Number[[i]], "")), tags$br(), coalesce(lines$Description[[i]], "")),
        tags$td(coalesce(lines$Unit[[i]], "EA")),
        tags$td(class = "num", format(lines$Quantity[[i]], big.mark = ",", trim = TRUE)),
        tags$td(class = "num", quotation_money(lines$Sell_Unit_Price[[i]])),
        tags$td(class = "num", quotation_money(lines$Sell_Line_Total[[i]]))
      )
    })
    
    div(
      class = "quote-preview-sheet",
      div(
        class = "quote-preview-header",
        div(
          tags$img(src = aurelis_logo_source, alt = "Aurelis logo"),
          tags$div(tags$strong(coalesce(input$quotation_company_name, "Aurelis Global Supply, Inc."))),
          tags$div(coalesce(input$quotation_company_address, "")),
          tags$div(coalesce(input$quotation_company_contact, ""))
        ),
        div(
          class = "quote-preview-title",
          tags$h1("QUOTATION"),
          tags$div(tags$strong("No.: "), coalesce(input$quotation_number, "")),
          tags$div(tags$strong("Date: "), format(as.Date(input$quotation_date), "%B %d, %Y")),
          tags$div(tags$strong("Valid through: "), format(validity_date, "%B %d, %Y")),
          tags$div(tags$strong("Currency: "), input$quotation_currency)
        )
      ),
      div(
        class = "quote-preview-meta",
        div(
          class = "quote-preview-panel",
          tags$strong("QUOTED TO"), tags$br(),
          coalesce(input$quotation_customer_name, ""), tags$br(),
          if (str_squish(coalesce(input$quotation_customer_contact, "")) != "") paste0("Attn: ", input$quotation_customer_contact) else NULL,
          tags$br(),
          HTML(gsub("\\n", "<br>", htmltools::htmlEscape(coalesce(input$quotation_customer_address, "")))),
          if (str_squish(coalesce(input$quotation_customer_email, "")) != "") tags$div(input$quotation_customer_email) else NULL
        ),
        div(
          class = "quote-preview-panel",
          tags$strong("COMMERCIAL DETAILS"), tags$br(),
          tags$span(tags$strong("RFQ: "), coalesce(input$quotation_rfq_reference, "Not provided")), tags$br(),
          tags$span(tags$strong("Prepared by: "), coalesce(input$quotation_prepared_by, "")), tags$br(),
          tags$span(tags$strong("Delivery: "), coalesce(input$quotation_delivery_location, "")), tags$br(),
          tags$span(tags$strong("Incoterm: "), coalesce(input$quotation_incoterm, "Not specified")), tags$br(),
          tags$span(tags$strong("Payment terms: "), coalesce(input$quotation_payment_terms, ""))
        )
      ),
      tags$table(
        class = "quote-preview-table",
        tags$thead(tags$tr(
          tags$th("#"), tags$th("Item / Description"), tags$th("Unit"),
          tags$th(class = "num", "Qty"), tags$th(class = "num", "Unit Price"), tags$th(class = "num", "Amount")
        )),
        tags$tbody(line_rows)
      ),
      tags$table(
        class = "quote-preview-summary",
        tags$tr(tags$td("List subtotal"), tags$td(quotation_money(selected$List_Subtotal[[1]]))),
        if (discount_pct > 0) tags$tr(tags$td(paste0("Commercial discount (", round(discount_pct, 2), "%)")), tags$td(paste0("-", quotation_money(selected$Displayed_Discount[[1]])))) else NULL,
        tags$tr(tags$td("Quotation subtotal"), tags$td(quotation_money(selected$Quote_Subtotal[[1]]))),
        if (quotation_safe_num(input$quotation_tax_pct, 0) > 0) tags$tr(tags$td(paste0("Tax / VAT (", round(quotation_safe_num(input$quotation_tax_pct, 0), 2), "%)")), tags$td(quotation_money(selected$Tax[[1]]))) else NULL,
        tags$tr(class = "total", tags$td("GRAND TOTAL"), tags$td(quotation_money(selected$Grand_Total[[1]])))
      ),
      div(
        class = "quote-preview-notes",
        tags$strong("Scope and commercial note"), tags$br(),
        coalesce(input$quotation_scope_notes, ""), tags$br(), tags$br(),
        tags$strong("Terms and conditions"), tags$br(),
        coalesce(input$quotation_terms_notes, ""), tags$br(), tags$br(),
        tags$strong("Estimated delivery: "), coalesce(input$quotation_delivery_statement, "To be confirmed")
      )
    )
  })
  
  output$quotation_preview <- renderUI(quotation_preview_tag())
  
  observeEvent(input$quotation_new_quote, {
    current_draft_id("")
    updateCheckboxInput(session, "quotation_share_draft", value = FALSE)
    updateTextInput(session, "quotation_number", value = paste0("Q-", format(Sys.time(), "%Y%m%d-%H%M%S")))
    updateDateInput(session, "quotation_date", value = Sys.Date())
    updateSelectizeInput(session, "quotation_customer_lookup", selected = "")
    updateSelectizeInput(session, "quotation_history_customer", selected = "")
    updateSelectizeInput(session, "quotation_source_po", choices = c("Select a customer first" = ""), selected = "")
    for (id in c(
      "quotation_customer_name", "quotation_customer_code", "quotation_rfq_reference",
      "quotation_customer_contact", "quotation_customer_email", "quotation_customer_address",
      "quotation_delivery_location"
    )) {
      if (id == "quotation_customer_address") updateTextAreaInput(session, id, value = "") else updateTextInput(session, id, value = "")
    }
    quotation_lines(quotation_new_line_frame())
    showNotification("New quotation started. Pricing policy and finance assumptions were retained.", type = "message")
  })
  
  observeEvent(input$quotation_copy_quote, {
    base <- str_squish(coalesce(input$quotation_number, "Q"))
    updateTextInput(session, "quotation_number", value = paste0(base, "-COPY-", format(Sys.time(), "%H%M%S")))
  })
  
  quotation_safe_filename <- function(value) {
    value <- str_replace_all(coalesce(as.character(value), "Quotation"), "[^A-Za-z0-9_-]+", "_")
    str_replace_all(value, "_+", "_")
  }
  
  quotation_logo_data_uri <- function() {
    logo <- report_logo_file()
    if (logo == "" || !file.exists(logo) || !requireNamespace("base64enc", quietly = TRUE)) return("")
    mime <- if (str_detect(str_to_lower(logo), "\\.png$")) "image/png" else "image/jpeg"
    base64enc::dataURI(file = logo, mime = mime)
  }
  
  quotation_document_html <- reactive({
    lines <- quotation_final_lines()
    selected <- quotation_selected_result()
    esc <- function(value) htmltools::htmlEscape(coalesce(as.character(value), ""))
    br <- function(value) gsub("\\n", "<br>", esc(value))
    validity_date <- as.Date(input$quotation_date) + quotation_safe_num(input$quotation_validity_days, 30)
    logo_uri <- quotation_logo_data_uri()
    logo_html <- if (logo_uri == "") "" else paste0("<img src='", logo_uri, "' alt='Aurelis logo'>")
    
    line_html <- paste(vapply(seq_len(nrow(lines)), function(i) {
      paste0(
        "<tr><td>", i, "</td><td><strong>", esc(lines$Part_Number[[i]]), "</strong><br>", esc(lines$Description[[i]]),
        "</td><td>", esc(lines$Unit[[i]]), "</td><td class='num'>", format(lines$Quantity[[i]], big.mark = ",", trim = TRUE),
        "</td><td class='num'>", esc(quotation_money(lines$Sell_Unit_Price[[i]])),
        "</td><td class='num'>", esc(quotation_money(lines$Sell_Line_Total[[i]])), "</td></tr>"
      )
    }, character(1)), collapse = "")
    
    discount_row <- if (quotation_safe_num(input$quotation_discount_pct, 0) > 0) {
      paste0(
        "<tr><td>Commercial discount (", round(quotation_safe_num(input$quotation_discount_pct, 0), 2),
        "%)</td><td>-", esc(quotation_money(selected$Displayed_Discount[[1]])), "</td></tr>"
      )
    } else ""
    tax_row <- if (quotation_safe_num(input$quotation_tax_pct, 0) > 0) {
      paste0(
        "<tr><td>Tax / VAT (", round(quotation_safe_num(input$quotation_tax_pct, 0), 2),
        "%)</td><td>", esc(quotation_money(selected$Tax[[1]])), "</td></tr>"
      )
    } else ""
    
    paste0(
      "<!doctype html><html><head><meta charset='utf-8'><style>",
      "@page{size:A4;margin:15mm 14mm 16mm 14mm;}body{font-family:Arial,sans-serif;color:#1f2937;font-size:10.5pt;margin:0;}",
      ".header{display:flex;justify-content:space-between;gap:20px;border-bottom:3px solid #17324D;padding-bottom:12px;margin-bottom:16px;}",
      ".header img{max-width:210px;max-height:68px;object-fit:contain}.title{text-align:right}.title h1{color:#17324D;margin:0;font-size:25pt;letter-spacing:1px}",
      ".meta{display:grid;grid-template-columns:1fr 1fr;gap:12px;margin:15px 0}.panel{background:#F7FAFC;border:1px solid #D7E1EA;border-radius:6px;padding:10px;}",
      "table.items{width:100%;border-collapse:collapse;margin:15px 0;font-size:9.5pt}table.items th{background:#17324D;color:white;padding:8px;text-align:left}",
      "table.items td{border-bottom:1px solid #D7E1EA;padding:8px;vertical-align:top}.num{text-align:right;white-space:nowrap}",
      "table.summary{width:48%;margin-left:auto;border-collapse:collapse}table.summary td{padding:6px 8px;border-bottom:1px solid #D7E1EA}table.summary td:last-child{text-align:right;font-weight:bold}",
      "table.summary tr.total td{background:#17324D;color:white;font-size:12pt;border:0}.notes{margin-top:20px;border-top:1px solid #D7E1EA;padding-top:12px;white-space:normal}",
      ".footer{position:fixed;bottom:0;left:0;right:0;border-top:1px solid #D7E1EA;padding-top:5px;color:#64748B;font-size:8pt;text-align:center}",
      "</style></head><body>",
      "<div class='header'><div>", logo_html, "<div><strong>", esc(input$quotation_company_name), "</strong></div><div>", esc(input$quotation_company_address), "</div><div>", esc(input$quotation_company_contact), "</div></div>",
      "<div class='title'><h1>QUOTATION</h1><div><strong>No.:</strong> ", esc(input$quotation_number), "</div><div><strong>Date:</strong> ", format(as.Date(input$quotation_date), "%B %d, %Y"),
      "</div><div><strong>Valid through:</strong> ", format(validity_date, "%B %d, %Y"), "</div><div><strong>Currency:</strong> ", esc(input$quotation_currency), "</div></div></div>",
      "<div class='meta'><div class='panel'><strong>QUOTED TO</strong><br>", esc(input$quotation_customer_name), "<br>Attn: ", esc(input$quotation_customer_contact), "<br>", br(input$quotation_customer_address), "<br>", esc(input$quotation_customer_email),
      "</div><div class='panel'><strong>COMMERCIAL DETAILS</strong><br><strong>RFQ:</strong> ", esc(input$quotation_rfq_reference), "<br><strong>Prepared by:</strong> ", esc(input$quotation_prepared_by),
      "<br><strong>Delivery:</strong> ", esc(input$quotation_delivery_location), "<br><strong>Incoterm:</strong> ", esc(input$quotation_incoterm), "<br><strong>Payment terms:</strong> ", esc(input$quotation_payment_terms), "</div></div>",
      "<table class='items'><thead><tr><th>#</th><th>Item / Description</th><th>Unit</th><th class='num'>Qty</th><th class='num'>Unit Price</th><th class='num'>Amount</th></tr></thead><tbody>", line_html, "</tbody></table>",
      "<table class='summary'><tr><td>List subtotal</td><td>", esc(quotation_money(selected$List_Subtotal[[1]])), "</td></tr>", discount_row,
      "<tr><td>Quotation subtotal</td><td>", esc(quotation_money(selected$Quote_Subtotal[[1]])), "</td></tr>", tax_row,
      "<tr class='total'><td>GRAND TOTAL</td><td>", esc(quotation_money(selected$Grand_Total[[1]])), "</td></tr></table>",
      "<div class='notes'><strong>Scope and commercial note</strong><br>", br(input$quotation_scope_notes), "<br><br><strong>Terms and conditions</strong><br>", br(input$quotation_terms_notes),
      "<br><br><strong>Estimated delivery:</strong> ", esc(input$quotation_delivery_statement), "</div>",
      "<div class='footer'>", esc(input$quotation_company_name), " — Quotation ", esc(input$quotation_number), "</div>",
      "</body></html>"
    )
  })
  
  output$quotation_download_pdf <- downloadHandler(
    filename = function() paste0(quotation_safe_filename(input$quotation_number), "_Quotation.pdf"),
    content = function(file) {
      if (!requireNamespace("pagedown", quietly = TRUE)) {
        stop("Install pagedown first: install.packages('pagedown')", call. = FALSE)
      }
      temporary_html <- tempfile(fileext = ".html")
      on.exit(unlink(temporary_html), add = TRUE)
      writeLines(quotation_document_html(), temporary_html, useBytes = TRUE)
      pagedown::chrome_print(input = temporary_html, output = file, wait = 2)
    }
  )
  
  output$quotation_download_excel <- downloadHandler(
    filename = function() paste0(quotation_safe_filename(input$quotation_number), "_Calculation.xlsx"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
      }
      lines <- quotation_final_lines()
      results <- quotation_results()
      selected <- quotation_selected_result()
      currency_format <- paste0(quotation_currency_symbol(input$quotation_currency), "#,##0.00")
      
      wb <- openxlsx::createWorkbook(creator = "Aurelis Global Supply")
      title_style <- openxlsx::createStyle(fontSize = 18, textDecoration = "bold", fontColour = "#17324D")
      header_style <- openxlsx::createStyle(fgFill = "#17324D", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
      money_style <- openxlsx::createStyle(numFmt = currency_format)
      
      openxlsx::addWorksheet(wb, "Quotation")
      logo <- report_logo_file()
      if (logo != "" && file.exists(logo)) {
        try(openxlsx::insertImage(wb, "Quotation", logo, startRow = 1, startCol = 1, width = 2.2, height = .78), silent = TRUE)
      }
      openxlsx::writeData(wb, "Quotation", "QUOTATION", startRow = 1, startCol = 6)
      openxlsx::addStyle(wb, "Quotation", title_style, rows = 1, cols = 6)
      header_values <- data.frame(
        Field = c("Quotation Number", "Date", "Valid Through", "Customer", "Customer Code", "Attention", "Email", "RFQ Reference", "Prepared By", "Delivery Location", "Incoterm", "Payment Terms", "Currency", "Selected Scenario"),
        Value = c(
          input$quotation_number, as.character(input$quotation_date),
          as.character(as.Date(input$quotation_date) + quotation_safe_num(input$quotation_validity_days, 30)),
          input$quotation_customer_name, input$quotation_customer_code, input$quotation_customer_contact,
          input$quotation_customer_email, input$quotation_rfq_reference, input$quotation_prepared_by,
          input$quotation_delivery_location, input$quotation_incoterm, input$quotation_payment_terms,
          input$quotation_currency, input$quotation_selected_scenario
        ),
        stringsAsFactors = FALSE
      )
      openxlsx::writeDataTable(wb, "Quotation", header_values, startRow = 4, startCol = 1, tableStyle = "TableStyleMedium2")
      
      export_lines <- lines %>%
        transmute(
          Line, `Part / Reference` = Part_Number, Description, Quantity, Unit, Supplier,
          `Vendor Unit Cost` = Vendor_Unit_Cost, `Supplier Discount %` = Supplier_Discount_Pct,
          `Net Unit Cost` = Net_Unit_Cost, `Vendor Line Cost` = Line_Cost,
          `Quoted Unit Price` = Sell_Unit_Price, `Quoted Line Total` = Sell_Line_Total
        )
      line_start <- 20
      openxlsx::writeDataTable(wb, "Quotation", export_lines, startRow = line_start, tableStyle = "TableStyleMedium2")
      money_cols <- match(c("Vendor Unit Cost", "Net Unit Cost", "Vendor Line Cost", "Quoted Unit Price", "Quoted Line Total"), names(export_lines))
      openxlsx::addStyle(wb, "Quotation", money_style, rows = (line_start + 1):(line_start + nrow(export_lines)), cols = money_cols, gridExpand = TRUE)
      
      total_start <- line_start + nrow(export_lines) + 3
      totals <- data.frame(
        Metric = c("List Subtotal", "Displayed Discount", "Quotation Subtotal", "Tax / VAT", "Grand Total"),
        Value = c(selected$List_Subtotal, selected$Displayed_Discount, selected$Quote_Subtotal, selected$Tax, selected$Grand_Total)
      )
      openxlsx::writeDataTable(wb, "Quotation", totals, startRow = total_start, startCol = 8, tableStyle = "TableStyleMedium2")
      openxlsx::addStyle(wb, "Quotation", money_style, rows = (total_start + 1):(total_start + nrow(totals)), cols = 9, gridExpand = TRUE)
      openxlsx::setColWidths(wb, "Quotation", cols = 1:12, widths = "auto")
      openxlsx::freezePane(wb, "Quotation", firstActiveRow = line_start + 1)
      
      openxlsx::addWorksheet(wb, "Scenario Audit")
      openxlsx::writeDataTable(wb, "Scenario Audit", results, tableStyle = "TableStyleMedium2")
      scenario_money_cols <- which(names(results) %in% c(
        "Item_Cost", "Pre_Finance_Landed_Cost", "Finance_Base", "Finance_Charge",
        "Landed_Cost", "Contingency", "Protected_Cost", "List_Subtotal",
        "Displayed_Discount", "Quote_Subtotal", "Tax", "Grand_Total", "Profit_After_Reserve"
      ))
      if (length(scenario_money_cols) > 0) openxlsx::addStyle(wb, "Scenario Audit", money_style, rows = 2:(nrow(results) + 1), cols = scenario_money_cols, gridExpand = TRUE)
      openxlsx::setColWidths(wb, "Scenario Audit", cols = seq_len(ncol(results)), widths = "auto")
      
      schedule_map <- c(Min = "min", Average = "avg", Max = "max")
      schedule <- bind_rows(lapply(names(schedule_map), function(scenario_name) {
        suffix <- unname(schedule_map[[scenario_name]])
        tibble(
          Scenario = scenario_name,
          Milestone = c("Advance", "30 days", "60 days", "90 days", "120 days", "150 days"),
          Month = 0:5,
          Percent_of_Finance_Base = quotation_schedule_for(suffix)
        )
      }))
      openxlsx::addWorksheet(wb, "Payment Schedule")
      openxlsx::writeDataTable(wb, "Payment Schedule", schedule, tableStyle = "TableStyleMedium2")
      openxlsx::setColWidths(wb, "Payment Schedule", cols = 1:4, widths = "auto")
      
      openxlsx::addWorksheet(wb, "Notes")
      notes <- data.frame(
        Section = c("Customer Address", "Scope / Commercial Note", "Terms and Conditions", "Internal Buyer Notes", "Calculation Method"),
        Content = c(
          input$quotation_customer_address, input$quotation_scope_notes, input$quotation_terms_notes,
          input$quotation_internal_notes,
          paste0(
            "Finance charge uses ", input$quotation_interest_method, " monthly interest on each supplier disbursement for the remaining months until expected customer payment. Pricing uses ",
            input$quotation_pricing_method, " at ", input$quotation_pricing_rate, "% after landed cost, finance charge and contingency."
          )
        ),
        stringsAsFactors = FALSE
      )
      openxlsx::writeDataTable(wb, "Notes", notes, tableStyle = "TableStyleMedium2")
      openxlsx::setColWidths(wb, "Notes", cols = c(1, 2), widths = c(28, 110))
      openxlsx::setRowHeights(wb, "Notes", rows = 2:(nrow(notes) + 1), heights = 55)
      openxlsx::addStyle(wb, "Notes", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(notes) + 1), cols = 2, gridExpand = TRUE)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  quotation_report_summary <- reactive({
    selected <- quotation_selected_result()
    tibble(
      Metric = c(
        "Quotation Number", "Customer", "Selected Scenario", "Item Cost",
        "Pre-finance Landed Cost", "Finance Charge", "Finance Charge %",
        "Protected Cost", "Quote Subtotal", "Grand Total", "Effective Margin"
      ),
      Value = c(
        input$quotation_number, input$quotation_customer_name, input$quotation_selected_scenario,
        quotation_money(selected$Item_Cost), quotation_money(selected$Pre_Finance_Landed_Cost),
        quotation_money(selected$Finance_Charge), percent(selected$Finance_Charge_Pct, accuracy = .01),
        quotation_money(selected$Protected_Cost), quotation_money(selected$Quote_Subtotal),
        quotation_money(selected$Grand_Total), percent(selected$Effective_Margin, accuracy = .01)
      )
    )
  })
  
  buyer_lookup_raw <- reactive({
    data_version()
    df <- filtered_inventory() %>% filter(Line_Type != "Information")
    
    search_term <- str_to_lower(str_squish(if (is.null(input$buyer_search_po)) "" else input$buyer_search_po))
    if (search_term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$PO_Number, ""),
        coalesce(df$Part_Number, ""),
        coalesce(df$Order_Description, "")
      ))
      df <- df[str_detect(searchable, fixed(search_term)), , drop = FALSE]
    }
    if (!is.null(input$buyer_search_client) && input$buyer_search_client != "all") {
      df <- df %>% filter(Client == input$buyer_search_client)
    }
    if (!is.null(input$buyer_search_supplier) && input$buyer_search_supplier != "all") {
      df <- df %>% filter(Supplier == input$buyer_search_supplier)
    }
    if (!is.null(input$buyer_search_buyer) && input$buyer_search_buyer != "all") {
      df <- df %>% filter(Buyer == input$buyer_search_buyer)
    }
    
    df %>%
      select(
        Order_Status, Order_Date, Delivery_Date, PO_Number,
        Customer = Client, Buyer, Seller_Supplier = Supplier,
        Part_Number, `Order Description` = Order_Description,
        Quantity, Received_Qty, Backordered_Qty,
        Unit_Price, Line_Amount = Amount, Open_Balance
      ) %>%
      arrange(desc(Order_Date), PO_Number)
  })
  
  output$buyer_po_lookup_table <- renderDT({
    data_version()
    datatable(
      buyer_lookup_raw(),
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(
        c("Unit_Price", "Line_Amount", "Open_Balance"),
        currency = "$", digits = 2
      )
  })
  
  ##############################################################################
  # 4.11 RELATIONSHIP DIRECTORY - Server Logic
  ##############################################################################
  
  new_client_registration_message <- reactiveVal("Ready to register a new public-demo client.")
  
  contact_catalog_filtered <- reactive({
    data_version()
    df <- seller_catalog_data
    
    search_term <- str_to_lower(str_squish(coalesce(input$contact_catalog_search, "")))
    if (search_term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$Product_Code, ""),
        coalesce(df$Product_Service, ""),
        coalesce(df$Category, ""),
        coalesce(df$Supplier, ""),
        coalesce(df$Manufacturer, "")
      ))
      df <- df[str_detect(searchable, fixed(search_term)), , drop = FALSE]
    }
    
    selected_supplier <- coalesce(input$contact_catalog_supplier, "all")
    if (selected_supplier != "all") {
      df <- df %>% filter(Supplier == selected_supplier)
    }
    
    df %>%
      arrange(Product_Service_Type, Category, Product_Service, Preferred_Rank, Supplier)
  })
  
  output$contact_seller_catalog_table <- renderDT({
    df <- contact_catalog_filtered() %>%
      transmute(
        Type = Product_Service_Type,
        Category,
        Code = Product_Code,
        `Product / Service` = Product_Service,
        Seller = Supplier,
        Country,
        `Typical Lead Days` = Typical_Lead_Days,
        `Last Quoted Cost` = Last_Quoted_Unit_Cost,
        Currency
      )
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      selection = "single",
      options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE)
    ) %>%
      formatCurrency("Last Quoted Cost", currency = "$", digits = 2)
  })
  
  observeEvent(input$contact_seller_catalog_table_rows_selected, {
    idx <- input$contact_seller_catalog_table_rows_selected
    if (length(idx) != 1) return()
    
    df <- contact_catalog_filtered()
    if (idx < 1 || idx > nrow(df)) return()
    supplier <- df$Supplier[[idx]]
    
    updateSelectInput(session, "contact_type", selected = "Seller / Supplier")
    updateSelectizeInput(session, "contact_entity", selected = supplier)
    updateSelectizeInput(session, "contact_catalog_supplier", selected = supplier)
  }, ignoreInit = TRUE)
  
  observeEvent(input$register_new_client, {
    company <- str_squish(coalesce(input$new_client_company, ""))
    country <- str_squish(coalesce(input$new_client_country, ""))
    requested_id <- str_squish(coalesce(input$new_client_id, ""))
    
    if (company == "") {
      new_client_registration_message("Company / billing name is required.")
      showNotification("Enter the new client company / billing name.", type = "error")
      return()
    }
    if (country == "") {
      new_client_registration_message("Country is required.")
      showNotification("Select or enter the client country.", type = "error")
      return()
    }
    
    pay_min <- suppressWarnings(as.numeric(input$new_client_pay_min))
    pay_avg <- suppressWarnings(as.numeric(input$new_client_pay_avg))
    pay_max <- suppressWarnings(as.numeric(input$new_client_pay_max))
    if (any(!is.finite(c(pay_min, pay_avg, pay_max))) || pay_min < 0 || pay_avg < pay_min || pay_max < pay_avg) {
      new_client_registration_message("Payment milestones must satisfy: 0 ≤ fast ≤ average ≤ slow.")
      showNotification("Review the payment milestone values.", type = "error")
      return()
    }
    
    existing_ids <- as.character(quotation_customer_reference$Customer_Code)
    existing_companies <- quotation_normalize_name(quotation_customer_reference$Company)
    
    if (quotation_normalize_name(company) %in% existing_companies) {
      new_client_registration_message("That billing name already exists in the customer directory.")
      showNotification("A customer with that billing name already exists.", type = "warning")
      return()
    }
    
    if (requested_id == "") {
      used <- suppressWarnings(as.integer(str_extract(existing_ids[str_detect(existing_ids, "^AGC-U[0-9]+$")], "[0-9]+$")))
      next_id <- if (length(used) == 0 || all(is.na(used))) 1L else max(used, na.rm = TRUE) + 1L
      requested_id <- sprintf("AGC-U%05d", next_id)
    }
    
    if (requested_id %in% existing_ids) {
      new_client_registration_message("That customer ID is already registered.")
      showNotification("Customer ID already exists.", type = "error")
      return()
    }
    
    record <- data.frame(
      Customer_Code = requested_id,
      Company = company,
      Country = country,
      Region = str_squish(coalesce(input$new_client_region, "")),
      City = str_squish(coalesce(input$new_client_city, "")),
      Parent_Company = str_squish(coalesce(input$new_client_parent, "Independent Entity")),
      Terms = str_squish(coalesce(input$new_client_terms, "NET 30 DAYS")),
      Payment_Min_Months = pay_min,
      Payment_Average_Months = pay_avg,
      Payment_Max_Months = pay_max,
      Contact_Name = str_squish(coalesce(input$new_client_contact, "")),
      Email = str_squish(coalesce(input$new_client_email, "")),
      Phone = str_squish(coalesce(input$new_client_phone, "")),
      Address = str_squish(coalesce(input$new_client_address, "")),
      Department = str_squish(coalesce(input$new_client_department, "Procurement")),
      Position = str_squish(coalesce(input$new_client_position, "Procurement Manager")),
      AURELIS_Rep = str_squish(coalesce(input$new_client_rep, "")),
      Registered_At = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    target_file <- file.path(AURELIS_DATA_DIR, "Aurelis_User_Clients.csv")
    
    # Atomic-style local write: build the complete updated file in a temporary
    # location first, then replace the public-demo registration file. This
    # avoids leaving a partially written CSV if Windows, antivirus, sync
    # software or file permissions interrupt the write.
    registration_result <- tryCatch({
      existing_registered <- if (file.exists(target_file)) {
        tryCatch(
          read_demo_csv("Aurelis_User_Clients.csv"),
          error = function(e) data.frame()
        )
      } else {
        data.frame()
      }
      
      updated_registered <- bind_rows(existing_registered, record)
      
      temp_file <- tempfile(
        pattern = "aurelis-client-registration-",
        tmpdir = AURELIS_DATA_DIR,
        fileext = ".csv"
      )
      
      write.table(
        updated_registered,
        temp_file,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        append = FALSE,
        quote = TRUE,
        qmethod = "double",
        fileEncoding = "UTF-8"
      )
      
      copied <- file.copy(temp_file, target_file, overwrite = TRUE)
      unlink(temp_file)
      
      if (!isTRUE(copied)) {
        stop("The public-demo client file could not be updated.", call. = FALSE)
      }
      
      TRUE
    }, error = function(error) error)
    
    if (inherits(registration_result, "error")) {
      new_client_registration_message(
        paste0("Registration was not saved: ", conditionMessage(registration_result))
      )
      showNotification(
        paste0("Client registration failed safely: ", conditionMessage(registration_result)),
        type = "error",
        duration = 9
      )
      return()
    }
    
    # Register the new customer in the public geographic Atlas as well.
    # Without an external geocoder, the location uses the mean coordinates of
    # the selected country and a tiny deterministic offset. City / region remain
    # exactly as entered by the user. This keeps the public build offline and
    # avoids inventing precise street coordinates.
    geography_target <- file.path(AURELIS_DATA_DIR, "Aurelis_Geography.csv")
    geography_result <- tryCatch({
      country_reference <- geography_data %>%
        filter(Country == country)
      
      base_lat <- if (nrow(country_reference) > 0) {
        mean(country_reference$Latitude, na.rm = TRUE)
      } else 0
      base_lon <- if (nrow(country_reference) > 0) {
        mean(country_reference$Longitude, na.rm = TRUE)
      } else 0
      continent_value <- if (nrow(country_reference) > 0) {
        country_reference$Continent[[1]]
      } else "Unknown"
      
      offset_seed <- sum(utf8ToInt(requested_id))
      offset_lat <- ((offset_seed %% 9) - 4) * .08
      offset_lon <- (((offset_seed %/% 9) %% 11) - 5) * .08
      
      geography_record <- data.frame(
        Entity_Type = "Customer",
        Entity_ID = requested_id,
        Entity = company,
        Continent = continent_value,
        Country = country,
        Region = str_squish(coalesce(input$new_client_region, "")),
        City = str_squish(coalesce(input$new_client_city, "")),
        Latitude = base_lat + offset_lat,
        Longitude = base_lon + offset_lon,
        Representative = str_squish(coalesce(input$new_client_rep, "")),
        Location_Source = "Locally registered public-demo client",
        stringsAsFactors = FALSE
      )
      
      updated_geography <- bind_rows(
        geography_data %>% filter(Entity_ID != requested_id),
        geography_record
      )
      
      temp_geo <- tempfile(
        pattern = "aurelis-geography-registration-",
        tmpdir = AURELIS_DATA_DIR,
        fileext = ".csv"
      )
      
      write.table(
        updated_geography,
        temp_geo,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        quote = TRUE,
        qmethod = "double",
        fileEncoding = "UTF-8"
      )
      
      copied_geo <- file.copy(temp_geo, geography_target, overwrite = TRUE)
      unlink(temp_geo)
      
      if (!isTRUE(copied_geo)) {
        stop("The customer was registered, but the Atlas geography file could not be updated.", call. = FALSE)
      }
      
      TRUE
    }, error = function(error) error)
    
    if (inherits(geography_result, "error")) {
      showNotification(
        paste0(
          "Client saved. Atlas location warning: ",
          conditionMessage(geography_result)
        ),
        type = "warning",
        duration = 9
      )
    }
    
    # Reload only the isolated demo datasets so the new registration becomes
    # immediately available in Directory, Quotation Studio and the Atlas.
    load_aurelis_demo_data(AURELIS_APP_ENV)
    data_version(isolate(data_version()) + 1L)
    update_live_filter_choices()
    
    customer_entities <- relationship_entities_for_type("Customer")
    updateSelectInput(session, "contact_type", selected = "Customer")
    updateSelectizeInput(
      session, "contact_entity",
      choices = c("All" = "all", setNames(customer_entities, customer_entities)),
      selected = company,
      server = TRUE
    )
    updateSelectizeInput(session, "quotation_customer_lookup", selected = company)
    updateSelectizeInput(session, "quotation_customer_id_lookup", selected = requested_id)
    updateTextInput(session, "quotation_customer_name", value = company)
    updateTextInput(session, "quotation_customer_code", value = requested_id)
    
    new_client_registration_message(
      paste0("Registered ", requested_id, " · ", company, " in the public-demo directory.")
    )
    
    showNotification(
      paste0(company, " registered as ", requested_id, "."),
      type = "message",
      duration = 6
    )
  }, ignoreInit = TRUE)
  
  output$new_client_registration_status <- renderUI({
    div(
      class = "directory-note",
      icon("info-circle"),
      paste0(" ", new_client_registration_message())
    )
  })
  
  relationship_entities_for_type <- function(contact_type) {
    directory_entities <- relationship_directory %>%
      filter(Contact_Type == contact_type) %>%
      pull(Entity)
    
    operational_entities <- switch(
      contact_type,
      Customer = customer_performance_clients,
      Buyer = sort(unique(na.omit(c(all_buyers, all_inventory_buyers)))),
      `Seller / Supplier` = all_inventory_suppliers,
      character(0)
    )
    
    sort(unique(na.omit(c(directory_entities, operational_entities))))
  }
  
  observeEvent(input$contact_type, {
    entities <- relationship_entities_for_type(input$contact_type)
    updateSelectizeInput(
      session,
      "contact_entity",
      choices = c("All" = "all", setNames(entities, entities)),
      selected = "all",
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$contact_type, input$contact_entity), {
    if (
      identical(input$contact_type, "Seller / Supplier") &&
      !is.null(input$contact_entity) &&
      input$contact_entity != "all"
    ) {
      updateSelectizeInput(session, "contact_catalog_supplier", selected = input$contact_entity)
    }
  }, ignoreInit = TRUE)
  
  
  filtered_contacts <- reactive({
    data_version()
    req(input$contact_type)
    df <- relationship_directory %>%
      filter(Contact_Type == input$contact_type)
    
    if (!is.null(input$contact_entity) && input$contact_entity != "all") {
      df <- df %>% filter(Entity == input$contact_entity)
    }
    if (!is.null(input$contact_country) && input$contact_country != "all") {
      df <- df %>% filter(Country == input$contact_country)
    }
    
    search_term <- str_to_lower(str_squish(if (is.null(input$contact_search)) "" else input$contact_search))
    if (search_term != "") {
      searchable <- str_to_lower(paste(
        coalesce(df$Entity, ""),
        coalesce(df$Contact_Name, ""),
        coalesce(df$Country, ""),
        coalesce(df$Function, ""),
        coalesce(df$Department, ""),
        coalesce(df$Position, ""),
        coalesce(df$Email, ""),
        coalesce(df$Phone, ""),
        coalesce(df$AURELIS_Rep, "")
      ))
      df <- df[str_detect(searchable, fixed(search_term)), , drop = FALSE]
    }
    
    df
  })
  
  contact_history <- reactive({
    data_version()
    req(input$contact_type)
    df <- relationship_order_history %>%
      filter(Line_Type != "Information")
    
    if (!is.null(input$global_year) && input$global_year != "all") {
      df <- df %>% filter(Year == as.numeric(input$global_year))
    }
    if (!is.null(input$global_month) && input$global_month != "all") {
      df <- df %>% filter(Month == as.numeric(input$global_month))
    }
    
    selected_entity <- if (is.null(input$contact_entity)) "all" else input$contact_entity
    
    if (selected_entity != "all") {
      df <- switch(
        input$contact_type,
        Customer = df %>% filter(Client == selected_entity),
        Buyer = df %>% filter(Buyer == selected_entity),
        `Seller / Supplier` = df %>% filter(Supplier == selected_entity),
        df
      )
    } else {
      df <- switch(
        input$contact_type,
        Customer = df %>% filter(!is.na(Client), Client != ""),
        Buyer = df %>% filter(!is.na(Buyer), Buyer != ""),
        `Seller / Supplier` = df %>% filter(!is.na(Supplier), Supplier != ""),
        df
      )
    }
    
    df
  })
  
  output$contact_total <- renderText({
    data_version()
    format(nrow(filtered_contacts()), big.mark = ",")
  })
  
  output$contact_entities <- renderText({
    data_version()
    entities <- relationship_entities_for_type(input$contact_type)
    if (!is.null(input$contact_entity) && input$contact_entity != "all") {
      1
    } else {
      format(length(entities), big.mark = ",")
    }
  })
  
  output$contact_order_count <- renderText({
    data_version()
    format(n_distinct(contact_history()$PO_Number), big.mark = ",")
  })
  
  output$contact_order_value <- renderText({
    data_version()
    paste0(
      "$",
      format(
        round(sum(contact_history()$Amount, na.rm = TRUE), 2),
        big.mark = ",", nsmall = 2
      )
    )
  })
  
  output$contact_profile <- renderUI({
    data_version()
    history <- contact_history()
    selected_entity <- if (is.null(input$contact_entity)) "all" else input$contact_entity
    directory_rows <- filtered_contacts()
    
    if (selected_entity == "all") {
      return(div(
        class = "profile-panel",
        tags$h3(paste("All", input$contact_type, "relationships")),
        tags$p(
          class = "profile-label", "Current filtered portfolio"
        ),
        tags$p(
          class = "profile-value",
          paste0(
            n_distinct(history$PO_Number), " POs • ",
            dollar(sum(history$Amount, na.rm = TRUE), accuracy = 1),
            " order value"
          )
        ),
        tags$p(
          "Select one company or person to display direct contact information, relationship tenure, customers, buyers, sellers, order volume, delivery status, and financial history."
        )
      ))
    }
    
    dates <- history$Order_Date[!is.na(history$Order_Date)]
    first_date <- if (length(dates) > 0) min(dates) else as.Date(NA)
    last_date <- if (length(dates) > 0) max(dates) else as.Date(NA)
    
    contact_names <- directory_rows$Contact_Name
    contact_names <- unique(contact_names[!is.na(contact_names) & contact_names != ""])
    emails <- unique(directory_rows$Email[!is.na(directory_rows$Email) & directory_rows$Email != ""])
    phones <- unique(directory_rows$Phone[!is.na(directory_rows$Phone) & directory_rows$Phone != ""])
    positions <- unique(directory_rows$Position[!is.na(directory_rows$Position) & directory_rows$Position != ""])
    
    counterparties <- switch(
      input$contact_type,
      Customer = n_distinct(history$Supplier[!is.na(history$Supplier) & history$Supplier != ""]),
      Buyer = n_distinct(history$Client[!is.na(history$Client) & history$Client != ""]),
      `Seller / Supplier` = n_distinct(history$Client[!is.na(history$Client) & history$Client != ""]),
      0
    )
    
    counterparty_label <- switch(
      input$contact_type,
      Customer = "suppliers involved",
      Buyer = "customers managed",
      `Seller / Supplier` = "customers served",
      "counterparties"
    )
    
    div(
      class = "profile-panel",
      tags$h3(selected_entity),
      tags$span(class = "source-badge", input$contact_type),
      tags$div(
        class = "profile-label", "Known contacts"
      ),
      tags$div(
        class = "profile-value",
        if (length(contact_names) > 0) {
          paste(head(contact_names, 6), collapse = ", ")
        } else {
          "No named contact in current files"
        }
      ),
      tags$div(class = "profile-label", "Role / position"),
      tags$div(
        class = "profile-value",
        if (length(positions) > 0) paste(head(positions, 5), collapse = ", ") else "Not available"
      ),
      tags$div(class = "profile-label", "Email / phone"),
      tags$div(
        class = "profile-value",
        paste0(
          if (length(emails) > 0) paste(emails, collapse = ", ") else "Email not available",
          " • ",
          if (length(phones) > 0) paste(phones, collapse = ", ") else "Phone not available"
        )
      ),
      tags$div(class = "profile-label", "Operational history"),
      tags$div(
        class = "profile-value",
        paste0(
          n_distinct(history$PO_Number), " POs • ", counterparties, " ",
          counterparty_label, " • ",
          if (!is.na(first_date)) format(first_date, "%b %Y") else "N/A",
          " to ",
          if (!is.na(last_date)) format(last_date, "%b %Y") else "N/A"
        )
      )
    )
  })
  
  output$contact_activity_trend <- renderPlotly({
    data_version()
    df <- contact_history() %>%
      filter(!is.na(Order_Date)) %>%
      mutate(Period = floor_date(Order_Date, "month")) %>%
      group_by(Period) %>%
      summarise(
        Order_Value = sum(Amount, na.rm = TRUE),
        PO_Count = n_distinct(PO_Number),
        Backordered = sum(Backordered_Qty, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Period)
    
    validate(need(nrow(df) > 0, "No order activity is available."))
    
    chart <- plot_ly(df, source = "contact_activity") %>%
      add_bars(
        x = ~Period, y = ~Order_Value,
        key = ~as.character(Period),
        customdata = ~as.character(Period),
        name = "Order Value", marker = list(color = "#2F6EA5"),
        hovertemplate = "%{x|%b %Y}<br>$%{y:,.2f}<extra></extra>"
      ) %>%
      add_lines(
        x = ~Period, y = ~PO_Count,
        key = ~as.character(Period),
        customdata = ~as.character(Period),
        name = "PO Count", yaxis = "y2",
        line = list(color = "#F59E0B", width = 3),
        hovertemplate = "%{x|%b %Y}<br>%{y} POs<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Order Value ($)", tickformat = "$,.0f"),
        yaxis2 = list(title = "PO Count", overlaying = "y", side = "right"),
        legend = list(orientation = "h", y = 1.12),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "contact_activity", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "contact_activity", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0) return()
      value <- if (!is.null(clicked$customdata)) clicked$customdata[[1]] else clicked$x[[1]]
      apply_global_period_from_date(value)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$contact_flow_reset, {
    updateSelectizeInput(session, "contact_entity", selected = "all")
  }, ignoreInit = TRUE)
  
  output$contact_relationship_flow <- renderPlotly({
    data_version()
    
    history <- contact_history() %>%
      filter(
        !is.na(Buyer), Buyer != "",
        !is.na(Client), Client != "",
        !is.na(Supplier), Supplier != ""
      )
    
    validate(need(nrow(history) > 0, "No complete buyer–customer–seller links are available."))
    
    metric <- coalesce(input$contact_flow_metric, "value")
    limit_n <- suppressWarnings(as.integer(coalesce(input$contact_flow_limit, 25)))
    if (!is.finite(limit_n) || limit_n < 5) limit_n <- 25L
    
    summarise_flow <- function(df, group_cols) {
      if (identical(metric, "orders")) {
        df %>%
          group_by(across(all_of(group_cols))) %>%
          summarise(Value = n_distinct(PO_Number), .groups = "drop")
      } else if (identical(metric, "backorders")) {
        df %>%
          group_by(across(all_of(group_cols))) %>%
          summarise(Value = sum(pmax(Open_Balance, 0), na.rm = TRUE), .groups = "drop")
      } else {
        df %>%
          group_by(across(all_of(group_cols))) %>%
          summarise(Value = sum(pmax(Amount, 0), na.rm = TRUE), .groups = "drop")
      }
    }
    
    buyer_customer <- summarise_flow(history, c("Buyer", "Client")) %>%
      arrange(desc(Value)) %>%
      slice_head(n = limit_n) %>%
      transmute(
        Source_Key = paste0("B:", Buyer),
        Target_Key = paste0("C:", Client),
        Source_Label = Buyer,
        Target_Label = Client,
        Value
      )
    
    customer_supplier <- summarise_flow(history, c("Client", "Supplier")) %>%
      arrange(desc(Value)) %>%
      slice_head(n = limit_n) %>%
      transmute(
        Source_Key = paste0("C:", Client),
        Target_Key = paste0("S:", Supplier),
        Source_Label = Client,
        Target_Label = Supplier,
        Value
      )
    
    links <- bind_rows(buyer_customer, customer_supplier) %>%
      filter(is.finite(Value), Value > 0)
    
    validate(need(nrow(links) > 0, "No relationship flows are available for the selected measure."))
    
    node_dictionary <- bind_rows(
      links %>% transmute(Key = Source_Key, Label = Source_Label),
      links %>% transmute(Key = Target_Key, Label = Target_Label)
    ) %>%
      distinct(Key, .keep_all = TRUE) %>%
      mutate(Node_ID = row_number() - 1L)
    
    links <- links %>%
      left_join(
        node_dictionary %>% select(Source_Key = Key, Source_ID = Node_ID),
        by = "Source_Key"
      ) %>%
      left_join(
        node_dictionary %>% select(Target_Key = Key, Target_ID = Node_ID),
        by = "Target_Key"
      ) %>%
      mutate(
        Hover_Value = case_when(
          metric == "orders" ~ paste0(format(Value, big.mark = ","), " distinct PO(s)"),
          metric == "backorders" ~ dollar(Value, accuracy = 1),
          TRUE ~ dollar(Value, accuracy = 1)
        )
      )
    
    metric_label <- switch(
      metric,
      orders = "Purchase-order count",
      backorders = "Backordered / open exposure",
      "Order value"
    )
    
    chart <- plot_ly(
      source = "contact_flow",
      type = "sankey",
      orientation = "h",
      node = list(
        label = node_dictionary$Label,
        customdata = node_dictionary$Key,
        pad = 14,
        thickness = 16,
        line = list(color = "#FFFFFF", width = 0.5),
        hovertemplate = "<b>%{label}</b><br>Click to focus this relationship<extra></extra>"
      ),
      link = list(
        source = links$Source_ID,
        target = links$Target_ID,
        value = links$Value,
        customdata = links$Hover_Value,
        hovertemplate = "%{source.label} → %{target.label}<br>%{customdata}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = paste0(metric_label, " relationship network"), x = .01),
        margin = list(t = 45, b = 20, l = 20, r = 20),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "contact_flow", priority = "event"),
    {
      clicked <- safe_plotly_event_data("plotly_click", source = "contact_flow", priority = "event")
      if (is.null(clicked) || nrow(clicked) == 0 || is.null(clicked$customdata)) return()
      
      key <- as.character(clicked$customdata[[1]])
      if (is.na(key) || !str_detect(key, "^[BCS]:")) return()
      
      prefix <- str_sub(key, 1, 1)
      entity <- str_sub(key, 3)
      target_type <- switch(
        prefix,
        B = "Buyer",
        C = "Customer",
        S = "Seller / Supplier",
        "Customer"
      )
      
      updateSelectInput(session, "contact_type", selected = target_type)
      session$onFlushed(function() {
        updateSelectizeInput(session, "contact_entity", selected = entity)
      }, once = TRUE)
    },
    ignoreInit = TRUE
  )
  
  output$contact_counterparty_chart <- renderPlotly({
    data_version()
    history <- contact_history()
    
    dimension <- switch(
      input$contact_type,
      Customer = "Supplier",
      Buyer = "Client",
      `Seller / Supplier` = "Client",
      "Client"
    )
    
    df <- history %>%
      mutate(Counterparty = .data[[dimension]]) %>%
      filter(!is.na(Counterparty), Counterparty != "") %>%
      group_by(Counterparty) %>%
      summarise(
        Order_Value = sum(Amount, na.rm = TRUE),
        PO_Count = n_distinct(PO_Number),
        Open_Exposure = sum(pmax(Open_Balance, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Order_Value)) %>%
      slice_head(n = 15) %>%
      arrange(Order_Value)
    
    validate(need(nrow(df) > 0, "No counterparty information is available."))
    df$Counterparty <- factor(df$Counterparty, levels = df$Counterparty)
    
    chart <- plot_ly(
      df,
      source = "contact_counterparty",
      y = ~Counterparty,
      x = ~Order_Value,
      key = ~as.character(Counterparty),
      customdata = ~as.character(Counterparty),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~PO_Count,
        colorscale = list(c(0, "#E0F2F1"), c(1, "#3C7474")),
        showscale = TRUE,
        colorbar = list(title = "POs")
      ),
      text = ~paste0("POs: ", PO_Count, "<br>Open exposure: ", dollar(Open_Exposure, accuracy = 1)),
      hovertemplate = "<b>%{y}</b><br>Order value: $%{x:,.2f}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Order Value ($)", tickformat = "$,.0f"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 190, r = 55),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
    
    register_plotly_click(chart)
  })
  
  observeEvent(
    safe_plotly_event_data("plotly_click", source = "contact_counterparty", priority = "event"),
    {
      selected <- plotly_clicked_value(
        safe_plotly_event_data("plotly_click", source = "contact_counterparty", priority = "event")
      )
      if (is.null(selected)) return()
      
      target_type <- switch(
        input$contact_type,
        Customer = "Seller / Supplier",
        Buyer = "Customer",
        `Seller / Supplier` = "Customer",
        "Customer"
      )
      
      updateSelectInput(session, "contact_type", selected = target_type)
      session$onFlushed(function() {
        updateSelectizeInput(session, "contact_entity", selected = selected)
      }, once = TRUE)
    },
    ignoreInit = TRUE
  )
  
  output$contact_history_table <- renderDT({
    data_version()
    df <- contact_history() %>%
      arrange(desc(Order_Date), PO_Number) %>%
      select(
        Order_Status,
        Order_Date,
        Delivery_Date,
        PO_Number,
        Customer = Client,
        Buyer,
        Seller_Supplier = Supplier,
        Part_Number,
        `Order Description` = Order_Description,
        Quantity,
        Received_Qty,
        Backordered_Qty,
        Line_Amount = Amount,
        Open_Balance
      )
    
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(c("Line_Amount", "Open_Balance"), "$", digits = 2)
  })
  
  output$contact_table <- renderDT({
    data_version()
    df <- filtered_contacts() %>%
      select(
        Contact_Type,
        Entity,
        Contact_Name,
        Country,
        Function,
        Department,
        Position,
        Email,
        Phone,
        Address,
        AURELIS_Rep,
        Directory_Source
      )
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$contact_download <- downloadHandler(
    filename = function() {
      category <- gsub("[^A-Za-z0-9]+", "_", input$contact_type)
      entity <- if (is.null(input$contact_entity) || input$contact_entity == "all") {
        "All"
      } else {
        gsub("[^A-Za-z0-9]+", "_", input$contact_entity)
      }
      paste0("Relationship_Directory_", category, "_", entity, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
      }
      
      wb <- openxlsx::createWorkbook(creator = "Aurelis Dashboard")
      openxlsx::addWorksheet(wb, "Directory")
      directory_export <- filtered_contacts()
      if (nrow(directory_export) > 0) {
        openxlsx::writeDataTable(wb, "Directory", directory_export, tableStyle = "TableStyleMedium2")
      } else {
        openxlsx::writeData(wb, "Directory", "No matching directory records.")
      }
      
      openxlsx::addWorksheet(wb, "Order History")
      history_export <- contact_history()
      if (nrow(history_export) > 0) {
        openxlsx::writeDataTable(wb, "Order History", history_export, tableStyle = "TableStyleMedium2")
      } else {
        openxlsx::writeData(wb, "Order History", "No matching order history.")
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  ##############################################################################
  # 4.12 UNIVERSAL PAGE REPORT AND EXPORT BUILDER
  ##############################################################################
  
  active_report_page <- reactiveVal("exec_overview")
  
  report_page_titles <- c(
    multi_user_workspace = "My Multi-User Workspace",
    exec_overview = "Executive Overview",
    global_network = "Global Network Atlas",
    kpi_board = "KPI Command Center",
    sale_performance = "Sales and Profitability",
    monthly_sales = "Purchase Orders and Buyers",
    buyer_activity = "Buyer Activity Intelligence",
    customer_performance = "Customer Performance",
    ar_tab = "Accounts Receivable",
    client_statement = "Client Statement",
    order_tracker = "Orders and Delivery",
    inventory_tab = "Inventory and Warehouse",
    buyer_tool = "Supplier Intelligence and Procurement",
    quotation_studio = "Quotation Studio",
    contacts_tab = "Relationship Directory",
    misc_tab = "Data Connections and Process"
  )
  
  report_table_ids <- c(
    global_network = "atlas_location_table",
    kpi_board = "kpi_board_health",
    monthly_sales = "sales_po_detail_table",
    buyer_activity = "buyer_act_table",
    sale_performance = "perf_detail_table",
    customer_performance = "cust_perf_order_table",
    ar_tab = "ar_full_table",
    client_statement = "stmt_detail_table",
    order_tracker = "tracker_po_table",
    inventory_tab = "inventory_detail_table",
    buyer_tool = "buyer_po_lookup_table",
    quotation_studio = "quotation_line_table",
    contacts_tab = "contact_table",
    misc_tab = "connection_source_table"
  )
  
  currency_text <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 0 || !is.finite(value[[1]])) value <- 0
    paste0("$", format(round(value[[1]], 2), big.mark = ",", nsmall = 2))
  }
  
  selected_or_all <- function(value, all_values = c("all", "global", "")) {
    if (is.null(value) || length(value) == 0) return("All")
    if (length(all_values) > 0 && any(value %in% all_values)) return("All")
    paste(as.character(value), collapse = ", ")
  }
  
  report_detail_data <- reactive({
    data_version()
    switch(
      active_report_page(),
      multi_user_workspace = aurelis_multiuser_active_sessions(10) %>%
        transmute(Buyer = buyer, Started = started_at, `Last Seen` = last_seen),
      exec_overview = filtered_revenue() %>%
        select(Date, Customer, Entity, Staff, Country, Parent_Company, Revenue, Cost, Gross_Profit, Margin_Pct),
      global_network = atlas_filtered_data() %>%
        select(
          Entity_Type, Entity_ID, Entity, Continent, Country, Region, City,
          Representative, Activity_Value, Gross_Profit, Open_Exposure,
          Record_Count, PO_Count, PO_Value, Backordered_Units,
          Latitude, Longitude
        ) %>%
        arrange(Continent, Country, Region, City, Entity_Type, Entity),
      kpi_board = tibble(
        KPI = c("Revenue", "Gross Profit", "PO Value", "Open AR", "Customers", "Buyers"),
        Value = c(
          sum(filtered_revenue()$Revenue, na.rm = TRUE),
          sum(filtered_revenue()$Gross_Profit, na.rm = TRUE),
          sum(filtered_po()$Total_PO, na.rm = TRUE),
          sum(pmax(filtered_ar()$Balance_Remaining, 0), na.rm = TRUE),
          length(unique(na.omit(c(filtered_revenue()$Customer, filtered_po()$Client, filtered_inventory()$Client)))),
          n_distinct(filtered_po()$Buyer[!is.na(filtered_po()$Buyer) & filtered_po()$Buyer != ""])
        )
      ),
      monthly_sales = sales_po() %>%
        select(Date, PO_Number, Client, Buyer, Total_PO, Month_Name, Year),
      sale_performance = perf_data() %>%
        select(Date, Customer, Entity, Staff, Country, Parent_Company, Revenue, Cost, Gross_Profit, Margin_Pct),
      buyer_activity = buyer_activity_filtered() %>%
        transmute(
          Date, Buyer, Customer = Client, PO_Number, PO_Value = Total_PO,
          Year, Month = Month_Name, Week = paste("Week", Week_Of_Month), Day = Day_Of_Month
        ) %>% arrange(desc(Date), Buyer, PO_Number),
      customer_performance = if (
        is.null(input$cust_perf_client) || input$cust_perf_client == ""
      ) {
        tibble()
      } else {
        {
          df <- customer_perf_history()
          focus <- cust_perf_status_focus()
          if (!is.null(focus) && focus != "") df <- df %>% filter(as.character(Order_Status) == focus)
          df
        } %>%
          select(
            Order_Status, History_Source, Order_Date, Delivery_Date,
            Planned_Execution_Days, PO_Number, Customer = Client, Buyer,
            Supplier, Part_Number, `Order Description` = Order_Description,
            Quantity, Received_Qty, Backordered_Qty, Unit_Price,
            Line_Amount = Amount, Open_Balance
          )
      },
      ar_tab = ar_view_data() %>%
        select(
          Txn_Date, Due_Date, Customer, Customer_ID, Invoice_Ref, PO_Number, Terms,
          Invoice_Amount, Balance_Remaining, Payment_Timing,
          Days_Until_Due, Days_Past_Due, Aging_Bucket, Staff
        ),
      client_statement = if (
        is.null(input$stmt_client) || input$stmt_client == ""
      ) {
        tibble()
      } else {
        stmt_view_data() %>%
          select(
            Txn_Date, Due_Date, Customer, Invoice_Ref, PO_Number, Terms,
            Invoice_Amount, Balance_Remaining, Payment_Timing,
            Days_Until_Due, Days_Past_Due, Aging_Bucket, Staff
          )
      },
      order_tracker = tracker_data() %>%
        select(Order_Status, Line_Type, Buyer, Customer = Client, Client_ID, PO_Number,
               Supplier, Delivery_Location, Part_Number, `Order Description (Memo)` = Memo,
               Order_Date, Delivery_Date, Quantity, Unit_Price, Received_Qty,
               Backordered_Qty, Line_Amount = Amount, Open_Balance, Days_To_Delivery),
      inventory_tab = if (
        !is.null(input$inventory_view) && input$inventory_view == "warehouse"
      ) {
        warehouse_filtered() %>%
          select(
            Stock_Status, Item_Code, Item_Description, Warehouse, Bin_Location,
            Supplier, On_Hand, Allocated, Available, On_Order, Reorder_Point,
            Monthly_Demand, Months_Of_Cover, Unit_Cost, Inventory_Value,
            Last_Updated, Data_Source
          )
      } else {
        inventory_filtered() %>%
          select(
            Order_Status, Line_Type, Buyer, Customer = Client, PO_Number, Supplier,
            Part_Number, `Order Description` = Order_Description, Delivery_Location,
            Order_Date, Delivery_Date, Delivery_Year, Delivery_Month_Name,
            Delivery_Week_Label, Quantity, Unit_Price, Received_Qty,
            Backordered_Qty, Line_Amount = Amount, Open_Balance, Days_To_Delivery
          )
      },
      buyer_tool = buyer_lookup_raw(),
      quotation_studio = quotation_final_lines() %>%
        transmute(
          Line, `Part / Reference` = Part_Number, Description, Quantity, Unit, Supplier,
          `Vendor Unit Cost` = Vendor_Unit_Cost, `Supplier Discount %` = Supplier_Discount_Pct,
          `Net Unit Cost` = Net_Unit_Cost, `Vendor Line Cost` = Line_Cost,
          `Quoted Unit Price` = Sell_Unit_Price, `Quoted Line Total` = Sell_Line_Total
        ),
      contacts_tab = filtered_contacts(),
      misc_tab = tibble(
        Dataset = c(
          "DAILY_PO", "REVENUE", "AR", "INVENTORY",
          "WAREHOUSE_INVENTORY", "CONTACTS", "DIRECTORY",
          "SELLER_CATALOG", "GEOGRAPHY"
        ),
        Records = c(
          nrow(daily_po), nrow(revenue_data), nrow(ar_data),
          nrow(inventory_data), nrow(warehouse_inventory_data),
          nrow(contacts_data), nrow(relationship_directory),
          nrow(seller_catalog_data), nrow(geography_data)
        )
      ) %>%
        left_join(
          source_registry_table() %>% select(Dataset, Source, Detail, Loaded_At),
          by = "Dataset"
        ),
      tibble()
    )
  })
  
  report_summary_data <- reactive({
    data_version()
    page <- active_report_page()
    df <- report_detail_data()
    
    switch(
      page,
      multi_user_workspace = tibble(
        Metric = c("Current Buyer", "Active Sessions", "Available Drafts", "Shared Refresh Version"),
        Value = c(
          safe_workspace_buyer(),
          nrow(aurelis_multiuser_active_sessions(10)),
          nrow(multiuser_drafts()),
          AURELIS_SHARED_REFRESH_STATE$version
        )
      ),
      exec_overview = tibble(
        Metric = c("Revenue", "Gross Profit", "Invoices", "Customers", "Outstanding AR"),
        Value = c(
          currency_text(sum(filtered_revenue()$Revenue, na.rm = TRUE)),
          currency_text(sum(filtered_revenue()$Gross_Profit, na.rm = TRUE)),
          format(nrow(filtered_revenue()), big.mark = ","),
          format(n_distinct(filtered_revenue()$Customer), big.mark = ","),
          currency_text(sum(filtered_ar()$Balance_Remaining, na.rm = TRUE))
        )
      ),
      global_network = {
        atlas_df <- atlas_filtered_data()
        selected_row <- atlas_selected_row()
        tibble(
          Metric = c(
            "Visible Locations", "Countries", "Customers", "Suppliers",
            "Representatives", "Activity Value", "Open Exposure", "Selected Relationship"
          ),
          Value = c(
            format(nrow(atlas_df), big.mark = ","),
            format(n_distinct(atlas_df$Country), big.mark = ","),
            format(sum(atlas_df$Entity_Type == "Customer"), big.mark = ","),
            format(sum(atlas_df$Entity_Type == "Supplier"), big.mark = ","),
            format(sum(atlas_df$Entity_Type == "Representative"), big.mark = ","),
            currency_text(sum(atlas_df$Activity_Value, na.rm = TRUE)),
            currency_text(sum(atlas_df$Open_Exposure, na.rm = TRUE)),
            if (nrow(selected_row) > 0) paste0(selected_row$Entity_Type[[1]], ": ", selected_row$Entity[[1]]) else "None"
          )
        )
      },
      kpi_board = {
        inv <- kpi_board_inventory()
        ordered <- sum(inv$Quantity, na.rm = TRUE)
        received <- sum(inv$Received_Qty, na.rm = TRUE)
        backordered <- sum(pmax(inv$Backordered_Qty, 0), na.rm = TRUE)
        revenue <- sum(filtered_revenue()$Revenue, na.rm = TRUE)
        gp <- sum(filtered_revenue()$Gross_Profit, na.rm = TRUE)
        tibble(
          Metric = c("Revenue", "Gross Profit", "Gross Margin", "PO Value", "Open AR", "Quantity Fulfillment", "Backorder Rate", "Customers", "Buyers"),
          Value = c(
            currency_text(revenue), currency_text(gp),
            if (revenue > 0) paste0(round(gp / revenue * 100, 1), "%") else "N/A",
            currency_text(sum(filtered_po()$Total_PO, na.rm = TRUE)),
            currency_text(sum(pmax(filtered_ar()$Balance_Remaining, 0), na.rm = TRUE)),
            if (ordered > 0) paste0(round(pmin(received / ordered, 1) * 100, 1), "%") else "N/A",
            if (ordered > 0) paste0(round(backordered / ordered * 100, 1), "%") else "N/A",
            format(length(unique(na.omit(c(filtered_revenue()$Customer, filtered_po()$Client, filtered_inventory()$Client)))), big.mark = ","),
            format(n_distinct(filtered_po()$Buyer[!is.na(filtered_po()$Buyer) & filtered_po()$Buyer != ""]), big.mark = ",")
          )
        )
      },
      monthly_sales = {
        po_count <- n_distinct(df$PO_Number[!is.na(df$PO_Number) & df$PO_Number != ""])
        po_value <- sum(df$Total_PO, na.rm = TRUE)
        tibble(
          Metric = c("Purchase Orders", "PO Value", "Average PO", "Clients", "Buyers"),
          Value = c(
            format(po_count, big.mark = ","),
            currency_text(po_value),
            currency_text(if (po_count > 0) po_value / po_count else 0),
            format(n_distinct(df$Client), big.mark = ","),
            format(n_distinct(df$Buyer), big.mark = ",")
          )
        )
      },
      sale_performance = tibble(
        Metric = c("Revenue Transactions", "Revenue", "Gross Profit", "Weighted Gross Margin", "Customers"),
        Value = c(
          format(nrow(df), big.mark = ","),
          currency_text(sum(df$Revenue, na.rm = TRUE)),
          currency_text(sum(df$Gross_Profit, na.rm = TRUE)),
          paste0(round(sum(df$Gross_Profit, na.rm = TRUE) / max(sum(df$Revenue, na.rm = TRUE), 1) * 100, 1), "%"),
          format(n_distinct(df$Customer), big.mark = ",")
        )
      ),
      buyer_activity = {
        po_count <- n_distinct(df$PO_Number[!is.na(df$PO_Number) & df$PO_Number != ""])
        po_value <- sum(df$PO_Value, na.rm = TRUE)
        tibble(
          Metric = c("Purchase Orders", "PO Value", "Average PO", "Customers", "Buyers", "Active Days"),
          Value = c(
            format(po_count, big.mark = ","), currency_text(po_value),
            currency_text(if (po_count > 0) po_value / po_count else 0),
            format(n_distinct(df$Customer), big.mark = ","),
            format(n_distinct(df$Buyer), big.mark = ","),
            format(n_distinct(df$Date), big.mark = ",")
          )
        )
      },
      customer_performance = if (
        is.null(input$cust_perf_client) || input$cust_perf_client == ""
      ) {
        tibble(Metric = "Customer", Value = "No customer selected")
      } else {
        customer_summary <- customer_perf_summary()
        tibble(
          Metric = c(
            "Customer", "Purchase Orders", "PO Value", "Revenue",
            "Gross Profit", "Completion Rate", "Average Planned Execution Days",
            "Outstanding AR", "Buyers", "Suppliers"
          ),
          Value = c(
            customer_summary$Customer,
            format(customer_summary$PO_Count, big.mark = ","),
            currency_text(customer_summary$PO_Value),
            currency_text(customer_summary$Revenue),
            currency_text(customer_summary$Gross_Profit),
            ifelse(
              is.na(customer_summary$Completion_Rate), "N/A",
              percent(customer_summary$Completion_Rate, accuracy = 0.1)
            ),
            ifelse(
              is.na(customer_summary$Average_Execution_Days), "N/A",
              paste0(round(customer_summary$Average_Execution_Days, 1), " days")
            ),
            currency_text(customer_summary$Outstanding_AR),
            customer_summary$Buyer_Count,
            customer_summary$Supplier_Count
          )
        )
      },
      ar_tab = tibble(
        Metric = c("Invoices", "Outstanding Balance", "Overdue Balance", "Customers", "90+ Day Balance"),
        Value = c(
          format(nrow(df), big.mark = ","),
          currency_text(sum(df$Balance_Remaining, na.rm = TRUE)),
          currency_text(sum(df$Balance_Remaining[df$Days_Past_Due > 0], na.rm = TRUE)),
          format(n_distinct(df$Customer), big.mark = ","),
          currency_text(sum(df$Balance_Remaining[as.character(df$Aging_Bucket) == "90+ Days"], na.rm = TRUE))
        )
      ),
      client_statement = tibble(
        Metric = c("Client", "Invoices", "Total Invoiced", "Outstanding", "Paid %"),
        Value = c(
          selected_or_all(input$stmt_client, c("")),
          format(nrow(df), big.mark = ","),
          currency_text(sum(df$Invoice_Amount, na.rm = TRUE)),
          currency_text(sum(df$Balance_Remaining, na.rm = TRUE)),
          paste0(round((1 - sum(df$Balance_Remaining, na.rm = TRUE) / max(sum(df$Invoice_Amount, na.rm = TRUE), 1)) * 100, 1), "%")
        )
      ),
      order_tracker = tibble(
        Metric = c("Order Lines", "POs", "Customers", "Suppliers", "Open Balance", "Backordered Units"),
        Value = c(
          format(nrow(df), big.mark = ","),
          format(n_distinct(df$PO_Number), big.mark = ","),
          format(n_distinct(df$Customer), big.mark = ","),
          format(n_distinct(df$Supplier), big.mark = ","),
          currency_text(sum(df$Open_Balance, na.rm = TRUE)),
          format(sum(df$Backordered_Qty, na.rm = TRUE), big.mark = ",")
        )
      ),
      inventory_tab = if (
        !is.null(input$inventory_view) && input$inventory_view == "warehouse"
      ) {
        tibble(
          Metric = c(
            "Inventory View", "Tracked Items", "Quantity On Hand",
            "Allocated Quantity", "Available Quantity", "On Order",
            "Inventory Value", "Items Requiring Attention", "Data Source Status"
          ),
          Value = c(
            "Warehouse Availability",
            format(n_distinct(df$Item_Code), big.mark = ","),
            ifelse(warehouse_inventory_available, format(sum(df$On_Hand, na.rm = TRUE), big.mark = ","), "Pending connection"),
            ifelse(warehouse_inventory_available, format(sum(df$Allocated, na.rm = TRUE), big.mark = ","), "Pending connection"),
            ifelse(warehouse_inventory_available, format(sum(df$Available, na.rm = TRUE), big.mark = ","), "Pending connection"),
            format(sum(df$On_Order, na.rm = TRUE), big.mark = ","),
            ifelse(warehouse_inventory_available, currency_text(sum(df$Inventory_Value, na.rm = TRUE)), "Pending connection"),
            format(
              sum(df$Stock_Status %in% c(
                "Negative Availability", "Out of Stock",
                "Reorder Required", "Data Unavailable", "Warehouse data pending"
              ), na.rm = TRUE),
              big.mark = ","
            ),
            ifelse(warehouse_inventory_available, "Connected warehouse data", "Inbound proxy only")
          )
        )
      } else {
        tibble(
          Metric = c(
            "Inventory View", "Inbound Lines", "Open Inbound Value",
            "Units Awaiting Receipt", "Customers", "Suppliers", "Overdue Lines"
          ),
          Value = c(
            "Inbound & Backorders",
            format(nrow(df), big.mark = ","),
            currency_text(sum(df$Open_Balance, na.rm = TRUE)),
            format(sum(df$Backordered_Qty, na.rm = TRUE), big.mark = ","),
            format(n_distinct(df$Customer), big.mark = ","),
            format(n_distinct(df$Supplier), big.mark = ","),
            format(sum(as.character(df$Order_Status) == "Overdue", na.rm = TRUE), big.mark = ",")
          )
        )
      },
      quotation_studio = quotation_report_summary(),
      buyer_tool = tibble(
        Metric = c(
          "Order Lines", "Purchase Orders", "Order Value",
          "Customers", "Buyers", "Sellers / Suppliers"
        ),
        Value = c(
          format(nrow(df), big.mark = ","),
          format(n_distinct(df$PO_Number), big.mark = ","),
          currency_text(sum(df$Line_Amount, na.rm = TRUE)),
          format(n_distinct(df$Customer), big.mark = ","),
          format(n_distinct(df$Buyer), big.mark = ","),
          format(n_distinct(df$Seller_Supplier), big.mark = ",")
        )
      ),
      contacts_tab = tibble(
        Metric = c(
          "Relationship Type", "Selected Entity", "Directory Records",
          "Entities", "Purchase Orders", "Order Value"
        ),
        Value = c(
          input$contact_type,
          selected_or_all(input$contact_entity),
          format(nrow(df), big.mark = ","),
          format(n_distinct(df$Entity), big.mark = ","),
          format(n_distinct(contact_history()$PO_Number), big.mark = ","),
          currency_text(sum(contact_history()$Amount, na.rm = TRUE))
        )
      ),
      misc_tab = tibble(
        Metric = c("Data Sources", "Total Loaded Records", "Demo Source Mode", "Session Note"),
        Value = c(
          nrow(df),
          format(sum(df$Records), big.mark = ","),
          AURELIS_DATA_SOURCE_MODE,
          ifelse(is.null(input$misc_notes) || input$misc_notes == "", "None", input$misc_notes)
        )
      ),
      tibble(Metric = "Records", Value = nrow(df))
    )
  })
  
  report_filter_data <- reactive({
    data_version()
    page <- active_report_page()
    rows <- list(
      "Global Year" = selected_or_all(input$global_year),
      "Global Month" = if (is.null(input$global_month) || input$global_month == "all") "All" else month.name[as.numeric(input$global_month)]
    )
    
    page_rows <- switch(
      page,
      multi_user_workspace = list("Workspace Owner" = safe_workspace_buyer()),
      global_network = list(
        "Relationship Types" = selected_or_all(input$atlas_entity_types, character(0)),
        "Continent" = selected_or_all(input$atlas_continent),
        "Country" = selected_or_all(input$atlas_country),
        "Region" = selected_or_all(input$atlas_region),
        "City / Town" = selected_or_all(input$atlas_city),
        "Location / Entity Search" = selected_or_all(input$atlas_search, c("")),
        "Map Metric" = selected_or_all(input$atlas_metric, character(0)),
        "Projection" = selected_or_all(input$atlas_projection, character(0))
      ),
      kpi_board = list(
        "Scope" = "Global year/month filters shown above"
      ),
      monthly_sales = list(
        "Buyer" = selected_or_all(input$sales_buyer),
        "Client" = selected_or_all(input$sales_client),
        "Specific Year" = selected_or_all(input$sales_year_specific)
      ),
      sale_performance = list(
        "Staff" = selected_or_all(input$perf_staff),
        "Country" = selected_or_all(input$perf_country),
        "Parent Company" = selected_or_all(input$perf_parent),
        "Margin Target" = paste0(input$perf_margin_target, "%"),
        "Interactive Country Focus" = selected_or_all(perf_country_focus(), c("", NA))
      ),
      buyer_activity = list(
        "Year" = selected_or_all(input$buyer_act_year),
        "Month" = if (is.null(input$buyer_act_month) || input$buyer_act_month == "all") "All" else month.name[as.numeric(input$buyer_act_month)],
        "Week" = selected_or_all(input$buyer_act_week),
        "Day" = selected_or_all(input$buyer_act_day),
        "Buyer" = selected_or_all(input$buyer_act_buyer)
      ),
      customer_performance = list(
        "Customer" = selected_or_all(input$cust_perf_client, c("")),
        "Analysis Period" = selected_or_all(input$cust_perf_scope, character(0)),
        "Trend Value" = selected_or_all(input$cust_perf_value_basis, character(0)),
        "Interactive Fulfillment Focus" = selected_or_all(cust_perf_status_focus(), c("", NA))
      ),
      ar_tab = list(
        "Interactive Aging Focus" = selected_or_all(ar_aging_focus(), c("", NA))
      ),
      client_statement = list(
        "Client" = selected_or_all(input$stmt_client, c("")),
        "Interactive Aging Focus" = selected_or_all(stmt_aging_focus(), c("", NA))
      ),
      order_tracker = list(
        "Customer" = selected_or_all(input$tracker_client),
        "Buyer" = selected_or_all(input$tracker_buyer),
        "Supplier" = selected_or_all(input$tracker_supplier),
        "Status" = selected_or_all(input$tracker_status),
        "Delivery Horizon" = selected_or_all(input$tracker_horizon),
        "Timeline Measure" = selected_or_all(input$tracker_timeline_metric, character(0)),
        "Interactive Status Focus" = selected_or_all(tracker_status_focus(), c("", NA)),
        "Search" = selected_or_all(input$tracker_search, c(""))
      ),
      inventory_tab = if (
        !is.null(input$inventory_view) && input$inventory_view == "warehouse"
      ) {
        list(
          "Inventory View" = "Warehouse Availability",
          "Warehouse" = selected_or_all(input$wh_warehouse),
          "Supplier" = selected_or_all(input$wh_supplier),
          "Stock Status" = selected_or_all(input$wh_status),
          "Interactive Status Focus" = selected_or_all(wh_status_focus(), c("", NA)),
          "Search" = selected_or_all(input$wh_search, c(""))
        )
      } else {
        list(
          "Inventory View" = "Inbound & Backorders",
          "Customer" = selected_or_all(input$inv_client),
          "Supplier" = selected_or_all(input$inv_supplier),
          "Delivery Status" = selected_or_all(input$inv_status),
          "Delivery Year" = selected_or_all(input$inv_delivery_year),
          "Delivery Month" = if (
            is.null(input$inv_delivery_month) || input$inv_delivery_month == "all"
          ) "All" else month.name[as.numeric(input$inv_delivery_month)],
          "Delivery Week" = selected_or_all(input$inv_delivery_week),
          "Search" = selected_or_all(input$inv_search, c("")),
          "Interactive Inbound Timing Focus" = selected_or_all(inv_timing_focus(), c("", NA))
        )
      },
      quotation_studio = list(
        "Quotation Number" = selected_or_all(input$quotation_number, c("")),
        "Customer" = selected_or_all(input$quotation_customer_name, c("")),
        "Currency" = selected_or_all(input$quotation_currency, character(0)),
        "Official Scenario" = selected_or_all(input$quotation_selected_scenario, character(0)),
        "Pricing Method" = selected_or_all(input$quotation_pricing_method, character(0)),
        "Target Rate" = paste0(input$quotation_pricing_rate, "%"),
        "Finance Method" = selected_or_all(input$quotation_interest_method, character(0))
      ),
      buyer_tool = list(
        "PO / Part Search" = selected_or_all(input$buyer_search_po, c("")),
        "Customer" = selected_or_all(input$buyer_search_client),
        "Seller / Supplier" = selected_or_all(input$buyer_search_supplier),
        "Buyer" = selected_or_all(input$buyer_search_buyer)
      ),
      contacts_tab = list(
        "Relationship Type" = selected_or_all(input$contact_type, character(0)),
        "Entity" = selected_or_all(input$contact_entity),
        "Country" = selected_or_all(input$contact_country),
        "Search" = selected_or_all(input$contact_search, c(""))
      ),
      list()
    )
    
    rows <- c(rows, page_rows)
    tibble(Filter = names(rows), Selection = unlist(rows, use.names = FALSE))
  })
  
  report_default_columns <- function(page, columns) {
    preferred <- switch(
      page,
      multi_user_workspace = c("Buyer", "Started", "Last Seen"),
      exec_overview = c("Date", "Customer", "Staff", "Country", "Revenue", "Gross_Profit", "Margin_Pct"),
      global_network = c(
        "Entity_Type", "Entity_ID", "Entity", "Continent", "Country", "Region", "City",
        "Representative", "Activity_Value", "Gross_Profit", "Open_Exposure",
        "Record_Count", "PO_Count", "PO_Value"
      ),
      kpi_board = c("KPI", "Value"),
      monthly_sales = c("Date", "PO_Number", "Client", "Buyer", "Total_PO"),
      buyer_activity = c("Date", "Buyer", "Customer", "PO_Number", "PO_Value", "Year", "Month", "Week", "Day"),
      sale_performance = c("Date", "Customer", "Staff", "Country", "Revenue", "Cost", "Gross_Profit", "Margin_Pct"),
      customer_performance = c(
        "Order_Status", "History_Source", "Order_Date", "Delivery_Date", "Planned_Execution_Days",
        "PO_Number", "Customer", "Buyer", "Supplier", "Part_Number",
        "Order Description", "Quantity", "Received_Qty", "Backordered_Qty",
        "Unit_Price", "Line_Amount", "Open_Balance"
      ),
      ar_tab = c(
        "Txn_Date", "Due_Date", "Customer", "Invoice_Ref", "PO_Number",
        "Invoice_Amount", "Balance_Remaining", "Payment_Timing",
        "Days_Until_Due", "Days_Past_Due", "Aging_Bucket"
      ),
      client_statement = c(
        "Txn_Date", "Due_Date", "Customer", "Invoice_Ref", "PO_Number",
        "Invoice_Amount", "Balance_Remaining", "Payment_Timing",
        "Days_Until_Due", "Days_Past_Due", "Aging_Bucket"
      ),
      order_tracker = c(
        "Order_Status", "Buyer", "Customer", "PO_Number", "Supplier",
        "Order Description (Memo)", "Order_Date", "Delivery_Date",
        "Quantity", "Unit_Price", "Backordered_Qty", "Open_Balance"
      ),
      inventory_tab = if (
        !is.null(input$inventory_view) && input$inventory_view == "warehouse"
      ) {
        c(
          "Stock_Status", "Item_Code", "Item_Description", "Warehouse",
          "Bin_Location", "Supplier", "On_Hand", "Allocated", "Available",
          "On_Order", "Reorder_Point", "Monthly_Demand", "Months_Of_Cover",
          "Unit_Cost", "Inventory_Value", "Last_Updated"
        )
      } else {
        c(
          "Order_Status", "Buyer", "Customer", "PO_Number", "Supplier",
          "Order Description", "Delivery_Date", "Delivery_Week_Label",
          "Quantity", "Backordered_Qty", "Open_Balance", "Days_To_Delivery"
        )
      },
      quotation_studio = c(
        "Line", "Part / Reference", "Description", "Quantity", "Unit", "Supplier",
        "Vendor Unit Cost", "Supplier Discount %", "Net Unit Cost",
        "Vendor Line Cost", "Quoted Unit Price", "Quoted Line Total"
      ),
      buyer_tool = c(
        "Order_Status", "Order_Date", "Delivery_Date", "PO_Number",
        "Customer", "Buyer", "Seller_Supplier", "Part_Number",
        "Order Description", "Quantity", "Backordered_Qty",
        "Unit_Price", "Line_Amount", "Open_Balance"
      ),
      contacts_tab = columns,
      misc_tab = columns,
      columns
    )
    intersect(preferred, columns)
  }
  
  report_scoped_data <- reactive({
    data_version()
    df <- report_detail_data()
    
    if (!is.null(input$report_scope) && input$report_scope == "visible") {
      table_id <- if (
        active_report_page() == "inventory_tab" &&
        !is.null(input$inventory_view) &&
        input$inventory_view == "warehouse"
      ) {
        "wh_inventory_table"
      } else {
        unname(report_table_ids[active_report_page()])
      }
      if (length(table_id) == 1 && !is.na(table_id)) {
        row_indices <- input[[paste0(table_id, "_rows_current")]]
        if (!is.null(row_indices) && length(row_indices) > 0) {
          row_indices <- row_indices[row_indices >= 1 & row_indices <= nrow(df)]
          if (length(row_indices) > 0) df <- df[row_indices, , drop = FALSE]
        }
      }
    }
    
    selected_columns <- input$report_columns
    if (!is.null(selected_columns) && length(selected_columns) > 0) {
      selected_columns <- intersect(selected_columns, names(df))
      if (length(selected_columns) > 0) df <- df[, selected_columns, drop = FALSE]
    }
    df
  })
  
  report_total_pages <- reactive({
    data_version()
    rows_per_page <- if (is.null(input$report_rows_per_page)) 25 else input$report_rows_per_page
    details_pages <- max(1, ceiling(nrow(report_scoped_data()) / max(rows_per_page, 1)))
    content_mode <- if (is.null(input$report_content)) "summary_details" else input$report_content
    if (content_mode == "summary_only") return(1)
    if (content_mode == "details_only") return(details_pages)
    1 + details_pages
  })
  
  output$report_page_estimate <- renderUI({
    data_version()
    total <- report_total_pages()
    package_messages <- c(
      if (requireNamespace("openxlsx", quietly = TRUE)) "Excel ready" else "Install openxlsx for Excel",
      if (requireNamespace("pagedown", quietly = TRUE)) "PDF engine ready" else "Install pagedown for PDF"
    )
    
    div(
      class = "report-modal-status",
      tags$strong(paste0("Estimated report pages: ", total)),
      tags$br(),
      paste(package_messages, collapse = " • "),
      tags$br(),
      tags$small("PDF pages are explicit report pages. Enter All or a selection such as 1-3,5.")
    )
  })
  
  open_report_builder <- function(page_key) {
    if (is.null(page_key) || !page_key %in% names(report_page_titles)) {
      page_key <- "exec_overview"
    }
    
    active_report_page(page_key)
    df <- isolate(report_detail_data())
    page_title <- unname(report_page_titles[[page_key]])
    default_columns <- report_default_columns(page_key, names(df))
    if (length(default_columns) == 0) default_columns <- names(df)
    
    showModal(modalDialog(
      title = paste0("Report & Export — ", page_title),
      size = "l",
      easyClose = FALSE,
      fade = TRUE,
      footer = tagList(
        modalButton("Close"),
        downloadButton("page_report_excel", "Download Excel", class = "btn-success"),
        downloadButton("page_report_pdf", "Download PDF", class = "btn-danger")
      ),
      fluidRow(
        column(6, textInput(
          "report_title", "Report Title:",
          value = paste0("Aurelis ", page_title, " Report")
        )),
        column(3, selectInput(
          "report_content", "Report Content:",
          choices = c(
            "Summary + Detailed Records" = "summary_details",
            "Summary Only" = "summary_only",
            "Detailed Records Only" = "details_only"
          ),
          selected = "summary_details"
        )),
        column(3, selectInput(
          "report_scope", "Record Scope:",
          choices = c(
            "All Filtered Records" = "filtered",
            "Current Visible Table Page" = "visible"
          ),
          selected = "filtered"
        ))
      ),
      fluidRow(
        column(3, numericInput(
          "report_rows_per_page", "Rows per PDF Page:",
          value = 25, min = 5, max = 100, step = 5
        )),
        column(3, selectInput(
          "report_orientation", "PDF Orientation:",
          choices = c("Landscape" = "landscape", "Portrait" = "portrait"),
          selected = "landscape"
        )),
        column(3, selectInput(
          "report_paper_size", "Paper Size:",
          choices = c("A4 (International)" = "A4", "Letter (North America)" = "Letter"),
          selected = "A4"
        )),
        column(3, textInput(
          "report_page_spec", "PDF Pages:",
          value = "All",
          placeholder = "All or 1-3,5"
        ))
      ),
      fluidRow(
        column(4, selectInput(
          "report_sort_direction", "Record Order:",
          choices = c("Dashboard Order" = "dashboard", "Reverse Dashboard Order" = "reverse"),
          selected = "dashboard"
        )),
        column(8, tags$div(
          class = "data-quality-note",
          icon("shield-alt"),
          " The builder remains open until you use Close. Select fields, columns and page options without losing your work."
        ))
      ),
      selectizeInput(
        "report_columns", "Columns to Include:",
        choices = names(df),
        selected = default_columns,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), maxOptions = 5000)
      ),
      textAreaInput(
        "report_notes", "Optional Report Note:",
        value = "",
        placeholder = "Add context, conclusions, or follow-up actions for this report...",
        width = "100%", height = "85px"
      ),
      uiOutput("report_page_estimate")
    ))
  }
  
  observeEvent(input$report_current_page, {
    open_report_builder(input$sidebar_tabs)
  })
  observeEvent(input$report_exec, open_report_builder("exec_overview"))
  observeEvent(input$report_atlas, open_report_builder("global_network"))
  observeEvent(input$report_kpi_board, open_report_builder("kpi_board"))
  observeEvent(input$report_sales, open_report_builder("monthly_sales"))
  observeEvent(input$report_buyer_activity, open_report_builder("buyer_activity"))
  observeEvent(input$report_performance, open_report_builder("sale_performance"))
  observeEvent(
    input$report_customer_performance,
    open_report_builder("customer_performance")
  )
  observeEvent(input$report_ar, open_report_builder("ar_tab"))
  observeEvent(input$report_statement, open_report_builder("client_statement"))
  observeEvent(input$report_tracker, open_report_builder("order_tracker"))
  observeEvent(input$report_inventory, open_report_builder("inventory_tab"))
  observeEvent(input$report_buyer, open_report_builder("buyer_tool"))
  observeEvent(input$report_contacts, open_report_builder("contacts_tab"))
  observeEvent(input$report_misc, open_report_builder("misc_tab"))
  
  # Direct one-click page summary exports.
  summary_export_pages <- c(
    summary_exec = "exec_overview",
    summary_atlas = "global_network",
    summary_sales = "monthly_sales",
    summary_performance = "sale_performance",
    summary_customer_performance = "customer_performance",
    summary_ar = "ar_tab",
    summary_statement = "client_statement",
    summary_tracker = "order_tracker",
    summary_inventory = "inventory_tab",
    summary_buyer = "buyer_tool",
    summary_contacts = "contacts_tab",
    summary_misc = "misc_tab"
  )
  
  report_metric_definitions <- function(page_key) {
    switch(
      page_key,
      sale_performance = tibble(
        Metric = c("Weighted Gross Margin", "Target Delta"),
        Definition = c(
          "Total gross profit divided by total revenue for the selected filters.",
          "Weighted gross margin minus the adjustable target, in percentage points."
        )
      ),
      ar_tab = tibble(
        Metric = c("Days Until Due", "Days Past Due", "Payment Timing"),
        Definition = c(
          "Calendar days remaining before the due date; zero once due or overdue.",
          "Calendar days since the due date; zero while current.",
          "Plain-language due status for the invoice."
        )
      ),
      order_tracker = tibble(
        Metric = c("Backordered Quantity", "Planned Lead Time"),
        Definition = c(
          "Ordered quantity not yet received.",
          "Calendar days from order date to scheduled delivery date."
        )
      ),
      inventory_tab = tibble(
        Metric = c("Available", "On Order", "Reorder Point", "Backordered Quantity"),
        Definition = c(
          "On-hand quantity minus allocated quantity, unless directly supplied.",
          "Quantity ordered from suppliers and not yet received.",
          "Availability threshold that indicates replenishment is required.",
          "Ordered quantity not yet received; it is not automatically overdue."
        )
      ),
      tibble(
        Metric = "Filtered Records",
        Definition = "Data shown after the current page and global filters are applied."
      )
    )
  }
  
  for (summary_output_id in names(summary_export_pages)) {
    local({
      output_id <- summary_output_id
      page_key <- unname(summary_export_pages[[summary_output_id]])
      
      output[[output_id]] <- downloadHandler(
        filename = function() {
          paste0(
            safe_report_filename(report_page_titles[[page_key]]),
            "_Summary_", Sys.Date(), ".xlsx"
          )
        },
        content = function(file) {
          if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("Install openxlsx first: install.packages('openxlsx')", call. = FALSE)
          }
          
          previous_page <- active_report_page()
          on.exit(active_report_page(previous_page), add = TRUE)
          active_report_page(page_key)
          
          summary_data <- isolate(report_summary_data())
          filter_data <- isolate(report_filter_data())
          detail_data <- isolate(report_detail_data())
          definitions <- report_metric_definitions(page_key)
          
          wb <- openxlsx::createWorkbook(creator = "Aurelis Dashboard")
          
          openxlsx::addWorksheet(wb, "Summary")
          logo_path <- report_logo_file()
          if (logo_path != "") {
            try(openxlsx::insertImage(
              wb, "Summary", logo_path,
              startRow = 1, startCol = 1, width = 2.25, height = .78
            ), silent = TRUE)
          }
          openxlsx::writeData(
            wb, "Summary",
            paste0("Aurelis — ", report_page_titles[[page_key]]),
            startRow = 1, startCol = 4
          )
          openxlsx::writeData(
            wb, "Summary",
            paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M")),
            startRow = 2, startCol = 4
          )
          openxlsx::setRowHeights(wb, "Summary", rows = 1:3, heights = c(30, 20, 8))
          if (nrow(summary_data) > 0) {
            openxlsx::writeDataTable(
              wb, "Summary", summary_data,
              startRow = 4, tableStyle = "TableStyleMedium2"
            )
          }
          openxlsx::setColWidths(wb, "Summary", cols = 1:2, widths = c(34, 42))
          
          openxlsx::addWorksheet(wb, "Active Filters")
          openxlsx::writeDataTable(
            wb, "Active Filters", filter_data,
            tableStyle = "TableStyleMedium2"
          )
          openxlsx::setColWidths(wb, "Active Filters", cols = 1:2, widths = c(34, 55))
          
          openxlsx::addWorksheet(wb, "Key Data")
          if (nrow(detail_data) > 0 && ncol(detail_data) > 0) {
            openxlsx::writeDataTable(
              wb, "Key Data", head(detail_data, 100),
              tableStyle = "TableStyleMedium2"
            )
            openxlsx::freezePane(wb, "Key Data", firstActiveRow = 2)
            openxlsx::setColWidths(
              wb, "Key Data", cols = seq_len(ncol(detail_data)), widths = "auto"
            )
          } else {
            openxlsx::writeData(wb, "Key Data", "No data matches the current selection.")
          }
          
          openxlsx::addWorksheet(wb, "Metric Definitions")
          openxlsx::writeDataTable(
            wb, "Metric Definitions", definitions,
            tableStyle = "TableStyleMedium2"
          )
          openxlsx::setColWidths(wb, "Metric Definitions", cols = 1:2, widths = c(30, 80))
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
    })
  }
  
  output$page_report_excel <- downloadHandler(
    filename = function() {
      paste0(
        safe_report_filename(if (is.null(input$report_title)) "Aurelis_Report" else input$report_title),
        "_", Sys.Date(), ".xlsx"
      )
    },
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Install the openxlsx package first: install.packages('openxlsx')", call. = FALSE)
      }
      
      detail <- report_scoped_data()
      if (!is.null(input$report_sort_direction) && input$report_sort_direction == "reverse" && nrow(detail) > 0) {
        detail <- detail[rev(seq_len(nrow(detail))), , drop = FALSE]
      }
      
      wb <- openxlsx::createWorkbook(creator = "Aurelis Dashboard")
      title_style <- openxlsx::createStyle(
        fontSize = 16, textDecoration = "bold", fontColour = "#1E3A8A"
      )
      header_style <- openxlsx::createStyle(
        fgFill = "#1E3A8A", fontColour = "#FFFFFF", textDecoration = "bold",
        border = "Bottom", borderColour = "#CBD5E1"
      )
      
      settings <- tibble(
        Setting = c("Report", "Dashboard Page", "Generated", "Content", "Record Scope", "PDF Page Selection", "Report Note"),
        Value = c(
          input$report_title,
          unname(report_page_titles[[active_report_page()]]),
          format(Sys.time(), "%Y-%m-%d %H:%M"),
          input$report_content,
          input$report_scope,
          input$report_page_spec,
          input$report_notes
        )
      )
      
      openxlsx::addWorksheet(wb, "Report Settings")
      logo_path <- report_logo_file()
      if (logo_path != "") {
        try(openxlsx::insertImage(
          wb, "Report Settings", logo_path,
          startRow = 1, startCol = 1, width = 2.25, height = .78
        ), silent = TRUE)
      }
      openxlsx::writeData(wb, "Report Settings", input$report_title, startRow = 1, startCol = 4)
      openxlsx::addStyle(wb, "Report Settings", title_style, rows = 1, cols = 4)
      openxlsx::writeDataTable(wb, "Report Settings", settings, startRow = 4, tableStyle = "TableStyleMedium2")
      openxlsx::setRowHeights(wb, "Report Settings", rows = 1:3, heights = c(30, 20, 8))
      openxlsx::setColWidths(wb, "Report Settings", cols = 1:4, widths = c(24, 70, 3, 40))
      
      openxlsx::addWorksheet(wb, "Active Filters")
      openxlsx::writeDataTable(wb, "Active Filters", report_filter_data(), tableStyle = "TableStyleMedium2")
      openxlsx::setColWidths(wb, "Active Filters", cols = 1:2, widths = c(28, 60))
      
      if (input$report_content %in% c("summary_details", "summary_only")) {
        openxlsx::addWorksheet(wb, "Summary")
        if (logo_path != "") {
          try(openxlsx::insertImage(
            wb, "Summary", logo_path,
            startRow = 1, startCol = 1, width = 2.1, height = .72
          ), silent = TRUE)
        }
        openxlsx::writeData(
          wb, "Summary", unname(report_page_titles[[active_report_page()]]),
          startRow = 1, startCol = 4
        )
        openxlsx::writeDataTable(
          wb, "Summary", report_summary_data(), startRow = 4,
          tableStyle = "TableStyleMedium4"
        )
        openxlsx::setRowHeights(wb, "Summary", rows = 1:3, heights = c(30, 20, 8))
        openxlsx::setColWidths(wb, "Summary", cols = 1:4, widths = c(30, 35, 3, 40))
      }
      
      if (input$report_content %in% c("summary_details", "details_only")) {
        openxlsx::addWorksheet(wb, "Detailed Records")
        if (nrow(detail) > 0 && ncol(detail) > 0) {
          openxlsx::writeDataTable(
            wb, "Detailed Records", detail,
            tableStyle = "TableStyleMedium2", withFilter = TRUE
          )
          openxlsx::freezePane(wb, "Detailed Records", firstActiveRow = 2)
          widths <- pmin(
            45,
            pmax(12, vapply(seq_along(detail), function(column_index) {
              values <- c(names(detail)[column_index], as.character(head(detail[[column_index]], 200)))
              values <- values[!is.na(values)]
              if (length(values) == 0) return(12)
              max(nchar(values), na.rm = TRUE) + 2
            }, numeric(1)))
          )
          openxlsx::setColWidths(wb, "Detailed Records", cols = seq_along(widths), widths = widths)
        } else {
          openxlsx::writeData(wb, "Detailed Records", "No records match the selected filters.")
        }
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$page_report_pdf <- downloadHandler(
    filename = function() {
      paste0(
        safe_report_filename(if (is.null(input$report_title)) "Aurelis_Report" else input$report_title),
        "_", Sys.Date(), ".pdf"
      )
    },
    content = function(file) {
      if (!requireNamespace("pagedown", quietly = TRUE)) {
        stop("Install the pagedown package first: install.packages('pagedown')", call. = FALSE)
      }
      
      detail <- report_scoped_data()
      if (!is.null(input$report_sort_direction) && input$report_sort_direction == "reverse" && nrow(detail) > 0) {
        detail <- detail[rev(seq_len(nrow(detail))), , drop = FALSE]
      }
      
      report_html <- build_aurelis_pdf_html(
        title = input$report_title,
        page_name = unname(report_page_titles[[active_report_page()]]),
        filters = report_filter_data(),
        summary = report_summary_data(),
        detail = detail,
        notes = input$report_notes,
        content_mode = input$report_content,
        rows_per_page = input$report_rows_per_page,
        orientation = input$report_orientation,
        page_specification = input$report_page_spec,
        paper_size = if (is.null(input$report_paper_size)) "A4" else input$report_paper_size
      )
      
      temporary_html <- tempfile(fileext = ".html")
      temporary_pdf <- tempfile(fileext = ".pdf")
      writeLines(report_html, temporary_html, useBytes = TRUE)
      
      pagedown::chrome_print(
        input = temporary_html,
        output = temporary_pdf,
        wait = 1
      )
      
      if (!file.copy(temporary_pdf, file, overwrite = TRUE)) {
        stop("The PDF report could not be copied to the download location.", call. = FALSE)
      }
    }
  )
  
  ##############################################################################
  # 4.13 DATA CONNECTIONS & PROCESS - Server Logic
  ##############################################################################
  
  output$connection_mode <- renderText({ data_version(); "SYNTHETIC DEMO" })
  output$connection_access_config <- renderText({ data_version(); "Local CSV package" })
  output$connection_qb_config <- renderText({ data_version(); "Disabled in public demo" })
  output$connection_fallback <- renderText({ data_version(); "Production access disabled" })
  
  connection_tests <- reactiveValues(
    access = "Not tested",
    quickbooks = "Not tested",
    quickbooks_online = "Not tested"
  )
  
  observeEvent(input$test_access_connection, {
    connection_tests$access <- "Testing..."
    result <- tryCatch(
      {
        tables <- list_access_tables()
        paste0("Connected — ", length(tables), " synthetic dataset(s) available")
      },
      error = function(error) paste0("Failed — ", conditionMessage(error))
    )
    connection_tests$access <- result
  })
  
  observeEvent(input$test_qb_connection, {
    connection_tests$quickbooks <- "Testing..."
    result <- tryCatch(
      {
        tables <- list_quickbooks_tables()
        paste0("Connected — ", length(tables), " synthetic dataset(s) available")
      },
      error = function(error) paste0("Failed — ", conditionMessage(error))
    )
    connection_tests$quickbooks <- result
  })
  
  observeEvent(input$test_qbo_connection, {
    connection_tests$quickbooks_online <- "Authorizing..."
    result <- tryCatch(
      {
        qb_sdk_authorize()
        "Connected — External Accounting authorization succeeded"
      },
      error = function(error) paste0("Failed — ", conditionMessage(error))
    )
    connection_tests$quickbooks_online <- result
  })
  
  output$connection_test_status <- renderUI({
    data_version()
    status_row <- function(label, value) {
      success <- str_detect(value, regex("^Connected", ignore_case = TRUE))
      failed <- str_detect(value, regex("^Failed", ignore_case = TRUE))
      badge_color <- if (success) "#DCFCE7" else if (failed) "#FEE2E2" else "#E2E8F0"
      text_color <- if (success) "#166534" else if (failed) "#991B1B" else "#334155"
      
      div(
        style = paste0(
          "padding:8px 10px;margin-bottom:7px;border-radius:7px;background:",
          badge_color, ";color:", text_color, ";"
        ),
        tags$strong(paste0(label, ": ")),
        value
      )
    }
    
    tagList(
      status_row("Synthetic Dataset", connection_tests$access),
      status_row("External Systems", "Disabled — public demo is isolated")
    )
  })
  
  output$connection_source_table <- renderDT({
    data_version()
    df <- source_registry_table() %>%
      mutate(Loaded_At = format(Loaded_At, "%Y-%m-%d %H:%M:%S"))
    
    datatable(
      df,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  connection_query_templates <- tibble(
    Dataset=c(
      "DAILY_PO","REVENUE","AR","INVENTORY","WAREHOUSE_INVENTORY",
      "CONTACTS","DIRECTORY","SELLER_CATALOG","GEOGRAPHY"
    ),
    Demo_File=c(
      "Aurelis_Daily_PO.csv","Aurelis_Revenue.csv","Aurelis_Accounts_Receivable.csv",
      "Aurelis_Inventory.csv","Aurelis_Warehouse.csv","Aurelis_Contacts.csv",
      "Aurelis_Directory.csv","Aurelis_Seller_Catalog.csv","Aurelis_Geography.csv"
    ),
    Purpose=c(
      "Purchase-order volume, value, customers and buyer activity",
      "Revenue, cost, gross profit, country and sales performance",
      "Invoice aging, outstanding balances and client statements",
      "Order lines, delivery planning, supplier analysis and backorders",
      "On-hand, allocated, available and reorder intelligence",
      "Fictional customer contacts",
      "Fictional customer, buyer and supplier relationship records",
      "Searchable supplier products and service capabilities",
      "Customer, supplier and representative locations for the interactive world Atlas"
    ),
    Isolation_Status=rep("Synthetic only — no production connection",9)
  )
  
  output$connection_schema_diagnostics <- renderDT({
    data_version()
    
    messages <- AURELIS_DATA_DIAGNOSTICS$messages
    df <- if (length(messages) == 0) {
      tibble(
        Status = "Ready",
        Dataset_or_Field = "Adaptive import layer",
        Detail = "No schema substitutions were required for the currently loaded public-demo files."
      )
    } else {
      tibble(
        Status = "Adapted",
        Dataset_or_Field = paste0("Import note ", seq_along(messages)),
        Detail = messages
      )
    }
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 12,
        searching = TRUE,
        scrollX = TRUE,
        dom = "tip"
      )
    )
  })
  
  output$connection_query_template_table <- renderDT({
    data_version()
    datatable(
      connection_query_templates,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$misc_save_note, {
    showNotification("Note saved for this session.", type = "message", duration = 3)
  })
  
} # end server

################################################################################
# SECTION 5: LAUNCH APPLICATION
################################################################################

shinyApp(ui = ui, server = server)
