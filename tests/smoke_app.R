app <- source("app.R")$value
stopifnot(inherits(app,"shiny.appobj"))
rendered <- htmltools::renderTags(ui)
html <- paste(rendered$head,rendered$html)
stopifnot(grepl("lab-dataset",html,fixed=TRUE),grepl("workspace.css",html,fixed=TRUE),
  grepl("aurelis-help-button",html,fixed=TRUE),grepl("aurelis-help.js",html,fixed=TRUE))
ids <- regmatches(html,gregexpr(' id="[^"]+"',html))[[1]]
duplicates <- unique(ids[duplicated(ids)])
if(length(duplicates)) stop(paste("Duplicate UI IDs:",paste(duplicates,collapse=", ")))
message("PASS: full application loads, workspace assets registered, no duplicate UI IDs.")
