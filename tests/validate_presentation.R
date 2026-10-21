library(shiny)
source("R/workspace_engine.R")
source("R/analysis_workspace.R")
source("R/presentation_generator.R")
fixture <- data.frame(Date = as.Date(c("2026-01-01", "2026-01-15", "2026-02-01", "2026-02-10")),
  Revenue = c(10, 20, -5, NA), Country = c("A", "A", "B", "B"))
evidence <- ap_evidence(fixture, "Revenue", "Country", "Date", target = 30)
stopifnot(evidence$summary$value == 25, evidence$summary$usable == 3,
  evidence$summary$missing_or_nonfinite == 1, sum(evidence$segments$value) == 25,
  identical(evidence$periods$value, c(30, -5)), evidence$target == 30)
stopifnot(ap_evidence(fixture, "Revenue", aggregation = "mean")$summary$value == 25/3)
stopifnot(!nrow(ap_evidence(fixture, "Revenue", "Country", "Date", focus = "summary")$segments))
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
expect_error(ap_evidence(fixture, "Country"))
expect_error(ap_evidence(fixture[FALSE, ], "Revenue"))
expect_error(ap_evidence(transform(fixture, Revenue = NA_real_), "Revenue"))
settings <- list(template = "executive", title = "<script>alert(1)</script>", audience = "Executives",
  question = "Where should we focus?", context = "Test context", source = "Fixture", scope = "2026 Jan-Feb", units = "USD")
stopifnot(length(ap_templates()) == 12)
headings <- character()
for (template in names(ap_templates())) {
  settings$template <- template
  deck <- ap_draft(evidence, settings)
  stopifnot(length(deck$slides) >= 7, any(vapply(deck$slides, function(s) !is.null(s$chart), logical(1))))
  headings <- c(headings, deck$slides[[3]]$title)
}
stopifnot(length(unique(headings)) == 12)
file <- tempfile(fileext = ".html"); ap_export_html(deck, file)
html <- paste(readLines(file), collapse = "\n")
stopifnot(grepl("&lt;script&gt;alert", html, fixed = TRUE), !grepl("<script>alert(1)", html, fixed = TRUE),
  grepl("<svg", html, fixed = TRUE), grepl("Sources and limitations", html, fixed = TRUE))
unlink(file)
old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
Sys.setenv(OPENAI_API_KEY = "test-placeholder-not-a-real-key")
mock <- function(payload) {
  stopifnot(identical(payload$store, FALSE), !grepl("test-placeholder-not-a-real-key", payload$input, fixed = TRUE))
  slides <- lapply(deck$slides, function(s) list(title = s$title, bullets = as.list(s$bullets), notes = s$notes))
  list(output = list(list(type = "message", content = list(list(type = "output_text", text = jsonlite::toJSON(list(slides = slides), auto_unbox = TRUE))))))
}
assisted <- ap_ai(deck, "mock-model", request = mock)
stopifnot(grepl("AI-assisted", assisted$engine), identical(assisted$slides[[4]]$chart, deck$slides[[4]]$chart))
expect_error(ap_ai(deck, "mock-model", request = function(payload) list(output = list())))
if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key)
shiny::testServer(ap_server, args = list(sources = list(lab = reactive(fixture))), {
  session$setInputs(source = "lab", template = "executive", audience = "Executives", title = "Test", question = "Focus?", context = "Test", units = "USD",
    measure = "Revenue", group = "Country", date = "Date", aggregation = "sum", focus = c("summary", "segments", "trend"), engine = "local", date_filter = FALSE)
  session$setInputs(generate = 1)
  stopifnot(deck()$evidence$summary$value == 25)
  session$setInputs(slide = "1", slide_title = "Edited", bullets = "One\nTwo", speaker = "Notes", save = 1)
  stopifnot(deck()$slides[[1]]$title == "Edited")
  session$setInputs(date_filter = TRUE, range = as.Date(c("2026-01-01", "2026-01-31")))
  stopifnot(grepl("changed", output$stale$html))
  session$setInputs(generate = 2)
  stopifnot(deck()$evidence$summary$value == 30)
})
message("PASS: 12 templates, exact aggregates, negative/missing values, focus controls, escaped HTML export, mocked AI validation, slide editing, snapshot staleness and date filtering.")
