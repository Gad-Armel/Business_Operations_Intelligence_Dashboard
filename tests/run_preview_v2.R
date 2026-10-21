# Local visual review: load once and keep synthetic source files unchanged.
try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)
preview <- new.env(parent = globalenv())
app <- source("app.R", local = preview, encoding = "UTF-8")$value
preview$aurelis_multiuser_log <- function(...) invisible(TRUE)
app$staticPaths <- list(`/` = httpuv::staticPath(normalizePath("www"), indexhtml = FALSE, fallthrough = TRUE))
shiny::runApp(app, host = "127.0.0.1", port = 3876, launch.browser = FALSE)
