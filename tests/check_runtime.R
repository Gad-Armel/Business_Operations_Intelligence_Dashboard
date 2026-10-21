cat("R libraries:\n", paste(.libPaths(), collapse = "\n"), "\n")
for (package in c("openxlsx", "pagedown", "chromote", "processx", "zip", "jsonlite", "rmarkdown", "knitr")) {
  cat(package, if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package)) else "MISSING", "\n")
}
