# Existing optional dependencies used by the dashboard's download handlers.
missing <- c("openxlsx", "pagedown")
missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) utils::install.packages(missing, repos = "https://cloud.r-project.org", type = "binary")
stopifnot(requireNamespace("openxlsx", quietly = TRUE), requireNamespace("pagedown", quietly = TRUE))
message("Existing Excel and PDF export dependencies are available.")
