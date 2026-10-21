# Pure R analysis functions. No source data is modified by the workspace.
aw_numeric <- function(x) is.numeric(x) && !inherits(x, c("Date", "POSIXt"))
aw_finite <- function(x) x[is.finite(x)]
aw_label <- function(x) gsub("_", " ", x, fixed = TRUE)
aw_number <- function(x, digits = 2) {
  ifelse(is.finite(x), formatC(x, digits = digits, format = "f", big.mark = ","), "-")
}
aw_dates <- function(data) names(data)[vapply(data, inherits, logical(1), what = "Date")]
aw_measures <- function(data) names(data)[vapply(data, aw_numeric, logical(1))]

aw_catalog <- function(root) {
  paths <- list.files(root, pattern = "\\.(csv|tsv|xlsx|xls|rds|json)$", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  # Only files physically inside the data directory may be opened.
  base <- paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  paths <- paths[startsWith(tolower(paths), tolower(base))]
  relative <- substring(paths, nchar(base) + 1L)
  stats <- file.info(paths)
  data.frame(id = relative, path = paths, name = aw_label(sub("\\.[^.]+$", "", relative)),
             bytes = stats$size, modified = stats$mtime, stringsAsFactors = FALSE)
}

aw_read <- function(path, sheet = NULL, extension = tools::file_ext(path), delimiter = ",") {
  ext <- tolower(extension)
  if (!file.exists(path)) stop("This file is no longer available. Refresh the file catalog.")
  if (file.info(path)$size > 100 * 1024^2) stop("Open a file smaller than 100 MB in this interactive workspace.")
  content <- NULL
  if (ext %in% c("csv", "tsv")) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    content <- rawToChar(bytes)
    if (length(bytes) && !validUTF8(content)) content <- iconv(content, from="windows-1252", to="UTF-8")
    Encoding(content) <- "UTF-8"
    content <- sub("^\ufeff", "", content)
  }
  data <- switch(ext,
    csv = utils::read.csv(text = content, sep = delimiter, check.names = FALSE, stringsAsFactors = FALSE,
                         encoding = "UTF-8", na.strings = c("", "NA", "N/A", "NULL")),
    tsv = utils::read.delim(text = content, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet)),
    xls = as.data.frame(readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet)),
    rds = readRDS(path),
    json = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite to open JSON tables.")
      jsonlite::fromJSON(path)
    }, stop("Supported tables: CSV, TSV, Excel, RDS data frames and tabular JSON."))
  if (!is.data.frame(data)) stop("This file does not contain a single tabular dataset.")
  if (!ncol(data)) stop("The file has no columns.")
  names(data) <- make.unique(trimws(sub("^\ufeff", "", names(data))))
  for (name in names(data)) {
    x <- data[[name]]
    if (is.list(x)) data[[name]] <- vapply(x, function(z) paste(z, collapse = "; "), character(1))
    if (inherits(x, "POSIXt")) data[[name]] <- as.Date(x)
    if (is.character(x)) {
      good <- !is.na(x) & nzchar(x)
      if (any(good) && all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x[good]))) {
        dates <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
        if (all(!is.na(dates[good]))) data[[name]] <- dates
      }
    }
  }
  data
}

aw_profile <- function(data) {
  if (!ncol(data)) return(data.frame())
  do.call(rbind, lapply(names(data), function(name) {
    x <- data[[name]]
    missing <- is.na(x) | (is.character(x) & trimws(x) == "")
    numeric <- aw_numeric(x)
    v <- if (numeric) aw_finite(x) else numeric(0)
    outliers <- if (length(v) > 3) {
      q <- stats::quantile(v, c(.25, .75), names = FALSE)
      sum(v < q[1] - 1.5 * diff(q) | v > q[2] + 1.5 * diff(q))
    } else 0L
    data.frame(Field = name, Type = class(x)[1], Missing = sum(missing),
      `Missing %` = if (length(x)) round(100 * mean(missing), 2) else 0,
      Distinct = length(unique(x[!missing])), `IQR outliers` = outliers,
      Minimum = if (length(v)) min(v) else NA_real_,
      Median = if (length(v)) stats::median(v) else NA_real_,
      Maximum = if (length(v)) max(v) else NA_real_, check.names = FALSE)
  }))
}

aw_describe <- function(x, confidence = .95) {
  x <- aw_finite(x); n <- length(x)
  if (!n) stop("Choose a measure with at least one finite observation.")
  m <- mean(x); s <- if (n > 1) stats::sd(x) else NA_real_
  centered <- x - m; m2 <- mean(centered^2)
  half <- if (n > 1) stats::qt((1 + confidence)/2, n - 1) * s/sqrt(n) else NA_real_
  q <- stats::quantile(x, c(.01, .05, .25, .5, .75, .95, .99), names = FALSE)
  data.frame(Statistic = c("Observations", "Mean", "Median", "Standard deviation", "Variance",
    "Minimum", "Maximum", "IQR", "Coefficient of variation (%)", "Skewness (moment)",
    "Excess kurtosis (moment)", "Mean CI lower", "Mean CI upper", "1st percentile", "5th percentile",
    "25th percentile", "75th percentile", "95th percentile", "99th percentile"),
    Value = c(n, m, stats::median(x), s, s^2, min(x), max(x), stats::IQR(x),
      if (m != 0) 100*s/abs(m) else NA_real_, if (m2 > 0) mean(centered^3)/m2^1.5 else NA_real_,
      if (m2 > 0) mean(centered^4)/m2^2 - 3 else NA_real_, m-half, m+half, q[c(1,2,3,5,6,7)]))
}

aw_filter <- function(data, filters = list(), search = "", keep_missing = TRUE, deduplicate = FALSE) {
  keep <- rep(TRUE, nrow(data))
  for (field in intersect(names(filters), names(data))) {
    choice <- filters[[field]]; x <- data[[field]]
    if (is.null(choice) || !length(choice)) next
    missing <- is.na(x)
    if (inherits(x, "Date") || aw_numeric(x)) {
      if (length(choice) != 2 || anyNA(choice)) next
      ok <- x >= choice[1] & x <= choice[2]
    } else if (length(choice) == 1 && startsWith(choice, "text:")) {
      needle <- substring(choice, 6)
      if (!nzchar(needle)) next
      ok <- grepl(tolower(needle), tolower(as.character(x)), fixed = TRUE)
    } else ok <- as.character(x) %in% choice
    ok[missing] <- keep_missing
    keep <- keep & ok
  }
  data <- data[keep, , drop = FALSE]
  if (nzchar(trimws(search)) && nrow(data)) {
    needle <- tolower(trimws(search))
    matches <- lapply(data, function(x) !is.na(x) & grepl(needle, tolower(as.character(x)), fixed = TRUE))
    data <- data[Reduce(`|`, matches), , drop = FALSE]
  }
  if (deduplicate) data <- unique(data)
  data
}

aw_outliers <- function(data, measure, policy = "keep") {
  if (policy == "keep" || !measure %in% aw_measures(data)) return(data)
  x <- data[[measure]]; v <- aw_finite(x)
  if (length(v) < 4) return(data)
  if (policy == "iqr") {
    q <- stats::quantile(v, c(.25,.75)); limits <- q + c(-1,1)*1.5*diff(q)
  } else {
    s <- stats::sd(v); if (!is.finite(s) || s == 0) return(data)
    limits <- mean(v) + c(-3,3)*s
  }
  data[!is.finite(x) | (x >= limits[1] & x <= limits[2]), , drop = FALSE]
}

# Evaluate only a small, explicit arithmetic language. Never eval user code.
aw_formula <- function(text, data) {
  if (!nzchar(trimws(text))) return(NULL)
  if (nchar(text) > 500) stop("Keep the expression under 500 characters.")
  expr <- parse(text = text, keep.source = FALSE)
  if (length(expr) != 1L) stop("Enter one arithmetic expression.")
  allowed <- list(`+` = `+`, `-` = `-`, `*` = `*`, `/` = `/`, `^` = `^`,
    `(` = function(x) x, abs = abs, sqrt = sqrt, log = log, log10 = log10,
    round = round, pmin = pmin, pmax = pmax)
  walk <- function(node, depth = 0L) {
    if (depth > 20) stop("Expression is too deeply nested.")
    if (is.numeric(node) && length(node) == 1) return(node)
    if (is.symbol(node)) {
      key <- as.character(node)
      if (!key %in% aw_measures(data)) stop(paste("Unknown numeric field:", key))
      return(data[[key]])
    }
    if (!is.call(node) || !is.symbol(node[[1]])) stop("Use numeric fields and arithmetic functions only.")
    fn <- as.character(node[[1]])
    if (!fn %in% names(allowed)) stop(paste("Function is not allowed:", fn))
    do.call(allowed[[fn]], lapply(as.list(node)[-1], walk, depth = depth + 1L))
  }
  value <- walk(expr[[1]])
  if (!is.numeric(value) || !length(value) %in% c(1L, nrow(data))) stop("The expression must return a numeric column.")
  value <- rep_len(value, nrow(data)); value[!is.finite(value)] <- NA_real_
  value
}

aw_test <- function(data, measure, method = "one", group = "", groups = character(),
                    benchmark = 0, alpha = .05, alternative = "two.sided") {
  x <- data[[measure]]
  if (!aw_numeric(x)) stop("Choose a numeric outcome.")
  if (!alpha > 0 || !alpha < 1) stop("Alpha must be between zero and one.")
  if (method == "one") {
    v <- aw_finite(x); if (length(v) < 3) stop("At least three observations are required.")
    result <- stats::t.test(v, mu = benchmark, alternative = alternative, conf.level = 1-alpha)
    effect <- (mean(v) - benchmark)/stats::sd(v)
  } else if (method == "normality") {
    v <- aw_finite(x)
    if (length(v) < 3 || length(v) > 5000) stop("Shapiro-Wilk requires 3-5,000 finite observations. Filter the dataset first.")
    result <- stats::shapiro.test(v); effect <- NA_real_
  } else {
    if (!group %in% names(data) || !length(groups)) stop("Select a grouping field and comparison groups.")
    keep <- is.finite(x) & !is.na(data[[group]]) & as.character(data[[group]]) %in% groups
    values <- x[keep]; labels <- droplevels(factor(as.character(data[[group]][keep]), levels = groups))
    if (any(table(labels) < 3) || nlevels(labels) < 2) stop("Each selected group needs at least three finite observations.")
    if (method %in% c("welch", "rank")) {
      if (nlevels(labels) != 2) stop("Choose exactly two groups for this test.")
      a <- values[labels == levels(labels)[1]]; b <- values[labels == levels(labels)[2]]
      if (method == "welch") {
        result <- stats::t.test(a, b, alternative = alternative, conf.level = 1-alpha)
        pooled <- sqrt(((length(a)-1)*stats::var(a)+(length(b)-1)*stats::var(b))/(length(a)+length(b)-2))
        effect <- (mean(a)-mean(b))/pooled
      } else { result <- stats::wilcox.test(a,b, alternative = alternative, exact = FALSE); effect <- NA_real_ }
    } else if (method == "anova") {
      result <- stats::oneway.test(values ~ labels, var.equal = FALSE); effect <- NA_real_
    } else { result <- stats::kruskal.test(values ~ labels); effect <- NA_real_ }
  }
  list(test = result, effect = effect, alpha = alpha,
       conclusion = if (result$p.value < alpha) "Evidence against the null hypothesis" else "Insufficient evidence against the null hypothesis")
}

aw_series <- function(data, date, measure, granularity = "month", aggregation = "sum") {
  if (length(date)!=1 || !date %in% aw_dates(data)) stop("Choose a date field. This table may not have a recognized date column.")
  if (length(measure)!=1 || !measure %in% aw_measures(data)) stop("Choose a numeric measure.")
  good <- !is.na(data[[date]]) & is.finite(data[[measure]])
  if (!any(good)) stop("No complete date / measure pairs remain.")
  dates <- as.Date(lubridate::floor_date(data[[date]][good], granularity, week_start = 1))
  frame <- data.frame(period = dates, value = data[[measure]][good])
  fn <- if (aggregation == "mean") mean else sum
  frame <- stats::aggregate(value ~ period, frame, fn)
  frame[order(frame$period), , drop = FALSE]
}

aw_forecast <- function(series, horizon = 6L, confidence = .95) {
  if (nrow(series) < 6) stop("At least six observed periods are required.")
  if (any(!is.finite(series$value))) stop("The series contains non-finite values.")
  # Honest, interpretable baseline; intervals assume independent normal residuals.
  fit <- stats::lm(value ~ index, data = data.frame(value = series$value, index = seq_len(nrow(series))))
  predictions <- stats::predict(fit, newdata = data.frame(index = nrow(series)+seq_len(horizon)),
                                interval = "prediction", level = confidence)
  list(fit = fit, prediction = predictions)
}

aw_simulate <- function(x, shift = 0, volatility = 1, iterations = 5000, seed = 42L, confidence = .95) {
  x <- aw_finite(x)
  if (length(x) < 3) stop("At least three finite observations are required.")
  if (length(iterations)!=1 || !is.finite(iterations) || iterations<100 || iterations>10000) stop("Use 100 to 10,000 iterations.")
  if (!is.finite(shift) || !is.finite(volatility) || volatility<0 || confidence<=0 || confidence>=1) stop("Invalid scenario parameters.")
  # Restore the caller's random stream so analysis cannot alter other app behavior.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  center <- mean(x)*(1+shift/100)
  draws <- center + (sample(x, iterations, replace = TRUE)-mean(x))*volatility
  tail <- stats::quantile(draws, 1-confidence, names = FALSE)
  list(draws = draws, summary = data.frame(Mean = mean(draws), Median = stats::median(draws),
    Lower = stats::quantile(draws, (1-confidence)/2, names = FALSE),
    Upper = stats::quantile(draws, (1+confidence)/2, names = FALSE),
    `Lower tail threshold` = tail, `Lower tail mean` = mean(draws[draws <= tail]),
    `Probability below zero` = mean(draws < 0), check.names = FALSE))
}
