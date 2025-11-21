# handles >= x.y.z; >= x.y.z, < x.y.z; etc.

skip_unless_r_compat <- function(range) {
  # If testthat isn't even installed, there's nothing sensible to do.
  if (!requireNamespace("testthat", quietly = TRUE)) {
    return(invisible(NULL))
  }

  # If we're on a new enough testthat, just delegate.
  if ("skip_unless_r" %in% getNamespaceExports("testthat")) {
    return(testthat::skip_unless_r(range))
  }

  # ---- Fallback implementation for older testthat ----

  cur <- getRversion()

  # Split on comma: e.g. ">= 4.2, < 4.5.2"
  parts <- strsplit(range, ",")[[1]]
  parts <- trimws(parts)

  # Helper: check a single constraint like "< 4.5.2"
  satisfies_one <- function(expr) {
    # expr is e.g. "< 4.5.2" or ">= 4.2"
    # Supported operators: <, <=, >, >=, ==, !=
    m <- regexec("^(<=|>=|<|>|==|!=)\\s*([0-9]+(\\.[0-9]+){0,2})$", expr)
    reg <- regmatches(expr, m)[[1]]

    if (length(reg) == 0L) {
      warning(
        "skip_unless_r_compat(): unsupported range expression '", expr,
        "'. Treating it as always TRUE on old testthat."
      )
      return(TRUE)
    }

    op <- reg[2]
    ver <- reg[3]

    cmp <- utils::compareVersion(as.character(cur), ver)

    switch(
      op,
      "<"  = cmp <  0L,
      "<=" = cmp <= 0L,
      ">"  = cmp >  0L,
      ">=" = cmp >= 0L,
      "==" = cmp == 0L,
      "!=" = cmp != 0L,
      TRUE
    )
  }

  ok <- TRUE
  for (p in parts) {
    if (!satisfies_one(p)) {
      ok <- FALSE
      break
    }
  }

  if (!ok) {
    testthat::skip(paste0("Requires R ", range, " (compat for older testthat)"))
  }

  invisible(NULL)
}
