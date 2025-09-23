#' Check if running inside testthat
#'
#' @return TRUE if running in testthat/CI, FALSE otherwise
is_testing <- function() {
  # testthat >= 3.0 sets internal variable
  if (exists(".is_testing", envir = asNamespace("testthat"), inherits = FALSE)) {
    return(get(".is_testing", envir = asNamespace("testthat")))
  }
  # fallback
  "testthat" %in% loadedNamespaces() && !interactive()
}
