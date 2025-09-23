#' Check if running inside testthat
#'
#' @return TRUE if running in testthat/CI, FALSE otherwise
is_testing <- function() {
  # testthat >= 3.0 sets internal variable
  if (exists(x = ".is_testing",
             envir = asNamespace("testthat"),
             inherits = FALSE)) {
    get_is_testing <- get(".is_testing", envir = asNamespace("testthat"))
    return(get_is_testing)
  }
  # fallback
  get_is_testing <- "testthat" %in% loadedNamespaces() && !interactive()
  return(get_is_testing)
}
