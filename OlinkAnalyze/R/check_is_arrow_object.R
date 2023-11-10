#' Help function to check if a variable is an R6 ArrowObject.
#'
#' @param var Variable to check.
#'
#' @return An error if the variable is not an R6 ArrowObject.
#'
check_is_arrow_object <- function(var) {

  if (!all(c("ArrowObject", "R6") %in% class(var))) {

    # error if the file does not exist
    cli::cli_abort(
      c(
        "x" = "{.arg {rlang::caller_arg(var)}} is not an R6 ArrowObject!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
