#' Help function checking with the olink_platform is acceptable
#'
#' @param x The name of the Olink platform. One of "Target 48", "Flex",
#' "Target 96", "Explore 3072", "Explore HT" or "Focus".
#' @param broader_platform Name of the broader Olink platform. On of "qPCR" or
#' "NGS".
#'
#' @return
#' Nothing if platform is ok, otherwise an error.
#'
check_olink_platform <- function(x,
                                 broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)
  if (!is.null(broader_platform)) {
    check_is_scalar_character(string = broader_platform,
                              error = TRUE)
  }

  # check broader platform ----

  if (!is.null(broader_platform)
      && !(broader_platform %in% unique(accepted_olink_platforms$broader_platform))) { # nolint object_usage_linter

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink broader platform
        {.arg {rlang::caller_arg(broader_platform)}}!",
        "i" = "Expected one of:
        {unique(accepted_olink_platforms$broader_platform)}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check platform ----

  # filter the global variable accepted_olink_platforms to have a collection
  # of platforms available.
  if (is.null(broader_platform)) {

    olink_platforms <- accepted_olink_platforms

  } else {

    olink_platforms <- accepted_olink_platforms |>
      dplyr::filter(.data[["broader_platform"]] == .env[["broader_platform"]])

  }

  # Throw an error if unexpected platform
  if (!(x %in% olink_platforms$name)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink platform {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of: {olink_platforms$name}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
