#' Get names of current Olink platforms.
#'
#' @param broader_platform Name of the \var{broader_platform} to filter the
#' Olink platforms for. If set to `NULL`, all Olink platforms are returned.
#'
#' @return A character vector with names of Olink platforms.
#'
get_olink_platforms <- function(broader_platform = NULL) {

  # if broader_platform is null, then we want all platforms
  olink_broad_platforms <- get_olink_broader_platforms()

  if (is.null(broader_platform)) {
    broader_platform <- olink_broad_platforms
  } else {
    check_olink_broader_platform(x = broader_platform)
  }

  # return relevant Olink platforms
  olk_platforms <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["broader_platform"]] %in% .env[["broader_platform"]]
    ) |>
    dplyr::pull(
      .data[["name"]]
    ) |>
    sort()

  return(olk_platforms)
}

#' Get names of current qPCR Olink platforms.
#'
#' @return A character vector with names of qPCR Olink platforms.
#'
get_olink_qpcr_platforms <- function() {
  get_olink_platforms(broader_platform = "qPCR")
}

#' Get names of current NGS Olink platforms.
#'
#' @return A character vector with names of NGS Olink platforms.
#'
get_olink_ngs_platforms <- function() {
  get_olink_platforms(broader_platform = "NGS")
}

#' Help function checking that \var{olink_platform} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Name of Olink platform. One of
#' `r ansi_collapse_quot(x = get_olink_platforms())`.
#' @param broader_platform Name of the broader Olink platform. One of
#' `r ansi_collapse_quot(x = get_olink_broader_platforms())`.
#'
#' @return
#' `NULL` if platform is expected, otherwise an error.
#'
check_olink_platform <- function(x,
                                 broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant platforms ----

  olink_platforms <- get_olink_platforms(broader_platform = broader_platform)

  # Throw an error if unexpected platform
  if (!(x %in% olink_platforms)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink platform {.val {x}}!",
        "i" = "Expected one of: {.val {get_olink_platforms()}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Get names of current broader Olink platforms.
#'
#' @return A character vector with names of the broader Olink platforms.
#'
get_olink_broader_platforms <- function() {
  accepted_olink_platforms |>
    dplyr::pull(
      .data[["broader_platform"]]
    ) |>
    unique() |>
    sort()
}

#' Help function checking that the broader Olink platform is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Name of the broader Olink platform. One of
#' `r ansi_collapse_quot(x = get_olink_broader_platforms())`.
#'
#' @return
#' `NULL` if broader Olink platform is expected, otherwise an error.
#'
check_olink_broader_platform <- function(x) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # check broader platform ----

  if (!(x %in% unique(accepted_olink_platforms$broader_platform))) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink broader platform: {.val {x}}!",
        "i" = "Expected one of:
        {.val {get_olink_broader_platforms()}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Get names of current Olink quantification methods.
#'
#' @param broader_platform Name of the \var{broader_platform} to filter the
#' Olink platforms for. If set to `NULL`, all Olink platforms are returned.
#'
#' @return A character vector with names of Olink quantification methods.
#'
get_olink_data_types <- function(broader_platform = NULL) {

  # if broader_platform is null, then we want all platforms
  olink_broad_platforms <- get_olink_broader_platforms()

  if (is.null(broader_platform)) {
    broader_platform <- olink_broad_platforms
  } else {
    check_olink_broader_platform(x = broader_platform)
  }

  # return relevant Olink platforms
  olk_quant_methods <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["broader_platform"]] %in% .env[["broader_platform"]]
    ) |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unlist() |>
    unique() |>
    sort()

  return(olk_quant_methods)
}

#' Get names of current data types for qPCR Olink platforms.
#'
#' @return A character vector with names of the quantification methods for qPCR
#' Olink platforms.
#'
get_olink_qpcr_data_types <- function() {
  get_olink_data_types(broader_platform = "qPCR")
}

#' Get names of current data types for NGS Olink platforms.
#'
#' @return A character vector with names of the quantification methods for NGS
#' Olink platforms.
#'
get_olink_ngs_data_types <- function() {
  get_olink_data_types(broader_platform = "NGS")
}

#' Help function checking that \var{data_type} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x The name of the Olink data type. One of
#' `r ansi_collapse_quot(x = get_olink_data_types())`.
#' @param broader_platform Name of the broader Olink platform. One of
#' `r ansi_collapse_quot(x = get_olink_broader_platforms())`.
#'
#' @return
#' `NULL` if data_type is expected, otherwise an error.
#'
check_olink_data_type <- function(x,
                                  broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant platforms ----

  olink_quant_methods <- get_olink_data_types(
    broader_platform = broader_platform
  )

  # Throw an error if unexpected data_type
  if (!(x %in% olink_quant_methods)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink data type {.val x}!",
        "i" = "Expected one of: {.val {get_olink_data_types()}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
