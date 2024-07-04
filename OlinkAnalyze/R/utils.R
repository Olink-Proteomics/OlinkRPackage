#' Help function checking that the olink_platform is acceptable.
#'
#' @param x The name of the Olink platform. One of `Explore 3072`, `Explore HT`,
#' `Target 96`, `Target 48`, `Flex` or `Focus`.
#' @param broader_platform Name of the broader Olink platform. One of `qPCR` or
#' `NGS`.
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
    check_olink_broader_platform(x = broader_platform)
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

#' Help function checking that the Olink data_type is acceptable.
#'
#' @param x The name of the Olink data type. One of `NPX`, `Quantified` or `Ct`.
#' @param broader_platform Name of the broader Olink platform. One of `qPCR` or
#' `NGS`.
#'
#' @return
#' Nothing if data_type is ok, otherwise an error.
#'
check_olink_data_type <- function(x,
                                  broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)
  if (!is.null(broader_platform)) {
    check_olink_broader_platform(x = broader_platform)
  }

  # check data_type ----

  # filter the global variable accepted_olink_platforms to have a collection
  # of data types available.
  if (is.null(broader_platform)) {

    olink_quant_methods <- accepted_olink_platforms

  } else {

    olink_quant_methods <- accepted_olink_platforms |>
      dplyr::filter(
        .data[["broader_platform"]] == .env[["broader_platform"]]
      )
  }

  olink_quant_methods <- olink_quant_methods |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unlist() |>
    unique()

  # Throw an error if unexpected data_type
  if (!(x %in% olink_quant_methods)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink data type {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of: {olink_quant_methods}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking that the broader Olink platform is acceptable.
#'
#' @param x Name of the broader Olink platform. One of `qPCR` or `NGS`.
#'
#' @return
#' Nothing if broader Olink platform is ok, otherwise an error.
#'
check_olink_broader_platform <- function(x) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # check broader platform ----

  if (!(x %in% unique(accepted_olink_platforms$broader_platform))) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink broader platform
        {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of:
        {unique(accepted_olink_platforms$broader_platform)}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Utility function removing columns with all values NA from a tibble or an
#' arrow object.
#'
#' @param df A tibble or an arrow object.
#'
#' @return The same data frame as input without all-NA columns.
#'
remove_all_na_cols <- function(df) {

  # input check ----

  check_is_arrow_or_tibble(df = df,
                           error = TRUE)

  # identify all NA cols ----

  na_cols <- sapply(df, \(x) sum(is.na(x)) == nrow(df))
  na_cols <- na_cols[na_cols == TRUE]
  na_cols <- names(na_cols)

  # remove all NA cols ----

  if (length(na_cols) > 0L) {
    df <- df |>
      dplyr::select(
        -dplyr::all_of(na_cols)
      )
  }

  # return ----

  return(df)
}
