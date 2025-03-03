#' Get names of all Olink platforms.
#'
#' @author
#'   Klev Diamanti
#'
#' @return A character vector with names of all Olink platforms.
#'
get_all_olink_platforms <- function() {
  # return all Olink platforms
  olk_all_platforms <- accepted_olink_platforms |>
    dplyr::pull(
      .data[["name"]]
    ) |>
    unique() |>
    sort()

  return(olk_all_platforms)
}

#' Get names of selected Olink platforms.
#'
#' @author
#'   Klev Diamanti
#'
#' @param broad_platform Name of the \var{broad_platform} to filter the
#' Olink platforms for. If `NULL` all relevant Olink platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_broader_platforms())` is
#' expected.
#' @param data_type Name of the \var{data_type} to filter the Olink platforms
#' for. If `NULL` all relevant Olink platforms are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_data_types())` is expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink platforms
#' for. If `NULL` all relevant Olink platforms are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return A character vector with names of Olink platforms filtered by
#' \var{broad_platform}, \var{data_type}, and \var{quant_type}.
#'
get_olink_platforms <- function(broad_platform = NULL,
                                data_type = NULL,
                                quant_type = NULL) {

  # if broad_platform is null, then we want all platforms
  if (is.null(broad_platform)) {
    broad_platform <- get_all_olink_broader_platforms()
  } else {
    check_olink_broader_platform(x = broad_platform)
  }

  # if data_type is null, then we want all platforms
  if (is.null(data_type)) {
    data_type <- get_all_olink_data_types()
  } else {
    check_olink_data_type(x = data_type)
  }

  # if quant_type is null, then we want all platforms
  if (is.null(quant_type)) {
    quant_type <- get_all_olink_quant_types()
  } else {
    check_olink_quant_type(x = quant_type)
  }

  # return relevant Olink platforms
  olk_platforms <- accepted_olink_platforms |>
    dplyr::select(
      dplyr::all_of(
        c("name", "broader_platform", "quant_method", "quant_type")
      )
    ) |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::filter(
      .data[["broader_platform"]] %in% .env[["broad_platform"]]
      & .data[["quant_method"]] %in% .env[["data_type"]]
      & .data[["quant_type"]] %in% .env[["quant_type"]]
    ) |>
    dplyr::pull(
      .data[["name"]]
    ) |>
    unique() |>
    sort()

  return(olk_platforms)
}

#' Help function checking that \var{olink_platform} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Name of Olink platform. One of
#' `r ansi_collapse_quot(get_olink_platforms())`.
#' @param broad_platform Name of the \var{broad_platform} to filter the
#' Olink platforms for. If `NULL` all relevant Olink platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_broader_platforms())` is
#' expected.
#' @param data_type Name of the \var{data_type} to filter the Olink platforms
#' for. If `NULL` all relevant Olink platforms are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_data_types())` is expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink platforms
#' for. If `NULL` all relevant Olink platforms are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return
#' `NULL` if platform is expected, otherwise an error.
#'
check_olink_platform <- function(x,
                                 broad_platform = NULL,
                                 data_type = NULL,
                                 quant_type = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant platforms ----

  olink_platforms <- get_olink_platforms(
    broad_platform = broad_platform,
    data_type = data_type,
    quant_type = quant_type
  )

  # check platform ----

  if (length(olink_platforms) == 0L) {

    cli::cli_abort(
      message = c(
        "x" = "No Olink platform detected!",
        "i" = "Please check input criteria {.arg broad_platform},
        {.arg data_type}, and {.arg quant_type}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (!(x %in% olink_platforms)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink platform {.val {x}}!",
        "i" = "Expected one of:
        {.val {get_olink_platforms(broad_platform = broad_platform,
        data_type = data_type, quant_type = quant_type)}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Get names of all broader Olink platforms.
#'
#' @author
#'   Klev Diamanti
#'
#' @return A character vector with names of all Olink broader platforms.
#'
get_all_olink_broader_platforms <- function() {
  # return all Olink broader platforms
  olk_all_broad_platforms <- accepted_olink_platforms |>
    dplyr::pull(
      .data[["broader_platform"]]
    ) |>
    unique() |>
    sort()

  return(olk_all_broad_platforms)
}

#' Get names of selected broader Olink platforms.
#'
#' @author
#'   Klev Diamanti
#'
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' broader platforms for. If `NULL` all relevant Olink broader platforms are
#' returned, otherwise one of `r ansi_collapse_quot(get_olink_platforms())` is
#' expected.
#' @param data_type Name of the \var{data_type} to filter the Olink broader
#' platforms for. If `NULL` all relevant Olink broader platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_data_types())` is expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink broader
#' platforms for. If `NULL` all relevant Olink broader platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return A character vector with names of Olink broader platforms filtered by
#' \var{platform_name}, \var{data_type}, and \var{quant_type}.
#'
get_olink_broader_platforms <- function(platform_name = NULL,
                                        data_type = NULL,
                                        quant_type = NULL) {

  # if platform_name is null, then we want all broad platforms
  if (is.null(platform_name)) {
    platform_name <- get_olink_platforms()
  } else {
    check_olink_platform(x = platform_name)
  }

  # if data_type is null, then we want all broad platforms
  if (is.null(data_type)) {
    data_type <- get_all_olink_data_types()
  } else {
    check_olink_data_type(x = data_type)
  }

  # if quant_type is null, then we want all broad platforms
  if (is.null(quant_type)) {
    quant_type <- get_olink_quant_types()
  } else {
    check_olink_quant_type(x = quant_type)
  }

  # return relevant Olink broader platforms
  olk_broad_platforms <- accepted_olink_platforms |>
    dplyr::select(
      dplyr::all_of(
        c("name", "broader_platform", "quant_method", "quant_type")
      )
    ) |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::filter(
      .data[["name"]] %in% .env[["platform_name"]]
      & .data[["quant_method"]] %in% .env[["data_type"]]
      & .data[["quant_type"]] %in% .env[["quant_type"]]
    ) |>
    dplyr::pull(
      .data[["broader_platform"]]
    ) |>
    unique() |>
    sort()

  return(olk_broad_platforms)
}

#' Help function checking that \var{broad_platform} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Name of the broader Olink platform. One of
#' `r ansi_collapse_quot(get_olink_broader_platforms())`.
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' broader platforms for. If `NULL` all relevant Olink broader platforms are
#' returned, otherwise one of `r ansi_collapse_quot(get_olink_platforms())` is
#' expected.
#' @param data_type Name of the \var{data_type} to filter the Olink broader
#' platforms for. If `NULL` all relevant Olink broader platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_data_types())` is expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink broader
#' platforms for. If `NULL` all relevant Olink broader platforms are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return
#' `NULL` if broader Olink platform is expected, otherwise an error.
#'
check_olink_broader_platform <- function(x,
                                         platform_name = NULL,
                                         data_type = NULL,
                                         quant_type = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant broader platforms ----

  olink_broad_platforms <- get_olink_broader_platforms(
    platform_name = platform_name,
    data_type = data_type,
    quant_type = quant_type
  )

  # check broader platform ----

  if (length(olink_broad_platforms) == 0L) {

    cli::cli_abort(
      message = c(
        "x" = "No Olink broad platform detected!",
        "i" = "Please check input criteria {.arg platform_name},
        {.arg data_type}, and {.arg quant_type}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (!(x %in% olink_broad_platforms)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink broader platform: {.val {x}}!",
        "i" = "Expected one of:
        {.val {get_olink_broader_platforms(platform_name = platform_name,
        data_type = data_type, quant_type = quant_type)}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Get names of all Olink quantification methods (data types).
#'
#' @author
#'   Klev Diamanti
#'
#' @return A character vector with names of all Olink quantification methods
#' (data types).
#'
get_all_olink_data_types <- function() {
  # return all Olink quantification methods
  olk_all_quant_methods <- accepted_olink_platforms |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unique() |>
    sort()

  return(olk_all_quant_methods)
}

#' Get names of selected Olink quantification methods (data types).
#'
#' @author
#'   Klev Diamanti
#'
#' @param broad_platform Name of the \var{broad_platform} to filter the Olink
#' data types for. If `NULL` all relevant Olink data types are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_broader_platforms())` is
#' expected.
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' data types for. If `NULL` all relevant Olink data types are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_platforms())` is
#' expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink data types
#' for. If `NULL` all relevant Olink data types are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return A character vector with names of Olink quantification methods (data
#' types) filtered by \var{broad_platform}, \var{platform_name}, and
#' \var{quant_type}.
#'
get_olink_data_types <- function(broad_platform = NULL,
                                 platform_name = NULL,
                                 quant_type = NULL) {

  # if broad_platform is null, then we want all data types
  if (is.null(broad_platform)) {
    broad_platform <- get_olink_broader_platforms()
  } else {
    check_olink_broader_platform(x = broad_platform)
  }

  # if platform_name is null, then we want all data types
  if (is.null(platform_name)) {
    platform_name <- get_olink_platforms()
  } else {
    check_olink_platform(x = platform_name)
  }

  # if quant_type is null, then we want all data types
  if (is.null(quant_type)) {
    quant_type <- get_olink_quant_types()
  } else {
    check_olink_quant_type(x = quant_type)
  }

  # return relevant Olink quantification methods
  olk_quant_methods <- accepted_olink_platforms |>
    dplyr::select(
      dplyr::all_of(
        c("name", "broader_platform", "quant_method", "quant_type")
      )
    ) |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::filter(
      .data[["broader_platform"]] %in% .env[["broad_platform"]]
      & .data[["name"]] %in% .env[["platform_name"]]
      & .data[["quant_type"]] %in% .env[["quant_type"]]
    ) |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unique() |>
    sort()

  return(olk_quant_methods)
}

#' Help function checking that \var{data_type} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x The name of the Olink data type. One of
#' `r ansi_collapse_quot(get_olink_data_types())`.
#' @param broad_platform Name of the \var{broad_platform} to filter the Olink
#' data types for. If `NULL` all relevant Olink data types are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_broader_platforms())` is
#' expected.
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' data types for. If `NULL` all relevant Olink data types are returned,
#' otherwise one of `r ansi_collapse_quot(get_olink_platforms())` is
#' expected.
#' @param quant_type Name of the \var{quant_type} to filter the Olink data types
#' for. If `NULL` all relevant Olink data types are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_quant_types())` is expected.
#'
#' @return
#' `NULL` if quantification method (data type) is expected, otherwise an error.
#'
check_olink_data_type <- function(x,
                                  broad_platform = NULL,
                                  platform_name = NULL,
                                  quant_type = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant data types ----

  olink_quant_methods <- get_olink_data_types(
    broad_platform = broad_platform,
    platform_name = platform_name,
    quant_type = quant_type
  )

  # check data type ----

  if (length(olink_quant_methods) == 0L) {

    cli::cli_abort(
      message = c(
        "x" = "No Olink data type detected!",
        "i" = "Please check input criteria {.arg broad_platform},
        {.arg platform_name}, and {.arg quant_type}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (!(x %in% olink_quant_methods)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink data type {.val {x}}!",
        "i" = "Expected one of:
        {.val {get_olink_data_types(broad_platform = broad_platform,
        platform_name = platform_name, quant_type = quant_type)}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Get names of all Olink quantification types.
#'
#' @author
#'   Klev Diamanti
#'
#' @return A character vector with names of all Olink quantification types.
#'
get_all_olink_quant_types <- function() {
  # return all Olink quantification types
  olk_all_data_types <- accepted_olink_platforms |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::pull(
      .data[["quant_type"]]
    ) |>
    unique() |>
    sort()

  return(olk_all_data_types)
}

#' Get names of selected Olink quantification types.
#'
#' @author
#'   Klev Diamanti
#'
#' @param broad_platform Name of the \var{broad_platform} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_broader_platforms())` is expected.
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of `r ansi_collapse_quot(get_olink_platforms())`
#' is expected.
#' @param data_type Name of the \var{data_type} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of `r ansi_collapse_quot(get_olink_data_types())`
#' is expected.
#'
#' @return A character vector with names of Olink quantification types filtered
#' by \var{broad_platform}, \var{platform_name}, and \var{data_type}.
#'
get_olink_quant_types <- function(broad_platform = NULL,
                                  platform_name = NULL,
                                  data_type = NULL) {

  # if broad_platform is null, then we want all quant types
  if (is.null(broad_platform)) {
    broad_platform <- get_all_olink_broader_platforms()
  } else {
    check_olink_broader_platform(x = broad_platform)
  }

  # if platform_name is null, then we want all quant types
  if (is.null(platform_name)) {
    platform_name <- get_all_olink_platforms()
  } else {
    check_olink_platform(x = platform_name)
  }

  # if data_type is null, then we want all quant types
  if (is.null(data_type)) {
    data_type <- get_all_olink_data_types()
  } else {
    check_olink_data_type(x = data_type)
  }

  # return relevant Olink quantification types
  olk_data_types <- accepted_olink_platforms |>
    dplyr::select(
      dplyr::all_of(
        c("name", "broader_platform", "quant_method", "quant_type")
      )
    ) |>
    tidyr::unnest(
      cols = dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    dplyr::filter(
      .data[["broader_platform"]] %in% .env[["broad_platform"]]
      & .data[["name"]] %in% .env[["platform_name"]]
      & .data[["quant_method"]] %in% .env[["data_type"]]
    ) |>
    dplyr::pull(
      .data[["quant_type"]]
    ) |>
    unique() |>
    sort()

  return(olk_data_types)
}

#' Help function checking that \var{data_type} is expected.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x The name of the Olink quantification type. One of
#' `r ansi_collapse_quot(get_olink_quant_types())`.
#' @param broad_platform Name of the \var{broad_platform} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of
#' `r ansi_collapse_quot(get_olink_broader_platforms())` is expected.
#' @param platform_name Name of the \var{platform_name} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of `r ansi_collapse_quot(get_olink_platforms())`
#' is expected.
#' @param data_type Name of the \var{data_type} to filter the Olink
#' quantification types for. If `NULL` all relevant Olink quantification types
#' are returned, otherwise one of `r ansi_collapse_quot(get_olink_data_types())`
#' is expected.
#'
#' @return
#' `NULL` if quantification type is expected, otherwise an error.
#'
check_olink_quant_type <- function(x,
                                   broad_platform = NULL,
                                   platform_name = NULL,
                                   data_type = NULL) {
  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # get relevant data types ----

  olink_quant_types <- get_olink_quant_types(
    broad_platform = broad_platform,
    platform_name = platform_name,
    data_type = data_type
  )

  # check data type ----

  if (length(olink_quant_types) == 0L) {

    cli::cli_abort(
      message = c(
        "x" = "No Olink quantification type detected!",
        "i" = "Please check input criteria {.arg broad_platform},
        {.arg platform_name}, and {.arg data_type}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (!(x %in% olink_quant_types)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink quantification type {.val {x}}!",
        "i" = "Expected one of:
        {.val {get_olink_quant_types(broad_platform = broad_platform,
        platform_name = platform_name, data_type = data_type)}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }
}
