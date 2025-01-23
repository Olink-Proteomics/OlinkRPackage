#' Help function utilizing functions from \code{\link{read_npx_format}} and
#' \code{\link{read_npx_wide}} to streamline \code{\link{read_npx_legacy}}
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output file in wide format. Expecting file
#' extensions
#' `r accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))] |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param out_df The class of output data frame. One of "tibble" (default) or
#' "arrow" for ArrowObject.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR" & grepl("Target", .data[["name"]])) |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["quant_method"]] != "Ct") |> dplyr::pull(.data[["quant_method"]]) |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param data_type_no_accept Character vector of data types that should be
#' rejected (default = "Ct").
#'
#' @return A list of objects containing the following:
#' \itemize{
#' \item \strong{olink_platform}: auto-detected Olink platform. One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR" & grepl("Target", .data[["name"]])) |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' \item \strong{long_format}: auto-detected Olink format. Should always be
#' "FALSE".
#' \item \strong{data_type}: auto-detected Olink data type. One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["quant_method"]] != "Ct") |> dplyr::pull(.data[["quant_method"]]) |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' \item \strong{df_split}: list of 2 tibbles. Top matrix from the Olink wide
#' file, and middle combined with bottom matrix.
#' \item \strong{npxs_v}: Olink NPX software version.
#' \item \strong{bottom_mat_v}: bottom matrix version based on
#' \var{olink_wide_bottom_matrix}.
#' \item \strong{format_spec}: specifications of the wide format based on
#' \var{olink_wide_spec}.
#' \item \strong{accept_platforms}: accepted Olink platforms and some
#' specifications, derived from \var{accepted_olink_platforms}.
#' }
#'
read_npx_legacy_help <- function(file,
                                 out_df,
                                 olink_platform = NULL,
                                 data_type = NULL,
                                 data_type_no_accept = c("Ct")) {
  # check input ----

  check_is_character(string = data_type_no_accept,
                     error = TRUE)

  # check that file can be processed ----

  # send the input file to read_npx_format with legacy = TRUE so that it returns
  # information on the file format, the data type and olink platform, as well as
  # the data frame itself. This is done to utilize the tools we have already
  # developed to detect various features of the data and to read it in.

  list_format <- read_npx_format(file = file,
                                 out_df = out_df,
                                 sep = NULL,
                                 long_format = NULL,
                                 olink_platform = olink_platform,
                                 data_type = data_type,
                                 quiet = TRUE,
                                 legacy = TRUE)

  # checks ----

  # legacy wide files are supported only

  # only wide files are accepted
  if (list_format$long_format == TRUE) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only wide format files!",
        "i" = "Detected long format!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # only Target 48 and Target 96 platforms are accepted
  accept_platforms <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["broader_platform"]] == "qPCR"
      & grepl("Target", .data[["name"]])
    )

  if (!(list_format$olink_platform %in% accept_platforms$name)) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only data from
        {.val {accept_platforms$name}}!",
        "i" = "Detected {.val {list_format$olink_platform}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # only NPX and Quantified values
  accept_quant <- accepted_olink_platforms |>
    # keep only relevant quantification methods
    dplyr::filter(
      .data[["broader_platform"]] == "qPCR"
      & .data[["name"]] == list_format$olink_platform
    ) |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unlist() |>
    # remove Ct as is in not supported
    (\(.x) .x[!(.x %in% data_type_no_accept)])()
  if (!(list_format$data_type %in% accept_quant)) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only {.val {accept_quant}} data ran
        on {.val {list_format$olink_platform}}!",
        "i" = "Detected {.val {list_format$data_type}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # split data frame into sub-matrices ----

  # format acceptable specifications
  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == list_format$data_type
    )

  # split data frame into header, top, middle and bottom matrices. Here we used
  # the tools we have already developed for read_npx_wide.
  df_split <- read_npx_wide_split_row(
    df = list_format$df,
    file = file,
    data_type = list_format$data_type,
    format_spec = format_spec
  )

  # get NPX software version ----

  # function was developed for read_npx_wide
  npxs_v <- read_npx_wide_npxs_version(
    df = df_split$df_head
  )

  # get bottom matrix version ----

  bottm_mat_v <- read_npx_wide_bottom_version(
    df = df_split$df_bottom,
    file = file,
    data_type = list_format$data_type,
    olink_platform = list_format$olink_platform
  ) |>
    dplyr::pull(
      .data[["version"]]
    ) |>
    unique() |>
    as.integer()

  # modify variables ----

  # combine df_mid with df_bottom
  df_na <- rep(NA_character_, ncol(df_split$df_mid)) |>
    t() |>
    dplyr::as_tibble(
      .name_repair = "minimal"
    )
  names(df_na) <- names(df_split$df_mid)

  df_split$df_data <- df_split$df_mid |>
    dplyr::bind_rows(
      df_na
    ) |>
    dplyr::bind_rows(
      df_split$df_bottom
    )

  # remove df_head from the list
  df_split <- df_split[-which(names(df_split) %in% c("df_head",
                                                     "df_mid",
                                                     "df_bottom"))]

  # return ----

  return(
    list(
      olink_platform = list_format$olink_platform,
      long_format = list_format$long_format,
      data_type = list_format$data_type,
      df_split = df_split,
      npxs_v = npxs_v,
      bottom_mat_v = bottm_mat_v,
      format_spec = format_spec,
      accept_platforms = accept_platforms |>
        dplyr::filter(
          .data[["name"]] == list_format$olink_platform
        )
    )
  )
}

#' Help function ensuring \code{\link{read_npx_legacy}} works
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output file in wide format. Expecting file
#' extensions
#' `r accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))] |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param df_top Top matrix of Olink dataset in wide format.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR" & grepl("Target", .data[["name"]])) |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["quant_method"]] != "Ct") |> dplyr::pull(.data[["quant_method"]]) |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param bottom_mat_v Version of the rows in the bottom matrix of the Olink
#' file in wide format based on the local environment variable
#' \var{olink_wide_bottom_matrix}.
#'
#' @return `NULL` unless unsupported file is detected.
#'
read_npx_legacy_check <- function(file,
                                  df_top,
                                  data_type,
                                  olink_platform,
                                  bottom_mat_v) {
  # check input ----

  check_is_dataset(df = df_top,
                   error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  # help vars ----

  format_spec <- olink_wide_spec |> # nolint
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # test columns are sorted ----

  # Expected order of panels, assays, int ctrl, plate_id, qc_warning and
  # deviations from int ctrl:
  # - assay + int ctrl per panel
  # - plate_id per panel
  # - qc_warning per panel
  # - inc and det ctrl per panel

  df_top_t <- t(df_top) # transpose the wide df
  colnames(df_top_t) <- df_top_t[1L, ] # add colnames
  df_top_t <- df_top_t |>
    # convert to tibble
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    # remove first row becuase it contained colnames
    dplyr::slice(
      2L:dplyr::n()
    ) |>
    # remove unnecessary columns
    dplyr::select(
      -dplyr::any_of(
        c("col_index", "Unit")
      )
    ) |>
    # create a new column to mark the type of entry.
    # this is used to mark:
    # - assays as "assay"
    # - internal controls as "int_ctrl"
    # - plate_id and qc_warning as "pid_qc"
    # - deviations from internal controls as "dev_int_ctrl"
    dplyr::mutate(
      assay_type = dplyr::case_when(
        !is.na(.data[["OlinkID"]]) ~ "assay",
        .data[["Assay"]] %in% unlist(format_spec$top_matrix_assay_int_ctrl) ~
          "int_ctrl",
        .data[["Assay"]] %in% unlist(format_spec$top_matrix_assay_dev_int_ctrl)
        & .data[["Uniprot ID"]] %in%
          unlist(format_spec$top_matrix_uniprot_dev_int_ctrl) ~ "dev_int_ctrl",
        .data[["Assay"]] %in% unlist(format_spec$top_matrix_assay_labels) ~
          "pid_qc",
        TRUE ~ NA_character_,
        .default = NA_character_
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("Panel", "assay_type")
      )
    ) |>
    dplyr::distinct()

  # get number of panels
  n_panel <- df_top_t |>
    dplyr::filter(
      .data[["assay_type"]] == "assay"
    ) |>
    dplyr::pull(
      .data[["Panel"]]
    ) |>
    unique() |>
    length()

  # reconstruct the column "assay_type" from df_top_t to make sure it matches
  assay_type_expected <- c("assay")
  if ("int_ctrl" %in% df_top_t$assay_type) {
    assay_type_expected <- c(assay_type_expected, "int_ctrl")
  }
  assay_type_expected <- rep(assay_type_expected, times = n_panel)
  assay_type_expected <- c(assay_type_expected,
                           rep("pid_qc", times = n_panel))
  if ("dev_int_ctrl" %in% df_top_t$assay_type) {
    assay_type_expected <- c(assay_type_expected,
                             rep("dev_int_ctrl", times = n_panel))
  }

  if (!identical(df_top_t$assay_type, assay_type_expected)) {

    cli::cli_abort(
      message = c(
        "x" = "Columns of the wide file {.file {file}} should be sorted!",
        "i" = "Expected:",
        "i" = "- {.val {c(\"Assays\", \"Internal controls\")}} for each panel",
        "i" = "- {.val {\"Plate ID\"}} for each panel",
        "i" = "- {.val {\"QC Warning\"}} for each panel",
        "i" = "- {.val {\"QC Deviation from median\"}} for each panel",
        "i" = "Consider disabling the {.arg legacy} argument!"
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )

  }

  # check if dev int ctrl are accompanied by int ctrl ----

  dev_incl_int_ctrl <- any("dev_int_ctrl" %in% df_top_t$assay_type) &&
    !("int_ctrl" %in% df_top_t$assay_type)

  if (dev_incl_int_ctrl) {

    cli::cli_abort(
      message = c(
        "x" = "File {.file {file}} contains {.val {\"QC Deviation from
        median\"}} but lacks {.val {\"Internal controls\"}}!",
        "i" = "When {.val {\"QC Deviation from median\"}} are reported, then
        {.val {\"Internal controls\"}} are required too!",
        "i" = "Consider disabling the {.arg legacy} argument!"
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )

  }

  # check for unsupported file versions (NPX T48 v2 or NPX T96 v3) ----

  is_npx_t48_v2 <- olink_platform == "Target 48" &&
    data_type == "NPX" && bottom_mat_v == 2L
  is_npx_t96_v3 <- olink_platform == "Target 96" &&
    data_type == "NPX" && bottom_mat_v == 3L

  if (is_npx_t48_v2 || is_npx_t96_v3) {

    cli::cli_abort(
      message = c(
        "x" = "File {.file {file}} contains bottom matrix with unsupported
        labels!",
        "i" = "Consider disabling the {.arg legacy} argument!"
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )

  }

}

#' Olink legacy function for reading NPX or absolute quantification data in wide
#' format into R from qPCR Olink products.
#'
#' @description
#' This implementation of read_NPX does not cover the latest versions of Olink
#' files in wide format. Specifically, it supports:
#' \itemize{
#' \item \strong{Target 96} output files in wide format \strong{(T96 reports
#' only NPX)} with the bottom matrix containing one of the following
#' combinations of rows:
#' \itemize{
#' \item `r olink_wide_bottom_matrix |> dplyr::filter(.data[["olink_platform"]] == "Target 96" & .data[["version"]] <= 1L) |> dplyr::pull(.data[["variable_name"]]) |> cli::ansi_collapse()`. # nolint
#' \item `r olink_wide_bottom_matrix |> dplyr::filter(.data[["olink_platform"]] == "Target 96" & .data[["version"]] %in% c(0L, 2L)) |> dplyr::pull(.data[["variable_name"]]) |> cli::ansi_collapse()`. # nolint
#' }
#' \item \strong{Target 48} output files in wide format \strong{NPX} with the
#' bottom matrix containing the following rows:
#' `r olink_wide_bottom_matrix |> dplyr::filter(.data[["olink_platform"]] == "Target 48" & .data[["data_type"]] == "NPX" & .data[["version"]] <= 1L) |> dplyr::pull(.data[["variable_name"]]) |> cli::ansi_collapse()`. # nolint
#' \item \strong{Target 48} output files in wide format \strong{absolute
#' Quantification} with the bottom matrix containing the following rows:
#' `r olink_wide_bottom_matrix |> dplyr::filter(.data[["olink_platform"]] == "Target 48" & .data[["data_type"]] == "Quantified") |> dplyr::pull(.data[["variable_name"]]) |> cli::ansi_collapse()`. # nolint
#' }
#'
#' This function would accept data exported in wide format from Olink NPX
#' Signature 1.7.1 or earlier, or NPX Manager.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt;
#'   Christoffer Cambronero;
#'   Boxi Zhang;
#'   Olof Mansson;
#'   Marianne Sandin
#'
#' @param file Path to Olink software output file in wide format. Expecting file
#' extensions
#' `r accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))] |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param out_df The class of output data frame. One of "tibble" (default) or
#' "arrow" for ArrowObject.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR" & grepl("Target", .data[["name"]])) |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default),
#' `r accepted_olink_platforms |> dplyr::filter(.data[["quant_method"]] != "Ct") |> dplyr::pull(.data[["quant_method"]]) |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. "TRUE" (default) to not print
#' and "FALSE" to print.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx_format_read}}
#'   \code{\link{read_npx_format_get_format}}
#'   \code{\link{read_npx_format_get_platform}}
#'   \code{\link{read_npx_format_get_quant}}
#'
read_npx_legacy <- function(file,
                            out_df = "tibble",
                            olink_platform = NULL,
                            data_type = NULL,
                            quiet = TRUE) {
  cli::cli_warn(
    c("You are using the function read_npx_legacy()!",
      "This function imports Olink data in wide format from MS Excel files
      exported by \"Olink NPX Manager\" or \"Olink NPX Signature\" version
      earlier than 1.8.0, but fails for data exported from more recent software
      versions.")
  )

  # check input ----

  check_is_scalar_boolean(bool = quiet,
                          error = TRUE)

  # the remaining variables are checked in read_npx_legacy_help

  # read_npx_legacy_help ----

  # call read_npx_legacy_help to get information about the input file as well as
  # import the dataset.
  # At the same time, read_npx_legacy_help will also check all the values of the
  # input.
  df_format_list <- read_npx_legacy_help(file = file,
                                         out_df = out_df,
                                         olink_platform = olink_platform,
                                         data_type = data_type,
                                         data_type_no_accept = c("Ct"))

  # additional checks of the input data ----

  read_npx_legacy_check(file = file,
                        df_top = df_format_list$df_split$df_top,
                        data_type = df_format_list$data_type,
                        olink_platform = df_format_list$olink_platform,
                        bottom_mat_v = df_format_list$bottom_mat_v)

  # Check if the data type is NPX or absolute concentration ----

  is_npx <- ifelse(df_format_list$data_type == "NPX",
                   TRUE,
                   FALSE)

  # Message of data type detected

  if (!quiet) {
    cli::cli_inform(
      message = "{df_format_list$olink_platform} data in wide form detected.",
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # help vars ----

  # in earlier versions of Olink wide files, the row Normalization on the bottom
  # matrix was absent. In more recent versions, it was included. This flag marks
  # its presence or absence.
  norm_flag <-  FALSE
  # number of assays per panel. 45 for T48 and 92 for T96.
  # Newer T48 panels might not work.
  base_index <- ifelse(df_format_list$olink_platform == "Target 96", 92L, 45L)

  # datasets ----

  # metadata: top and bottom matrices
  meta_dat <- df_format_list$df_split$df_top
  names(meta_dat) <- c("Name", 2L:ncol(meta_dat))

  # data: middle matrix
  dat <- df_format_list$df_split$df_data
  names(dat) <- c("Name", 2L:ncol(dat))

  # update datasets ----

  # locations of internal controls
  control_index <- stringr::str_detect(
    string = meta_dat[2L, ],
    pattern = "Det Ctrl|Inc Ctrl|Inc Ctrl 1|Inc Ctrl 2|Ext Ctrl"
  )

  # update meta_dat
  meta_dat[4L, 1L] <- "SampleID"
  meta_dat[4L, control_index] <- meta_dat[2L, control_index]
  meta_dat[3L, control_index] <- "-"

  # total number of internal controls
  nr_controls <- sum(control_index)

  # number of deviations from internal controls
  nr_deviations <- stringr::str_detect(
    string = meta_dat[2L, ],
    pattern = "QC Deviation from median"
  ) |>
    sum()

  # number of panels
  nr_panel <- (ncol(meta_dat)
               - 1L
               - nr_deviations
               - nr_controls) / (base_index + 2L)

  # df of plate cols and number of plates
  plates <- dat[, ((base_index * nr_panel)
                   + nr_controls
                   + 2L):(ncol(dat)
                          - nr_panel
                          - nr_deviations)] |>
    dplyr::distinct() |>
    tidyr::drop_na()

  nr_plates <- plates |>
    dplyr::pull(
      dplyr::all_of(1L)
    ) |>
    unique() |>
    length()

  # update base_index
  base_index <- ifelse(nr_controls > 0L,
                       base_index + nr_controls / nr_panel,
                       base_index)

  # add bottom matrix to top matrix `meta_dat` ----

  # missing frequency
  meta_dat <- meta_dat |>
    dplyr::bind_rows(
      dat |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["Name"]],
            pattern = "Missing Data freq."
          )
        )
    )

  # if it is quantified data
  if (is_npx == FALSE) {
    meta_dat <- meta_dat |>
      # lower limit of quantification
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "LLOQ"
            )
          )
      ) |>
      # upper limit of quantification
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "ULOQ"
            )
          )
      ) |>
      # assay warning
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Assay warning"
            )
          )
      ) |>
      # plate lower quantifiable level
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Lowest quantifiable level"
            )
          )
      ) |>
      # plate LOD
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Plate LOD|PlateLOD|Plate_LOD"
            )
          )
      )
  } else { # if it is NPX data
    # LOD
    meta_dat <- meta_dat |>
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "LOD"
            )
          )
      )
  }

  # normalization
  meta_dat <- meta_dat |>
    dplyr::bind_rows(
      dat |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["Name"]],
            pattern = "Normalization"
          )
        )
    )

  # remove the meta data from dat ----

  norm_flag <- grepl(pattern = "Normalization", x = dat$Name) |>
    any()

  nbr_meta_data_rows_bottom <- ifelse(is_npx == TRUE,
                                      3L,
                                      4L + (3L * nr_plates))
  nbr_meta_data_rows_bottom <- ifelse(norm_flag == TRUE,
                                      nbr_meta_data_rows_bottom,
                                      nbr_meta_data_rows_bottom - 1L)

  dat <- dat[c(-1L * (nrow(dat) - nbr_meta_data_rows_bottom):nrow(dat)), ]

  # sample id and index vector
  sample_id <- dat$Name
  index_nr <- seq_len(length(sample_id))

  # initialize list for outputs ----

  # Initiate lists for later use
  panel_data <- list() ##NPX values to be stored
  qc_list <- list()    ##QC data
  meta_data_list <- list() ## meta data
  panel_list <- list()  ## combination of panel data and QC
  assay_name_list <- list()
  panel_list_long <- list()

  # wide to long ----

  # Construct a list of tibbles that match the long format
  for (i in seq_len(nr_panel)) {

    panel_data[[i]] <- dat[, (2L + ((i - 1L) * base_index)):
                             ((base_index + 1L) + ((i - 1L) * base_index))]

    if (nr_deviations == 0L) {

      qc_list[[i]] <- dat[, c(
        (2L + (nr_panel * base_index) + (i - 1L)),
        (2L + (nr_panel * base_index) + (i - 1L)) + nr_panel
      )]

      meta_data_list[[i]] <- meta_dat[, c(
        (2L + ((i - 1L) * base_index)):
          ((base_index + 1L) + ((i - 1L) * base_index)),
        (2L + (nr_panel * base_index) + (i - 1L)),
        (2L + (nr_panel * base_index) + (i - 1L)) + nr_panel
      )]

    } else {

      qc_list[[i]] <- dat[, c(
        2L + (nr_panel * base_index) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + nr_panel,
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel) + i
      )]

      meta_data_list[[i]] <- meta_dat[, c(
        (2L + (i - 1L) * base_index):(base_index + 1L + (i - 1L) * base_index),
        2L + (nr_panel * base_index) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + nr_panel,
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel),
        2L + (nr_panel * base_index) + (i - 1L) + (3L * nr_panel)
      )]

      meta_data_list[[i]][4L, (base_index + 3L)] <- "QC Deviation Inc Ctrl"
      meta_data_list[[i]][4L, (base_index + 4L)] <- "QC Deviation Det Ctrl"

    }

    meta_data_list[[i]][4L, (base_index + 1L)] <-
      meta_data_list[[i]][2L, (base_index + 1L)]
    meta_data_list[[i]][4L, (base_index + 2L)] <-
      meta_data_list[[i]][2L, (base_index + 2L)]

    panel_list[[i]] <- dplyr::bind_cols(panel_data[[i]], qc_list[[i]])
    colnames(panel_list[[i]]) <- unlist(meta_data_list[[i]][4L, ])

    panel_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))] <-
      lapply(panel_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))],
             function(.x) {
               stringr::str_replace_all(
                 string = .x,
                 pattern = c("#" = "",
                             "," = ".",
                             "No Data" = NA_character_,
                             "> ULOQ" = NA_character_,
                             "< LLOQ" = NA_character_)
               ) |>
                 as.numeric()
             })

    # Remove the last two columns since they contain redundant meta data and
    # will only cause warnings
    meta_data_list[[i]] <-
      meta_data_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))]

    assay_name_list[[i]] <- tidyr::tibble(
      ID = meta_data_list[[i]][4L, ] |> as.character(),
      Name = meta_data_list[[i]][2L, ] |> as.character(),
      UniProt = meta_data_list[[i]][3L, ] |> as.character(),
      Panel = meta_data_list[[i]][1L, ] |> as.character()
    )

    if (is_npx == TRUE) {

      assay_name_list[[i]] <- assay_name_list[[i]] |>
        dplyr::bind_cols(
          dplyr::tibble(
            MissingFreq = meta_data_list[[i]][5L, ] |> as.character(),
            LOD = meta_data_list[[i]][6L, ] |> as.numeric()
          )
        )

      if (norm_flag == TRUE) {

        assay_name_list[[i]] <- assay_name_list[[i]] |>
          dplyr::bind_cols(
            dplyr::tibble(
              Normalization = meta_data_list[[i]][7L, ] |> as.character()
            )
          )
      }

      panel_list_long[[i]] <- panel_list[[i]] |>
        dplyr::mutate(
          SampleID = sample_id,
          Index = index_nr
        ) |>
        tidyr::pivot_longer(
          -dplyr::any_of(
            c("SampleID", "QC Warning", "Plate ID", "Index",
              "QC Deviation Inc Ctrl", "QC Deviation Det Ctrl")
          ),
          names_to = "Assay",
          values_to = "NPX"
        ) |>
        dplyr::left_join(
          assay_name_list[[i]],
          by = c("Assay" = "ID")
        ) |>
        dplyr::select(
          dplyr::all_of(
            c("SampleID", "Index", "OlinkID" = "Assay", "UniProt",
              "Assay" = "Name", "MissingFreq", "Panel", "PlateID" = "Plate ID",
              "QC_Warning" = "QC Warning", "LOD", "NPX")
          ),
          dplyr::any_of(
            c("Assay_Warning", "Normalization", "QC Deviation Inc Ctrl",
              "QC Deviation Det Ctrl")
          )
        )

    } else {
      for (j in 1:nr_plates) {

        assay_name_by_plate <- assay_name_list[[i]] |>
          dplyr::bind_cols(
            dplyr::tibble(
              Unit = meta_data_list[[i]][5L, ] |> as.character(),
              MissingFreq = meta_data_list[[i]][6L, ] |> as.character(),
              LLOQ = meta_data_list[[i]][7L, ] |> as.numeric(),
              ULOQ = meta_data_list[[i]][8L, ] |> as.numeric(),
              Assay_Warning = meta_data_list[[i]][(9L + j - 1L), ] |>
                as.character(),
              Plate_LQL = meta_data_list[[i]][(9L + nr_plates + j - 1L), ] |>
                as.numeric(),
              LOD = meta_data_list[[i]][(9L + (2L * nr_plates) + j - 1L), ] |>
                as.numeric()
            )
          )

        if (norm_flag == TRUE) {

          assay_name_by_plate <- assay_name_by_plate |>
            dplyr::bind_cols(
              dplyr::tibble(
                Normalization = meta_data_list[[i]][9L + (3L * nr_plates), ] |>
                  as.character()
              )
            )

        }

        panel_lst_long_idx <- ifelse(nr_plates > 1L,
                                     (i - 1L) * 2L + j,
                                     i)

        panel_list_long[[panel_lst_long_idx]] <- panel_list[[i]] |>
          dplyr::mutate(
            SampleID = sample_id,
            Index = index_nr
          ) |>
          tidyr::pivot_longer(
            -dplyr::any_of(
              c("SampleID", "QC Warning", "Plate ID", "Index",
                "QC Deviation Inc Ctrl", "QC Deviation Det Ctrl")
            ),
            names_to = "Assay",
            values_to = "NPX"
          ) |>
          dplyr::filter(
            .data[["Plate ID"]] == as.character(plates[j, i])
          ) |>
          dplyr::left_join(
            assay_name_by_plate,
            by = c("Assay" = "ID")
          ) |>
          dplyr::select(
            dplyr::all_of(
              c("SampleID", "Index", "OlinkID" = "Assay", "UniProt",
                "Assay" = "Name", "MissingFreq", "Panel",
                "PlateID" = "Plate ID", "QC_Warning" = "QC Warning", "LOD",
                "NPX", "Unit", "ULOQ", "LLOQ", "Plate_LQL", "Assay_Warning")
            ),
            dplyr::any_of(
              c("Normalization", "QC Deviation Inc Ctrl",
                "QC Deviation Det Ctrl")
            )
          )
      }
    }
  }

  # reshape and cleanup final dataset ----

  # rename columns LOD and NPX to Plate_LOD and Quantified_value, respectively.
  # if data is absolute quantification
  if (is_npx == FALSE) {
    panel_list_long <- lapply(
      panel_list_long,
      function(.x) {
        lookup_rename <- c("Plate_LOD" = "LOD",
                           "Quantified_value" = "NPX")
        .x |>
          dplyr::rename(
            dplyr::all_of(lookup_rename)
          )
      }
    )
  }

  df_long <- panel_list_long |>
    dplyr::bind_rows() |>
    # remove any row with SampleID = NA
    dplyr::filter(
      !is.na(.data[["SampleID"]])
    ) |>
    # extract Panel_Version
    dplyr::mutate(
      Panel_Version = stringr::str_replace_all(string = .data[["Panel"]],
                                               pattern = ".*\\(",
                                               replacement = "") |>
        stringr::str_replace_all(pattern = "\\)",
                                 replacement = "")
    ) |>
    # adjust Panel
    dplyr::mutate(
      Panel = stringr::str_replace_all(string = .data[["Panel"]],
                                       pattern = "\\(.*\\)",
                                       replacement = "") |>
        stringr::str_to_title() |>
        stringr::str_replace_all(pattern = "Target 96",
                                 replacement = "") |>
        stringr::str_replace_all(pattern = "Target 48",
                                 replacement = "") |>
        stringr::str_replace_all(pattern = "Olink",
                                 replacement = "") |>
        trimws(which = "left")
    ) |>
    # split panel into two columns for further adjustment
    tidyr::separate(
      col = .data[["Panel"]],
      sep = " ",
      into = c("Panel_Start", "Panel_End"),
      fill = "right"
    ) |>
    # cleanup Panel_End and recombine Panel_Start with Panel_End to Panel
    dplyr::mutate(
      Panel_End = dplyr::if_else(grepl("Ii", .data[["Panel_End"]]),
                                 stringr::str_to_upper(.data[["Panel_End"]]),
                                 .data[["Panel_End"]]),
      Panel_End = dplyr::if_else(is.na(.data[["Panel_End"]]),
                                 " ",
                                 .data[["Panel_End"]]),
      Panel = paste("Olink", .data[["Panel_Start"]], .data[["Panel_End"]]) |>
        trimws(which = "right")
    ) |>
    # keep relevant columns
    dplyr::select(
      -dplyr::all_of(c("Panel_Start", "Panel_End"))
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("SampleID", "Index", "OlinkID", "UniProt", "Assay", "MissingFreq",
          "Panel", "Panel_Version", "PlateID", "QC_Warning")
      ),
      dplyr::any_of(
        c("Plate_LQL", "LOD", "Plat_LOD", "LLOQ", "ULOQ", "NPX",
          "Quantified_value", "Unit", "Assay_Warning", "Normalization")
      ),
      dplyr::matches(c("*Inc Ctrl*", "*Det Ctrl*"))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("NPX", "Quantified_value")),
        ~ as.numeric(.x)
      )
    )

  return(df_long)
}
