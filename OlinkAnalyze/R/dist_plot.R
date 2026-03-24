#' Function to plot the NPX distribution by panel
#'
#' Generates boxplots of NPX vs. SampleID colored by QC_Warning (default)
#' or any other grouping variable
#' and faceted by Panel using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format. Must have columns SampleID, NPX and
#' Panel
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param color_g Character value indicating which column to use as fill color.
#' (default: QC_Warning)
#' @param ... Color option passed to specify color order.
#' @return An object of class "ggplot" which displays NPX distribution
#' for each sample per panel
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#'
#' olink_dist_plot(npx_data1,
#' color_g = "QC_Warning")
#'
#' }
#' @importFrom dplyr filter mutate group_by ungroup if_else
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes scale_x_discrete geom_boxplot xlab facet_wrap
#' @importFrom stringr str_replace str_detect
#' @importFrom rlang ensym

olink_dist_plot <- function(df,
                            check_log = NULL,
                            color_g = "QC_Warning", # nolint: object_name_linter
                            ...) {

  #checking ellipsis
  if (length(list(...)) > 0) {

    ellipsis_variables <- names(list(...))

    if (length(ellipsis_variables) == 1) {

      if (!(ellipsis_variables == "coloroption")) {

        stop(paste0("The ... option only takes the coloroption argument.",
                    "... currently contains the variable ",
                    ellipsis_variables,
                    "."))

      }

    } else {

      stop(paste0("The ... option only takes one argument.",
                  "... currently contains the variables ",
                  paste(ellipsis_variables, collapse = ", "),
                  "."))
    }
  }

  # check input
  check_is_dataset(x = df, error = TRUE)

  # check if color column is present
  check_columns(df = df, col_list = list(color_g))

  # Check if check_log is correct
  check_log <- run_check_npx(df = df, check_log = check_log)

  # Remove invalid OlinkID, assays with all NA values, and convert non-unique
  # Uniprot IDs. Note that we do not remove samples with duplicate SampleID,
  # control samples or assays, or samples/assays with QC warnings, as this
  # would be the user's decision.
  df <- clean_npx(
    df,
    check_log = check_log,
    remove_assay_na = TRUE,
    remove_invalid_oid = TRUE,
    remove_dup_sample_id = FALSE,
    remove_control_assay = FALSE,
    remove_control_sample = FALSE,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE,
    convert_nonunique_uniprot = TRUE,
    out_df = "tibble",
    verbose = FALSE
  ) |>
    suppressMessages()


  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun) # nolint: return_linter
  }

  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...) # nolint: return_linter
  }

  # Format Panel names
  df <- df |>
    dplyr::mutate(!!check_log$col_names$panel :=
                    .data[[check_log$col_names$panel]] |>
                    stringr::str_replace("Olink ", ""))

  # If QC selected to plot
  # If not all are Pass, the QC_Warning is set as warning for plotting purposes
  if (color_g %in% column_name_dict$col_names$qc_warning) {

    df <- df |>
      dplyr::group_by(.data[["SampleID"]], .data[["Panel"]]) |>
      dplyr::mutate(
        !!check_log$col_names$qc_warning := dplyr::if_else(
          all(
            toupper(.data[[check_log$col_names$qc_warning]]) == "PASS"
          ),
          "Pass",
          "Warning"
        )
      ) |>
      dplyr::ungroup()

  }

  df |> # nolint: return_linter
    ggplot2::ggplot(ggplot2::aes(
      x = reorder_within(factor(SampleID), # nolint: object_usage_linter
                         NPX, # nolint: object_usage_linter
                         Panel, # nolint: object_usage_linter
                         median # nolint: object_usage_linter
      ),
      y = NPX,
      fill = !!rlang::ensym(color_g)
    )) +
    ggplot2::geom_boxplot() +
    scale_x_reordered() +
    ggplot2::xlab("Samples") +
    ggplot2::facet_wrap(as.formula(paste("~", check_log$col_names$panel)),
                        scale = "free") +
    OlinkAnalyze::set_plot_theme() +
    OlinkAnalyze::olink_fill_discrete(...)
}
