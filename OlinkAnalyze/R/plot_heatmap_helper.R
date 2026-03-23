#' Checking for needed packages and valid inputs
#'
#' @param colnames Character. Determines how to label the columns.
#' Must be 'assay', 'oid', or 'both' (default 'both').
#' @param ... Additional arguments used in \code{pheatmap::pheatmap}
#'
#' @return Null or error/warnings
#'
#' @keywords internal
#'
plot_heatmap_check_inputs <- function(colnames, ...) {
  #Checking if packages are installed
  rlang::check_installed(
    pkg = c("ggplotify", "pheatmap"),
    call = rlang::caller_env()
  )

  check_is_scalar_character(x = colnames, error = TRUE)
  accepted_colnames <- c("assay", "oid", "both")
  if (!(colnames %in% accepted_colnames)) {
    cli::cli_abort( # nolint: return_linter
      c(
        "x" = "{.arg colnames} has to be {.or {.val {accepted_colnames}}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  accepted_add_vars <- c("mat",
                         "silent",
                         "scale",
                         "annotation_row",
                         "annotation_col")
  if (length(list(...)) > 0L) {
    ellipsis_variables <- names(list(...))
    if (any(ellipsis_variables %in% accepted_add_vars)) {
      cli::cli_warn(
        message = c(
          "Argument{?s}
          {.val {intersect(ellipsis_variables, accepted_add_vars)}} cannot be
          manually set in {.fn pheatmap}! Ignoring!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }
  }

  return(NULL)
}

#' remove low var assays and add colnames
#'
#' @param df Data frame in long format with SampleID, NPX, OlinkID, Assay and
#' columns of choice for annotations.
#' @param check_log output from check_npx on \code{df}
#' @param colnames Character. Determines how to label the columns.
#' Must be 'assay', 'oid', or 'both' (default 'both').
#'
#' @return df w/o low var assays with added colnames
#'
#' @keywords internal
#'
plot_heatmap_clean_df <- function(df, check_log, colnames) {
  df <- clean_npx(df = df,
                  check_log = check_log,
                  remove_assay_na = TRUE,
                  remove_invalid_oid = TRUE,
                  remove_control_assay = TRUE,
                  remove_control_sample = TRUE,
                  remove_dup_sample_id = TRUE,
                  remove_assay_warning = FALSE,
                  remove_qc_warning = FALSE,
                  convert_df_cols = TRUE,
                  convert_nonunique_uniprot = TRUE,
                  out_df = "tibble",
                  verbose = FALSE)
  #Remove assays with no variance
  df <- df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(check_log$col_names$olink_id)
      )
    ) |>
    dplyr::mutate(
      assay_var = stats::var(x = .data[[check_log$col_names$quant]],
                             na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !(.data[["assay_var"]] == 0 | is.na(.data[["assay_var"]]))
    ) |>
    dplyr::select(
      -dplyr::all_of("assay_var")
    )

  df <- df |>
    dplyr::mutate(
      both = paste(.data[[check_log$col_names$assay]],
                   .data[[check_log$col_names$olink_id]],
                   sep = "_")
    ) |>
    dplyr::rename(
      "oid" = check_log$col_names$olink_id,
      "assay" = check_log$col_names$assay
    )
  return(df)
}

#' Convert long df to wide
#'
#' @inheritParams olink_heatmap_plot
#'
#' @keywords internal
#'
plot_heatmap_df_to_wide <- function(df,
                                    check_log,
                                    colnames) {
  df_wide <- df |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(check_log$col_names$sample_id),
      names_from = dplyr::all_of(colnames),
      values_from =
        dplyr::all_of(check_log$col_names$quant)
    ) |>
    tibble::column_to_rownames(
      check_log$col_names$sample_id
    )
  return(df_wide)
}

#' create list of arguments to pass to pheatmap function
#'
#' @param df_wide wide version of df
#' @inheritParams olink_heatmap_plot
#'
#' @return list of arguments for pheatmap
#'
#' @keywords internal
#'
plot_heatmap_pheatmap_args <- function(df_wide,
                                       df,
                                       check_log,
                                       center_scale,
                                       cluster_rows,
                                       cluster_cols,
                                       na_col,
                                       show_rownames,
                                       show_colnames,
                                       annotation_legend,
                                       fontsize,
                                       variable_row_list,
                                       variable_col_list,
                                       colnames,
                                       ...) {
  pheatmap_args <- list(mat = df_wide,
                        scale = ifelse(center_scale, "column", "none"),
                        silent = TRUE,
                        cluster_rows = cluster_rows,
                        cluster_cols = cluster_cols,
                        na_col  = na_col,
                        show_rownames = show_rownames,
                        show_colnames = show_colnames,
                        annotation_legend = annotation_legend,
                        fontsize = fontsize)

  #### Extract Ellipsis Variables ####
  if (length(list(...)) > 0L) {
    pheatmap_args <- pheatmap_extract_ellipsis_arg(
      pheatmap_args = pheatmap_args,
      ...
    )
  }

  #### Add row and column annotations ####
  if (!is.null(variable_row_list) || !is.null(variable_col_list)) {
    pheatmap_args <- pheatmap_annotate_heatmap(
      df = df,
      check_log = check_log,
      colnames = colnames,
      pheatmap_args = pheatmap_args,
      variable_row_list = variable_row_list,
      variable_col_list = variable_col_list
    )
  }

  #### Add color assignments ####
  if (!is.null(variable_row_list) ||
      !is.null(variable_col_list) ||
      any(names(pheatmap_args) %in% c("annot_col_int"))) {
    pheatmap_args <- pheatmap_color_heatmap(
      df = df,
      check_log = check_log,
      pheatmap_args = pheatmap_args,
      variable_row_list,
      variable_col_list
    )
  }

  return(pheatmap_args)
}

#' extract ellipsis arguments and add to pheatmap arguments
#'
#' @param pheatmap_args pheatmap argument list
#' @param ... additional arguments to be passed to pheatmap function
#'
#' @return updated pheatmap arguments list with ellipsis variables
#'
#' @keywords internal
#'
pheatmap_extract_ellipsis_arg <- function(pheatmap_args,
                                          ...) {
  ellipsis_variables <- names(list(...))
  annot_col_int <- NULL
  additional_args <- list(...)[!(ellipsis_variables %in% c("mat",
                                                           "silent",
                                                           "scale",
                                                           "annotation_row",
                                                           "annotation_col",
                                                           "annotation_colors")
  )]
  if (any(ellipsis_variables == "annotation_colors")) {
    annot_col_int <- list(...)[["annotation_colors"]]
  }

  if (length(additional_args) != 0L && !is.null(annot_col_int)) {
    additional_args <- append(additional_args,
                              list(annot_col_int = annot_col_int))
  } else if (!is.null(annot_col_int)) {
    additional_args <- list(annot_col_int = annot_col_int)
  }
  pheatmap_args <- append(pheatmap_args, additional_args)
  return(pheatmap_args)
}

#' Add annotations to pheatmap arguments
#'
#' @inheritParams olink_heatmap_plot
#'
#' @return updated pheatmap arguments
#'
#' @keywords internal
#'
pheatmap_annotate_heatmap <- function(df,
                                      check_log,
                                      colnames,
                                      pheatmap_args,
                                      variable_row_list,
                                      variable_col_list) {
  if (!is.null(variable_row_list)) {
    pheatmap_args[["annotation_row"]] <- df |>
      dplyr::select(
        dplyr::all_of(
          c(check_log$col_names$sample_id,
            variable_row_list)
        )
      ) |>
      dplyr::distinct() |>
      tibble::column_to_rownames(
        var = check_log$col_names$sample_id
      )
  }

  if (!is.null(variable_col_list)) {
    pheatmap_args[["annotation_col"]] <- df |>
      dplyr::select(
        dplyr::all_of(
          c(colnames,
            variable_col_list)
        )
      ) |>
      dplyr::distinct() |>
      tibble::column_to_rownames(
        var = colnames
      )
  }

  return(pheatmap_args)
}

#' add colors to pheatmap arguments
#'
#' @inheritParams olink_heatmap_plot
#'
#' @return updated pheatmap_args
#'
#' @keywords internal
#'
pheatmap_color_heatmap <- function(df,
                                   check_log,
                                   pheatmap_args,
                                   variable_row_list,
                                   variable_col_list) {
  col_classes <- sapply(df, "class")
  vars_to_color <- c(sort(variable_col_list, decreasing = TRUE),
                     sort(variable_row_list, decreasing = TRUE))

  vars_to_color <- vars_to_color[col_classes[vars_to_color] %in%
                                   c("character", "factor")]

  vars_to_color <- vars_to_color[!(vars_to_color %in%
                                     names(pheatmap_args[["annot_col_int"]]))]

  if (length(vars_to_color) > 0L) {
    colors <- olink_pal()(
      sum(sapply(dplyr::select(df,
                               dplyr::all_of(vars_to_color)),
                 function(x) length(unique(x))))
    )
  }
  variable_list <- as.list(sapply(dplyr::select(df,
                                                dplyr::all_of(vars_to_color)),
                                  function(x) length(unique(x))))

  rep_num <- rep(names(variable_list), times = variable_list)

  color_assignments <- with(data.frame(variable = rep_num, color = colors),
                            split(color, variable))

  pheatmap_args[["annot_col_int"]] <- append(pheatmap_args[["annot_col_int"]],
                                             color_assignments)
  return(pheatmap_args)
}

#' Run the pheatmap using args
#'
#' @param pheatmap_args
#'
#' @return ggplot of heatmap as generated by pheatmap
#'
#' @keywords internal
#'
pheatmap_run <- function(pheatmap_args) {
  p <- NULL
  tryCatch({
    p <- do.call(pheatmap::pheatmap, args = pheatmap_args)$gtable
  },
  error = function(e) {
    if (grepl("NA/NaN/Inf", e$message, fixed = TRUE)) {
      cli::cli_abort(cli::cli_bullets(c("x" = "Error when clustering.",
                                        "i" = paste("Try setting cluster of",
                                                    "rows or columns to",
                                                    "`FALSE`."))))
    } else {
      cli::cli_abort(e$message)
    }
    return(NULL)
  },
  finally = {
    # Apply retroactive theming for the existing parts of the plot
    p <- pheatmap_set_plot_theme(
      x = p,
      fontsize = pheatmap_args$fontsize
    )
    # Convert to ggplot object, and set theming
    # (which applies to components added post-hoc)
    p <- ggplotify::as.ggplot(p) +
      OlinkAnalyze::set_plot_theme() +
      ggplot2::theme(
        text = ggplot2::element_text(size = pheatmap_args$fontsize),
        axis.line  = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text  = ggplot2::element_blank()
      ) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)
  }
  )
  return(p)
}

#' Add theme to pheatmap
#'
#' @param x pheatmap plot
#' @param fontsize size of font
#' @param col color
#' @param font1 font to use as default
#' @param font2 secondary font to use
#'
#' @return pheatmap with updated theme
#'
#' @keywords internal
#'
pheatmap_set_plot_theme <- function(x,
                                    fontsize,
                                    col = "#737373",
                                    font1 = "Arial Regular",
                                    font2 = "Arial") {
  set_font1 <- FALSE
  set_font2 <- FALSE

  # Prepare fonts
  if (getOption("OlinkAnalyze.allow.font.load", default = TRUE)) {
    if (requireNamespace("showtext", quietly = TRUE) &&
          requireNamespace("systemfonts", quietly = TRUE)) {
      set_font1 <- font1 %in% fonts_system()
      set_font2 <- font2 %in% fonts_system()
      showtext::showtext_auto()
    } else if (requireNamespace("systemfonts", quietly = TRUE)) {
      set_font1 <- font1 %in% unique(systemfonts::registry_fonts()$family)
      set_font2 <- font2 %in% unique(systemfonts::registry_fonts()$family)
    }
  }

  font <- ifelse(set_font1,
                 font1,
                 ifelse(set_font2,
                        font2,
                        NA))
  # Styling
  styling_location <- list(
    col_tree_i = which(x$layout$name == "col_tree"),
    row_tree_i = which(x$layout$name == "row_tree"),
    main_i = which(x$layout$name == "main"),
    col_names_i = which(x$layout$name == "col_names"),
    row_names_i = which(x$layout$name == "row_names"),
    row_annotation_names_i = which(x$layout$name == "row_annotation_names"),
    col_annotation_names_i = which(x$layout$name == "col_annotation_names"),
    annotation_legend_i = which(x$layout$name == "annotation_legend"),
    legend_i = which(x$layout$name == "legend")
  )

  styling <- data.frame(
    grob = c("col_tree_i",
             "row_tree_i",
             "main_i",
             "col_names_i",
             "row_names_i",
             "row_annotation_names_i",
             "col_annotation_names_i",
             "annotation_legend_i",
             "legend_i"),
    col = c(col,
            col,
            col,
            "black",
            "black",
            "black",
            "black",
            col,
            col),
    lwd = c(0.4,
            0.4,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA),
    fontface = c(NA,
                 NA,
                 "bold",
                 NA,
                 NA,
                 "bold",
                 "bold",
                 NA,
                 NA),
    fontfamily = c(NA,
                   NA,
                   font,
                   font,
                   font,
                   font,
                   font,
                   font,
                   font),
    fontsize = c(NA,
                 NA,
                 NA,
                 fontsize,
                 fontsize,
                 fontsize,
                 fontsize,
                 fontsize,
                 fontsize)
  )

  for (i in seq_along(styling_location)) {
    x <- pheatmap_lst_styling(i = i,
                              x = x,
                              styling_location = styling_location,
                              styling = styling)
  }
  return(x)
}

#' Function to edit grob
#'
#' @param i index of attribute
#' @param x plot
#' @param styling_location list of locations for different attributes
#' @param styling grob altered parameters for theme
#'
#' @return updated plot with updated grob
#'
#' @keywords internal
#'
pheatmap_lst_styling <- function(i,
                                 x,
                                 styling_location,
                                 styling) {
  style_i <- styling |>
    dplyr::slice(i) |>
    dplyr::select(dplyr::where(~!all(is.na(.x)))) |>
    dplyr::select(-dplyr::any_of("grob"))
  style_i <- as.list(style_i)
  if (length(styling_location[[i]]) > 0L) {
    grob <- x$grobs[[styling_location[[i]]]]
    x$grobs[[styling_location[[i]]]] <- grid::editGrob(
      grob = grob,
      gp = do.call(grid::gpar, style_i)
    )
  }
  return(x)
}
