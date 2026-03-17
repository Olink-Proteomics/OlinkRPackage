#' Title
#'
#' @param colnames
#' @param ...
#'
#' @return
#' @keywords internal
check_heatmap_inputs <- function(colnames, ...) {
  #Checking if packages are installed
  rlang::check_installed(
    pkg = c("ggplotify", "pheatmap"),
    call = rlang::caller_env()
  )

  if (!colnames %in% c("assay", "oid", "both")) {
    cli::cli_abort("colnames has to be \"assay\", \"oid\", or \"both\"")
  }

  if (length(list(...)) > 0L) {
    ellipsis_variables <- names(list(...))
    if (any(ellipsis_variables %in% c("mat",
                                      "silent",
                                      "scale",
                                      "annotation_row",
                                      "annotation_col"))) {
      cli::cli_warn(paste("Argument",
                          intersect(ellipsis_variables, c("mat",
                                                          "silent",
                                                          "scale",
                                                          "annotation_row",
                                                          "annotation_col")),
                          "cannot be manually set in pheatmap - ignoring."))
    }
  }


  return(NULL)
}

#' Title
#'
#' @param df
#' @param check_log
#' @param colnames
#'
#' @return
#' @keywords internal
clean_heatmap_df <- function(df, check_log, colnames) {
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
    dplyr::group_by(all(check_log$col_names$OlinkID)) |>
    dplyr::mutate(assay_var = var(.data[["NPX"]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(!(.data[["assay_var"]] == 0 | is.na(.data[["assay_var"]]))) |>
    dplyr::select(-dplyr::all_of("assay_var"))

  df <- df |>
    dplyr::mutate(both = paste0(.data[["Assay"]],
                                "_",
                                .data[["OlinkID"]])) |>
    dplyr::rename("oid" = "OlinkID",
                  "assay" = "Assay") |>
    tidyr::pivot_wider(id_cols = .data[["SampleID"]],
                       names_from = .data[[colnames]],
                       values_from = .data[["NPX"]]) |>
    tibble::column_to_rownames("SampleID")

  return(df)
}

#' Title
#'
#' @param df_wide
#' @param df
#' @param check_log
#' @param center_scale
#' @param cluster_rows
#' @param cluster_cols
#' @param na_col
#' @param show_rownames
#' @param show_colnames
#' @param annotation_legend
#' @param fontsize
#' @param variable_row_list
#' @param variable_col_list
#' @param colnames
#' @param ...
#'
#' @return
#' @keywords internal
create_pheatmap_args <- function(df_wide,
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
    pheatmap_args <- extract_ellipsis_arg(pheatmap_args = pheatmap_args,
                                          ...)
  }

  #### Add row and column annotations ####
  if (!is.null(variable_row_list) || !is.null(variable_col_list)) {
    pheatmap_args <- annotate_heatmap(df = df,
                                      check_log = check_log,
                                      colnames = colnames,
                                      pheatmap_args = pheatmap_args,
                                      variable_row_list = variable_row_list,
                                      variable_col_list = variable_col_list)
  }

  #### Add color assignments ####
  if (!is.null(variable_row_list) ||
        !is.null(variable_col_list) ||
        any(names(pheatmap_args) %in% c("annot_col_int"))) {
    pheatmap_args <- color_heatmap(df = df,
                                   check_log = check_log,
                                   pheatmap_args = pheatmap_args,
                                   variable_row_list,
                                   variable_col_list)
  }


  return(pheatmap_args)

}

#' Title
#'
#' @param pheatmap_args
#' @param ...
#'
#' @return
#' @keywords internal
extract_ellipsis_arg <- function(pheatmap_args,
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

  if (length(additional_args) != 0L &&
        !is.null(annot_col_int)) {
    additional_args <- append(additional_args,
                              list(annot_col_int =
                                     annot_col_int))
  } else if (!is.null(annot_col_int)) {
    additional_args <- list(annot_col_int =
                              annot_col_int)
  }
  pheatmap_args <- append(pheatmap_args, additional_args)
  return(pheatmap_args)
}

#' Title
#'
#' @param df
#' @param check_log
#' @param colnames
#' @param pheatmap_args
#' @param variable_row_list
#' @param variable_col_list
#'
#' @return
#' @keywords internal
annotate_heatmap <- function(df,
                             check_log,
                             colnames,
                             pheatmap_args,
                             variable_row_list,
                             variable_col_list) {
  if (!is.null(variable_row_list)) {
    pheatmap_args[["annotation_row"]] <- df |>
      dplyr::select(dplyr::all_of(c(check_log$col_names$sample_id,
                                    variable_row_list))) |>
      dplyr::distinct() |>
      tibble::column_to_rownames(check_log$col_names$sample_id)
  }

  if (!is.null(variable_col_list)) {
    pheatmap_args[["annotation_col"]] <- df |>
      dplyr::select(dplyr::all_of(c(colnames, variable_col_list))) |>
      dplyr::distinct() |>
      tibble::column_to_rownames(colnames)
  }

  return(pheatmap_args)
}

#' Title
#'
#' @param df
#' @param check_log
#' @param pheatmap_args
#' @param variable_row_list
#' @param variable_col_list
#'
#' @return
#' @keywords internal
color_heatmap <- function(df,
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

#' Title
#'
#' @param pheatmap_args
#'
#' @return
#' @keywords internal
run_pheatmap <- function(pheatmap_args) {
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
    p <- set_plot_theme_pheatmap(x = p, fontsize = pheatmap_args$fontsize)
    # Convert to ggplot object, and set theming
    # (which applies to components added post-hoc)
    p <- ggplotify::as.ggplot(p) +
      OlinkAnalyze::set_plot_theme() +
      ggplot2::theme(text = ggplot2::element_text(size =
                                                    pheatmap_args$fontsize),
                     axis.line  = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text  = ggplot2::element_blank()) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)
  }
  )
  return(p)
}

#' Title
#'
#' @param x
#' @param fontsize
#' @param col
#' @param font1
#' @param font2
#'
#' @return
#' @keywords internal
set_plot_theme_pheatmap <- function(x,
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
    x <- lst_styling(i,
                     x,
                     styling_location,
                     styling)
  }
  return(x)
}


#' Title
#'
#' @param i
#' @param x
#' @param styling_location
#' @param styling
#'
#' @return
#' @keywords internal
lst_styling <- function(i,
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
    x$grobs[[styling_location[[i]]]] <- grid::editGrob(grob,
                                                       gp = do.call(grid::gpar,
                                                                    style_i))

  }
  return(x)
}
