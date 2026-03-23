#' Generate boxplots for selected Olink proteins
#'
#' This function creates boxplots (optionally annotated with posthoc or
#' t-test results) for a set of proteins specified by OlinkID.
#'
#' ## Steps performed internally:
#'
#' 1. **Input validation**  
#'    - Check dataset format  
#'    - Validate variable names, olinkid list, verbosity, integer parameters  
#'    - Validate ellipsis arguments (`coloroption` only)
#'
#' 2. **QC & NPX Cleaning**  
#'    - Run QC log checks  
#'    - Clean NPX using Olink standard workflows  
#'    - Warn if sample type is missing
#'
#' 3. **Column validation**  
#'    - Ensure all required variables exist  
#'    - Ensure only the first two variables are used if more are provided
#'
#' 4. **Tidy evaluation setup**  
#'    - Convert variables to tidy-eval symbols for ggplot
#'
#' 5. **Loop over batches of proteins**  
#'    - For each `number_of_proteins_per_plot` chunk:  
#'      * Filter NPX values  
#'      * Prepare combined label column  
#'      * Build the boxplot  
#'      * Optionally add posthoc or t-test annotation  
#'
#' 6. **Return list of ggplot objects (invisible)**  
#'
#' @keywords internal
#'
olink_boxplot <- function(df,
                          variable,
                          olinkid_list,
                          verbose = FALSE,
                          number_of_proteins_per_plot = 6L,
                          posthoc_results = NULL,
                          ttest_results = NULL,
                          check_log = NULL,
                          ...) {
  # ---- Helper: rounding for significance labels --------------------------------
  myRound <- function(x) {
    if (x >= 0.00009) {
      return(as.character(round(x, 4)))
    }
    out <- as.character(x)
    if (nchar(out) > 8) {
      out <- paste0(
        substring(out, 1, 4),
        substring(out, nchar(out) - 3, nchar(out))
      )
    }
    out
  }
  
  # ---- Input validation -------------------------------------------------
  check_is_dataset(x = df, error = TRUE)
  check_is_character(variable, error = TRUE)
  check_is_character(olinkid_list, error = TRUE)
  check_is_scalar_boolean(verbose, error = TRUE)
  number_of_proteins_per_plot <- as.integer(number_of_proteins_per_plot)
  check_is_scalar_integer(number_of_proteins_per_plot, error = TRUE)
  
  # Validate ellipsis
  dots <- list(...)
  if (length(dots) > 0) {
    if (length(dots) != 1 || names(dots) != "coloroption") {
      stop(
        paste0(
          "The ... option only accepts 'coloroption'. Provided: ",
          paste(names(dots), collapse = ", ")
        )
      )
    }
  }
  
  # ---- QC & CLEANING ----------------------------------------------------
  check_log <- run_check_npx(df = df, check_log = check_log)
  
  df <- clean_npx(
    df = df,
    check_log = check_log,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE,
    verbose = FALSE
  ) |>
    suppressMessages() |>
    suppressWarnings()
  check_log_clean <- check_npx(df = df) |>
    suppressMessages() |>
    suppressWarnings()
  if (!("sample_type" %in% names(check_log_clean$col_names))) {
    cli::cli_inform(
      "No sample type column detected in input {.arg df}. Control samples may not be filtered out."
    )
  }
  
  
  # ---- EARLY COLUMN TRIMMING (Performance Critical) -----------------------------
  # Keep only columns required for boxplot generation
  required_cols <- c("OlinkID", "UniProt", "Assay", "NPX", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  # Trim df to minimal needed structure
  df <- df[, required_cols, drop = FALSE]
  
  # ---- Tidy eval setup ----------------------------------------------------------
  var_syms <- rlang::syms(variable)
  x_var <- var_syms[[1]]
  fill_var <- if (length(var_syms) > 1) var_syms[[2]] else x_var
  
  topX <- length(olinkid_list)
  protein_index <- seq(1, topX, by = number_of_proteins_per_plot)
  
  list_of_plots <- vector("list", length(protein_index))
  counter <- 1
  
  # ---- PRECOMPUTE STATIC LOOKUPS ------------------------------------------------
  # Avoid repeating expensive lookups inside the loop
  if (!is.null(posthoc_results)) {
    posthoc_results <- posthoc_results |>
      dplyr::mutate(
        Name_OID = forcats::as_factor(
          paste(Assay, OlinkID)
        )
      )
  }
  
  if (!is.null(ttest_results)) {
    ttest_results <- ttest_results |>
      dplyr::mutate(
        Name_OID = forcats::as_factor(
          paste(Assay, OlinkID)
        )
      )
  }
  
  # ---- STEP 5: MAIN LOOP --------------------------------------------------------
  for (i in seq_along(protein_index)) {
    from <- protein_index[i]
    to <- min(from + number_of_proteins_per_plot - 1, topX)
    
    assays <- olinkid_list[from:to]
    
    # ---- FAST NPX EXTRACTION ---------------------------------------------------
    npx_plot <- df[df$OlinkID %in% assays, , drop = FALSE]
    npx_plot$OlinkID <- factor(npx_plot$OlinkID, levels = assays)
    
    # Compute Name_OID once
    Name_OID <- paste(npx_plot$Assay, npx_plot$OlinkID)
    npx_plot$Name_OID <- forcats::as_factor(Name_OID)
    
    # ---- CASE 1: Basic boxplot --------------------------------------------------
    if (is.null(posthoc_results) && is.null(ttest_results)) {
      p <- ggplot2::ggplot(
        data = npx_plot,
        mapping = ggplot2::aes(y = NPX, x = !!x_var)
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var)) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text  = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
      
      # ---- CASE 2: POSTHOC ANNOTATION --------------------------------------------
    } else if (!is.null(posthoc_results)) {
      
      levs <- levels(addNA(npx_plot[[variable]]))
      
      star.info <- data.frame(
        x.vals = levs,
        id = seq_along(levs),
        stringsAsFactors = FALSE
      )
      star.info$x.vals[is.na(star.info$x.vals)] <- "NA"
      
      posthoc.tmp <- posthoc_results[posthoc_results$OlinkID %in% assays, , drop = FALSE]
      
      # Precompute scale factors
      scale_inf <- npx_plot |>
        dplyr::group_by(Name_OID) |>
        dplyr::summarise(
          maxNPX = max(NPX),
          rangeNPX = diff(range(NPX)),
          .groups = "drop"
        )
      
      # Build annotation dataframe
      # (kept logically identical to original version)
      line.data <- posthoc.tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = sub(" .*", "", contrast),
          C2 = sub(".* ", "", contrast),
          C1 = gsub("[()]", "", C1),
          C2 = gsub("[()]", "", C2),
          rp = myRound(Adjusted_pval),
          p.value = paste0(rp, " Contrast: ", contrast)
        ) |>
        dplyr::group_by(Name_OID, contrast) |>
        dplyr::arrange(pmin(C1, C2), .by_group = TRUE) |>
        dplyr::mutate(
          rowNum = dplyr::n():1,
          y.anchor = maxNPX + rowNum * rangeNPX * 0.5 / max(rowNum)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = c(C1, C2),
          names_to = "tmp",
          values_to = "x.vals"
        ) |>
        dplyr::mutate(
          Star = dplyr::case_when(
            Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
            Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
            Adjusted_pval <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star.info, by = "x.vals") |>
        dplyr::group_by(contrast, Name_OID) |>
        dplyr::mutate(x.m = mean(id)) |>
        dplyr::ungroup() |>
        dplyr::filter(Threshold == "Significant")
      
      p <- ggplot2::ggplot(
        data = npx_plot,
        mapping = ggplot2::aes(y = NPX, x = !!x_var)
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var)) +
        ggplot2::geom_line(
          data = line.data,
          mapping = ggplot2::aes(x = x.vals, y = y.anchor, group = p.value)
        ) +
        ggplot2::geom_text(
          data = line.data[line.data$tmp == "C1", ],
          mapping = ggplot2::aes(x = x.m, y = y.anchor + 0.1, label = Star)
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text  = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
      
      # ---- CASE 3: T-TEST ANNOTATION ---------------------------------------------
    } else if (!is.null(ttest_results)) {
      
      # Precompute all once
      uniq_vals <- unique(npx_plot[[variable]])
      uniq_vals <- uniq_vals[!is.na(uniq_vals)]
      
      star.info <- data.frame(
        x.vals = uniq_vals,
        id = seq_along(uniq_vals),
        stringsAsFactors = FALSE
      )
      
      ttest.tmp <- ttest_results[ttest_results$OlinkID %in% assays, , drop = FALSE]
      
      scale_inf <- npx_plot |>
        dplyr::group_by(Name_OID) |>
        dplyr::summarise(
          maxNPX = max(NPX),
          rangeNPX = diff(range(NPX)),
          .groups = "drop"
        )
      
      line.data <- ttest.tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = uniq_vals[1],
          C2 = uniq_vals[2],
          y.anchor = maxNPX + rangeNPX * 0.2
        ) |>
        tidyr::pivot_longer(
          cols = c(C1, C2),
          names_to = "tmp",
          values_to = "x.vals"
        ) |>
        dplyr::mutate(
          Star = dplyr::case_when(
            Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
            Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
            Adjusted_pval <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star.info, by = "x.vals") |>
        dplyr::group_by(Name_OID) |>
        dplyr::mutate(x.m = mean(id)) |>
        dplyr::ungroup() |>
        dplyr::filter(Threshold == "Significant")
      
      p <- ggplot2::ggplot(
        data = npx_plot,
        mapping = ggplot2::aes(y = NPX, x = !!x_var)
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var)) +
        ggplot2::geom_line(
          data = line.data,
          mapping = ggplot2::aes(x = x.vals, y = y.anchor, group = Name_OID)
        ) +
        ggplot2::geom_text(
          data = line.data[line.data$tmp == "C1", ],
          mapping = ggplot2::aes(x = x.m, y = y.anchor + 0.1, label = Star)
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text  = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    }
    
    # ---- Final styling ---------------------------------------------------------
    if (length(variable) == 1) {
      p <- p +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank()
        )
    }
    
    if (verbose) methods::show(p)
    
    list_of_plots[[counter]] <- p
    counter <- counter + 1
  }
  
  invisible(list_of_plots)
}