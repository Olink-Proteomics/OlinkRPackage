# Global Variables Defined

utils::globalVariables(unique(c(
  "plate", "SampleID", "group.var", "percent", "unique.id", "column", "OlinkID", "NPX", "n_na", "SampleID",
  "Index", "n_levels", ".", "Assay", "UniProt", "Panel", "term", "p.value", "Adjusted_pval", "Threshold", "sumsq",
  "meansq", "statistic", "lower.CL", "upper.CL", "contrast", "estimate", "conf.low", "conf.high", "Name_OID",
  "QC_Warning", "sample_median", "median_high", "median_low", "iqr_low", "iqr_high", "Outlier", "LOD", "Warnings",
  "Outliers", "PercAssaysBelowLOD", "MeanNPX", "desc", "Order", "emmean", "everything", "Reference_NPX",
  "Assay_Median", "LOD", "Project", "Diff", "Median", "n_colors", "assay_var", "pca_colors", "desc", "abs_loading",
  "variables", "well", "scramble", ":=", "estimate1", "estimate2", "1", "Name", "Plate ID", "MissingFreq", "Unit",
  "Assay_Warning", "Panel_Version", "QC Warning", "Name_Assay", "PlateID", "Adj_factor", "Panel_End", "Panel_Start",
  "SampleIDPlot", "PCX_low", "PCX_high", "PCY_low", "PCY_high", "BgRatio", "Count", "Description", "Detected",
  "GeneRatio", "NES", "Pathway", "geneList", "Sum","gene_symbol", "gs_name", "gs_subcat", ".y.", "SE", "df", "group1",
  "group2", "method", "p", "p.adj", "parameter", "t.ratio", "Assay_OlinkID", "C1", "C2", "N", "NrNA", "Star",
  "SumNA", "c.sort", "colors", "id", "maxNPX", "pvalue","rangeNPX", "rowNum", "tmp", "x.m", "x.vals", "y.anchor",
  "SubjectID", "study", "SubjectID_old", "Friedman_remove", "P.adj", "Comparison", "Z", "number of NA values", "umapX_high",
  "umapX_low", "umapY_high", "umapY_low", "LX", "LY", "PCX", "PCY", "rowname", "Normalization", "Quantified_value", "v1",
  "v2", "v", "v_name", "duplicateID", "SampleID_df1", "name", "data", "normalize_to", "PlateID1", "QC_Warning1", "V1", "V2",
  "cols", "ID", "AssayQC", "AssayType", "Block", "DataAnalysisRefID", "ExploreVersion", "ExtNPX", "PCNormalizedNPX", "SampleQC", "SampleType", "WellID",
  "Max LOD", "Plate LOD", "Plate_LOD", "Normalization_df1", "Normalization_df2",   "Max_LOD", "maxLOD", "plateLOD", ".env", ".data", "PCNormalizedLOD"
)))

olink_norm_modes <- list(
  "bridge" = "bridge",
  "subset" = "subset",
  "ref_median" = "ref_median"
)

olink_norm_mode_combos <- expand.grid(df1 = c(FALSE, TRUE),
            df2 = c(FALSE, TRUE),
            overlapping_samples_df1 = c(FALSE, TRUE),
            overlapping_samples_df2 = c(FALSE, TRUE),
            reference_medians = c(FALSE, TRUE)) |>
  dplyr::mutate(
    error_msg = dplyr::case_when(
      df1 == FALSE ~ "Required {.var df1} is missing!",
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ "When {.var df1} is provided, either {.var df2} or {.var reference_medians} is required!",
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ "When {.var df1} and {.var reference_medians} are provided, {.var overlapping_samples_df1} is required!",
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ "When {.var df1} and {.var df2} are provided, at least {.var overlapping_samples_df1} is required!",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ NA_character_,
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    warning_msg = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ "{.var overlapping_samples_df2} will be ignored",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ "{.var reference_medians} will be ignored",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ "{.var reference_medians} will be ignored",
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    inform_msg = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ "Reference median normalization will be performed!",
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ "Reference median normalization will be performed!",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ "Bridge normalization will be performed!",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ "Bridge normalization will be performed!",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ "Subset normalization will be performed!",
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ "Subset normalization will be performed!",
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    norm_mode = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ olink_norm_modes$ref_median,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ olink_norm_modes$ref_median,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ olink_norm_modes$bridge,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ olink_norm_modes$bridge,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ olink_norm_modes$subset,
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ olink_norm_modes$subset,
      TRUE ~ NA_character_,
      .default = NA_character_
    )
  ) |>
  dplyr::arrange(
    .data[["df1"]],
    .data[["df2"]],
    .data[["overlapping_samples_df1"]],
    .data[["overlapping_samples_df2"]],
    .data[["reference_medians"]]
  )
