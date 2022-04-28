npxProcessing_forDimRed <- function(df, color_g, drop_assays, drop_samples, verbose){
  #Set up plotting colors
  if (color_g == "QC_Warning"){
    df_temp <- df %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::mutate(QC_Warning = dplyr::if_else(any(QC_Warning == "Warning"|QC_Warning == "WARN" ), "Warning", "Pass")) %>%
      dplyr::ungroup()

    plotColors <- df_temp %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::summarise(colors = unique(!!rlang::ensym(color_g))) %>%
      dplyr::ungroup()

  } else {
    number_of_sample_w_more_than_one_color <- df %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::summarise(n_colors = dplyr::n_distinct(!!rlang::ensym(color_g), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_colors > 1) %>%
      nrow(.)

    if(number_of_sample_w_more_than_one_color > 0) {
      stop(paste0("There are ", number_of_sample_w_more_than_one_color, " samples that do not have a unique color. Only one color per sample is allowed."))
    } else {
      df_temp <- df

      plotColors <- df_temp %>%
        dplyr::group_by(SampleID, Index) %>%
        dplyr::summarise(colors = unique(!!rlang::ensym(color_g))) %>%
        dplyr::ungroup()
    }
  }

  #remove proteins with 0 variance
  df_temp <- df_temp %>%
    dplyr::group_by(OlinkID) %>%
    dplyr::mutate(assay_var = var(NPX, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(assay_var == 0 | is.na(assay_var))) %>%
    dplyr::select(-assay_var)

  #wide format
  df_wide <- df_temp %>%
    dplyr::select(SampleID, Index, OlinkID, NPX) %>%
    dplyr::filter(!is.na(NPX)) %>%
    tidyr::spread(OlinkID, NPX)

  #Dropping assays with any missing values
  if(drop_assays){

    dropped_assays.na <- colnames(df_wide[, -c(1:2)])[apply(df_wide[, -c(1:2)], 2, anyNA)]

    df_wide <- df_wide %>%
      dplyr::select(-tidyselect::all_of(dropped_assays.na))

    if(verbose){
      warning(paste0(length(dropped_assays.na)),
              " assay(s) contain NA and are dropped. ")
    }

    if(ncol(df_wide) < 4){
      stop('Too many assays removed. Set drop_assays = FALSE for imputation.')
    }
  } else{
    dropped_assays.na <- NULL
  }

  #Dropping samples with any missing values
  if(drop_samples){

    dropped_samples <- apply(df_wide[, -c(1:2)], 1, anyNA)
    df_wide <- df_wide[!dropped_samples, ]

    if(verbose){
      warning(paste0(sum(dropped_samples)),
              " sample(s) contain NA and are dropped. ")
    }

    if(nrow(df_wide) < 2){
      stop('Too many samples removed. Set drop_samples = FALSE for imputation.')
    }
  }

  #Missingness per sample
  percent_missingness <- colSums(is.na(df_wide[, -c(1:2)]))/nrow(df_wide)

  # assays with missingness > 10% are dropped from the PCA
  PERCENT_CUTOFF <- 0.1

  #If there are fewer samples than one plate (88), the PERCENT_CUTOFF is 0.05
  if(nrow(df_wide) <= 88){
    PERCENT_CUTOFF <- 0.05
  }

  if(any(percent_missingness > PERCENT_CUTOFF)){

    removed_assays_index <- which(percent_missingness > PERCENT_CUTOFF)
    percent_missingness <- percent_missingness[-removed_assays_index]

    removed_assays_index <- removed_assays_index + 2
    dropped_assays.missingness <- colnames(df_wide)[removed_assays_index]

    df_wide <- df_wide[, -removed_assays_index]

    if(verbose){
      warning(paste0("There are ",
                     paste0(length(dropped_assays.missingness)),
                     " assay(s) dropped due to high missingness (>",
                     round(PERCENT_CUTOFF*100),
                     "%)."))
    }
  } else {
    dropped_assays.missingness <- NULL
  }

  return(list(df_wide = df_wide,
              plotColors = plotColors,
              dropped_assays.na = dropped_assays.na,
              dropped_assays.missingness = dropped_assays.missingness))
}
