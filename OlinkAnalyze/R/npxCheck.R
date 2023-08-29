#This function is called by various other functions to perform checks of NPX-data. For now, it only looks for assays which have all NPX = NA, but there are other redundant tasks that could be moved here
npxCheck <- function(df) {
  # # Check whether df contains NPX or QUANT
  if ("NPX" %in% colnames(df)) {
    data_type <- "NPX"
  } else if ("Quantified_value" %in% colnames(df)) {
    data_type <- "Quantified_value"
  } else {
    stop("Neither NPX or Quantified_value column present in the data")}

  #### Identify assays that have only NA:s ####
  all_nas <- df  |>
    dplyr::group_by(OlinkID) |>
    dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(!!rlang::ensym(data_type))), .groups = "drop") |>
    dplyr::filter(n == n_na) |>
    dplyr::pull(OlinkID)

  if(length(all_nas) > 0) {

    warning(paste0("The assays ",
                   paste(all_nas, collapse = ", "),
                   " have NPX = NA for all samples. They will be excluded from the analysis"),
            call. = FALSE)

  }

  return(list(all_nas = all_nas,
              data_type = data_type))
}
