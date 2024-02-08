# Help function that returns column name matches from `column_name_dict`

#' @author
#'

#' @description
#' A short description...
#'
#' @param
#' @return


detect_column_name <- function(df, preferred_name_list = NULL) {
  check_is_arrow_object(df, error = TRUE)

  if (!is.null(preferred_name_list)) {
    check_is_list(preferred_name_list, error = TRUE)
  }

  # extract column names
  col_names <- names(df)

  # Find matches
  sample_id <- col_names[which(
     toupper(col_names) %in% toupper(column_name_dict$SampleID)
  )]
  sample_type <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$Sample_Type)
  )]
  olink_id <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$OlinkID)
  )]
  plate_id <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$PlateID)
  )]
  qc_warning <- sample_type <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$QC_Warning)
  )]
  lod <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$LOD)
  )]
  npx <- col_names[which(
    toupper(col_names) %in% toupper(column_name_dict$NPX)
  )]


}
