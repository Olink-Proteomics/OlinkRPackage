## code to prepare `column_name_dict` dataset goes here

column_name_dict <- list(
  SampleID = c("SampleID", "sample_id"),
  Sample_Type = c("Sample_Type", "SampleType"),
  OlinkID = c("OlinkID", "OID", "olink_id"),
  # Panel = c("Panel"),
  PlateID = c("PlateID", "plate_id"),
  QC_Warning = c("QC_Warning", "SampleQC", "sample_qc"),
  LOD = c("LOD", "Plate LOD", "Plate_LOD", "Max LOD", "Max_LOD"),
  NPX = c("NPX", "Ct")
)

# Save the dictionary as an R data object
usethis::use_data(column_name_dict, overwrite = TRUE)
