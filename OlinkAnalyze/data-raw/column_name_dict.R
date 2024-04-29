## code to prepare `column_name_dict` dataset

column_name_dict <- list(
  sample_id = c("SampleID", "sampleid", "sample_id"),
  sample_type = c("Sample_Type", "SampleType",
                  "sample_type", "sampletype", NA_character_),
  olink_id = c("OlinkID", "OID",
               "olinkid", "oid", "olink_id"),
  plate_id = c("PlateID","plateid", "plate_id"),
  qc_warning = c("QC_Warning", "SampleQC",
                 "qc_warning", "sample_qc"),
  lod = c("LOD", "Plate LOD", "Plate_LOD", "Max LOD", "Max_LOD",
          "lod", "plate lod", "plate_lod", "max lod", "max_lod",
                  "PlateLOD", "MaxLOD", NA_character_),
  quant = c("NPX", "Ct", "Quantified", "Quantified_value",
            "npx", "ct", "quantified", "quantified_value")
)

# Save the dictionary as an R data object
usethis::use_data(column_name_dict, overwrite = TRUE)
