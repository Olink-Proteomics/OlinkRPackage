## code to prepare internal dataset `column_name_dict` goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata
##
## alternative names for columns of Olink files

column_name_dict <- dplyr::tibble(
  # internal keys used for alternative column names
  col_key = c(
    "sample_id",
    "sample_type",
    "assay_type",
    "olink_id",
    "uniprot",
    "assay",
    "panel",
    "plate_id",
    "panel_version",
    "lod",
    "quant",
    "ext_npx",
    "count",
    "qc_warning",
    "assay_warn",
    "normalization"
  ),
  # list of alternative column names matching the keys
  col_names = list(
    "sample_id" =     c("SampleID",
                        "sampleid",
                        "sample_id"),
    "sample_type" =   c("Sample_Type",
                        "SampleType",
                        "sample_type",
                        "sampletype"),
    "assay_type" =    c("Assay_Type",
                        "AssayType",
                        "assay_type",
                        "assaytype"),
    "olink_id" =      c("OlinkID",
                        "OID",
                        "olinkid",
                        "oid",
                        "olink_id"),
    "uniprot" =       c("UniProt",
                        "uniprot"),
    "assay" =         c("Assay",
                        "assay"),
    "panel" =         c("Panel",
                        "panel"),
    "plate_id" =      c("PlateID",
                        "plateid",
                        "plate_id"),
    "panel_version" = c("Panel_Lot_Nr",
                        "panel_lot_nr",
                        "Panel_Version",
                        "panel_version",
                        "PanelVersion",
                        "panelversion",
                        "DataAnalysisRefID",
                        "data_analysis_ref_id",
                        "dataanalysisrefid"),
    "lod" =           c("LOD",
                        "lod",
                        "Plate LOD",
                        "Plate_LOD",
                        "PlateLOD",
                        "plate lod",
                        "plate_lod",
                        "platelod",
                        "Max LOD",
                        "Max_LOD",
                        "MaxLOD",
                        "max lod",
                        "max_lod",
                        "maxlod",
                        "LODNPX",
                        "lodnpx",
                        "LOD_NPX",
                        "lod_npx",
                        "PCNormalizedLOD",
                        "pcnormalizedlod",
                        "pc_normalized_lod",
                        "NCLOD",
                        "NCPCNormalizedLOD",
                        "FixedLOD",
                        "FixedPCNormalizedLOD",
                        "LODIPCNormalizedNPX",
                        "LODQuant"),
    # note that quant values should be ordered by preference
    "quant" =         c("NPX",
                        "npx",
                        "PCNormalizedNPX",
                        "pcnormalizednpx",
                        "pc_normalized_npx",
                        "IPCNormalizedNPX",
                        "ipcnormalizednpx",
                        "ipc_normalized_npx",
                        "Quantified",
                        "Quantified_value",
                        "QuantifiedValue",
                        "quantifiedvalue",
                        "quantified",
                        "quantified_value",
                        "Ct",
                        "ct"),
    "ext_npx" =       c("ExtNPX",
                        "extnpx",
                        "ext_npx"),
    "count" =         c("Count",
                        "count"),
    "qc_warning" =    c("QC_Warning",
                        "SampleQC",
                        "qc_warning",
                        "sample_qc"),
    "assay_warn" =    c("Assay_Warning",
                        "assay_warning",
                        "AssayQC",
                        "assay_qc"),
    "normalization" = c("Normalization",
                        "normalization")
  ),
  # boolean array marking if a column is allowed to be absent from the dataset
  col_miss = c(
    FALSE, # sample_id
    TRUE, # sample_type
    TRUE, # assay_type
    FALSE, # olink_id
    FALSE, # uniprot
    FALSE, # assay
    FALSE, # panel
    FALSE, # plate_id
    FALSE, # panel_version
    TRUE, # lod
    FALSE, # quant
    TRUE, # ext_npx
    TRUE, # count
    FALSE, # qc_warning
    TRUE, # assay_warn
    TRUE # normalization
  ),
  # boolean array marking if a column is allowed to have multiple names on
  # the same dataset (e.g. "PlateLOD" and "MaxLOD")
  col_multi = c(
    FALSE, # sample_id
    FALSE, # sample_type
    FALSE, # assay_type
    FALSE, # olink_id
    FALSE, # uniprot
    FALSE, # assay
    FALSE, # panel
    FALSE, # plate_id
    FALSE, # panel_version
    TRUE, # lod
    FALSE, # quant
    FALSE, # ext_npx
    FALSE, # count
    FALSE, # qc_warning
    FALSE, # assay_warn
    FALSE # normalization
  ),
  # boolean array marking if columns with multiple names on the same dataset
  # should have a unique match to use in the downstream functions; for example,
  # when both "NPX" and "Quantified_value" are present in the data, we should
  # use only one of them to bridhe normalize or perform statistical analysis.
  col_order = c(
    FALSE, # sample_id
    FALSE, # sample_type
    FALSE, # assay_type
    FALSE, # olink_id
    FALSE, # uniprot
    FALSE, # assay
    FALSE, # panel
    FALSE, # plate_id
    FALSE, # panel_version
    FALSE, # lod
    TRUE, # quant
    FALSE, # ext_npx
    FALSE, # count
    FALSE, # qc_warning
    FALSE, # assay_warn
    FALSE # normalization
  ),
  col_class = c(
    "character", # sample_id
    "character", # sample_type
    "character", # assay_type
    "character", # olink_id
    "character", # uniprot
    "character", # assay
    "character", # panel
    "character", # plate_id
    "character", # panel_version
    "numeric", # lod
    "numeric", # quant
    "numeric", # ext_npx
    "numeric", # count
    "character", # qc_warning
    "character", # assay_warn
    "character" # normalization
  ),
  col_class_check = c(
    FALSE, # sample_id
    FALSE, # sample_type
    FALSE, # assay_type
    FALSE, # olink_id
    FALSE, # uniprot
    FALSE, # assay
    FALSE, # panel
    FALSE, # plate_id
    FALSE, # panel_version
    TRUE, # lod
    TRUE, # quant
    TRUE, # ext_npx
    TRUE, # count
    FALSE, # qc_warning
    FALSE, # assay_warn
    FALSE # normalization
  )
)
