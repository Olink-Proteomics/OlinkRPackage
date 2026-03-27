## code to prepare internal dataset goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata

# rename local names from Olink wide files to match equivalent long export ----

olink_wide_rename_npxs <- dplyr::tribble(
  ~OA_internal,                  ~NPXS,
  "SampleID",                    "SampleID",
  "Ct",                          "Ct",
  "Panel",                       "Panel",
  "Assay",                       "Assay",
  "Uniprot ID",                  "UniProt",
  "OlinkID",                     "OlinkID",
  "PlateID",                     "PlateID",
  "NPX",                         "NPX",
  "QC_Warning",                  "QC_Warning",
  "Inc Ctrl 1",                  "QC Deviation Inc Ctrl",
  "Det Ctrl",                    "QC Deviation Det Ctrl",
  "LOD",                         "LOD",
  "Missing Data freq.",          "MissingFreq",
  "Normalization",               "Normalization",
  "Inc Ctrl 2",                  "QC Deviation Inc Ctrl",
  "Inc Ctrl",                    "QC Deviation Inc Ctrl",
  "Plate LOD",                   "PlateLOD",
  "Plate_LOD",                   "PlateLOD",
  "PlateLOD",                    "PlateLOD",
  "Max LOD",                     "MaxLOD",
  "Max_LOD",                     "MaxLOD",
  "MaxLOD",                      "MaxLOD",
  "Quantified",                  "Quantified_value",
  "Unit",                        "Unit",
  "Assay warning",               "Assay_Warning",
  "LLOQ",                        "LLOQ",
  "Lowest quantifiable level",   "PlateLQL",
  "ULOQ",                        "ULOQ",
  "Olink NPX Signature Version", "Olink NPX Signature Version",
  "Panel_Version",               "Panel_Version"
)
