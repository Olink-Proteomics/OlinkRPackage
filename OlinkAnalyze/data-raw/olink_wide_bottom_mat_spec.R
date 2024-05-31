olink_wide_bottom_matrix <- dplyr::tribble(
  ~olink_platform, ~data_type,   ~plate_specific, ~version, ~variable_name,              ~variable_alt_names,                      # nolint
  "Flex",          "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Flex",          "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Flex",          "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint
  "Flex",          "NPX",        TRUE,            2L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Flex",          "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint
  "Flex",          "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint
  "Flex",          "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level",              # nolint
  "Flex",          "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Flex",          "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Flex",          "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Flex",          "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint
  "Focus",         "NPX",        TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint
  "Focus",         "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Focus",         "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Focus",         "NPX",        TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Focus",         "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint
  "Focus",         "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint
  "Focus",         "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Focus",         "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Focus",         "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Focus",         "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint
  "Target 48",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Target 48",     "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Target 48",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint
  "Target 48",     "NPX",        TRUE,            2L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Target 48",     "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint
  "Target 48",     "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint
  "Target 48",     "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level",              # nolint
  "Target 48",     "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Target 48",     "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint
  "Target 48",     "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Target 48",     "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint
  "Target 96",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint
  "Target 96",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint
  "Target 96",     "NPX",        FALSE,           2L,       "LOD",                       "LOD",                                    # nolint
  "Target 96",     "NPX",        FALSE,           2L,       "Normalization",             "Normalization",                          # nolint
  "Target 96",     "NPX",        FALSE,           3L,       "Max_LOD",                   c("Max LOD", "MaxLOD", "Max_LOD"),        # nolint
  "Target 96",     "NPX",        TRUE,            3L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint
  "Target 96",     "NPX",        FALSE,           3L,       "Normalization",             "Normalization"                           # nolint
)
