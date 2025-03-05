olink_wide_bottom_matrix <- dplyr::tribble(
  ~olink_platform, ~data_type,   ~plate_specific, ~version, ~variable_name,              ~variable_alt_names,                      # nolint line_length_linter
  "Flex",          "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Flex",          "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Flex",          "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint line_length_linter
  "Flex",          "NPX",        TRUE,            2L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Flex",          "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint line_length_linter
  "Flex",          "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint line_length_linter
  "Flex",          "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level",              # nolint line_length_linter
  "Flex",          "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Flex",          "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Flex",          "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Flex",          "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint line_length_linter
  "Focus",         "NPX",        TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint line_length_linter
  "Focus",         "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Focus",         "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Focus",         "NPX",        TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Focus",         "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint line_length_linter
  "Focus",         "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint line_length_linter
  "Focus",         "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Focus",         "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Focus",         "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Focus",         "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint line_length_linter
  "Target 48",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Target 48",     "NPX",        FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Target 48",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint line_length_linter
  "Target 48",     "NPX",        TRUE,            2L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Target 48",     "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",                          # nolint line_length_linter
  "Target 48",     "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                                   # nolint line_length_linter
  "Target 48",     "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level",              # nolint line_length_linter
  "Target 48",     "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Target 48",     "Quantified", FALSE,           0L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Target 48",     "Quantified", TRUE,            0L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Target 48",     "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                                   # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",                     # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                                    # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           2L,       "LOD",                       "LOD",                                    # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           2L,       "Normalization",             "Normalization",                          # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           3L,       "Max_LOD",                   c("Max LOD", "MaxLOD", "Max_LOD"),        # nolint line_length_linter
  "Target 96",     "NPX",        TRUE,            3L,       "Plate_LOD",                 c("Plate LOD", "PlateLOD", "Plate_LOD"),  # nolint line_length_linter
  "Target 96",     "NPX",        FALSE,           3L,       "Normalization",             "Normalization"                           # nolint line_length_linter
)
