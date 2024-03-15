olink_wide_bottom_matrix <- dplyr::tribble(
  ~olink_platform, ~data_type,   ~plate_specific, ~version, ~variable_name,              ~variable_alt_names,         # nolint object_usage_linter
  "Flex",          "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Flex",          "NPX",        FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Flex",          "NPX",        FALSE,           1L,       "LOD",                       "LOD",                       # nolint object_usage_linter
  "Flex",          "NPX",        TRUE,            2L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Flex",          "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",             # nolint object_usage_linter
  "Flex",          "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                      # nolint object_usage_linter
  "Flex",          "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level", # nolint object_usage_linter
  "Flex",          "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Flex",          "Quantified", FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Flex",          "Quantified", TRUE,            0L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Flex",          "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                      # nolint object_usage_linter
  "Focus",         "NPX",        TRUE,            0L,       "Assay warning",             "Assay warning",             # nolint object_usage_linter
  "Focus",         "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Focus",         "NPX",        FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Focus",         "NPX",        TRUE,            0L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Focus",         "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",             # nolint object_usage_linter
  "Focus",         "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                      # nolint object_usage_linter
  "Focus",         "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Focus",         "Quantified", FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Focus",         "Quantified", TRUE,            0L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Focus",         "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                      # nolint object_usage_linter
  "Target 48",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Target 48",     "NPX",        FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Target 48",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                       # nolint object_usage_linter
  "Target 48",     "NPX",        TRUE,            2L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Target 48",     "Quantified", TRUE,            0L,       "Assay warning",             "Assay warning",             # nolint object_usage_linter
  "Target 48",     "Quantified", FALSE,           0L,       "LLOQ",                      "LLOQ",                      # nolint object_usage_linter
  "Target 48",     "Quantified", TRUE,            0L,       "Lowest quantifiable level", "Lowest quantifiable level", # nolint object_usage_linter
  "Target 48",     "Quantified", FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Target 48",     "Quantified", FALSE,           0L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Target 48",     "Quantified", TRUE,            0L,       "PlateLOD",                  c("Plate LOD", "PlateLOD"),  # nolint object_usage_linter
  "Target 48",     "Quantified", FALSE,           0L,       "ULOQ",                      "ULOQ",                      # nolint object_usage_linter
  "Target 96",     "NPX",        FALSE,           0L,       "Missing Data freq.",        "Missing Data freq.",        # nolint object_usage_linter
  "Target 96",     "NPX",        FALSE,           1L,       "LOD",                       "LOD",                       # nolint object_usage_linter
  "Target 96",     "NPX",        FALSE,           2L,       "LOD",                       "LOD",                       # nolint object_usage_linter
  "Target 96",     "NPX",        FALSE,           2L,       "Normalization",             "Normalization",             # nolint object_usage_linter
  "Target 96",     "NPX",        FALSE,           3L,       "MaxLOD",                    c("Max LOD", "MaxLOD"),      # nolint object_usage_linter
  "Target 96",     "NPX",        TRUE,            3L,       "PlateLOD",                  c("Plate LOD", "PlateLOD")   # nolint object_usage_linter
)
