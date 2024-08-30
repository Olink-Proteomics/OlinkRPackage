# code to prepare `mapping` dataset goes here
# Raw data is generated here:

eHT_e3072_mapping_rds <- system.file("extdata", # nolint
                                     "OlinkIDMapping.rds",
                                     package = "OlinkAnalyze",
                                     mustWork = TRUE)
eHT_e3072_mapping <- readRDS(file = eHT_e3072_mapping_rds) |> #nolint
  dplyr::rename(
    "Assay" = "Gene",
    "OlinkID_HT" = "OlinkID",
    "OlinkID_E3072" = "OlinkID_Explore384",
    "Block_HT" = "Block",
    "Panel_E3072" = "Panel_Explore384",
    "Block_E3072" = "Block_Explore384"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("Assay", "UniProt")
    ),
    dplyr::ends_with("_HT"),
    dplyr::ends_with("_E3072")
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]], sep = "_")
  ) |>
  dplyr::as_tibble()
rm(eHT_e3072_mapping_rds)
