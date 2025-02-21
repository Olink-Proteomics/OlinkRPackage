# Explore 3072 to Explore HT Mapping

eHT_e3072_mapping_rds <- system.file("extdata",
                                     "OlinkID_HT_mapping.rds",
                                     package = "OlinkAnalyze",
                                     mustWork = TRUE)
eHT_e3072_mapping <- readRDS(file = eHT_e3072_mapping_rds) |>
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
    OlinkID = paste(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]], sep = "_"),
    Block_HT = as.character(.data[["Block_HT"]])
  ) |>
  dplyr::as_tibble()
rm(eHT_e3072_mapping_rds)

# Explore 3072 to Reveal Mapping

reveal_e3072_mapping_rds <- system.file("extdata",
                                        "OlinkID_Reveal_mapping.rds",
                                        package = "OlinkAnalyze",
                                        mustWork = TRUE)
reveal_e3072_mapping <- readRDS(file = reveal_e3072_mapping_rds) |>
  dplyr::rename(
    "Assay" = "gene_name",
    "UniProt" = "uniprot",
    "OlinkID_Reveal" = "olink_id_reveal",
    "OlinkID_E3072" = "olink_id",
    "Panel_E3072" = "panel",
    "Block_E3072" = "block"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("Assay", "UniProt")
    ),
    dplyr::ends_with("_Reveal"),
    dplyr::ends_with("_E3072")
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID_Reveal"]], .data[["OlinkID_E3072"]], sep = "_"),
    Block_Reveal = "Reveal"
  ) |>
  dplyr::as_tibble()
rm(reveal_e3072_mapping_rds)
