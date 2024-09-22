# code to prepare `mapping` dataset goes here
# Raw data is generated here:

eHT_e3072_mapping_rds <- system.file("extdata",
                                     "OlinkIDMapping.rds",
                                     package = "OlinkAnalyze",
                                     mustWork = TRUE)
eHT_e3072_mapping <- readRDS(file = eHT_e3072_mapping_rds) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID_ref" = "OlinkID",
        "OlinkID_notref" = "OlinkID_Explore384")
    )
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID_ref"]],
                    .data[["OlinkID_notref"]],
                    sep = "_"),
    Product_ref = "HT",
    Product_notref = "E3072"
  ) |>
  dplyr::as_tibble()
rm(eHT_e3072_mapping_rds)
