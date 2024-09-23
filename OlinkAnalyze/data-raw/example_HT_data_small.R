# load example HT data ----

data_ht_file <- system.file("tests", "data", "example_HT_data.rds",
                            package = "OlinkAnalyze",
                            mustWork = TRUE)
data_ht <- readRDS(data_ht_file)
rm(data_ht_file)

# keep a few of the rows from the dataset ----

## select bridge samples to keep ----

bridge_samples_small <- data_ht |>
  dplyr::filter(
    !grepl(pattern = "_HT$", x = .data[["SampleID"]])
    & .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique() |>
  sort() |>
  head(40L)

## select other samples ----

other_samples_small <- data_ht |>
  dplyr::filter(
    grepl(pattern = "_HT$", x = .data[["SampleID"]])
    & .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique() |>
  sort() |>
  head(10L)

## select assays to keep ----

assays_small <- eHT_e3072_mapping |>
  dplyr::arrange(
    .data[["OlinkID"]]
  ) |>
  dplyr::slice_head(
    n = 2L
  ) |>
  dplyr::pull(
    .data[["OlinkID_HT"]]
  )

# HT small dataset ----

data_ht_small <- data_ht |>
  dplyr::filter(
    .data[["SampleID"]] %in% c(.env[["bridge_samples_small"]],
                               .env[["other_samples_small"]])
    & .data[["OlinkID"]] %in% .env[["assays_small"]]
  )

# cleanup ----

rm(data_ht,
   assays_small, bridge_samples_small, other_samples_small)
