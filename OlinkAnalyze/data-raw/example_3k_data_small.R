# load example 3k data ----

data_3k_file <- system.file("tests", "data", "data_3k.rds",
                            package = "OlinkAnalyze",
                            mustWork = TRUE)
data_3k <- readRDS(data_3k_file)
rm(data_3k_file)

# keep a few of the rows from the dataset ----

## select bridge samples to keep ----

bridge_samples_small <- data_3k |>
  dplyr::filter(
    !grepl(pattern = "_3k$", x = .data[["SampleID"]])
    & .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique() |>
  sort() |>
  head(40L)

## select other samples ----

other_samples_small <- data_3k |>
  dplyr::filter(
    grepl(pattern = "_3k$", x = .data[["SampleID"]])
    & .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique() |>
  sort() |>
  head(10L)

## select assays to keep ----

assays_small <- data_3k |>
  dplyr::filter(
    .data[["AssayType"]] == "assay"
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  ) |>
  unique() |>
  sort() |>
  head(2L)

# 3k small dataset ----

data_3k_small <- data_3k |>
  dplyr::filter(
    .data[["SampleID"]] %in% c(.env[["bridge_samples_small"]],
                               .env[["other_samples_small"]])
    & .data[["OlinkID"]] %in% .env[["assays_small"]]
  )

# cleanup ----

rm(data_3k,
   assays_small, bridge_samples_small, other_samples_small)
