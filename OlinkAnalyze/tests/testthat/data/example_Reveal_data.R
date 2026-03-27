# Generating Example Reveal Data

set.seed(1234)

# sample identifiers
sample_id <- c(paste0("Sample_", LETTERS[1L:26L]),
               paste0("Sample_A", LETTERS[1L:26L]),
               paste0("Sample_B", LETTERS[1L:26L]),
               paste0("Sample_C", LETTERS[1L:26L]),
               paste0("Sample_D", LETTERS[1L:26L]),
               paste0("Sample_E", LETTERS[1L:26L]),
               paste0("Sample_F", LETTERS[1L:26L]))[1L:172L]
sample_id <- dplyr::if_else(
  nchar(sample_id) == 8L | grepl(pattern = "^Sample_A", x = sample_id),
  sample_id,
  paste0(sample_id, "_Reveal")
)
sample_id_ctrl <- paste0("CONTROL_SAMPLE_", 1L:6L)
sample_id_pc <- paste0("PLATE_CONTROL_", 1L:10L)
sample_id_nc <- paste0("NEGATIVE_CONTROL_", 1L:4L)

df_samples <- dplyr::tibble(
  SampleID = c(sample_id,
               sample_id_ctrl,
               sample_id_pc,
               sample_id_nc),
  SampleType = c(rep(x = "SAMPLE", times = length(sample_id)),
                 rep(x = "SAMPLE_CONTROL", times = length(sample_id_ctrl)),
                 rep(x = "PLATE_CONTROL", times = length(sample_id_pc)),
                 rep(x = "NEGATIVE_CONTROL", times = length(sample_id_nc))),
  WellID = rep(
    x = paste0(
      rep(x = LETTERS[1L:8L], each = 12L),
      rep(x = 1L:12L, times = 8L)
    ),
    times = 2L
  ),
  PlateID = c(rep(x = "Plate1", times = length(sample_id) / 2L),
              rep(x = "Plate2", times = length(sample_id) / 2L),
              rep(x = "Plate1", times = length(sample_id_ctrl) / 2L),
              rep(x = "Plate2", times = length(sample_id_ctrl) / 2L),
              rep(x = "Plate1", times = length(sample_id_pc) / 2L),
              rep(x = "Plate2", times = length(sample_id_pc) / 2L),
              rep(x = "Plate1", times = length(sample_id_nc) / 2L),
              rep(x = "Plate2", times = length(sample_id_nc) / 2L))
)

reveal_oids <- OlinkAnalyze:::reveal_e3072_mapping |>
  filter(OlinkID_E3072 %in% eHT_e3072_mapping$OlinkID_E3072[1L:100L]) |>
  select(OlinkID_Reveal, UniProt, Assay)

# Generate assays
df_assays <- dplyr::tibble(
  OlinkID = reveal_oids$OlinkID_Reveal,
  UniProt = reveal_oids$UniProt,
  Assay = reveal_oids$Assay,
  AssayType = rep(x = "assay", times = nrow(reveal_oids)),
  Panel = "Reveal",
  Block = "Reveal"
)
sample_oid <- sample(x = df_assays$OlinkID, size = 5L, replace = FALSE)

# Correlation Assay
oid <- "OID43204"



# Non overlapping assay
unique_assay <- dplyr::tibble(
  OlinkID = "OID54321",
  UniProt = "RAND_REVEAL",
  Assay = "TEST_Reveal",
  Panel = "Reveal",
  AssayType = "assay",
  Block =  "Reveal",
) |>
  dplyr::distinct()

df_assays <- rbind(df_assays, unique_assay)

# Combine data and generate NPX dataset
data_reveal <- tidyr::expand_grid(
  SampleID = df_samples$SampleID,
  OlinkID = df_assays$OlinkID
) |>
  as.data.frame() |>
  dplyr::left_join(
    df_samples,
    by = "SampleID",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    df_assays,
    by = "OlinkID",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(
    dplyr::pick(
      dplyr::all_of(
        c("OlinkID", "SampleType")
      )
    )
  ) |>
  dplyr::mutate(
    NPX = ifelse(
      .data[["SampleType"]] == "SAMPLE",
      rnorm(n = length(sample_id), sd = 3, mean = 1),
      ifelse(
        .data[["SampleType"]] == "NEGATIVE_CONTROL",
        rnorm(length(sample_id_nc), sd = 1, mean = -1),
        rnorm(length(c(sample_id_pc, sample_id_ctrl)), sd = 2, mean = 0)
      )
    ),
    NPX = ifelse(
      .data[["OlinkID"]] %in% sample_oid,
      rnorm(n = length(sample_id), sd = 0.5, mean = 0),
      .data[["NPX"]]
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    PCNormalizedNPX = .data[["NPX"]],
    Count = ifelse(
      OlinkID %in% reveal_oids$OlinkID_Reveal,
      sample(x = 1L:150L, size = nrow(df_samples), replace = TRUE),
      sample(x = 1L:1000, size = dplyr::n(), replace = TRUE)
    ),
    Normalization = "Plate control",
    AssayQC = "PASS",
    SampleQC = "PASS"
  ) |>
  dplyr::as_tibble() |>
  # add DAR ID
  dplyr::mutate(
    DataAnalysisRefID = paste0("DAR00", dplyr::cur_group_id()),
    .by = dplyr::all_of(c("Block"))
  )

rm(df_assays, df_samples,
   sample_id, sample_id_ctrl, sample_id_nc, sample_id_pc, sample_oid)

# Save to RDS

data_reveal_file <- "tests/testthat/data/example_Reveal_data.rds"

if (file.exists(data_reveal_file)) {
  # if file already exists, load it and compare it to the current calculation
  data_reveal_existing <- readRDS(
    file = data_reveal_file
  )

  # if objects are identical print a relevant message
  if (identical(x = data_reveal,
                y = data_reveal_existing)) {
    cli::cli_inform(
      c("Your recalculated set of values is identical to the existing one!")
    )
  } else {
    # if not identical print a message that this is the danger zone
    cli::cli_alert_danger(
      c("DANGER ZONE: ",
        "Your recalculated set of values differs from the existing one! You may
        delete the existing {.file {data_ht_file}} file and replace it with the
        recalculated one if you are ABSOLUTELY CERTAIN ABOUT WHAT YOU ARE
        DOING!")
    )
  }

} else {
  # if file does not exist just write a new version of it
  saveRDS(object = data_reveal,
          file = data_reveal_file,
          ascii = FALSE,
          version = 2L,
          compress = "gzip")
}
