# Generating Example HT Data

# sample identifiers
sample_id <- c(paste0("Sample_", LETTERS[1L:26L]),
               paste0("Sample_A", LETTERS[1L:26L]),
               paste0("Sample_B", LETTERS[1L:26L]),
               paste0("Sample_C", LETTERS[1L:26L]),
               paste0("Sample_D", LETTERS[1L:26L]),
               paste0("Sample_E", LETTERS[1L:26L]),
               paste0("Sample_F", LETTERS[1L:26L]))[1L:172L]
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
              rep(x = "Plate2", times = length(sample_id_nc) / 2L)
  )
)

# Generate assays
assays_map <- readRDS("../OlinkAnalyze/inst/extdata/OlinkIDMapping.rds")
assays <- data.frame(OlinkID = assays_map$OlinkID[1:100],
                     UniProt = assays_map$UniProt[1:100],
                     Assay = assays_map$Gene[1:100],
                     AssayType = rep("assay", times = 100),
                     Panel = "Explore HT",
                     Block = assays_map$Block[1:100])

# Combine data
data <- expand.grid(samples$SampleID, assays$OlinkID)
names(data) <- c("SampleID", "OlinkID")
oids <- sample(assays$OlinkID, 5)
data <- data |>
  dplyr::left_join(samples) |>
  dplyr::left_join(assays)

# Generate NPX
set.seed(1234)
data <-data |>
  dplyr::group_by(OlinkID, SampleType) |>
  dplyr::mutate(NPX = ifelse(SampleType == "SAMPLE", rnorm(n = length(sampleIDs), sd = 3, mean = 1),
                      ifelse(SampleType != "NEGATIVE_CONTROL",
                             rnorm(length(c(plate_control_IDs, control_sampleIDs)), sd = 2, mean = 0),
                             rnorm(length(negative_control_ids), sd = 1, mean = -1)))) |>
  dplyr::mutate(NPX = ifelse(OlinkID %in% oids, rnorm(n = length(sampleIDs), sd = 0.5, mean = 0), NPX)) |>
  dplyr::ungroup() |>
  dplyr::mutate(PCNormalizedNPX = NPX) |>
  dplyr::mutate(Count = sample(x = 1:1000, size = nrow(data), replace = TRUE)) |>
  dplyr::mutate(Count = ifelse(OlinkID %in% assays_map$OlinkID[5], sample(x = 1:150, size = nrow(samples), replace = TRUE), Count))

data |>  dplyr::filter(OlinkID %in% assays_map$OlinkID[5]) |> dplyr::group_by(OlinkID) |> dplyr::summarise(median = median(Count))
data <- data |>
  dplyr::mutate(Normalization = "Plate control",
         AssayQC = "PASS",
         SampleQC = "PASS")
data_ht <- data







