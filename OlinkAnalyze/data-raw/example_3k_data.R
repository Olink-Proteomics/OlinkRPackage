# Generating Example Explore 3072 Data

# sample identifiers
sample_id <- c(paste0("Sample_", LETTERS[1L:26L]),
               paste0("Sample_A", LETTERS[1L:26L]),
               paste0("Sample_B", LETTERS[1L:26L]),
               paste0("Sample_C", LETTERS[1L:26L]),
               paste0("Sample_D", LETTERS[1L:26L]),
               paste0("Sample_E", LETTERS[1L:26L]),
               paste0("Sample_F", LETTERS[1L:26L]))[1L:176L]
sample_id_ctrl <- paste0("CONTROL_SAMPLE_", 1L:4L)
sample_id_pc <- paste0("PLATE_CONTROL_", 1L:6L)
sample_id_nc <- paste0("NEGATIVE_CONTROL_", 1L:6L)

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
df_assays <- dplyr::tibble(
  OlinkID = eHT_e3072_mapping$OlinkID_E3072[1L:100L],
  UniProt = eHT_e3072_mapping$UniProt[1L:100L],
  Assay = eHT_e3072_mapping$Assay[1L:100L],
  AssayType = rep(x = "assay", times = 100L),
  Panel = eHT_e3072_mapping$Panel_E3072[1L:100L],
  Block = eHT_e3072_mapping$Block_E3072[1L:100L]
)

# Combine data
data <- expand.grid(samples$SampleID, assays$OlinkID)
names(data) <- c("SampleID", "OlinkID")

data <- data |>
  left_join(samples) |>
  left_join(assays)



data <- data |>
  mutate(Normalization = "Plate control",
         AssayQC = "PASS",
         SampleQC = "PASS")


# Generate NPX
set.seed(1234)
data <-data |>
  group_by(OlinkID, SampleType) |>
  mutate(NPX = ifelse(SampleType == "SAMPLE", rnorm(n = length(sampleIDs), sd = 3, mean = 2),
                      ifelse(SampleType != "NEGATIVE_CONTROL",
                             rnorm(length(c(plate_control_IDs, control_sampleIDs)), sd = 2, mean = 1),
                             rnorm(length(negative_control_ids), sd = 1, mean = -2)))) |>
  ungroup() |>
  mutate(PCNormalizedNPX = NPX) |>
  mutate(Count = sample(x = 1:1000, size = nrow(data), replace = TRUE))

### modifying NPX range for 3K OID20054 to mark as 'not bridgeable' in bridgeable func
data <- data |>
  mutate(NPX = if_else(
    OlinkID == "OID20054",
    jitter(NPX, factor = 1, amount = 2),
    NPX)
    )

data_3k <- data

olink_pca_plot(data_3k, color_g = "SampleType")

data_3k_renamed<- data_3k |>
  rename("OlinkID_Explore384" = OlinkID) |>
  left_join(assays_map[c(2,3)]) |>
  mutate(SampleID = paste0(SampleID,"3k")) |>
  mutate(Project = "Explore 3072") |>
  select(-Block)


data_ht_renamed <- data_ht |>
  mutate(Project = "Explore HT") |>
  select(-Block)

data <- bind_rows(data_3k_renamed, data_ht_renamed)

data |>
  olink_pca_plot(color_g = "Project")
