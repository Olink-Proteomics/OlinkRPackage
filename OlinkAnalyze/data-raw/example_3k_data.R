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

samples<-data.frame(SampleID = c(sampleIDs,
                                 control_sampleIDs,
                                 plate_control_IDs,
                                 negative_control_ids),
                    SampleType = c(rep("SAMPLE", times = length(sampleIDs)),
                                   rep("SAMPLE_CONTROL", times = length(control_sampleIDs)),
                                   rep("PLATE_CONTROL", times = length(plate_control_IDs)),
                                   rep("NEGATIVE_CONTROL", times = length(negative_control_ids))),
                    WellID = rep(paste0(rep(LETTERS[1:8], each = 12), rep(1:12, times = 8)), times = 2),
                    PlateID = c(rep("Plate1", times = length(sampleIDs)/2),
                                rep("Plate2", times = length(sampleIDs)/2),
                                rep("Plate1", times = length(control_sampleIDs)/2),
                                rep("Plate2", times = length(control_sampleIDs)/2),
                                rep("Plate1", times = length(plate_control_IDs)/2),
                                rep("Plate2", times = length(plate_control_IDs)/2),
                                rep("Plate1", times = length(negative_control_ids)/2),
                                rep("Plate2", times = length(negative_control_ids)/2)
                    ))
# Generate assays
assays_map <- readRDS("../OlinkAnalyze/inst/extdata/OlinkIDMapping.rds")
assays <- data.frame(OlinkID = assays_map$OlinkID_Explore384[1:100],
                     UniProt = assays_map$UniProt[1:100],
                     Assay = assays_map$Gene[1:100],
                     AssayType = rep("assay", times = 100),
                     Panel = assays_map$Panel_Explore384[1:100],
                     Block = assays_map$Block_Explore384[1:100])

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
