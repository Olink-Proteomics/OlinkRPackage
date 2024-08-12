sampleIDs <- c(paste0("Sample_", LETTERS[1:26]),
               paste0("Sample_A", LETTERS[1:26]), 
               paste0("Sample_B", LETTERS[1:26]), 
               paste0("Sample_G", LETTERS[1:26]),
               paste0("Sample_H", LETTERS[1:26]),
               paste0("Sample_I", LETTERS[1:26]), 
               paste0("Sample_J", LETTERS[1:26])  )[1:176]
control_sampleIDs <- c(paste0("Sample_Control_", 1:4))
plate_control_IDs <- c(paste0("Plate_Control_", 1:6))
negative_control_ids <- c(paste0("Negative_Control_", 1:6))  

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
    