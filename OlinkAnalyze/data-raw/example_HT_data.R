# Generating Example HT Data
# Create Sample data
sampleIDs <- c(paste0("Sample_", LETTERS[1:26]),
               paste0("Sample_A", LETTERS[1:26]),
               paste0("Sample_B", LETTERS[1:26]),
               paste0("Sample_C", LETTERS[1:26]),
               paste0("Sample_D", LETTERS[1:26]),
               paste0("Sample_E", LETTERS[1:26]),
               paste0("Sample_F", LETTERS[1:26])  )[1:172]
control_sampleIDs <- c(paste0("Sample_Control_", 1:6))
plate_control_IDs <- c(paste0("Plate_Control_", 1:10))
negative_control_ids <- c(paste0("Negative_Control_", 1:4))

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







