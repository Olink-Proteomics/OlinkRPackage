# Load data for tests ----

# load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

# sample subset used to reduce file size of the ref results
sampleSubset <- c("A6", "A38", "B47", "B22", "A43", "D75", "D79", "C66", "B43",
                  "B70", "D52", "A58", "B71", "A50", "D1", "B8")

# load data with hidden/excluded assays (all NPX = NA for specific assays)
load(file = '../data/npx_data_format221010.RData')
# loads npx_data_format221010 and npx_data_format221010.project2

# Help vars and datasets ----

# Warning if column names arent the same but results still match
npx_df2 <- OlinkAnalyze::npx_data2 |>
  dplyr::mutate(Project = 'P2',
                Normalization = "Intensity")  # adding Normalization to avoid warning

npx_df_test <- OlinkAnalyze::npx_data1 |>
  dplyr::mutate(Sample_type = "Sample") |>
  dplyr::select(SampleID, Sample_type, Index, OlinkID, UniProt, Assay,
                MissingFreq, Panel_Version, PlateID, QC_Warning, LOD, NPX,
                Subject) |>
  dplyr::mutate(Normalization = "Intensity") # adding Normalization to avoid warning

overlap_samples2 <- dplyr::intersect(npx_data1$SampleID,
                                     npx_data2$SampleID) |>
  tibble::enframe() |>
  dplyr::filter(!str_detect(value,
                            '^CONTROL_SAMPLE')) |> # Remove control samples
  dplyr::pull()

# Bridge normalization ----

## Simple bridge example ----

# select bridge samples
overlap_samples <- dplyr::intersect(npx_data1$SampleID, npx_data2$SampleID) |>
  tibble::enframe() |>  # to tibble
  dplyr::filter(!stringr::str_detect(string = value,
                                     pattern = 'CONTROL_SAMPLE')) |> # remove control samples
  dplyr::pull(value)

# bridge normalize
normalization_results.bridged <-
  olink_normalization_bridge(
    project_1_df =
      {
        npx_data1 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    project_2_df =
      {
        npx_data2 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    bridge_samples = list("DF1" = overlap_samples,
                          "DF2" = overlap_samples),
    project_1_name = '20200001',
    project_2_name = '20200002',
    project_ref_name = '20200001') |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) |> # removing Normalization to match instance from ref_results
  dplyr::select(SampleID, dplyr::everything())

## Bridge example with hidden/excluded assays ----

# Bridge normalization with excluded assays
excludedOIDs.proj1 <-
  npx_data_format221010 |>
  dplyr::group_by(OlinkID) |>
  dplyr::summarise(fracNA = sum(is.na(NPX)) / dplyr::n(),
                   .groups = 'drop') |>
  dplyr::filter(fracNA == 1) |>
  dplyr::pull(OlinkID)

excludedOIDs.proj2 <-
  npx_data_format221010.project2 |>
  dplyr::group_by(OlinkID) %>%
  dplyr::summarise(fracNA = sum(is.na(NPX)) / dplyr::n(),
                   .groups = 'drop') |>
  dplyr::filter(fracNA == 1) |>
  dplyr::pull(OlinkID)

overlap_samples_na <- intersect(npx_data_format221010$SampleID,
                                npx_data_format221010.project2$SampleID)

npxBridged <-
  olink_normalization_bridge(
    project_1_df = npx_data_format221010,
    project_2_df = npx_data_format221010.project2,
    bridge_samples = list("DF1" = overlap_samples_na,
                          "DF2" = overlap_samples_na),
    project_1_name = 'P1',
    project_2_name = 'P2',
    project_ref_name = 'P1')

npxBridged_proj2ref <-
  olink_normalization_bridge(
    project_1_df = npx_data_format221010,
    project_2_df = npx_data_format221010.project2,
    bridge_samples = list("DF1" = overlap_samples_na,
                          "DF2" = overlap_samples_na),
    project_1_name = 'P1',
    project_2_name = 'P2',
    project_ref_name = 'P2')

# Subset and Intensity normalization ----

## Intensity ----

normalization_results.intensity <-
  olink_normalization_subset(
    project_1_df =
      {
        npx_data1 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    project_2_df =
      {
        npx_data2 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    reference_samples = list("DF1" = unique(npx_data1$SampleID),
                             "DF2" = unique(npx_data2$SampleID)),
    project_1_name = 'P1',
    project_2_name = 'P2',
    project_ref_name = 'P1'
  ) |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) # removing Normalization to match instance from ref_results

## Subset ----

# NOTE: this is just a randomly selected subset of samples from npx_data2
#       that allows us to test the function
sampleSubset.adj <- c("C6", "C21", "C28", "C50", "C19", "D5", "A30", "C52",
                      "D77", "D3", "D16", "C72", "A52", "D67", "C77", "C22",
                      "D62", "D39", "C34", "C13")

normalization_results.subset <-
  olink_normalization_subset(
    project_1_df =
      {
        npx_data1 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    project_2_df =
      {
        npx_data2 |>
          dplyr::mutate(Normalization = "Intensity")
      },
    reference_samples = list("DF1" = unique(npx_data1$SampleID),
                             "DF2" = sampleSubset.adj),
    project_1_name = '20200001',
    project_2_name = '20200002',
    project_ref_name = '20200001'
  ) |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) # removing Normalization to match instance from ref_results

# Multi-project normalization ----

## Bridge normalization ----

### Simple bridge example ----

norm_schema_bridge.n <- dplyr::tibble(
  order              = c(1, 2),
  name               = c("20200001", "20200002"),
  data               = list("20200001" =
                              {
                                npx_data1 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              },
                            "20200002" =
                              {
                                npx_data2 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              }),
  samples            = list("20200001" = NA_character_,
                            "20200002" = list("DF1" = overlap_samples,
                                              "DF2" = overlap_samples)),
  normalization_type = c(NA_character_, "Bridge"),
  normalize_to       = c(NA_character_, "1")
)
normalization_results.bridged_n <-
  olink_normalization_n(norm_schema = norm_schema_bridge.n) |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) |> # removing Normalization to match instance from ref_results
  dplyr::select(SampleID:Time, Project, Panel, Adj_factor) # rearranging to match reference bridge

### Bridge normalization with excluded assays ----

norm_schema_npxBridged.n <- dplyr::tibble(
  order              = c(1, 2),
  name               = c("P1", "P2"),
  data               = list("P1" = npx_data_format221010,
                            "P2" = npx_data_format221010.project2),
  samples            = list("P1" = NA_character_,
                            "P2" = list("DF1" = overlap_samples_na,
                                        "DF2" = overlap_samples_na)),
  normalization_type = c(NA_character_, "Bridge"),
  normalize_to       = c(NA_character_, "1")
)
npxBridged.n <-
  olink_normalization_n(norm_schema = norm_schema_npxBridged.n)

norm_schema_npxBridged_proj2ref.n <- dplyr::tibble(
  order              = c(2, 1),
  name               = c("P1", "P2"),
  data               = list("P1" = npx_data_format221010,
                            "P2" = npx_data_format221010.project2),
  samples            = list("P1" = list("DF1" = overlap_samples_na,
                                        "DF2" = overlap_samples_na),
                            "P2" = NA_character_),
  normalization_type = c("Bridge", NA_character_),
  normalize_to       = c("1", NA_character_)
)
npxBridged_proj2ref.n <-
  olink_normalization_n(norm_schema = norm_schema_npxBridged_proj2ref.n)

## Subset/Intensity normalization ----

### Intensity ----

norm_schema_intensity.n <- dplyr::tibble(
  order              = c(1, 2),
  name               = c("P1", "P2"),
  data               = list("P1" =
                              {
                                npx_data1 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              },
                            "P2" =
                              {
                                npx_data2 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              }),
  samples            = list("P1" = NA_character_,
                            "P2" = list("DF1" = unique(npx_data1$SampleID),
                                        "DF2" = unique(npx_data2$SampleID))),
  normalization_type = c(NA_character_, "Subset"),
  normalize_to       = c(NA_character_, "1")
)
normalization_results.intensity_n <-
  olink_normalization_n(norm_schema = norm_schema_intensity.n) |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) |> # removing Normalization to match instance from ref_results
  dplyr::select(SampleID:Time, Project, Panel, Adj_factor) # rearranging to match reference bridge

### Subset ----

norm_schema_subset.n <- dplyr::tibble(
  order              = c(1, 2),
  name               = c("20200001", "20200002"),
  data               = list("20200001" =
                              {
                                npx_data1 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              },
                            "20200002" =
                              {
                                npx_data2 |>
                                  dplyr::select(-Project) |>
                                  dplyr::mutate(Normalization = "Intensity")
                              }),
  samples            = list("20200001" = NA_character_,
                            "20200002" = list("DF1" = unique(npx_data1$SampleID),
                                              "DF2" = sampleSubset.adj)),
  normalization_type = c(NA_character_, "Subset"),
  normalize_to       = c(NA_character_, "1")
)
normalization_results.subset_n <-
  olink_normalization_n(norm_schema = norm_schema_subset.n) |>
  dplyr::filter(SampleID %in% sampleSubset) |>
  dplyr::select(-Normalization) |> # removing Normalization to match instance from ref_results
  dplyr::select(SampleID:Time, Project, Panel, Adj_factor) # rearranging to match reference bridge

# Test that function works ----

## Test bridge function ----

test_that("olink_normalization bridge standalone function works",
          {
            # expect two warnings thrown:
            #   one from missing column "Sample_Type" in df2
            #   one from missing columns "Treatment,Site,Time,Project,Panel" in df1
            expect_warning(
              expect_warning(
                olink_normalization_bridge(project_1_df = npx_df_test,
                                           project_2_df = npx_df2,
                                           bridge_samples = list("DF1" = overlap_samples2,
                                                                 "DF2" = overlap_samples2),
                                           project_1_name = 'P1',
                                           project_2_name = 'P2',
                                           project_ref_name = 'P1')
              )
            )

            # check that simple bridging example is replicated in the reference set
            expect_equal(normalization_results.bridged,
                         ref_results$normalization_results.bridged)

            expect_error(
              {
                olink_normalization_bridge(project_1_df =
                                             {
                                               npx_data1 |>
                                                 dplyr::select(-Project) |>
                                                 dplyr::mutate(Normalization = "Intensity")
                                             },
                                           project_2_df =
                                             {
                                               npx_data2 |>
                                                 dplyr::select(-Project) |>
                                                 dplyr::mutate(Normalization = "Intensity")
                                             }
                )
              }
            ) # Non overlapping samples for bridging

            expect_error(
              {
                olink_normalization_bridge(project_1_df =
                                             {
                                               npx_data1 |>
                                                 dplyr::select(-Project) |>
                                                 dplyr::mutate(Normalization = "Intensity")
                                             },
                                           project_2_df =
                                             {
                                               npx_data2 |>
                                                 dplyr::select(-Project) |>
                                                 dplyr::mutate(Normalization = "Intensity")
                                             },
                                           bridge_samples = list("DF1" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"),
                                                                 "DF2" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"))
                )
              }
            ) # Non overlapping samples for bridging
          }
)

## Test subset/intensity function ----

test_that("olink_normalization subset standalone function works",
          {
            # check that intensity example is replicated in the reference set
            expect_equal(normalization_results.intensity,
                         ref_results$normalization_results.intensity)

            # check that subset example is replicated in the reference set
            expect_equal(normalization_results.subset,
                         ref_results$normalization_results.subset)
          }
)

## Test multi-batch function ----

test_that("olink_normalization multi-batch works",
          {
            # Errors ----

            ## error thrown when column is not provided ----
            expect_error(
              {
                olink_normalization_n(
                  norm_schema = dplyr::tibble(
                    order              = c(1, 2),
                    name               = c("P1", "P2"),
                    data               = list("P1" =
                                                {
                                                  npx_data1 |>
                                                    dplyr::select(-Project) |>
                                                    dplyr::mutate(Normalization = "Intensity")
                                                },
                                              "P2" =
                                                {
                                                  npx_data2 |>
                                                    dplyr::select(-Project) |>
                                                    dplyr::mutate(Normalization = "Intensity")
                                                }),
                    normalization_type = c(NA_character_, "Bridge"),
                    normalize_to       = c(NA_character_, "1")
                  )
                )
              }
            ) # Non overlapping samples for bridging

            ## error thrown if order is NA ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, NA),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if order is Inf ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, Inf),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if order is character ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, "A"),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if order is does not start from 1 ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(2L, 3L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if order is does not increase by 1 ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 3L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if data is not in a nested list ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = c("20200001" = 1L,
                                          "20200002" = 2L),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if data is the nested list is not a data.frame ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" = 1L,
                                          "20200002" = 2L),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if data is the nested list contains NA ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" = NA),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if column "Project" is present ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1, 2),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if column "Adj_factor" is present ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1, 2),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity",
                                                              Adj_factor = -1)
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity",
                                                              Adj_factor = -1)
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if samples is not a list ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = c("20200001" = NA_character_,
                                       "20200002" = NA),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if normalization_type is not a character vector ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = list(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1")
              )))

            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(1, 2),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if normalization_type does not contain expected value ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "A"),
                normalize_to       = c(NA_character_, "1")
              )))

            ## error thrown if normalize_to is not a character array ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = list(NA_character_, "1")
              )))

            ## error thrown if elements from normalize_to not present in order column ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1,3")
              )))

            ## error thrown if elements from normalize_to contain order column identifier from same row ----
            expect_error(olink_normalization_n(
              norm_schema = dplyr::tibble(
                order              = c(1L, 2L),
                name               = c("20200001", "20200002"),
                data               = list("20200001" =
                                            {
                                              npx_data1 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            },
                                          "20200002" =
                                            {
                                              npx_data2 |>
                                                dplyr::select(-Project) |>
                                                dplyr::mutate(Normalization = "Intensity")
                                            }),
                samples            = list("20200001" = NA_character_,
                                          "20200002" = list("DF1" = overlap_samples,
                                                            "DF2" = overlap_samples)),
                normalization_type = c(NA_character_, "Bridge"),
                normalize_to       = c(NA_character_, "1,2")
              )))

            ## error thrown when bridge/subset samples are not present ----

            expect_error(
              {
                olink_normalization_n(
                  norm_schema = dplyr::tibble(
                    order              = c(1, 2),
                    name               = c("P1", "P2"),
                    data               = list("P1" =
                                                {
                                                  npx_data1 |>
                                                    dplyr::select(-Project) |>
                                                    dplyr::mutate(Normalization = "Intensity")
                                                },
                                              "P2" =
                                                {
                                                  npx_data2 |>
                                                    dplyr::select(-Project) |>
                                                    dplyr::mutate(Normalization = "Intensity")
                                                }),
                    samples            = list("P1" = NA_character_,
                                              "P2" = list("DF1" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"),
                                                          "DF2" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"))),
                    normalization_type = c(NA_character_, "Bridge"),
                    normalize_to       = c(NA_character_, "1")
                  )
                )
              }
            ) # Non overlapping samples for bridging

            # Reference ----

            ## check that simple bridging example is replicated in the reference set ----
            expect_equal(normalization_results.bridged_n,
                         ref_results$normalization_results.bridged)

            # check that intensity example is replicated in the reference set ----
            expect_equal(normalization_results.intensity_n,
                         ref_results$normalization_results.intensity)

            # check that subset example is replicated in the reference set ----
            expect_equal(normalization_results.subset_n,
                         ref_results$normalization_results.subset)
            # Hidden/removed assays ----

            ### Testing the excluded assay bridging ###
            ## With P1 as the reference
            #Test that all excluded assays from project 1 remain NA after bridging
            expect_true(npxBridged.n %>%
                          filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                          pull(NPX) %>%
                          is.na() %>%
                          all())

            #Test that the non-excluded assays in project 2 remain unchanged
            expect_true(npxBridged.n %>%
                          filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                          left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                          mutate(match = NPX.x == NPX.y) %>%
                          pull(match) %>%
                          all())

            ## With P2 as the reference
            #Test that all excluded assays from project 1 remain NA after bridging
            expect_true(npxBridged_proj2ref.n %>%
                          filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                          pull(NPX) %>%
                          is.na() %>%
                          all())

            #Test that the non-excluded assays in project 2 remain unchanged
            expect_true(npxBridged_proj2ref.n %>%
                          filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                          left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                          mutate(match = NPX.x == NPX.y) %>%
                          pull(match) %>%
                          all())
          }
          )

## Other tests ----

### Test missing "Normalize" column ----

test_that("missing Normalization column works", {

  # Warning when both df1 and df2 are lacking column "Normalization" ----
  expect_warning(
    olink_normalization_bridge(project_1_df = npx_data1,
                               project_2_df = npx_data2,
                               bridge_samples = list("DF1" = overlap_samples,
                                                     "DF2" = overlap_samples),
                               project_1_name = '20200001',
                               project_2_name = '20200002',
                               project_ref_name = '20200001'),
    "Variable \"Normalization\" not present in df1 and df2")

  expect_warning(
    olink_normalization_n(
      norm_schema = dplyr::tibble(
        order              = c(1, 2),
        name               = c("20200001", "20200002"),
        data               = list("20200001" =
                                    {
                                      npx_data1 |>
                                        dplyr::select(-Project)
                                    },
                                  "20200002" =
                                    {
                                      npx_data2 |>
                                        dplyr::select(-Project)
                                    }),
        samples            = list("20200001" = NA_character_,
                                  "20200002" = list("DF1" = overlap_samples,
                                                    "DF2" = overlap_samples)),
        normalization_type = c(NA_character_, "Bridge"),
        normalize_to       = c(NA_character_, "1")
      )
    ),
    "Variable \"Normalization\" not present in df1 and df2")

  # conly df1 contains column "Normalization" ----
  expect_warning(
    expect_warning(
      olink_normalization_bridge(project_1_df =
                                   {
                                     npx_data1 |>
                                       dplyr::mutate(Normalization = "Intensity")
                                   },
                                 project_2_df = npx_data2,
                                 bridge_samples = list("DF1" = overlap_samples,
                                                       "DF2" = overlap_samples),
                                 project_1_name = '20200001',
                                 project_2_name = '20200002',
                                 project_ref_name = '20200001'),
      "Variable \"Normalization\" not present in df2."),
    "The following columns are found in df1 but not df2"
  )

  expect_warning(
    expect_warning(
      olink_normalization_n(
        norm_schema = dplyr::tibble(
          order              = c(1, 2),
          name               = c("20200001", "20200002"),
          data               = list("20200001" =
                                      {
                                        npx_data1 |>
                                          dplyr::select(-Project) |>
                                          dplyr::mutate(Normalization = "Intensity")
                                      },
                                    "20200002" =
                                      {
                                        npx_data2 |>
                                          dplyr::select(-Project)
                                      }),
          samples            = list("20200001" = NA_character_,
                                    "20200002" = list("DF1" = overlap_samples,
                                                      "DF2" = overlap_samples)),
          normalization_type = c(NA_character_, "Bridge"),
          normalize_to       = c(NA_character_, "1")
        )
      ),
      "Variable \"Normalization\" not present in df2."),
    "The following columns are found in df1 but not df2"
  )

  # 2 Warnings if only df2 contains column "Normalization" ----
  expect_warning(
    expect_warning(
      olink_normalization_bridge(project_1_df = npx_data1,
                                 project_2_df =
                                   {
                                     npx_data2 |>
                                       dplyr::mutate(Normalization = "Intensity")
                                   },
                                 bridge_samples = list("DF1" = overlap_samples,
                                                       "DF2" = overlap_samples),
                                 project_1_name = '20200001',
                                 project_2_name = '20200002',
                                 project_ref_name = '20200001'),
      "Variable \"Normalization\" not present in df1."),
    "The following columns are found in df2 but not df1"
  )

  expect_warning(
    expect_warning(
      olink_normalization_n(
        norm_schema = dplyr::tibble(
          order              = c(1, 2),
          name               = c("20200001", "20200002"),
          data               = list("20200001" =
                                      {
                                        npx_data1 |>
                                          dplyr::select(-Project)
                                      },
                                    "20200002" =
                                      {
                                        npx_data2 |>
                                          dplyr::select(-Project) |>
                                          dplyr::mutate(Normalization = "Intensity")
                                      }),
          samples            = list("20200001" = NA_character_,
                                    "20200002" = list("DF1" = overlap_samples,
                                                      "DF2" = overlap_samples)),
          normalization_type = c(NA_character_, "Bridge"),
          normalize_to       = c(NA_character_, "1")
        )
      ),
      "Variable \"Normalization\" not present in df1."),
    "The following columns are found in df2 but not df1"
  )
}
)

### Test that df1 and df2 are normalized similarly ----

test_that("df1 and df2 same normalization", {
  # different normalization with expected values in Normalization column ----
  expect_warning(
    olink_normalization_bridge(project_1_df =
                                 {
                                   npx_data1 |>
                                     dplyr::mutate(Normalization = "Intensity")
                                 },
                               project_2_df =
                                 {
                                   npx_data2 |>
                                     dplyr::mutate(Normalization = "Plate control")
                                 },
                               bridge_samples = list("DF1" = overlap_samples,
                                                     "DF2" = overlap_samples),
                               project_1_name = '20200001',
                               project_2_name = '20200002',
                               project_ref_name = '20200001'),
    "df1 and df2 are not normalized with the same approach. Consider renormalizing.")

  expect_warning(
    olink_normalization_n(
      norm_schema = dplyr::tibble(
        order              = c(1, 2),
        name               = c("20200001", "20200002"),
        data               = list("20200001" =
                                    {
                                      npx_data1 |>
                                        dplyr::select(-Project) |>
                                        dplyr::mutate(Normalization = "Intensity")
                                    },
                                  "20200002" =
                                    {
                                      npx_data2 |>
                                        dplyr::select(-Project) |>
                                        dplyr::mutate(Normalization = "Plate control")
                                    }),
        samples            = list("20200001" = NA_character_,
                                  "20200002" = list("DF1" = overlap_samples,
                                                    "DF2" = overlap_samples)),
        normalization_type = c(NA_character_, "Bridge"),
        normalize_to       = c(NA_character_, "1")
      )
    ),
    "df1 and df2 are not normalized with the same approach. Consider renormalizing.")

  # For future use ----
  # different normalization with unexpected values in Normalization column
  # currently we do not check values in this column, but in the future we might
  expect_warning(
    olink_normalization_bridge(project_1_df =
                                 {
                                   npx_data1 |>
                                     dplyr::mutate(Normalization = "A")
                                 },
                               project_2_df =
                                 {
                                   npx_data2 |>
                                     dplyr::mutate(Normalization = "B")
                                 },
                               bridge_samples = list("DF1" = overlap_samples,
                                                     "DF2" = overlap_samples),
                               project_1_name = '20200001',
                               project_2_name = '20200002',
                               project_ref_name = '20200001'),
    "df1 and df2 are not normalized with the same approach. Consider renormalizing.")

  expect_warning(
    olink_normalization_n(
      norm_schema = dplyr::tibble(
        order              = c(1, 2),
        name               = c("20200001", "20200002"),
        data               = list("20200001" =
                                    {
                                      npx_data1 |>
                                        dplyr::select(-Project) |>
                                        dplyr::mutate(Normalization = "A")
                                    },
                                  "20200002" =
                                    {
                                      npx_data2 |>
                                        dplyr::select(-Project) |>
                                        dplyr::mutate(Normalization = "B")
                                    }),
        samples            = list("20200001" = NA_character_,
                                  "20200002" = list("DF1" = overlap_samples,
                                                    "DF2" = overlap_samples)),
        normalization_type = c(NA_character_, "Bridge"),
        normalize_to       = c(NA_character_, "1")
      )
    ),
    "df1 and df2 are not normalized with the same approach. Consider renormalizing.")
}
)

### Test that Different columns in dfs can be normalized ----

test_that("Different columns in dfs can be normalized",{
  # Bridge ----
  mismatch_col_norm <- olink_normalization_bridge(project_1_df = npx_df_test,
                             project_2_df = npx_df2,
                             bridge_samples = list("DF1" = overlap_samples2,
                                                   "DF2" = overlap_samples2),
                             project_1_name = 'P1',
                             project_2_name = 'P2',
                             project_ref_name = 'P1') |>
    suppressWarnings() |>
    dplyr::filter(SampleID %in% sampleSubset)

  # Columns are as expected
  expect_equal(
    {
      mismatch_col_norm |>
        colnames() |>
        sort()
    },
    {
      c(colnames(npx_df_test),
        colnames(npx_df2),
        "Adj_factor") |>
        unique() |>
        sort()
    })

  # NAs are where expected
  col_notdf2 <- setdiff(colnames(mismatch_col_norm),
                        colnames(npx_df2)
                        ) |>
    stringr::str_remove("Adj_factor")

  col_notdf1 <- setdiff(colnames(mismatch_col_norm),
                        colnames(npx_df_test)
                        ) |>
    stringr::str_remove("Adj_factor")

  expect_equal(
    {
      mismatch_col_norm |>
        dplyr::filter(Project == "P2") |>
        dplyr::select(dplyr::all_of(col_notdf2[-length(col_notdf2)])) |>
        dplyr::distinct() |>
        dplyr::pull()
      }, "NA")

  expect_equal(
    {
      mismatch_col_norm |>
        dplyr::filter(Project == "P1") |>
        dplyr::select(dplyr::all_of(col_notdf1[-length(col_notdf1)])) |>
        dplyr::distinct() |>
        dplyr::pull()
    }, "NA")

  expect_equal(normalization_results.bridged$NPX, mismatch_col_norm$NPX)
})

### Test removed/hidden/missing assays ----

test_that("olink_normalization works form missing/hidden/removed assays",
          {
            ### Testing the excluded assay bridging ###
            ## With P1 as the reference
            #Test that all excluded assays from project 1 remain NA after bridging
            expect_true(npxBridged %>%
                          filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                          pull(NPX) %>%
                          is.na() %>%
                          all())

            #Test that the non-excluded assays in project 2 remain unchanged
            expect_true(npxBridged %>%
                          filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                          left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                          mutate(match = NPX.x == NPX.y) %>%
                          pull(match) %>%
                          all())

            ## With P2 as the reference
            #Test that all excluded assays from project 1 remain NA after bridging
            expect_true(npxBridged_proj2ref %>%
                          filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                          pull(NPX) %>%
                          is.na() %>%
                          all())

            #Test that the non-excluded assays in project 2 remain unchanged
            expect_true(npxBridged_proj2ref %>%
                          filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                          left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                          mutate(match = NPX.x == NPX.y) %>%
                          pull(match) %>%
                          all())
          }
)
