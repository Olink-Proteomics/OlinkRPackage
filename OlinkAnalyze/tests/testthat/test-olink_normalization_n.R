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

  complex_npx_file <- system.file("extdata", "complex_dataset.rds", package = "OlinkAnalyze", mustWork = TRUE)
  complex_npx_data <- readRDS(complex_npx_file)

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

  ## Multi-project mixed normalization ----

  ### Simple multi-project normalization example ----

  npx_multi_df1 <- npx_data1 |>
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
    dplyr::select(-Project) |>
    dplyr::mutate(Normalization = "Intensity")

  npx_multi_df2 <- npx_data2 |>
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
    dplyr::select(-Project) |>
    dplyr::mutate(Normalization = "Intensity")

  # manipulating the sample NPX datasets to create another two random ones
  npx_multi_df3 <- npx_data2 |>
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
    dplyr::mutate(SampleID = paste(SampleID, "_mod", sep = ""),
                  PlateID = paste(PlateID, "_mod", sep = "")) |>
    dplyr::select(-Project) |>
    dplyr::mutate(Normalization = "Intensity")

  npx_multi_df4 <- npx_data1 |>
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
    dplyr::mutate(SampleID = paste(SampleID, "_mod2", sep = ""),
                  PlateID = paste(PlateID, "_mod2", sep = "")) |>
    dplyr::select(-Project) |>
    dplyr::mutate(Normalization = "Intensity")

  ## samples to use for normalization
  # Bridge samples with same identifiers between npx_df1 and npx_df2
  overlap_samples_df1_df2 <- list("DF1" = overlap_samples,
                                  "DF2" = overlap_samples)

  # Bridge samples with different identifiers between npx_df2 and npx_df3
  overlap_samples_df2_df3 <- list("DF1" = c("A13", "A29", "A30", "A36", "A45", "A46", "A52", "A63", "A71", "A73"),
                                  "DF2" = c("C1_mod", "C10_mod", "C11_mod", "C12_mod", "C13_mod", "C14_mod", "C15_mod", "C16_mod", "C17_mod", "C18_mod"))

  # Samples to use for intensity normalization between npx_df4 and the
  # normalized dataset of npx_df1 and npx_df2
  overlap_samples_df13_df4 <- list("DF1" = c("A1", "A10", "A11", "A12", "A13", "A13_mod", "A14", "A15", "A16", "A17", "A18", "A19", "A2", "A20", "A21", "A22", "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A29_mod", "A3", "A30", "A30_mod", "A31", "A32", "A33", "A34", "A35", "A36", "A36_mod", "A37", "A38", "A39", "A4", "A40", "A41", "A42", "A43", "A44", "A45", "A45_mod", "A46", "A46_mod", "A47", "A48", "A49", "A5", "A50", "A51", "A52", "A52_mod", "A53", "A54", "A55", "A56", "A57", "A58", "A59", "A6", "A60", "A61", "A62", "A63", "A63_mod", "A64", "A65", "A66", "A67", "A68", "A69", "A7", "A70", "A71", "A71_mod", "A72", "A73", "A73_mod", "A74", "A75", "A76", "A77", "A8", "A9"),
                                   "DF2" = c("B1_mod2", "B10_mod2", "B11_mod2", "B12_mod2", "B13_mod2", "B14_mod2", "B15_mod2", "B16_mod2", "B17_mod2", "B18_mod2", "B19_mod2", "B2_mod2", "B20_mod2", "B21_mod2", "B22_mod2", "B23_mod2", "B24_mod2", "B25_mod2", "B26_mod2", "B27_mod2", "B28_mod2", "B29_mod2", "B3_mod2", "B30_mod2", "B31_mod2", "B32_mod2", "B33_mod2", "B34_mod2", "B35_mod2", "B36_mod2", "B37_mod2", "B38_mod2", "B39_mod2", "B4_mod2", "B40_mod2", "B41_mod2", "B42_mod2", "B43_mod2", "B44_mod2", "B45_mod2", "B46_mod2", "B47_mod2", "B48_mod2", "B49_mod2", "B5_mod2", "B50_mod2", "B51_mod2", "B52_mod2", "B53_mod2", "B54_mod2", "B55_mod2", "B56_mod2", "B57_mod2", "B58_mod2", "B59_mod2", "B6_mod2", "B60_mod2", "B61_mod2", "B62_mod2", "B63_mod2", "B64_mod2", "B65_mod2", "B66_mod2", "B67_mod2", "B68_mod2", "B69_mod2", "B7_mod2", "B70_mod2", "B71_mod2", "B72_mod2", "B73_mod2", "B74_mod2", "B75_mod2", "B76_mod2", "B77_mod2", "B78_mod2", "B79_mod2", "B8_mod2", "B9_mod2"))

  # create tibble for input
  norm_schema_npxMulti <- dplyr::tibble(
    order              = c(1, 2, 3, 4),
    name               = c("NPX_DF1", "NPX_DF2", "NPX_DF3", "NPX_DF4"),
    data               = list("NPX_DF1" = npx_multi_df1,
                              "NPX_DF2" = npx_multi_df2,
                              "NPX_DF3" = npx_multi_df3,
                              "NPX_DF4" = npx_multi_df4),
    samples            = list("NPX_DF1" = NA_character_,
                              "NPX_DF2" = overlap_samples_df1_df2,
                              "NPX_DF3" = overlap_samples_df2_df3,
                              "NPX_DF4" = overlap_samples_df13_df4),
    normalization_type = c(NA_character_, "Bridge", "Bridge", "Subset"),
    normalize_to       = c(NA_character_, "1", "2", "1,3")
  )

  normalization_results.multi <-
    olink_normalization_n(norm_schema = norm_schema_npxMulti) |>
    dplyr::mutate(SampleID_tmp =
                    {
                      SampleID |>
                        stringr::str_split(pattern = "_") |>
                        lapply(head, 1) |>
                        unlist()
                    }) |>
    dplyr::filter(SampleID_tmp %in% sampleSubset) |>
    dplyr::select(-SampleID_tmp) |>
    dplyr::arrange(Project, Panel, OlinkID, SampleID)

### Advanced  multi-project normalization example ----

normalization_results.complex_n <-
  olink_normalization_n(norm_schema = complex_npx_data)

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
                                           bridge_samples = list("DF1" = overlap_samples,
                                                                 "DF2" = overlap_samples),
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

            # check that multi-batch simple example is replicated in the reference set ----
            expect_equal(normalization_results.multi,
                         ref_results$normalization_results.multi)

            # check that multi-batch advanced example works as expected ----
            expect_equal({ normalization_results.complex_n |> nrow() },
                         14520L)

            expect_equal({ normalization_results.complex_n |> ncol() },
                         16L)

            expect_equal({ normalization_results.complex_n$Project |> unique() },
                         c("DF1b",
                           "DF2b",
                           "DF3b",
                           "DF4b",
                           "DF5b",
                           "DF6b",
                           "DF1s",
                           "DF2s",
                           "DF3s",
                           "DF4s",
                           "DF5s"))

            expect_equal(
              {
                c({ normalization_results.complex_n$Adj_factor |> min(na.rm = TRUE) },
                  { normalization_results.complex_n$Adj_factor |> max(na.rm = TRUE) },
                  { normalization_results.complex_n$Adj_factor |> median(na.rm = TRUE) },
                  { normalization_results.complex_n$Adj_factor |> mean(na.rm = TRUE) },
                  { normalization_results.complex_n$Adj_factor |> quantile(probs = 0.9, na.rm = TRUE, names = FALSE) },
                  { normalization_results.complex_n$Adj_factor |> quantile(probs = 0.1, na.rm = TRUE, names = FALSE) },
                  { normalization_results.complex_n$Adj_factor |> quantile(probs = 0.25, na.rm = TRUE, names = FALSE) },
                  { normalization_results.complex_n$Adj_factor |> quantile(probs = 0.75, na.rm = TRUE, names = FALSE) })
              },
              c(-0.4964, 0.56055, 0, -0.007244697, 0.20235, -0.22285, -0.10125, 0.06785))

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
                             bridge_samples = list("DF1" = overlap_samples,
                                                   "DF2" = overlap_samples),
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
        dplyr::pull() |>
        as.logical()
      }, NA)

  expect_equal(
    {
      mismatch_col_norm |>
        dplyr::filter(Project == "P1") |>
        dplyr::select(dplyr::all_of(col_notdf1[-length(col_notdf1)])) |>
        dplyr::distinct() |>
        dplyr::pull() |>
        as.logical()
    }, NA)

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
