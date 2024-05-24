#Load reference results
refRes_file <- testthat::test_path("../data/refResults.RData")
load(refRes_file)

#Load data with hidden/excluded assays (all NPX=NA)
load(file = testthat::test_path("../data/npx_data_format221010.RData"))
# loads npx_data_format221010 and npx_data_format221010.project2

# Sample subset used to reduce file size of the ref results
sampleSubset <- c("A6", "A38","B47","B22","A43","D75","D79","C66","B43","B70","D52","A58","B71","A50","D1", "B8")

# Bridging normalization
overlap_samples <- intersect(npx_data1$SampleID, npx_data2$SampleID) %>%
  data.frame() %>%
  filter(!str_detect(., 'CONTROL_SAMPLE')) %>% #Remove control samples
  pull(.)

normalization_results.bridged <- olink_normalization(df1 = { npx_data1 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                     df2 = { npx_data2 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                     overlapping_samples_df1 = overlap_samples,
                                                     df1_project_nr = '20200001',
                                                     df2_project_nr = '20200002',
                                                     reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset) |>
  select(-Normalization) # removing Normalization to match instance from ref_results


# Warning if column names arent the same but results still match
npx_df2 <- OlinkAnalyze::npx_data2 %>%
  dplyr::mutate(Project = 'P2') %>%
  mutate(Normalization = "Intensity")  # adding Normalization to avoid warning
npx_df_test<-OlinkAnalyze::npx_data1 %>%
  dplyr::mutate(Sample_type = "Sample") %>%
  dplyr::select(SampleID, Sample_type, Index, OlinkID, UniProt, Assay, MissingFreq, Panel_Version, PlateID, QC_Warning, LOD, NPX, Subject) %>%
  mutate(Normalization = "Intensity") # adding Normalization to avoid warning
overlap_samples2 <- intersect(npx_data1$SampleID, npx_data2$SampleID) %>%
  data.frame() %>%
  filter(!str_detect(., 'CONTROL_SAMPLE')) %>% #Remove control samples
  pull(.)
# Intensity normalization
normalization_results.intensity <- olink_normalization(df1 = { npx_data1 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                       df2 = { npx_data2 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                       overlapping_samples_df1 = npx_data1$SampleID,
                                                       overlapping_samples_df2 = npx_data2$SampleID) %>%
  filter(SampleID %in% sampleSubset) |>
  select(-Normalization) # removing Normalization to match instance from ref_results

# Subset normalization
#NOTE: this subset is just a random sample in order to test the function
sampleSubset.adj <- c("C6", "C21","C28","C50","C19","D5", "A30","C52","D77","D3", "D16","C72","A52","D67","C77","C22","D62","D39","C34","C13")
normalization_results.subset <- olink_normalization(df1 = { npx_data1 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                    df2 = { npx_data2 |> mutate(Normalization = "Intensity") }, # adding Normalization to avoid warning
                                                    overlapping_samples_df1 = npx_data1$SampleID,
                                                    overlapping_samples_df2 = sampleSubset.adj,
                                                    df1_project_nr = '20200001',
                                                    df2_project_nr = '20200002',
                                                    reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset) |>
  select(-Normalization) # removing Normalization to match instance from ref_results

# Bridge normalization with excluded assays
excludedOIDs.proj1 <-
  npx_data_format221010 %>%
  group_by(OlinkID) %>%
  summarise(fracNA = sum(is.na(NPX))/n()) %>%
  filter(fracNA == 1) %>%
  pull(OlinkID)

excludedOIDs.proj2<-
  npx_data_format221010.project2 %>%
  group_by(OlinkID) %>%
  summarise(fracNA = sum(is.na(NPX))/n()) %>%
  filter(fracNA == 1) %>%
  pull(OlinkID)

overlap_samples <- intersect(npx_data_format221010$SampleID, npx_data_format221010.project2$SampleID)
npxBridged <- olink_normalization(df1 = npx_data_format221010,
                                  df2 = npx_data_format221010.project2,
                                  overlapping_samples_df1 = overlap_samples,
                                  reference_project = 'P1')

npxBridged_proj2ref <- olink_normalization(df1 = npx_data_format221010,
                                           df2 = npx_data_format221010.project2,
                                           overlapping_samples_df1 = overlap_samples,
                                           reference_project = 'P2')

test_that("olink_normalization works", {
  expect_warning(expect_warning(olink_normalization(df1 = npx_df_test,
                                     df2 = npx_df2,
                                     overlapping_samples_df1 = overlap_samples2,
                                     df1_project_nr = 'P1',
                                     df2_project_nr = 'P2',
                                     reference_project = 'P1')))
  expect_equal(normalization_results.bridged, ref_results$normalization_results.bridged)
  expect_equal(normalization_results.intensity, ref_results$normalization_results.intensity)
  expect_equal(normalization_results.subset, ref_results$normalization_results.subset)
  expect_error(olink_normalization(df1 = { npx_data1 |> mutate(Normalization = "Intensity") },
                                   df2 = { npx_data2 |> mutate(Normalization = "Intensity") })) # No samples specified
  expect_error(olink_normalization(df1 = { npx_data1 |> mutate(Normalization = "Intensity") },
                                   df2 = { npx_data2 |> mutate(Normalization = "Intensity") },
                                   overlapping_samples_df1 = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"))) # Non overlapping samples for bridging

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

  npx_data_LOD <- npx_data2 %>%
    dplyr::mutate(`Max LOD` = LOD) %>%
    dplyr::mutate(`Plate LOD` = LOD) %>%
    dplyr::mutate(Plate_LOD = LOD) %>%
    dplyr::mutate(Project = 'P1')  %>%
    dplyr::mutate(Normalization = "Intensity")

  npx_df2 <- npx_data1 %>%
    dplyr::mutate(`Max LOD` = LOD) %>%
    dplyr::mutate(`Plate LOD` = LOD) %>%
    dplyr::mutate(Plate_LOD = LOD) %>%
    dplyr::mutate(Project = 'P2') %>%
    dplyr::mutate(Normalization = "Intensity")

  #Bridging normalization:
  # Find overlapping samples, but exclude Olink control
  overlap_samples <- intersect((npx_data_LOD %>%
                                  dplyr::filter(!grepl("control", SampleID,
                                                       ignore.case=TRUE)))$SampleID,
                               (npx_df2 %>%
                                  dplyr::filter(!grepl("control", SampleID,
                                                       ignore.case=TRUE)))$SampleID)
  # Normalize
  norm_data <- olink_normalization(df1 = npx_data_LOD,
                                   df2 = npx_df2,
                                   overlapping_samples_df1 = overlap_samples,
                                   df1_project_nr = 'P1',
                                   df2_project_nr = 'P2',
                                   reference_project = 'P1')
  norm_data_p1 <- norm_data |>
    dplyr::filter(Project == "P1")

  norm_data_p2 <- norm_data |>
    dplyr::filter(Project == "P2")

  expect_true(all(npx_df2$LOD + norm_data_p2$Adj_factor == norm_data_p2$LOD))
  expect_true(all(npx_df2$`Max LOD` + norm_data_p2$Adj_factor == norm_data_p2$`Max LOD`))
  expect_true(all(npx_df2$`Plate LOD` + norm_data_p2$Adj_factor == norm_data_p2$`Plate LOD`))
  expect_true(all(npx_df2$Plate_LOD + norm_data_p2$Adj_factor == norm_data_p2$Plate_LOD))


})

# Test if column "Normalize" is missing

test_that("missing Normalization column", {
  # both df1 and df2 are lacking column "Normalization"
  expect_warning(
    olink_normalization(df1 = npx_data1,
                        df2 = npx_data2,
                        overlapping_samples_df1 = {
                          intersect(npx_data1$SampleID, npx_data2$SampleID) |>
                            as_tibble() |>
                            filter(!str_detect(value, 'CONTROL_SAMPLE')) |> #Remove control samples
                            pull()
                        },
                        df1_project_nr = '20200001',
                        df2_project_nr = '20200002',
                        reference_project = '20200001'),
    "Variable \"Normalization\" not present in df1 and df2")

  # only df1 contains column "Normalization"
  expect_warning(
    expect_warning(
      olink_normalization(df1 = {
        npx_data1 |>
          mutate(Normalization = "Intensity")
      },
      df2 = npx_data2,
      overlapping_samples_df1 = {
        intersect(npx_data1$SampleID, npx_data2$SampleID) |>
          as_tibble() |>
          filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
          pull()
      },
      df1_project_nr = '20200001',
      df2_project_nr = '20200002',
      reference_project = '20200001'),
      "Variable \"Normalization\" not present in df2."),
    "The following columns are found in df1 but not df2"
  )

  # only df2 contains column "Normalization"
  expect_warning(
    expect_warning(
      olink_normalization(df1 = npx_data1,
                          df2 = {
                            npx_data2 |>
                              mutate(Normalization = "Intensity")
                          },
                          overlapping_samples_df1 = {
                            intersect(npx_data1$SampleID, npx_data2$SampleID) |>
                              as_tibble() |>
                              filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
                              pull()
                          },
                          df1_project_nr = '20200001',
                          df2_project_nr = '20200002',
                          reference_project = '20200001'),
      "Variable \"Normalization\" not present in df1."),
    "The following columns are found in df2 but not df1"
  )
})

# Test that df1 and df2 are normalized similarly

test_that("df1 and df2 same normalization", {
  skip_if_not(condition = getRversion() >= "4.2.0",
              message = "Skipping for R <= 4.2.0")

  # different normalization with expected values in Normalization column
  expect_warning(
    olink_normalization(df1 = {
      npx_data1 |>
        mutate(Normalization = "Intensity")
    },
    df2 = {
      npx_data2 |>
        mutate(Normalization = "Plate control")
    },
    overlapping_samples_df1 = {
      intersect(npx_data1$SampleID, npx_data2$SampleID) |>
        as_tibble() |>
        filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
        pull()
    },
    df1_project_nr = '20200001',
    df2_project_nr = '20200002',
    reference_project = '20200001'),
    "There are 184 assays not normalized with the same approach. Consider renormalizing.")

  # different normalization with unexpected values in Normalization column
  # currently we do not check values in this column, but in the future we might
  expect_warning(
    olink_normalization(df1 = {
      npx_data1 |>
        mutate(Normalization = "A")
    },
    df2 = {
      npx_data2 |>
        mutate(Normalization = "B")
    },
    overlapping_samples_df1 = {
      intersect(npx_data1$SampleID, npx_data2$SampleID) |>
        as_tibble() |>
        filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
        pull()
    },
    df1_project_nr = '20200001',
    df2_project_nr = '20200002',
    reference_project = '20200001'),
    "There are 184 assays not normalized with the same approach. Consider renormalizing.")

  # test that assays have identical Normalization column between the two
  # datasets
  expect_warning(
    olink_normalization(df1 = {
      npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(OlinkID %in% c("OID01216", "OID01217"),
                                         "Plate control",
                                         "Intensity")
        )
    },
    df2 = {
      npx_data2 |>
        mutate(Normalization = "Intensity")
    },
    overlapping_samples_df1 = {
      intersect(npx_data1$SampleID, npx_data2$SampleID) |>
        as_tibble() |>
        filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
        pull()
    },
    df1_project_nr = '20200001',
    df2_project_nr = '20200002',
    reference_project = '20200001'),
    "Assays OID01216 and OID01217 are not normalized with the same approach.")

  # test that assays have identical Normalization column between the two
  # datasets
  expect_warning(
    olink_normalization(df1 = {
      npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(OlinkID %in% { npx_data1$OlinkID |> unique() |> head(11) },
                                         "Plate control",
                                         "Intensity")
        )
    },
    df2 = {
      npx_data2 |>
        mutate(Normalization = "Intensity")
    },
    overlapping_samples_df1 = {
      intersect(npx_data1$SampleID, npx_data2$SampleID) |>
        as_tibble() |>
        filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
        pull()
    },
    df1_project_nr = '20200001',
    df2_project_nr = '20200002',
    reference_project = '20200001'),
    "There are 11 assays not normalized with the same approach. Consider renormalizing.")

  # it should work if all assays are normalized the same way
  expect_no_warning(
    olink_normalization(df1 = {
      npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(OlinkID %in% c("OID01216", "OID01217"),
                                         "Plate control",
                                         "Intensity")
        )
    },
    df2 = {
      npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(OlinkID %in% c("OID01216", "OID01217"),
                                         "Plate control",
                                         "Intensity")
        )
    },
    overlapping_samples_df1 = {
      intersect(npx_data1$SampleID, npx_data2$SampleID) |>
        as_tibble() |>
        filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
        pull()
    },
    df1_project_nr = '20200001',
    df2_project_nr = '20200002',
    reference_project = '20200001')
  )

  # check that EXCLUDED assays are ignored
  expect_no_warning(
    expect_no_warning(
      olink_normalization(df1 = {
        npx_data1 |>
          dplyr::mutate(
            Normalization = dplyr::if_else(OlinkID %in% c("OID01216", "OID01217"),
                                           "EXCLUDED",
                                           "Intensity")
          )
      },
      df2 = {
        npx_data2 |>
          dplyr::mutate(
            Normalization = dplyr::if_else(OlinkID %in% c("OID01216", "OID01217"),
                                           "Plate control",
                                           "Intensity")
          )
      },
      overlapping_samples_df1 = {
        intersect(npx_data1$SampleID, npx_data2$SampleID) |>
          as_tibble() |>
          filter(!str_detect(value, 'CONTROL_SAMPLE')) |>
          pull()
      },
      df1_project_nr = '20200001',
      df2_project_nr = '20200002',
      reference_project = '20200001')
    )
  )
})

test_that("Different columns in dfs can be normalized",{
  mismatch_col_norm <- suppressWarnings({olink_normalization(df1 = npx_df_test,
                                           df2 = npx_df2,
                                           overlapping_samples_df1 = overlap_samples2,
                                           df1_project_nr = 'P1',
                                           df2_project_nr = 'P2',
                                           reference_project = 'P1')}) %>%
    filter(SampleID %in% sampleSubset)
  # Columns are as expected
  expect_equal(sort(names(mismatch_col_norm)), sort(unique(c(names(npx_df_test), names(npx_df2), "Adj_factor"))))
  # NAs are where expected
  col_notdf2<-stringr::str_remove(setdiff(names(mismatch_col_norm), names(npx_df2)), "Adj_factor")
  col_notdf1<-stringr::str_remove(setdiff(names(mismatch_col_norm), names(npx_df_test)), "Adj_factor")


  expect_equal({mismatch_col_norm %>%
    filter(Project == "P2") %>%
    select(all_of(col_notdf2[-length(col_notdf2)])) %>%
    distinct() %>%
    pull() %>%
    as.logical()}, NA)

  expect_equal({mismatch_col_norm %>%
      filter(Project == "P1") %>%
      select(all_of(col_notdf1[-length(col_notdf1)])) %>%
      distinct() %>%
      pull()} %>%
      as.logical(), NA)
  expect_equal(normalization_results.bridged$NPX, mismatch_col_norm$NPX)

})
