testthat::test_that("olink_bridgeselector works", {
  load(file = testthat::test_path("data", "npx_data_format221010.RData"))
  bridgeSamples <- olink_bridgeselector(df = npx_data1, # nolint: object_name_linter
                                        sampleMissingFreq = .1, # nolint: object_name_linter
                                        n = 8)
  testthat::expect_equal(nrow(bridgeSamples), 8)
  testthat::expect_equal(ncol(bridgeSamples), 3)
  testthat::expect_equal(bridgeSamples[order(bridgeSamples$MeanNPX, decreasing = TRUE), ]
               $SampleID[3], "A70")
  testthat::expect_equal(round(bridgeSamples[order(bridgeSamples$MeanNPX,
                                         decreasing = TRUE), ]$MeanNPX[5],
                     digits = 2), 6.21)
  testthat::expect_error(olink_bridgeselector(df = npx_data1,
                                    n = 8))
  testthat::expect_warning(olink_bridgeselector(npx_data_format221010,
                                      sampleMissingFreq = 0.1, n = 2))
  ref_data <- npx_data1 |> olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  ml_data <- npx_data1 |>
    dplyr::mutate(`Max LOD` = LOD) |>
    dplyr::select(-LOD) |>
    olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  pl_data <- npx_data1 |>
    dplyr::mutate(`Plate LOD` = LOD) |>
    dplyr::select(-LOD) |>
    olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  p_l_data <- npx_data1 |>
    dplyr::mutate(Plate_LOD = LOD) |>
    dplyr::select(-LOD) |>
    olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  lodnpx_data <- npx_data1 |>
    dplyr::mutate(LODNPX = LOD) |>
    dplyr::select(-LOD) |>
    olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  sampleqc_data <- npx_data1 |>
    dplyr::mutate(SampleQC = QC_Warning) |>
    dplyr::select(-QC_Warning) |>
    olink_bridgeselector(sampleMissingFreq = 0.1, n = 8)
  testthat::expect_length(c(setdiff(ref_data$SampleID, ml_data$SampleID),
                  setdiff(ml_data$SampleID, ref_data$SampleID)), 0)
  testthat::expect_length(c(setdiff(ref_data$SampleID, pl_data$SampleID),
                  setdiff(pl_data$SampleID, ref_data$SampleID)), 0)
  testthat::expect_length(c(setdiff(ref_data$SampleID, p_l_data$SampleID),
                  setdiff(p_l_data$SampleID, ref_data$SampleID)), 0)
  testthat::expect_length(c(setdiff(ref_data$SampleID, sampleqc_data$SampleID),
                  setdiff(sampleqc_data$SampleID, ref_data$SampleID)), 0)
  testthat::expect_length(c(setdiff(ref_data$SampleID, lodnpx_data$SampleID),
                  setdiff(lodnpx_data$SampleID, ref_data$SampleID)), 0)
})

test_that("olink_bridgeselector max num of samples works", {
  testthat::expect_equal({
      olink_bridgeselector(df = npx_data1,
                           sampleMissingFreq = 0.1,
                           n = 150) |>
        nrow()}, 150)
})
