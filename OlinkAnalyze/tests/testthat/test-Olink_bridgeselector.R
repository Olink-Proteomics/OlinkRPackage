load(file = '../data/npx_data_format221010.RData')

bridgeSamples <- olink_bridgeselector(df = npx_data1,
                                      sampleMissingFreq = .1,
                                      n = 8)


test_that("olink_bridgeselector works", {
  expect_equal(nrow(bridgeSamples), 8)
  expect_equal(ncol(bridgeSamples), 3)
  expect_equal(bridgeSamples[order(bridgeSamples$MeanNPX, decreasing = TRUE),]$SampleID[3], 'A70')
  expect_equal(round(bridgeSamples[order(bridgeSamples$MeanNPX, decreasing = TRUE),]$MeanNPX[5], digits = 2), 6.21)
  expect_error(olink_bridgeselector(df = npx_data1,
                                    n = 8))
  expect_warning(olink_bridgeselector(npx_data_format221010, sampleMissingFreq = 0.1, n = 2))
})

test_that("olink_bridgeselector max num of samples works", {
  expect_equal(
    {
      olink_bridgeselector(df = npx_data1,
                           sampleMissingFreq = 0.1,
                           n = 150) |>
        nrow()
    },
    150)
})
