bridgeSamples <- olink_bridgeselector(df = npx_data1,
                                      sampleMissingFreq = .1,
                                      n = 8)

test_that("olink_bridgeselector works", {
  expect_equal(nrow(bridgeSamples), 8)
  expect_equal(ncol(bridgeSamples), 4)
  expect_equal(bridgeSamples$SampleID[3], 'A33')
  expect_equal(round(bridgeSamples$MeanNPX[5], digits = 2), 6.08)
  expect_error(olink_bridgeselector(df = npx_data1,
                                    n = 8))
})
