#Load reference results
refRes_file <- test_path('data','refResults.RData')
load(refRes_file)

#Run olink_plate_randomizer
randomized_result1 <- olink_plate_randomizer(manifest,
                                             seed=12345)
randomized_result2 <- olink_plate_randomizer(manifest,
                                             SubjectColumn="SubjectID",
                                             available.spots=c(88,88),
                                             seed=12345)

randomized_result3 <- olink_plate_randomizer({manifest |> dplyr::mutate(
  study = ifelse(Site == "Site1", "study1", "study2"))},
                                             SubjectColumn="SubjectID",
                                             available.spots=c(88,88),
                                             seed=12345)
randomized_result4 <- olink_plate_randomizer({manifest |> dplyr::mutate(
  study = ifelse(Site == "Site1", "study1", "study2"))},
  available.spots=c(88,88),
  seed=12345)

randomized_result5 <- olink_plate_randomizer(manifest,
                                             SubjectColumn = "SubjectID",
                                             num_ctrl = 10,
                                             rand_ctrl = TRUE,
                                             seed = 12345)
randomized_result6 <- olink_plate_randomizer(manifest,
                                             Product = "Target 96",
                                             seed = 12345)

# Clean up factors in old R
if(as.numeric(R.Version()$major) < 4){
  cat("We are running on an old R...")
  randomized_result1$plate = as.character(randomized_result1$plate)
  randomized_result2$plate = as.character(randomized_result2$plate)
  randomized_result3$plate = as.character(randomized_result3$plate)
  randomized_result4$plate = as.character(randomized_result4$plate)
  randomized_result5$plate = as.character(randomized_result5$plate)
  randomized_result6$plate = as.character(randomized_result6$plate)
}

test_that("olink_plate_randomizer works", {

  expect_equal(droplevels(randomized_result1), droplevels(ref_results$randomized_result1))
  expect_equal(droplevels(randomized_result2), droplevels(ref_results$randomized_result2))
  expect_equal(droplevels(randomized_result3), droplevels(ref_results$randomized_result3))
  expect_equal(droplevels(randomized_result4), droplevels(ref_results$randomized_result4))
  expect_equal(randomized_result1, randomized_result6)
  expect_error(olink_plate_randomizer(manifest, SubjectColumn = "SubjectID", Product = "Olink"))
  expect_equal(randomized_result5 %>%
                 dplyr::filter(SampleID == "CONTROL_SAMPLE") %>%
                 dplyr::group_by(plate) %>%
                 dplyr::tally() %>%
                 dplyr::select(n) %>%
                 unique() %>%
                 dplyr::pull(), 10)
  skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
  if (requireNamespace("vdiffr", quietly = TRUE) ){
  vdiffr::expect_doppelganger("Randomized_Data",olink_displayPlateLayout(randomized_result5, num_ctrl = 10,
                                                                        rand_ctrl = TRUE, fill.color = "Visit"))
  }
})
