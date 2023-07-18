#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Run olink_plate_randomizer
randomized_result1 <- olink_plate_randomizer(manifest,
                                             seed=12345)
randomized_result2 <- olink_plate_randomizer(manifest,
                                             SubjectColumn="SubjectID",
                                             available.spots=c(88,88),
                                             seed=12345)
randomized_result3 <- olink_plate_randomizer(manifest, 
                                             SubjectColumn = "SubjectID",
                                             num_ctrl = 10, 
                                             rand_ctrl = TRUE,
                                             seed = 12345)
randomized_result4 <- olink_plate_randomizer(manifest,
                                             Product = "Target 96",
                                             seed = 12345)
olink_displayPlateLayout(randomized_result3, "SampleID")
# Clean up factors in old R
if(as.numeric(R.Version()$major) < 4){
  cat("We are running on an old R...")
  randomized_result1$plate = as.character(randomized_result1$plate)
  randomized_result2$plate = as.character(randomized_result2$plate)
}


test_that("olink_plate_randomizer works", {
  expect_equal(droplevels(randomized_result1), droplevels(ref_results$randomized_result1))
  expect_equal(droplevels(randomized_result2), droplevels(ref_results$randomized_result2))
  expect_equal(randomized_result1, randomized_result4)
  expect_error(olink_plate_randomizer(manifest, SubjectColumn = "SubjectID", Product = "Olink"))
})
