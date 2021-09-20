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
# Clean up factors in old R
if(as.numeric(R.Version()$major) < 4){
  cat("We are running on an old R...")
  randomized_result1$plate = as.character(randomized_result1$plate)
  randomized_result2$plate = as.character(randomized_result2$plate)
}


test_that("olink_plate_randomizer works", {
  expect_equal(randomized_result1, ref_results$randomized_result1)
  expect_equal(randomized_result2, ref_results$randomized_result2)
})
