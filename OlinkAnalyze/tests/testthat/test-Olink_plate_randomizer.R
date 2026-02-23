#Load reference results
ref_res_file <- test_path("data", "refResults.RData")
load(ref_res_file)

#Run olink_plate_randomizer
randomized_result1 <- olink_plate_randomizer(manifest,
                                             seed = 12345)
randomized_result2 <- olink_plate_randomizer(manifest,
                                             SubjectColumn = "SubjectID",
                                             available.spots = c(88, 88),
                                             seed = 12345)

randomized_result3 <- olink_plate_randomizer({
  manifest |>
    dplyr::mutate(study = ifelse(Site == "Site1",
                                 "study1",
                                 "study2"))
},
SubjectColumn = "SubjectID",
available.spots = c(88, 88),
seed = 12345)

randomized_result4 <- olink_plate_randomizer({
  manifest |>
    dplyr::mutate(study = ifelse(Site == "Site1",
                                 "study1",
                                 "study2"))
},
available.spots = c(88, 88),
seed = 12345)

randomized_result5 <- olink_plate_randomizer(manifest,
                                             SubjectColumn = "SubjectID",
                                             num_ctrl = 10,
                                             rand_ctrl = TRUE,
                                             seed = 12345)
randomized_result6 <- olink_plate_randomizer(manifest,
                                             Product = "Target 96",
                                             seed = 12345)

# Clean up factors in old R
if (as.numeric(R.Version()$major) < 4) {
  cat("We are running on an old R...")
  randomized_result1$plate <- as.character(randomized_result1$plate)
  randomized_result2$plate <- as.character(randomized_result2$plate)
  randomized_result3$plate <- as.character(randomized_result3$plate)
  randomized_result4$plate <- as.character(randomized_result4$plate)
  randomized_result5$plate <- as.character(randomized_result5$plate)
  randomized_result6$plate <- as.character(randomized_result6$plate)
}

test_that("olink_plate_randomizer works", {
  expect_equal(droplevels(randomized_result1),
               droplevels(ref_results$randomized_result1))
  expect_equal(droplevels(randomized_result2),
               droplevels(ref_results$randomized_result2))
  expect_equal(droplevels(randomized_result3),
               droplevels(ref_results$randomized_result3))
  expect_equal(droplevels(randomized_result4),
               droplevels(ref_results$randomized_result4))
  expect_equal(randomized_result1, randomized_result6)

  expect_error(olink_plate_randomizer(manifest,
                                      SubjectColumn = "SubjectID",
                                      Product = "Olink"))
  expect_equal(randomized_result5 |>
                 dplyr::filter(SampleID == "CONTROL_SAMPLE") |>
                 dplyr::group_by(plate) |>
                 dplyr::tally() |>
                 dplyr::select(n) |>
                 unique() |>
                 dplyr::pull(), 10)
  skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
})

test_that("olink_plate_randomizer works - vdiffr", {
  skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  plate_randomizer_name <- "Randomized_Data"
  check_snap_exist(test_dir_name = "Olink_plate_randomizer",
                   snap_name = plate_randomizer_name)
  vdiffr::expect_doppelganger(plate_randomizer_name,
                              olink_displayPlateLayout(randomized_result5,
                                                       num_ctrl = 10,
                                                       rand_ctrl = TRUE,
                                                       fill.color = "Visit"))
  expect_contains(randomized_result5$SampleID, "CONTROL_SAMPLE")
  expect_error(olink_plate_randomizer(manifest,
                                      num_ctrl = "A"),
               "positive integer")
  expect_error(olink_plate_randomizer(manifest,
                                      num_ctrl = 10.6),
               "positive integer")
  
  expect_error(olink_plate_randomizer(Manifest = {
    manifest |>
      rename("sample_id" = "SampleID")
  },
  "SampleID not found"))
  
  expect_warning(olink_plate_randomizer(Manifest = {
    manifest |>
      rbind(manifest[1, ])
  }),
  "duplicated")
  
  expect_error(olink_plate_randomizer(Manifest = {
    manifest |>
      dplyr::mutate(SampleID = ifelse(SampleID = "A 1", NA, SampleID))
  },
  "No NA"))
  
  expect_error(olink_plate_randomizer(manifest,
                                      SubjectColumn = "Hi"),
               "SubjectColumn name was not found")
  
  expect_error(olink_displayPlateLayout(randomized_result5,
                                        num_ctrl = 10,
                                        rand_ctrl = TRUE,
                                        fill.color = "Visit",
                                        PlateSize = 100),
               "Plate size needs to be either 48 or 96")
  
})

test_that("product_to_platesize works", {
  expect_equal(96, product_to_platesize("Target 96"))
  expect_equal(48, product_to_platesize("Target 48"))
})

test_that("olink_displayPlateLayout works", {
  expect_error(olink_displayPlateLayout(randomized_result1,
                                        PlateSize = 15),
               "Plate size needs to be either 48 or 96")
  expect_equal(olink_displayPlateLayout(randomized_result1,
                                        fill.color = "plate"),
               olink_displayPlateLayout(randomized_result1))
})

test_that("olink_displayPlateDistributions works", {
  expect_equal(olink_displayPlateDistributions(randomized_result1),
               olink_displayPlateDistributions(data = randomized_result1,
                                               fill.color = "plate"))
})

test_that("generate_plate_holder works", {
  expect_error(generate_plate_holder(nplates = 2,
                                     nspots = c(22, 22, 22),
                                     nsamples = 16,
                                     plate_size = 96,
                                     num_ctrl = 8,
                                     rand_ctrl = FALSE),
               "Vector of available spots must equal number of plates")
  
  expect_error(generate_plate_holder(nplates = 2,
                                     nspots = c(22, 100),
                                     nsamples = 16,
                                     plate_size = 96,
                                     num_ctrl = 8,
                                     rand_ctrl = FALSE),
               "Number of samples per plates cannot exceed")
  
  expect_error(generate_plate_holder(nplates = 2,
                                     nspots = c(22, 22),
                                     nsamples = 100,
                                     plate_size = 96,
                                     num_ctrl = 8,
                                     rand_ctrl = FALSE),
               "More samples than available spots")
})
