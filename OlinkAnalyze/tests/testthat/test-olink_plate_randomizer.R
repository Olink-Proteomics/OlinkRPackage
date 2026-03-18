# Test product_to_plate_size ----

test_that(
  "product_to_platesize works",
  {
    expect_equal(
      object = product_to_plate_size(product = "Target 96"),
      expected = 96L
    )
    expect_equal(
      object = product_to_plate_size(product = "Target 48"),
      expected = 48L
    )
  }
)

# Test olink_display_plate_layout ----

test_that(
  "olink_display_plate_layout - works",
  {
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
    
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    # correctness of randomized_result1 is tested here
    # "olink_plate_randomizer - works - v1"
    
    expect_equal(
      object = olink_display_plate_layout(
        data = reference_results$randomized_samples,
        fill.color = "plate"
      ),
      expected = olink_display_plate_layout(
        data = reference_results$randomized_samples
      )
    )
  }
)

test_that(
  "olink_display_plate_layout - works - vdiffr",
  {
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed("vdiffr")
    skip_on_cran()
    
    plate_randomizer_name <- "Randomized_Data"
    check_snap_exist(test_dir_name = "olink_plate_randomizer",
                     snap_name = plate_randomizer_name)
    
    # Run olink_plate_randomizer
    randomized_result5 <- olink_plate_randomizer(
      Manifest = manifest,
      SubjectColumn = "SubjectID",
      num_ctrl = 10L,
      rand_ctrl = TRUE,
      seed = 12345
    ) |>
      suppressMessages() |>
      suppressWarnings()
    
    vdiffr::expect_doppelganger(
      title = plate_randomizer_name,
      fig = olink_display_plate_layout(
        data = randomized_result5,
        num_ctrl = 10L,
        rand_ctrl = TRUE,
        fill.color = "Visit"
      )
    )
  }
)

test_that(
  "olink_display_plate_layout - error - incorrect plate size",
  {
    expect_error(
      object = olink_display_plate_layout(
        data = manifest,
        plate_size = 15
      ),
      regexp = "Plate size needs to be either 48 or 96"
    )
  }
)

# Test olink_display_plate_dist ----

test_that(
  "olink_display_plate_dist - works",
  {
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
    
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    # correctness of randomized_result1 is tested here
    # "olink_plate_randomizer - works - v1"
    
    expect_equal(
      object = olink_display_plate_dist(
        data = reference_results$randomized_samples
      ),
      expected = olink_display_plate_dist(
        data = reference_results$randomized_samples,
        fill.color = "plate"
      )
    )
  }
)

# Test assign_subject2plate ----

test_that(
  "assign_sample2plate - works",
  {
    expect_equal(object = {
      assign_subject2plate(
        plate_map = generate_plate_holder(
          nplates = 2L,
          nspots = c(12L,
                     12L),
          nsamples = 16L,
          plate_size = 96L,
          num_ctrl = 8L,
          rand_ctrl =
            FALSE) |>
          dplyr::mutate(SampleID = NA_character_),
        manifest = manifest |>
          head(16) |>
          dplyr::mutate(SubjectID = "A"),
        subject_id = "A")
    },
    expected = "This Sample does not fit!"
    )
    expect_equal(object = {
      assign_subject2plate(
        plate_map = generate_plate_holder(
          nplates = 2L,
          nspots = c(22L,
                     22L),
          nsamples = 16L,
          plate_size = 96L,
          num_ctrl = 8L,
          rand_ctrl =
            FALSE) |>
          dplyr::mutate(SampleID = NA_character_),
        manifest = manifest |>
          head(16),
        subject_id = "A") |>
        dplyr::filter(!is.na(SampleID)) |>
        dplyr::distinct(plate) |>
        nrow()
    },
    expected = 1L
    )
  }
)

# Test generate_plate_holder ----

test_that(
  "generate_plate_holder - errors",
  {
    expect_error(
      object = generate_plate_holder(
        nplates = 2L,
        nspots = c(22L, 22L, 22L),
        nsamples = 16L,
        plate_size = 96L,
        num_ctrl = 8L,
        rand_ctrl = FALSE
      ),
      regexp = "Vector of available spots must equal number of plates"
    )
    
    expect_error(
      object = generate_plate_holder(
        nplates = 2L,
        nspots = c(22L, 100L),
        nsamples = 16L,
        plate_size = 96L,
        num_ctrl = 8L,
        rand_ctrl = FALSE
      ),
      regexp = "Number of samples per plates cannot exceed"
    )
    
    expect_error(
      object = generate_plate_holder(
        nplates = 2L,
        nspots = c(22L, 22L),
        nsamples = 100L,
        plate_size = 96L,
        num_ctrl = 8L,
        rand_ctrl = FALSE
      ),
      regexp = "More samples than available spots"
    )
  }
)

# Test olink_plate_randomizer ----

test_that(
  "olink_plate_randomizer - works - total randomization",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    # Run olink_plate_randomizer
    expect_message(
      object = randomized_result1 <- olink_plate_randomizer(
        Manifest = manifest,
        seed = 12345
      ),
      regexp = "Random assignment of SAMPLES to plates"
    )
    
    expect_equal(
      object = droplevels(randomized_result1),
      expected = droplevels(reference_results$randomized_samples)
    )
    
    # Run olink_plate_randomizer
    expect_message(
      object = randomized_result6 <- olink_plate_randomizer(
        Manifest = manifest,
        Product = "Target 96",
        seed = 12345
      ),
      regexp = "Random assignment of SAMPLES to plates"
    )
    
    expect_equal(
      object = randomized_result1,
      expected = randomized_result6
    )
  }
)

test_that(
  "olink_plate_randomizer - works - by subject",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    msgs <- capture_messages(
      {
        randomized_result2 <- olink_plate_randomizer(
          Manifest = manifest,
          SubjectColumn = "SubjectID",
          available.spots = c(88L, 88L),
          seed = 12345
        )
      }
    ) |>
      # Drop empty or whitespace-only lines
      stringr::str_replace_all(
        pattern = "\n",
        replacement = ""
      ) |>
      stringr::str_replace_all(
        pattern = "^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$",
        replacement = ""
      ) |>
      stringr::str_trim(
        side = "both"
      ) |>
      (\(x) x[x != ""])()
    
    expect_true(
      object = grepl(
        pattern = "Random assignment of SUBJECTS to plates",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_equal(
      object = droplevels(randomized_result2),
      expected = droplevels(reference_results$randomized_subjects)
    )
  }
)

test_that(
  "olink_plate_randomizer - works - study and subject",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    msgs <- capture_messages(
      {
        # Run olink_plate_randomizer
        randomized_result3 <- olink_plate_randomizer(
          Manifest = manifest |>
            dplyr::mutate(
              study = dplyr::if_else(
                .data[["Site"]] == "Site1",
                "study1",
                "study2"
              )
            ),
          SubjectColumn = "SubjectID",
          available.spots = c(88L, 88L),
          seed = 12345
        )
      }
    ) |>
      # Drop empty or whitespace-only lines
      stringr::str_replace_all(
        pattern = "\n",
        replacement = ""
      ) |>
      stringr::str_replace_all(
        pattern = "^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$",
        replacement = ""
      ) |>
      stringr::str_trim(
        side = "both"
      ) |>
      (\(x) x[x != ""])()
    
    expect_true(
      object = grepl(
        pattern = "`study` column detected in manifest",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Keeping studies together during randomization",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "study1 successful",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "study2 successful",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Random assignment of SUBJECTS to plates",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Included total of 4 empty well(s) in first and/or",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Please try another seed or increase the number of iteration",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_equal(
      object = droplevels(randomized_result3),
      expected = droplevels(reference_results$randomized_subjects_spots)
    )
  }
)

test_that(
  "olink_plate_randomizer - works - within study",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    msgs <- capture_messages(
      {
        # Run olink_plate_randomizer
        randomized_result4 <- olink_plate_randomizer(
          Manifest = manifest |>
            dplyr::mutate(
              study = dplyr::if_else(
                .data[["Site"]] == "Site1",
                "study1",
                "study2"
              )
            ),
          available.spots = c(88L, 88L),
          seed = 12345
        )
      }
    ) |>
      # Drop empty or whitespace-only lines
      stringr::str_replace_all(
        pattern = "\n",
        replacement = ""
      ) |>
      stringr::str_replace_all(
        pattern = "^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$",
        replacement = ""
      ) |>
      stringr::str_trim(
        side = "both"
      ) |>
      (\(x) x[x != ""])()
    
    expect_true(
      object = grepl(
        pattern = "`study` column detected in manifest",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Studies will be kept together during randomization",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_true(
      object = grepl(
        pattern = "Random assignment of SAMPLES to plates by study",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_equal(
      object = droplevels(randomized_result4),
      expected = droplevels(reference_results$randomized_samples_spots)
    )
  }
)

test_that(
  "olink_plate_randomizer - works - by subject with rand controls",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    msgs <- capture_messages(
      {
        # Run olink_plate_randomizer
        randomized_result5 <- olink_plate_randomizer(
          Manifest = manifest,
          SubjectColumn = "SubjectID",
          num_ctrl = 10L,
          rand_ctrl = TRUE,
          seed = 12345
        )
      }
    ) |>
      # Drop empty or whitespace-only lines
      stringr::str_replace_all(
        pattern = "\n",
        replacement = ""
      ) |>
      stringr::str_replace_all(
        pattern = "^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$",
        replacement = ""
      ) |>
      stringr::str_trim(
        side = "both"
      ) |>
      (\(x) x[x != ""])()
    
    expect_true(
      object = grepl(
        pattern = "Random assignment of SUBJECTS to plates",
        x = msgs,
        fixed = TRUE
      ) |>
        any()
    )
    
    expect_equal(
      object = randomized_result5 |>
        dplyr::filter(
          .data[["SampleID"]] == "CONTROL_SAMPLE"
        ) |>
        dplyr::group_by(
          dplyr::pick("plate")
        ) |>
        dplyr::tally() |>
        dplyr::select(
          dplyr::all_of("n")
        ) |>
        dplyr::distinct() |>
        dplyr::pull(),
      expected = 10L
    )
    
    expect_contains(
      object = randomized_result5$SampleID,
      expected = "CONTROL_SAMPLE"
    )
  }
)

test_that(
  "olink_plate_randomizer - error - incorrect product",
  {
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest,
        SubjectColumn = "SubjectID",
        Product = "Olink"
      ),
      regexp = paste("Product must be one of the following: Target 48, Flex,",
                     "Target 96, Explore 3072, Explore HT, Focus, Reveal")
    )
  }
)

test_that(
  "olink_plate_randomizer - error - incorrect num_ctrl",
  {
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest,
        num_ctrl = "A"
      ),
      regexp = "`num_ctrl` must be a positive integer"
    )
    
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest,
        num_ctrl = 10.6
      ),
      regexp = "`num_ctrl` must be a positive integer"
    )
  }
)

test_that(
  "olink_plate_randomizer - error - incorrect column names",
  {
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest |>
          dplyr::rename(
            "sample_id" = "SampleID"
          )
      ),
      regexp = paste("SampleID not found! Be sure the column of samples ID's",
                     "is named'SampleID'")
    )
    
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest,
        SubjectColumn = "I_Shall_Not_Pass"
      ),
      regexp = paste("The user assigned SubjectColumn name was not found! Make",
                     "sure the SubjectColumn is present in the dataset.")
    )
  }
)

test_that(
  "olink_plate_randomizer - error - incorrect sample identifiers",
  {
    expect_error(
      object = olink_plate_randomizer(
        Manifest = manifest |>
          dplyr::mutate(
            SampleID = dplyr::if_else(
              .data[["SampleID"]] == "A 1",
              NA_character_,
              .data[["SampleID"]]
            )
          )
      ),
      regexp = paste("No NA allowed in the SampleID column. Check that all the",
                     "samples are named."),
      fixed = TRUE
    )
  }
)

test_that(
  "olink_plate_randomizer - error - incorrect plate size",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")
    
    expect_error(
      object = olink_display_plate_layout(
        data = reference_results$randomized_samples,
        num_ctrl = 10L,
        rand_ctrl = TRUE,
        fill.color = "Visit",
        PlateSize = 100L
      ),
      regexp = "Plate size needs to be either 48 or 96.",
      fixed = TRUE
    )
  }
)

test_that(
  "olink_plate_randomizer - warning - sample identifiers has NA",
  {
    expect_message(
      object = expect_warning(
        object = olink_plate_randomizer(
          Manifest = manifest |>
            dplyr::bind_rows(
              manifest |>
                dplyr::slice_head(n = 1L)
            )
        ),
        regexp = "Following SampleID(s) was/were duplicated: A 1",
        fixed = TRUE
      ),
      regexp = "Random assignment of SAMPLES to plates"
    )
  }
)
