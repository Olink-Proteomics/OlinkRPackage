test_that(
  "olink_ttest - works - non-paired t-test",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    check_log <- check_npx(df = npx_data1) |>
      suppressMessages()

    # run olink_ttest
    expect_message(
      object = expect_message(
        object = expect_message(
          object = t_test_res <- olink_ttest(
            df = npx_data1,
            variable = "Treatment",
            check_log = check_log
          ),
          regexp = paste("Samples removed due to missing variable levels:",
                         "CONTROL_SAMPLE_AS 1, CONTROL_SAMPLE_AS 2")
        ),
        regexp = "Variable converted from character to factor: Treatment"
      ),
      regexp = "T-test is performed on Treated - Untreated."
    )

    expect_equal(
      object = t_test_res,
      expected = reference_results$t_test
    )
  }
)

test_that(
  "olink_ttest - works - paired t-test",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    npx_df <- npx_data1 |>
      dplyr::filter(
        .data[["Time"]] %in% c("Baseline", "Week.6")
      )

    check_log <- check_npx(df = npx_df) |>
      suppressMessages() |>
      suppressWarnings()

    # run olink_ttest
    expect_message(
      object = expect_message(
        object = paired_t_test_res <- olink_ttest(
          df = npx_df,
          variable = "Time",
          pair_id = "Subject",
          check_log = check_log
        ),
        regexp = "Variable converted from character to factor: Time"
      ),
      regexp = "Paired t-test is performed on Baseline - Week.6."
    )

    expect_equal(
      object = paired_t_test_res,
      expected = reference_results$t_test_paired
    )
  }
)

test_that(
  "olink_ttest - error - no df and/or variable provided",
  {
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_ttest(),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_ttest(
        df = npx_data1
      ),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_ttest(
        variable = "Treatment"
      ),
      regexp = "The df and variable arguments need to be specified."
    )
  }
)

test_that(
  "olink_ttest - error - more than 1 levels",
  {
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = expect_message(
        object = expect_message(
          object = olink_ttest(
            df = npx_data1,
            variable = "Time"
          ),
          regexp = paste("Samples removed due to missing variable levels:",
                         "CONTROL_SAMPLE_AS 1, CONTROL_SAMPLE_AS 2")
        ),
        regexp = "Variable converted from character to factor: Time"
      ),
      regexp = paste("The number of levels in the factor needs to be 2.",
                     "Your factor has 3 levels.")
    )
  }
)
