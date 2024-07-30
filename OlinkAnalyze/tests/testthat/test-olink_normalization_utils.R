# Test olink_norm_input_validate ----

# NOTE: we will try all possible combinations of inputs

## df1 is missing ----

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=0, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=0, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=0, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=0, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=1, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=1, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=1, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=0, s1=1, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=0, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=0, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=0, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=0, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=1, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=1, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=1, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

test_that(
  "olink_norm_input_validate - error - df1=0, df2=1, s1=1, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Required `df1` is missing!"
    )
  }
)

## df1 is present ----

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=0, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = paste("When `df1` is provided, either `df2` or",
                     "`reference_medians` is required")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=0, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = paste("When `df1` and `reference_medians` are provided,",
                     "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=0, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = paste("When `df1` is provided, either `df2` or",
                     "`reference_medians` is required")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=0, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = paste("When `df1` and `reference_medians` are provided,",
                     "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=1, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = paste("When `df1` is provided, either `df2` or",
                     "`reference_medians` is required")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=1, s2=0, rm=1",
  {
    expect_message(
      object = norm_mode <- olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp = "Reference median normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$ref_median
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=1, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = NULL,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = paste("When `df1` is provided, either `df2` or",
                     "`reference_medians` is required")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=0, s1=1, s2=1, rm=1",
  {
    expect_message(
      object = expect_warning(
        object = norm_mode <- olink_norm_input_validate(
          df1 = npx_data1,
          df2 = NULL,
          overlapping_samples_df1 = c("E", "F", "G", "H"),
          overlapping_samples_df2 = c("A", "B", "C", "D"),
          reference_medians = dplyr::tibble(
            "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
            "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
          )
        ),
        regexp = "`overlapping_samples_df2` will be ignored"
      ),
      regexp = "Reference median normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$ref_median
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=0, s2=0, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = paste("When `df1` and `df2` are provided, at least",
                     "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=0, s2=0, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df2 = NULL,
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp =  paste("When `df1` and `df2` are provided, at least",
                      "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=0, s2=1, rm=0",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp =  paste("When `df1` and `df2` are provided, at least",
                      "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=0, s2=1, rm=1",
  {
    expect_error(
      object = olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
        )
      ),
      regexp =  paste("When `df1` and `df2` are provided, at least",
                      "`overlapping_samples_df1` is required!")
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=1, s2=0, rm=0",
  {
    expect_message(
      object = norm_mode <- olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = NULL,
        reference_medians = NULL
      ),
      regexp = "Bridge normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$bridge
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=1, s2=0, rm=1",
  {
    expect_message(
      object = expect_warning(
        object = norm_mode <- olink_norm_input_validate(
          df1 = npx_data1,
          df2 = npx_data2,
          overlapping_samples_df1 = c("E", "F", "G", "H"),
          overlapping_samples_df2 = NULL,
          reference_medians = dplyr::tibble(
            "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
            "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
          )
        ),
        regexp = "`reference_medians` will be ignored"
      ),
      regexp = "Bridge normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$bridge
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=1, s2=1, rm=0",
  {
    expect_message(
      object = norm_mode <- olink_norm_input_validate(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("E", "F", "G", "H"),
        overlapping_samples_df2 = c("A", "B", "C", "D"),
        reference_medians = NULL
      ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$subset
    )
  }
)

test_that(
  "olink_norm_input_validate - df1=1, df2=1, s1=1, s2=1, rm=1",
  {
    expect_message(
      object = expect_warning(
        object = norm_mode <- olink_norm_input_validate(
          df1 = npx_data1,
          df2 = npx_data2,
          overlapping_samples_df1 = c("E", "F", "G", "H"),
          overlapping_samples_df2 = c("A", "B", "C", "D"),
          reference_medians = dplyr::tibble(
            "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
            "Reference_NPX" = c(0.1, 0.1, 0.2, 0.3)
          )
        ),
        regexp = "`reference_medians` will be ignored"
      ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = norm_mode,
      expected = olink_norm_modes$subset
    )
  }
)

# Test olink_norm_input_class ----

test_that(
  "olink_norm_input_class - error - df1",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_class(
        df1 = c(1L, 2L)
      ),
      regexp = "`df1` should be a tibble or an R6 ArrowObject"
    )
  }
)

test_that(
  "olink_norm_input_class - df2",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = c("A", "B"),
        overlapping_samples_df1 = c("A", "B", "C"),
        norm_mode = olink_norm_modes$bridge
      ),
      regexp = "`df2` should be a tibble or an R6 ArrowObject"
    )
  }
)

test_that(
  "olink_norm_input_class - error - overlapping_samples_df1",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        overlapping_samples_df1 = TRUE
      ),
      regexp = "`overlapping_samples_df1` should be a character vector"
    )

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1 |>
          arrow::as_arrow_table(),
        overlapping_samples_df1 = TRUE
      ),
      regexp = "`overlapping_samples_df1` should be a character vector"
    )
  }
)

test_that(
  "olink_norm_input_class - overlapping_samples_df2",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = 1L,
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P1",
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`overlapping_samples_df2` should be a character vector"
    )

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1 |>
          arrow::as_arrow_table(),
        df2 = npx_data2 |>
          arrow::as_arrow_table(),
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P1",
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = npx_data1,
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`overlapping_samples_df2` should be a character vector"
    )
  }
)

test_that(
  "olink_norm_input_class - df*_project_nr & reference_project",
  {
    skip_if_not_installed("arrow")

    ## df1_project_nr ----

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = c("A", "B", "C"),
        df1_project_nr = TRUE,
        df2_project_nr = "P2",
        reference_project = "P1",
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`df1_project_nr` should be a character vector"
    )

    ## df2_project_nr ----

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = c("A", "B", "C"),
        df1_project_nr = "P1",
        df2_project_nr = 1L,
        reference_project = "P1",
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`df2_project_nr` should be a character vector"
    )

    ## reference_project ----

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = c("A", "B", "C"),
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = npx_data2 |>
          arrow::as_arrow_table(),
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`reference_project` should be a character vector"
    )

    ## reference project not in df*_project_nr ----

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = c("A", "B", "C"),
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P3",
        norm_mode = olink_norm_modes$subset
      ),
      regexp = "`reference_project` should be one of \"P1\" or \"P2\"!"
    )

    ## df1_project_nr == df2_project_nr ----

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        df2 = npx_data2,
        overlapping_samples_df1 = c("A", "B", "C"),
        overlapping_samples_df2 = c("A", "B", "C"),
        df1_project_nr = "P1",
        df2_project_nr = "P1",
        reference_project = "P1",
        norm_mode = olink_norm_modes$subset
      ),
      regexp = paste("Values of `df1_project_nr` and `df2_project_nr` should",
                     "be different!")
    )
  }
)

test_that(
  "olink_norm_input_class - reference_medians",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1,
        overlapping_samples_df1 = c("A", "B", "C"),
        reference_medians = 1L,
        norm_mode = olink_norm_modes$ref_median
      ),
      regexp = "`reference_medians` should be a tibble or an R6 ArrowObject"
    )

    expect_error(
      object = olink_norm_input_class(
        df1 = npx_data1 |>
          arrow::as_arrow_table(),
        overlapping_samples_df1 = c("A", "B", "C"),
        reference_medians = TRUE,
        norm_mode = olink_norm_modes$ref_median
      ),
      regexp = "`reference_medians` should be a tibble or an R6 ArrowObject"
    )
  }
)

# Test olink_norm_input_check_df_cols ----

test_that(
  "olink_norm_input_check_df_cols - Normalization col - 1 dataset",
  {
    skip_if_not_installed("arrow")

    # df does not have Normalization col ----

    expect_warning(
      object = lst_col <- olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1
        ) |>
          lapply(function(l_df) {
            l_df |>
              dplyr::select(
                -dplyr::any_of(c("Normalization"))
              )
          })
      ),
      regexp = "Dataset \"p1\" does not contain a column named"
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        )
      )
    )

    # df does not have Normalization col - arrow ----

    expect_warning(
      object = lst_col <- olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1
        ) |>
          lapply(function(l_df) {
            l_df |>
              dplyr::select(
                -dplyr::any_of(c("Normalization"))
              ) |>
              arrow::as_arrow_table()
          })
      ),
      regexp = "Dataset \"p1\" does not contain a column named"
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        )
      )
    )

    # df has Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_col <- olink_norm_input_check_df_cols(
            lst_df = list(
              "p1" = npx_data1
            ) |>
              lapply(function(l_df) {
                l_df |>
                  dplyr::mutate(
                    Normalization = "Intensity"
                  )
              })
          )
        )
      )
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        )
      )
    )

    # df has Normalization col - arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_col <- olink_norm_input_check_df_cols(
            lst_df = list(
              "p1" = npx_data1
            ) |>
              lapply(function(l_df) {
                l_df |>
                  dplyr::mutate(
                    Normalization = "Intensity"
                  ) |>
                  arrow::as_arrow_table()
              })
          )
        )
      )
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        )
      )
    )
  }
)

test_that(
  "olink_norm_input_check_df_cols - Normalization col - 2 datasets",
  {
    skip_if_not_installed("arrow")

    # no df has Normalization col ----

    expect_warning(
      object = lst_col <- olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1,
          "p2" = npx_data2
        ) |>
          lapply(function(l_df) {
            l_df |>
              dplyr::select(
                -dplyr::any_of(c("Normalization"))
              )
          })
      ),
      regexp = "Datasets \"p1\" and \"p2\" do not contain a column named"
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        ),
        "p2" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        )
      )
    )

    # both df have same Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_col <- olink_norm_input_check_df_cols(
            lst_df = list(
              "p1" = npx_data1,
              "p2" = npx_data2
            ) |>
              lapply(function(l_df) {
                l_df |>
                  dplyr::mutate(
                    Normalization = "Intensity"
                  )
              })
          )
        )
      )
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        ),
        "p2" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        )
      )
    )

    # both df have different Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_col <- olink_norm_input_check_df_cols(
            lst_df = list(
              "p1" = npx_data1 |>
                dplyr::mutate(Normalization = "Intensity"),
              "p2" = npx_data2 |>
                dplyr::mutate(Normalization = "Plate control")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        ),
        "p2" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        )
      )
    )

    # ERROR - one df has Normalization col ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::mutate(Normalization = "Intensity"),
          "p2" = npx_data2
        )
      ),
      regexp = "Dataset \"p2\" does not contain a column named \"Normalization"
    )

    # ERROR - one df has Normalization col - arrow ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::mutate(Normalization = "Intensity") |>
            arrow::as_arrow_table(),
          "p2" = npx_data2
        )
      ),
      regexp = "Dataset \"p2\" does not contain a column named \"Normalization"
    )

  }
)

test_that(
  "olink_norm_input_check_df_cols - Normalization col - 3+ datasets",
  {
    skip_if_not_installed("arrow")

    # ERROR - df 1 and 2 do not have Normalization col ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1 |>
                        arrow::as_arrow_table(),
                      "p2" = OlinkAnalyze::npx_data2,
                      "p3" = OlinkAnalyze::npx_data1 |>
                        dplyr::mutate(Normalization = "Intensity"),
                      "p4" = OlinkAnalyze::npx_data2 |>
                        dplyr::mutate(Normalization = "Intensity"))
      ),
      regexp = "Datasets \"p1\" and \"p2\" do not contain a column named"
    )

    # ERROR - df 1 does not have Normalization col ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
                      "p2" = OlinkAnalyze::npx_data2 |>
                        dplyr::mutate(Normalization = "Intensity") |>
                        arrow::as_arrow_table(),
                      "p3" = OlinkAnalyze::npx_data1 |>
                        dplyr::mutate(Normalization = "Intensity"),
                      "p4" = OlinkAnalyze::npx_data2 |>
                        dplyr::mutate(Normalization = "Intensity"))
      ),
      regexp = "Dataset \"p1\" does not contain a column named"
    )

    # some Normalization col are different ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_col <- olink_norm_input_check_df_cols(
            lst_df = list("p1" = OlinkAnalyze::npx_data1 |>
                            dplyr::mutate(Normalization = "Plate control"),
                          "p2" = OlinkAnalyze::npx_data2 |>
                            dplyr::mutate(Normalization = "Intensity"),
                          "p3" = OlinkAnalyze::npx_data1 |>
                            dplyr::mutate(Normalization = "Intensity") |>
                            arrow::as_arrow_table(),
                          "p4" = OlinkAnalyze::npx_data2 |>
                            dplyr::mutate(Normalization = "Plate control"))
          )
        )
      )
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        ),
        "p2" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        ),
        "p3" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        ),
        "p4" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = "Normalization"
        )
      )
    )

    # no df has Normalization col ----

    expect_warning(
      object = lst_col <- olink_norm_input_check_df_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
                      "p2" = OlinkAnalyze::npx_data2 |>
                        arrow::as_arrow_table(),
                      "p3" = OlinkAnalyze::npx_data1,
                      "p4" = OlinkAnalyze::npx_data2)
      ),
      regexp = "Datasets \"p1\", \"p2\", \"p3\", and \"p4\" do not contain a"
    )

    expect_identical(
      object = lst_col,
      expected = list(
        "p1" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        ),
        "p2" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        ),
        "p3" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        ),
        "p4" = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          normalization = character(0L)
        )
      )
    )

  }
)

test_that(
  "olink_norm_input_check_df_cols - error - missing cols",
  {
    skip_if_not_installed("arrow")

    # 2 df missing cols v1 ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("SampleID"))
            ) |>
            arrow::as_arrow_table(),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("OlinkID", "PlateID"))
            )
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets with missing column"
    )

    # 2 df missing cols v2 ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Panel_Version", "NPX"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("QC_Warning"))
            ) |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets with missing column"
    )

    # one df missing cols v1 ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("NPX"))
            ),
          "p2" = npx_data2 |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Dataset with missing column"
    )

    # one df missing cols v2 ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1,
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("NPX"))
            ) |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Dataset with missing column"
    )

    # multiple df missing cols ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = OlinkAnalyze::npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Panel_Version", "NPX"))
            ),
          "p2" = OlinkAnalyze::npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Panel", "Panel_Version"))
            ) |>
            arrow::as_arrow_table(),
          "p3" = OlinkAnalyze::npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Assay", "UniProt"))
            ),
          "p4" = OlinkAnalyze::npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("QC_Warning"))
            ) |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets with missing column"
    )

  }
)

test_that(
  "olink_norm_input_check_df_cols - error - different quant methdos",
  {
    skip_if_not_installed("arrow")

    # 2 df with different quant method ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::rename(
              "Quantified_value" = "NPX"
            ),
          "p2" = npx_data2 |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets are not quantified with the same method"
    )

    # multiple df with different quant method ----

    expect_error(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::rename(
              "Quantified_value" = "NPX"
            ),
          "p2" = npx_data2,
          "p3" = OlinkAnalyze::npx_data1,
          "p4" = OlinkAnalyze::npx_data2 |>
            dplyr::rename(
              "Ct" = "NPX"
            ) |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets are not quantified with the same method"
    )

  }
)

test_that(
  "olink_norm_input_check_df_cols - error - missing non-required cols",
  {
    skip_if_not_installed("arrow")

    # df 1 missing 1 col ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index"))
            ),
          "p2" = npx_data2 |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Column not present across datasets"
    )

    # df 1 missing 2 col & df 2 missing 1 col ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index", "Site"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Treatment"))
            ) |>
            arrow::as_arrow_table()
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Columns not present across datasets"
    )

    # one df - no warn ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_df_cols(
            lst_df = list(
              "p1" = npx_data1 |>
                dplyr::select(
                  -dplyr::all_of(c("Index"))
                ) |>
                arrow::as_arrow_table()
            ) |>
              lapply(dplyr::mutate, Normalization = "Intensity")
          )
        )
      )
    )

    # multiple df missing non-required cols ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index", "Site"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Treatment"))
            ) |>
            arrow::as_arrow_table(),
          "p3" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Treatment", "Project", "Subject"))
            ),
          "p4" = npx_data2
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Columns not present across datasets"
    )

  }
)

# Test olink_norm_input_check_samples ----

test_that(
  "olink_norm_input_check_samples - works - 1 dataset",
  {
    skip_if_not_installed("arrow")

    # one df - with 6 reference samples ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_samples(
            lst_df_samples = list(
              "p1" = unique(npx_data1$SampleID)
            ),
            lst_ref_samples = list(
              "p1" = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "CONTROL_SAMPLE",
                         x = .data[["SampleID"]],
                         fixed = TRUE)
                ) |>
                dplyr::pull(.data[["SampleID"]]) |>
                unique() |>
                sort() |>
                head(n = 6L)
            ),
            norm_mode = "ref_median"
          )
        )
      )
    )

    # one df - with all reference samples ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_samples(
            lst_df_samples = list(
              "p1" = unique(npx_data1$SampleID)
            ),
            lst_ref_samples = list(
              "p1" = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "CONTROL_SAMPLE",
                         x = .data[["SampleID"]],
                         fixed = TRUE)
                ) |>
                dplyr::pull(.data[["SampleID"]]) |>
                unique() |>
                sort()
            ),
            norm_mode = "ref_median"
          )
        )
      )
    )
  }
)

test_that(
  "olink_norm_input_check_samples - works - 2 datasets",
  {
    skip_if_not_installed("arrow")

    # bridge norm - reference samples in datasets ----

    ref_samples_bridge <- intersect(x = npx_data1$SampleID,
                                    y = npx_data2$SampleID) |>
      (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_samples(
            lst_df_samples = list(
              "p1" = unique(npx_data1$SampleID),
              "p2" = npx_data2 |>
                arrow::as_arrow_table() |>
                dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
                unique()
            ),
            lst_ref_samples = list(
              "p1" = ref_samples_bridge,
              "p2" = ref_samples_bridge
            ),
            norm_mode = "bridge"
          )
        )
      )
    )

    # subset norm - reference samples in datasets ----

    ref_samples_subset_1 <- npx_data1 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()
    ref_samples_subset_2 <- npx_data2 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_samples(
            lst_df_samples = list(
              "p1" = unique(npx_data1$SampleID),
              "p2" = npx_data2 |>
                arrow::as_arrow_table() |>
                dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
                unique()
            ),
            lst_ref_samples = list(
              "p1" = ref_samples_subset_1,
              "p2" = ref_samples_subset_2
            ),
            norm_mode = "subset"
          )
        )
      )
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - missing samples",
  {
    skip_if_not_installed("arrow")

    # bridge norm ----

    ref_samples_bridge <- intersect(x = npx_data1$SampleID,
                                    y = npx_data2$SampleID) |>
      (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()

    ## reference samples not in datasets v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_bridge[1L:12L],
                   paste0(ref_samples_bridge[13L:16L], "_not_here")),
          "p2" = ref_samples_bridge
        ),
        norm_mode = "bridge"
      ),
      regexp = "B37_not_here, B45_not_here, B63_not_here, and B75_not_here"
    )

    ## reference samples not in datasets v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_bridge[1L:12L],
                   paste0(ref_samples_bridge[13L:16L], "_not_here")),
          "p2" = c(paste0(ref_samples_bridge[1L:2L], "_not_here"),
                   ref_samples_bridge[3L:12L])
        ),
        norm_mode = "bridge"
      ),
      regexp = "A13_not_here and A29_not_here"
    )

    ## reference samples not in datasets v3 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = ref_samples_bridge,
          "p2" = c(paste0(ref_samples_bridge[1L:2L], "_not_here"),
                   ref_samples_bridge[3L:12L])
        ),
        norm_mode = "bridge"
      ),
      regexp = "A13_not_here and A29_not_here"
    )

    # subset norm ----

    ref_samples_subset_1 <- npx_data1 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()
    ref_samples_subset_2 <- npx_data2 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()

    ## reference samples not in datasets v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_subset_1[1L:50L],
                   paste0(ref_samples_subset_1[51L:60L], "_not_here"),
                   ref_samples_subset_1[61L:156L]),
          "p2" = ref_samples_subset_2
        ),
        norm_mode = "subset"
      ),
      regexp = "A52_not_here, A53_not_here, A54_not_here, A55_not_here, A56_not"
    )

    ## reference samples not in datasets v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_subset_1[1L:50L],
                   paste0(ref_samples_subset_1[51L:60L], "_not_here"),
                   ref_samples_subset_1[61L:156L]),
          "p2" = c(paste0(ref_samples_subset_2[1L:50L], "_not_here"),
                   ref_samples_subset_2)
        ),
        norm_mode = "subset"
      ),
      regexp = "A52_not_here, A53_not_here, A54_not_here, A55_not_here, A56_not"
    )

    ## reference samples not in datasets v3 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = ref_samples_subset_1,
          "p2" = c(paste0(ref_samples_subset_2[1L:50L], "_not_here"),
                   ref_samples_subset_2)
        ),
        norm_mode = "subset"
      ),
      regexp = "C44_not_here, C45_not_here, C46_not_here, C47_not_here, C48_not"
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - duplicate samples",
  {
    skip_if_not_installed("arrow")

    # bridge norm ----

    ref_samples_bridge <- intersect(x = npx_data1$SampleID,
                                    y = npx_data2$SampleID) |>
      (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()

    ## duplicate reference samples in datasets v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_bridge[1L:2L],
                   ref_samples_bridge),
          "p2" = ref_samples_bridge
        ),
        norm_mode = "bridge"
      ),
      regexp = "* p1: A13 and A29"
    )

    ## duplicate reference samples in datasets v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = ref_samples_bridge,
          "p2" = c(ref_samples_bridge[1L:2L],
                   ref_samples_bridge)
        ),
        norm_mode = "bridge"
      ),
      regexp = "* p2: A13 and A29"
    )

    # subset norm ----

    ref_samples_subset_1 <- npx_data1 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()
    ref_samples_subset_2 <- npx_data2 |>
      dplyr::filter(
        !grepl(pattern = "CONTROL_SAMPLE",
               x = .data[["SampleID"]],
               fixed = TRUE)
        & .data[["QC_Warning"]] == "Pass"
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()

    ## reference samples not in datasets v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_subset_1[1L:50L],
                   ref_samples_subset_1),
          "p2" = ref_samples_subset_2
        ),
        norm_mode = "subset"
      ),
      regexp = "* p1: A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,"
    )

    ## reference samples not in datasets v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = c(ref_samples_subset_1[1L:50L],
                   ref_samples_subset_1),
          "p2" = c(ref_samples_subset_2[1L:50L],
                   ref_samples_subset_2)
        ),
        norm_mode = "subset"
      ),
      regexp = "* p2: C1, C2, C3, C4, C5, C6, C7, C8, C9, C10,"
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - uneven number of bridge samples",
  {
    skip_if_not_installed("arrow")

    # bridge norm ----

    ref_samples_bridge <- intersect(x = npx_data1$SampleID,
                                    y = npx_data2$SampleID) |>
      (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()

    ## uneven bridge samples in datasets v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = head(x = ref_samples_bridge, 10L),
          "p2" = ref_samples_bridge
        ),
        norm_mode = "bridge"
      ),
      regexp = "There are 10 bridge samples for dataset `p1` and 16 bridge samp"
    )

    ## duplicate reference samples in datasets v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = npx_data2 |>
            arrow::as_arrow_table() |>
            dplyr::pull(.data[["SampleID"]], as_vector = TRUE) |>
            unique()
        ),
        lst_ref_samples = list(
          "p1" = ref_samples_bridge,
          "p2" = head(x = ref_samples_bridge, -5L)
        ),
        norm_mode = "bridge"
      ),
      regexp = "There are 16 bridge samples for dataset `p1` and 11 bridge samp"
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - no or too many df sample sets",
  {
    skip_if_not_installed("arrow")

    # no df samples ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(),
        lst_ref_samples = list(),
        norm_mode = "ref_median"
      ),
      regexp = "No sets of samples provided in `lst_df_samples`!"
    )

    # too many df samples ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = unique(npx_data2$SampleID),
          "p3" = unique(npx_data1$SampleID)
        ),
        lst_ref_samples = list(),
        norm_mode = "ref_median"
      ),
      regexp = "More than 2 sets of samples provided in `lst_df_samples`!"
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - no or too many ref sample sets",
  {
    skip_if_not_installed("arrow")

    # no ref samples ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = unique(npx_data2$SampleID)
        ),
        lst_ref_samples = list(),
        norm_mode = "ref_median"
      ),
      regexp = "No sets of samples provided in `lst_ref_samples`!"
    )

    # too many ref samples ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = unique(npx_data2$SampleID)
        ),
        lst_ref_samples = list(
          "p1" = c("A", "B"),
          "p2" = c("A", "B"),
          "p3" = c("A", "B")
        ),
        norm_mode = "ref_median"
      ),
      regexp = "More than 2 sets of samples provided in `lst_ref_samples`!"
    )

  }
)

test_that(
  "olink_norm_input_check_samples - error - uneven df-ref samples",
  {
    skip_if_not_installed("arrow")

    # uneven sample vectors v1 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID),
          "p2" = unique(npx_data2$SampleID)
        ),
        lst_ref_samples = list(
          "p1" = intersect(x = npx_data1$SampleID,
                           y = npx_data2$SampleID) |>
            (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()
        ),
        norm_mode = "bridge"
      ),
      regexp = "Number of sample vectors in `lst_df_samples` differs from the n"
    )

    # uneven sample vectors v2 ----

    expect_error(
      object = olink_norm_input_check_samples(
        lst_df_samples = list(
          "p1" = unique(npx_data1$SampleID)
        ),
        lst_ref_samples = list(
          "p1" = intersect(x = npx_data1$SampleID,
                           y = npx_data2$SampleID) |>
            (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])(),
          "p2" = c("A", "B", "C")
        ),
        norm_mode = "bridge"
      ),
      regexp = "Number of sample vectors in `lst_df_samples` differs from the n"
    )

  }
)

# Test olink_norm_input_ref_medians ----

test_that(
  "olink_norm_input_ref_medians - check cols",
  {
    skip_if_not_installed("arrow")

    ## no error - tibble ----

    expect_no_warning(
      object = expect_no_message(
        object = expect_no_error(
          object = olink_norm_input_ref_medians(
            reference_medians = dplyr::tibble(
              "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
              "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
            )
          )
        )
      )
    )

    ## no error - arrow ----

    expect_no_warning(
      object = expect_no_message(
        object = expect_no_error(
          object = olink_norm_input_ref_medians(
            reference_medians = arrow::arrow_table(
              "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
              "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
            )
          )
        )
      )
    )

    ## ERROR - additional columns ----

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(1.1, 0.4, 0.3, 2),
          "I_Am_Extra" = c("a", "b", "c", "d")
        )
      ),
      regexp = paste("`reference_medians` should have",
                     length(olink_norm_ref_median_cols), "columns!")
    )

    ## no error - arrow ----

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = arrow::arrow_table(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678")
        )
      ),
      regexp = paste("`reference_medians` should have",
                     length(olink_norm_ref_median_cols), "columns!")
    )
  }
)

test_that(
  "olink_norm_input_ref_medians - check class",
  {
    skip_if_not_installed("arrow")

    ## ERROR - OlinkID

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678") |>
            as.factor(),
          "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
        )
      ),
      regexp = "Column \"OlinkID\" of `reference_medians` has the wrong"
    )

    ## error - Reference_NPX

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678"),
          "Reference_NPX" = c(1L, 4L, 3L, 2L)
        )
      ),
      regexp = "Column \"Reference_NPX\" of `reference_medians` has the wrong"
    )

    ## error - both OlinkID and Reference_NPX

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID23456", "OID34567", "OID45678") |>
            as.factor(),
          "Reference_NPX" = c(1L, 4L, 3L, 2L)
        )
      ),
      regexp = paste("Columns \"OlinkID\" and \"Reference_NPX\" of",
                     "`reference_medians` have the wrong")
    )

  }
)

test_that(
  "olink_norm_input_ref_medians - check OID duplicates",
  {
    skip_if_not_installed("arrow")

    ## ERROR - 1 duplicate

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID12345", "OID34567", "OID45678"),
          "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
        )
      ),
      regexp = "Found 1 duplicated assay in `reference_medians`: \"OID12345\""
    )

    ## error - 1 duplicate many times

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID12345", "OID12345", "OID45678"),
          "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
        )
      ),
      regexp = "Found 1 duplicated assay in `reference_medians`: \"OID12345\""
    )

    ## error - many duplicates

    expect_error(
      object = olink_norm_input_ref_medians(
        reference_medians = dplyr::tibble(
          "OlinkID" = c("OID12345", "OID12345", "OID45678", "OID45678"),
          "Reference_NPX" = c(1.1, 0.4, 0.3, 2)
        )
      ),
      regexp = "Found 2 duplicated assays in `reference_medians`: \"OID12345\""
    )

  }
)

# Test olink_norm_input_clean_assays ----

test_that(
  "olink_norm_input_clean_assays - works - invalid OID in df*",
  {
    skip_if_not_installed("arrow")

    ## all assays start with OID in 1 df ----

    lst_df_v0 <- list(
      "p1" = npx_data1
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v0,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = character(0L))
            )
          )
        )
      )
    )

    expect_identical(object = lst_out$lst_df,
                     expected = lst_df_v0)

    ## 1 assay does not start with OID in 1 df ----

    lst_df_v2 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                   "OID1234",
                                   .data[["OlinkID"]])
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v2,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = "* p1: OID1234"
    )

    expect_identical(object = lst_out$lst_df$p1,
                     expected = lst_df_v2$p1 |>
                       dplyr::filter(.data[["OlinkID"]] != "OID1234"))

    ## multiple assays do not start with OID in 1 df ----

    lst_df_v3 <- list(
      "p1" = lst_out <- npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::case_match(
            .data[["OlinkID"]],
            "OID00471" ~ "OID00471A",
            "OID00472" ~ "OID00471B",
            "OID00474" ~ "OID00471C",
            "OID00475" ~ "OID00471D",
            "OID00476" ~ "OID00471E",
            "OID00477" ~ "OID0047",
            "OID00478" ~ "OID00471#",
            "OID00479" ~ "OID00471&",
            .default = .data[["OlinkID"]]
          )
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v3,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = paste("* p1: OID00471A, OID00471B, OID00471C, OID00471D,",
                     "OID00471E, OID0047, OID00471#, and OID00471&")
    )

    expect_identical(object = lst_out$lst_df$p1,
                     expected = lst_df_v3$p1 |>
                       dplyr::filter(
                         !(.data[["OlinkID"]] %in% c("OID00471A", "OID00471B",
                                                     "OID00471C", "OID00471D",
                                                     "OID00471E", "OID0047",
                                                     "OID00471#", "OID00471&"))
                       ))

    ## all assays start with OID in 2 df ----

    lst_df_v4 <- list(
      "p1" = npx_data1,
      "p2" = npx_data2
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v4,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = character(0L)),
              "p2" = list(olink_id = "OlinkID",
                          normalization = character(0L))
            )
          )
        )
      )
    )

    expect_identical(object = lst_out$lst_df,
                     expected = lst_df_v4)

    ## 1 assay does not start with OID in 1 df ----

    lst_df_v5 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                   "OID1234",
                                   .data[["OlinkID"]])
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01224",
                                   "OID01224B",
                                   .data[["OlinkID"]])
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v5,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p2" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = "* p2: OID01224B"
    )

    lst_df_v5$p1 <- lst_df_v5$p1 |>
      dplyr::filter(.data[["OlinkID"]] != "OID1234")
    lst_df_v5$p2 <- lst_df_v5$p2 |>
      dplyr::filter(.data[["OlinkID"]] != "OID01224B")

    expect_identical(object = lst_out$lst_df,
                     expected = lst_df_v5)

    # multiple assays do not start with OID in 2 df ----

    lst_df_v6 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::case_match(
            .data[["OlinkID"]],
            "OID00471" ~ "OID00471A",
            "OID00472" ~ "OID00471B",
            "OID00474" ~ "OID00471C",
            "OID00475" ~ "OID00471D",
            "OID00476" ~ "OID00471E",
            "OID00477" ~ "OID0047",
            "OID00478" ~ "OID00471#",
            "OID00479" ~ "OID00471&",
            .default = .data[["OlinkID"]]
          )
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          OlinkID = dplyr::case_match(
            .data[["OlinkID"]],
            "OID01301" ~ "OID01301A",
            "OID01302" ~ "OID01302B",
            "OID01303" ~ "OID01303C",
            "OID01304" ~ "OID01304D",
            "OID01305" ~ "OID01305E",
            "OID01306" ~ "OID0130",
            "OID01307" ~ "OID01307#",
            .default = .data[["OlinkID"]]
          )
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v6,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p2" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = paste("* p2: OID01301A, OID01302B, OID01303C, OID01304D,",
                     "OID01305E, OID0130, and OID01307#")
    )

    lst_df_v6$p1 <- lst_df_v6$p1 |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% c("OID00471A", "OID00471B", "OID00471C",
                                    "OID00471D", "OID00471E", "OID0047",
                                    "OID00471#", "OID00471&"))
      )
    lst_df_v6$p2 <- lst_df_v6$p2 |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% c("OID01301A", "OID01302B", "OID01303C",
                                    "OID01304D", "OID01305E", "OID0130",
                                    "OID01307#"))
      )

    expect_identical(object = lst_out$lst_df,
                     expected = lst_df_v6)

    # arrow datasets - 1 assay in each df ----

    lst_df_v7 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                   "OID1234",
                                   .data[["OlinkID"]])
        ) |>
        arrow::as_arrow_table(),
      "p2" = npx_data2 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01224",
                                   "OID01224B",
                                   .data[["OlinkID"]])
        ) |>
        arrow::as_arrow_table()
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v7,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p2" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = "* p2: OID01224B"
    )

    lst_df_v7$p1 <- lst_df_v7$p1 |>
      dplyr::filter(.data[["OlinkID"]] != "OID1234") |>
      dplyr::collect()
    lst_df_v7$p2 <- lst_df_v7$p2 |>
      dplyr::filter(.data[["OlinkID"]] != "OID01224B") |>
      dplyr::collect()

    expect_identical(object = lst_out$lst_df |>
                       lapply(dplyr::collect),
                     expected = lst_df_v7)

    # multiple datasets ----

    lst_df_v8 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                   "OID1234",
                                   .data[["OlinkID"]])
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01224",
                                   "OID01224B",
                                   .data[["OlinkID"]])
        ),
      "p3" = npx_data1,
      "p4" = npx_data2 |>
        dplyr::mutate(
          OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01301",
                                   "OID01301B",
                                   .data[["OlinkID"]])
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v8,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p2" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p3" = list(olink_id = "OlinkID",
                      normalization = character(0L)),
          "p4" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = "* p4: OID01301B"
    )

    lst_df_v8$p1 <- lst_df_v8$p1 |>
      dplyr::filter(.data[["OlinkID"]] != "OID1234")
    lst_df_v8$p2 <- lst_df_v8$p2 |>
      dplyr::filter(.data[["OlinkID"]] != "OID01224B")
    lst_df_v8$p4 <- lst_df_v8$p4 |>
      dplyr::filter(.data[["OlinkID"]] != "OID01301B")

    expect_identical(object = lst_out$lst_df,
                     expected = lst_df_v8)

  }
)

test_that(
  "olink_norm_input_clean_assays - works - invalid OID in reference_medians",
  {
    skip_if_not_installed("arrow")

    ## all assays start with OID in reference_medians ----

    reference_medians_v0 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = list(
              "p1" = npx_data1
            ),
            reference_medians = reference_medians_v0,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = character(0L))
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v0
    )

    ## 1 assay does not start with OID in reference_medians ----

    reference_medians_v1 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1,
        OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                 "OID1234",
                                 .data[["OlinkID"]])
      )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1
        ),
        reference_medians = reference_medians_v1,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = paste("from the reference median dataset have been excluded",
                     "from normalization: OID1234")
    )

    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v1 |>
        dplyr::filter(.data[["OlinkID"]] != "OID1234")
    )

    ## multiple assays do not start with OID in reference_medians----

    reference_medians_v2 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1,
        OlinkID = dplyr::case_match(
          .data[["OlinkID"]],
          "OID00471" ~ "OID00471A",
          "OID00472" ~ "OID00471B",
          "OID00474" ~ "OID00471C",
          "OID00475" ~ "OID00471D",
          "OID00476" ~ "OID00471E",
          "OID00477" ~ "OID0047",
          "OID00478" ~ "OID00471#",
          "OID00479" ~ "OID00471&",
          .default = .data[["OlinkID"]]
        )
      )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1
        ),
        reference_medians = reference_medians_v2,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = paste("from the reference median dataset have been excluded",
                     "from normalization: OID00471A,")
    )

    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v2 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID00471A", "OID00471B", "OID00471C",
                                      "OID00471D", "OID00471E", "OID0047",
                                      "OID00471#", "OID00471&"))
        )
    )

    ## arrow - 1 assays with wrong OID ----

    reference_medians_v3 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1,
        OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                 "OID1234",
                                 .data[["OlinkID"]])
      ) |>
      arrow::as_arrow_table()

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1
        ),
        reference_medians = reference_medians_v3,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = paste("from the reference median dataset have been excluded",
                     "from normalization: OID1234")
    )

    expect_identical(
      object = lst_out$reference_medians |>
        dplyr::collect(),
      expected = reference_medians_v3 |>
        dplyr::filter(
          .data[["OlinkID"]] != "OID1234"
        ) |>
        dplyr::collect()
    )

  }
)

test_that(
  "olink_norm_input_clean_assays - error - all OID removed reference_medians",
  {
    skip_if_not_installed("arrow")

    expect_error(
      object = olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1
        ),
        reference_medians = npx_data1 |>
          dplyr::select(
            dplyr::all_of("OlinkID")
          ) |>
          dplyr::distinct() |>
          dplyr::mutate(
            Reference_NPX = 0.1,
            OlinkID = paste0(.data[["OlinkID"]], "X")
          ),
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = character(0L))
        )
      ),
      regexp = "All assays were removed from input `reference_medians`!"
    )
  }
)

test_that(
  "olink_norm_input_clean_assays - works - no excluded assays",
  {
    skip_if_not_installed("arrow")

    # 1 df ----

    lst_df_v0 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v0,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = "Normalization")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v0
    )

    # 1 df arrow ----

    lst_df_v1 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ) |>
        arrow::as_arrow_table()
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v1,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = "Normalization")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df |>
        lapply(dplyr::collect),
      expected = lst_df_v1 |>
        lapply(dplyr::collect)
    )

    # 2 df ----

    lst_df_v2 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = "Intensity"
        )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v2,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = "Normalization"),
              "p2" = list(olink_id = "OlinkID",
                          normalization = "Normalization")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v2
    )

    # 2 df arrow ----

    lst_df_v3 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ) |>
        arrow::as_arrow_table(),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ) |>
        arrow::as_arrow_table()
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v3,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = "Normalization"),
              "p2" = list(olink_id = "OlinkID",
                          normalization = "Normalization")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df |>
        lapply(dplyr::collect),
      expected = lst_df_v3 |>
        lapply(dplyr::collect)
    )
  }
)

test_that(
  "olink_norm_input_clean_assays - works - excluded assays in df*",
  {
    skip_if_not_installed("arrow")

    ## 1 excluded assay in 1 df ----

    lst_df_v0 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(.data[["OlinkID"]] == "OID01307",
                                         "EXCLUDED",
                                         "Intensity")
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v0,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "* p1: OID01307"
    )

    lst_df_v0$p1 <- lst_df_v0$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v0
    )

    ## multiple excluded assays in 1 df ----

    lst_df_v1 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID00471", "OID00472", "OID00474",
                                      "OID00475", "OID00476", "OID00477",
                                      "OID00478", "OID00479"),
            "EXCLUDED",
            "Intensity"
          )
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v1,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = paste("p1: OID00471, OID00472, OID00474, OID00475, OID00476")
    )

    lst_df_v1$p1 <- lst_df_v1$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v1
    )

    ## 1 excluded assay in the other df ----

    lst_df_v2 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(.data[["OlinkID"]] == "OID01224",
                                         "EXCLUDED",
                                         "Intensity")
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v2,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p2" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "* p2: OID01224"
    )

    lst_df_v2$p1 <- lst_df_v2$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")
    lst_df_v2$p2 <- lst_df_v2$p2 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v2
    )

    # multiple excluded assays in 2 df ----

    lst_df_v3 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID00471", "OID00472", "OID00474",
                                      "OID00475", "OID00476", "OID00477",
                                      "OID00478", "OID00479"),
            "EXCLUDED",
            "Intensity"
          )
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID01269", "OID01270", "OID01271",
                                      "OID01272", "OID01273", "OID01274",
                                      "OID01275", "OID01276"),

            "EXCLUDED",
            "Plate control"
          )
        )
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v3,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p2" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "* p2: OID01269, OID01270, OID01271, OID01272"
    )

    lst_df_v3$p1 <- lst_df_v3$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")
    lst_df_v3$p2 <- lst_df_v3$p2 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v3
    )

    # multiple excluded assays in 2 df - arrow ----

    lst_df_v4 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID00471", "OID00472", "OID00474",
                                      "OID00475", "OID00476", "OID00477",
                                      "OID00478", "OID00479"),
            "EXCLUDED",
            "Intensity"
          )
        ) |>
        arrow::as_arrow_table(),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID01269", "OID01270", "OID01271",
                                      "OID01272", "OID01273", "OID01274",
                                      "OID01275", "OID01276"),

            "EXCLUDED",
            "Plate control"
          )
        ) |>
        arrow::as_arrow_table()
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v4,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p2" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "* p2: OID01269, OID01270, OID01271, OID01272"
    )

    lst_df_v4$p1 <- lst_df_v4$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED") |>
      dplyr::collect()
    lst_df_v4$p2 <- lst_df_v4$p2 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED") |>
      dplyr::collect()

    expect_identical(
      object = lst_out$lst_df |>
        lapply(dplyr::collect),
      expected = lst_df_v4
    )

    # multiple datasets ----

    lst_df_v5 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID00471", "OID00472", "OID00474",
                                      "OID00475", "OID00476", "OID00477",
                                      "OID00478", "OID00479"),
            "EXCLUDED",
            "Intensity"
          )
        ),
      "p2" = npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID01269", "OID01270", "OID01271",
                                      "OID01272", "OID01273", "OID01274",
                                      "OID01275", "OID01276"),

            "EXCLUDED",
            "Plate control"
          )
        ),
      "p3" = npx_data1 |>
        arrow::as_arrow_table(),
      "p4" = npx_data2 |>
        dplyr::mutate(
          Normalization = dplyr::if_else(
            .data[["OlinkID"]] %in% c("OID01269", "OID01270", "OID01271"),
            "EXCLUDED",
            "Intensity"
          )
        ) |>
        arrow::as_arrow_table()
    )

    expect_message(
      object = lst_out <- olink_norm_input_clean_assays(
        lst_df = lst_df_v5,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p2" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p3" = list(olink_id = "OlinkID"),
          "p4" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "* p4: OID01269, OID01270, and OID01271"
    )

    lst_df_v5$p1 <- lst_df_v5$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")
    lst_df_v5$p2 <- lst_df_v5$p2 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")
    lst_df_v5$p3 <- lst_df_v5$p3 |>
      dplyr::collect()
    lst_df_v5$p4 <- lst_df_v5$p4 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED") |>
      dplyr::collect()

    expect_identical(
      object = lst_out$lst_df |>
        lapply(dplyr::collect),
      expected = lst_df_v5
    )

    ## 1 df does not contain normalization column ----

    lst_df_v6 <- list(
      "p1" = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ),
      "p2" = npx_data2
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_clean_assays(
            lst_df = lst_df_v6,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID",
                          normalization = "Normalization"),
              "p2" = list(olink_id = "OlinkID")
            )
          )
        )
      )
    )

    lst_df_v6$p1 <- lst_df_v6$p1 |>
      dplyr::filter(.data[["Normalization"]] != "EXCLUDED")

    expect_identical(
      object = lst_out$lst_df |>
        lapply(dplyr::collect),
      expected = lst_df_v6
    )

  }
)

test_that(
  "olink_norm_input_clean_assays - error - all OID removed df*",
  {
    skip_if_not_installed("arrow")

    # 1 empty df ----

    expect_error(
      object = olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::mutate(
              Normalization = "EXCLUDED"
            )
        ),
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ),
      regexp = "All assays were removed from dataset \"p1\"!"
    )

    # multiple empty df ----

    expect_error(
      object = olink_norm_input_clean_assays(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::mutate(
              Normalization = "EXCLUDED"
            ),
          "p2" = npx_data2 |>
            dplyr::mutate(
              OlinkID = paste0(.data[["OlinkID"]], "X"),
              Normalization = "Intensity"
            )
        ),
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID",
                      normalization = "Normalization"),
          "p2" = list(olink_id = "OlinkID",
                      normalization = "Normalization")
        )
      ) |>
        suppressMessages(),
      regexp = "All assays were removed from datasets \"p1\" and \"p2\"!"
    )
  }
)

# Test olink_norm_input_assay_overlap ----

test_that(
  "olink_norm_input_assay_overlap - works - all assays shared",
  {
    skip_if_not_installed("arrow")

    # 1 df and reference medians ----

    lst_df_v0 <- list(
      "p1" = npx_data1
    )
    reference_medians_v0 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_assay_overlap(
            lst_df = lst_df_v0,
            reference_medians = reference_medians_v0,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v0
    )
    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v0
    )

    # 2 df ----

    lst_df_v1 <- list(
      "p1" = npx_data1,
      "p2" = npx_data2
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_assay_overlap(
            lst_df = lst_df_v1,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID"),
              "p2" = list(olink_id = "OlinkID")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v1
    )

    # multiple df ----

    lst_df_v2 <- list(
      "p1" = npx_data1,
      "p2" = npx_data2,
      "p3" = npx_data1,
      "p4" = npx_data2
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lst_out <- olink_norm_input_assay_overlap(
            lst_df = lst_df_v2,
            reference_medians = NULL,
            lst_cols = list(
              "p1" = list(olink_id = "OlinkID"),
              "p2" = list(olink_id = "OlinkID"),
              "p3" = list(olink_id = "OlinkID"),
              "p4" = list(olink_id = "OlinkID")
            )
          )
        )
      )
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v2
    )

  }
)

test_that(
  "olink_norm_input_assay_overlap - works - non-shared assays",
  {
    skip_if_not_installed("arrow")

    # 1 df and reference_medians - 1 non-shared assay in df ----

    lst_df_v0 <- list(
      "p1" = npx_data1 |>
        dplyr::filter(
          .data[["OlinkID"]] != "OID00471"
        )
    )
    reference_medians_v0 <- npx_data1 |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1
      )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v0,
        reference_medians = reference_medians_v0,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID")
        )
      ),
      regexp = "Assay \"OID00471\" not shared across input dataset"
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lst_df_v0
    )
    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v0 |>
        dplyr::filter(
          .data[["OlinkID"]] != "OID00471"
        )
    )

    # 1 df and reference_medians - 1 non-shared assay in reference_medians ----

    lst_df_v1 <- list(
      "p1" = npx_data1
    )
    reference_medians_v1 <- npx_data1 |>
      dplyr::filter(
        .data[["OlinkID"]] != "OID00471"
      ) |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1
      )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v1,
        reference_medians = reference_medians_v1,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID")
        )
      ),
      regexp = "Assay \"OID00471\" not shared across input dataset"
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lapply(lst_df_v1, function(x) {
        x |>
          dplyr::filter(
            .data[["OlinkID"]] != "OID00471"
          )
      })
    )
    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v1
    )

    # 1 df and reference_medians - non-shared assays in both ----

    lst_df_v2 <- list(
      "p1" = npx_data1 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID00471", "OID00472",
                                      "OID00474", "OID00475"))
        )
    )
    reference_medians_v2 <- npx_data1 |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% c("OID00479", "OID00480", "OID00481"))
      ) |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Reference_NPX = 0.1
      )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v2,
        reference_medians = reference_medians_v2,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID")
        )
      ),
      regexp = paste("Assays \"OID00471\", \"OID00472\", \"OID00474\",",
                     "\"OID00475\", \"OID00479\", \"OID00480\", and",
                     "\"OID00481\" not shared across input dataset")
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lapply(lst_df_v2, function(x) {
        x |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% c("OID00471", "OID00472",
                                        "OID00474", "OID00475",
                                        "OID00479", "OID00480",
                                        "OID00481"))
          )
      })
    )
    expect_identical(
      object = lst_out$reference_medians,
      expected = reference_medians_v1 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID00471", "OID00472",
                                      "OID00474", "OID00475",
                                      "OID00479", "OID00480",
                                      "OID00481"))
        )
    )

    # 2 df - 1 non-shared assay in df ----

    lst_df_v3 <- list(
      "p1" = npx_data1 |>
        dplyr::filter(
          .data[["OlinkID"]] != "OID00471"
        ),
      "p2" = npx_data2
    )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v3,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID"),
          "p2" = list(olink_id = "OlinkID")
        )
      ),
      regexp = paste("Assay \"OID00471\" not shared across input dataset")
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lapply(lst_df_v3, function(x) {
        x |>
          dplyr::filter(
            .data[["OlinkID"]] != "OID00471"
          )
      })
    )

    # 2 df - multiple non-shared assay in df ----

    lst_df_v4 <- list(
      "p1" = npx_data1 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID01300", "OID01301",
                                      "OID01302", "OID01303"))
        ),
      "p2" = npx_data2 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID01282", "OID01283",
                                      "OID01284", "OID01285"))
        )
    )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v4,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID"),
          "p2" = list(olink_id = "OlinkID")
        )
      ),
      regexp = paste("Assays \"OID01300\", \"OID01301\", \"OID01302\",",
                     "\"OID01303\", \"OID01282\", \"OID01283\"")
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lapply(lst_df_v4, function(x) {
        x |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% c("OID01300", "OID01301",
                                        "OID01302", "OID01303",
                                        "OID01282", "OID01283",
                                        "OID01284", "OID01285"))
          )
      })
    )

    # multiple df - multiple non-shared assay in df ----

    lst_df_v5 <- list(
      "p1" = npx_data1 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID01300", "OID01301",
                                      "OID01302", "OID01303"))
        ),
      "p2" = npx_data2 |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% c("OID01282", "OID01283",
                                      "OID01284", "OID01285"))
        ),
      "p3" = npx_data1,
      "p4" = npx_data2 |>
        dplyr::filter(
          .data[["OlinkID"]] != "OID05124"
        )
    )

    expect_warning(
      object = lst_out <- olink_norm_input_assay_overlap(
        lst_df = lst_df_v5,
        reference_medians = NULL,
        lst_cols = list(
          "p1" = list(olink_id = "OlinkID"),
          "p2" = list(olink_id = "OlinkID"),
          "p3" = list(olink_id = "OlinkID"),
          "p4" = list(olink_id = "OlinkID")
        )
      ),
      regexp = paste("Assays \"OID01300\", \"OID01301\", \"OID01302\",",
                     "\"OID01303\", \"OID01282\", \"OID01283\"")
    )

    expect_identical(
      object = lst_out$lst_df,
      expected = lapply(lst_df_v5, function(x) {
        x |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% c("OID01300", "OID01301",
                                        "OID01302", "OID01303",
                                        "OID01282", "OID01283",
                                        "OID01284", "OID01285",
                                        "OID05124"))
          )
      })
    )
  }
)
