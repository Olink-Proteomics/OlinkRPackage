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

# Test olink_norm_input_check_df_cols ----

test_that(
  "olink_norm_input_check_df_cols - Normalization col - 1 dataset",
  {
    skip_if_not_installed("arrow")

    # df does not have Normalization col ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
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

    # df does not have Normalization col - arrow ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
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

    # df has Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_df_cols(
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

    # df has Normalization col - arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_df_cols(
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
  }
)

test_that(
  "olink_norm_input_check_df_cols - Normalization col - 2 datasets",
  {
    skip_if_not_installed("arrow")

    # no df has Normalization col ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
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

    # both df have same Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_df_cols(
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

    # both df have different Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_input_check_df_cols(
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

    # one df has Normalization col ----

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

    # one df has Normalization col - arrow ----

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

    # df 1 and 2 do not have Normalization col ----

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

    # df 1 does not have Normalization col ----

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
          object = olink_norm_input_check_df_cols(
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

    # no df has Normalization col ----

    expect_warning(
      object = olink_norm_input_check_df_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
                      "p2" = OlinkAnalyze::npx_data2 |>
                        arrow::as_arrow_table(),
                      "p3" = OlinkAnalyze::npx_data1,
                      "p4" = OlinkAnalyze::npx_data2)
      ),
      regexp = "Datasets \"p1\", \"p2\", \"p3\", and \"p4\" do not contain a"
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
