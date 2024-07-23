# Test olink_norm_check_input_cols ----

test_that(
  "olink_norm_check_input_cols - Normalization col - 1 dataset",
  {
    # df does not have Normalization col ----

    expect_warning(
      object = olink_norm_check_input_cols(
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

    # df has Normalization col ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_check_input_cols(
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
  }
)

test_that(
  "olink_norm_check_input_cols - Normalization col - 2 datasets",
  {
    # no df has Normalization col ----

    expect_warning(
      object = olink_norm_check_input_cols(
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
          object = olink_norm_check_input_cols(
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
          object = olink_norm_check_input_cols(
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

    # one df has Normalization col p1 ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::mutate(Normalization = "Intensity"),
          "p2" = npx_data2
        )
      ),
      regexp = "Dataset \"p2\" does not contain a column named \"Normalization"
    )

  }
)

test_that(
  "olink_norm_check_input_cols - Normalization col - 3+ datasets",
  {
    # df 1 and 2 do not have Normalization col ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
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
      object = olink_norm_check_input_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
                      "p2" = OlinkAnalyze::npx_data2 |>
                        dplyr::mutate(Normalization = "Intensity"),
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
          object = olink_norm_check_input_cols(
            lst_df = list("p1" = OlinkAnalyze::npx_data1 |>
                            dplyr::mutate(Normalization = "Plate control"),
                          "p2" = OlinkAnalyze::npx_data2 |>
                            dplyr::mutate(Normalization = "Intensity"),
                          "p3" = OlinkAnalyze::npx_data1 |>
                            dplyr::mutate(Normalization = "Intensity"),
                          "p4" = OlinkAnalyze::npx_data2 |>
                            dplyr::mutate(Normalization = "Plate control"))
          )
        )
      )
    )

    # no df has Normalization col ----

    expect_warning(
      object = olink_norm_check_input_cols(
        lst_df = list("p1" = OlinkAnalyze::npx_data1,
                      "p2" = OlinkAnalyze::npx_data2,
                      "p3" = OlinkAnalyze::npx_data1,
                      "p4" = OlinkAnalyze::npx_data2)
      ),
      regexp = "Datasets \"p1\", \"p2\", \"p3\", and \"p4\" do not contain a"
    )

  }
)

test_that(
  "olink_norm_check_input_cols - error - missing cols",
  {
    # 2 df missing cols v1 ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("SampleID"))
            ),
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
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Panel_Version", "NPX"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("QC_Warning"))
            )
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets with missing column"
    )

    # one df missing cols ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("NPX"))
            )
          ,
          "p2" = npx_data2
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Dataset with missing column"
    )

    # multiple df missing cols ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = OlinkAnalyze::npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Panel_Version", "NPX"))
            ),
          "p2" = OlinkAnalyze::npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Panel", "Panel_Version"))
            ),
          "p3" = OlinkAnalyze::npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Assay", "UniProt"))
            ),
          "p4" = OlinkAnalyze::npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("QC_Warning"))
            )
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets with missing column"
    )

  }
)

test_that(
  "olink_norm_check_input_cols - error - different quant methdos",
  {
    # 2 df with different quant method ----

    expect_error(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::rename(
              "Quantified_value" = "NPX"
            ),
          "p2" = npx_data2
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets are not quantified with the same method"
    )

    # multiple df with different quant method ----

    expect_error(
      object = olink_norm_check_input_cols(
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
            )
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Datasets are not quantified with the same method"
    )

  }
)

test_that(
  "olink_norm_check_input_cols - error - missing non-required cols",
  {
    # df 1 missing 1 col ----

    expect_warning(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index"))
            ),
          "p2" = npx_data2
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Column not present across datasets"
    )

    # df 1 missing 2 col & df 2 missing 1 col ----

    expect_warning(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index", "Site"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Treatment"))
            )
        ) |>
          lapply(dplyr::mutate, Normalization = "Intensity")
      ),
      regexp = "Columns not present across datasets"
    )

    # one df - no warn ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_norm_check_input_cols(
            lst_df = list(
              "p1" = npx_data1 |>
                dplyr::select(
                  -dplyr::all_of(c("Index"))
                )
            ) |>
              lapply(dplyr::mutate, Normalization = "Intensity")
          )
        )
      )
    )

    # multiple df missing non-required cols ----

    expect_warning(
      object = olink_norm_check_input_cols(
        lst_df = list(
          "p1" = npx_data1 |>
            dplyr::select(
              -dplyr::all_of(c("Index", "Site"))
            ),
          "p2" = npx_data2 |>
            dplyr::select(
              -dplyr::all_of(c("Treatment"))
            ),
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
