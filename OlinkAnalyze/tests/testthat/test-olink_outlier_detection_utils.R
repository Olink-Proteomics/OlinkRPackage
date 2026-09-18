test_that(
  "olink_summarize_qc_warning - works - summarizes by group (QC_Warning)",
  {
    df <- tibble::tibble(
      SampleID = c("S1", "S1", "S2", "S2", "S3", "S3"),
      QC_Warning = c("Pass", "Warning", "pass", "FAIL", "PASS", NA)
    )

    expect_no_warning(
      object = df_summary <- olink_summarize_qc_warning(
        df = df,
        qc_warning = "QC_Warning",
        group = "SampleID"
      )
    )

    expect_identical(
      object = df_summary |>
        dplyr::distinct(.data[["SampleID"]], .data[["QC_Warning"]]) |>
        dplyr::arrange(.data[["SampleID"]]) |>
        dplyr::pull(.data[["QC_Warning"]]),
      expected = c("Warning", "Fail", "Pass")
    )
  }
)

test_that(
  "olink_summarize_qc_warning - works - summarize by QC_Warning & PlateID",
  {
    # duplicate sample identifiers are allowed in different plates. This
    # function allows duplicates, but not OA as whole.

    df <- tibble::tibble(
      SampleID = c("S1", "S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2"),
      PlateID = c("P1", "P1", "P2", "P2", "P3", "P3", "P1", "P1", "P1"),
      QC_Warning = c(
        "Pass", "Warning",
        "PASS", "Fail",
        "Pass", NA_character_,
        "FAIL", "PASS", "WARN"
      )
    )

    expect_no_warning(
      object = df_summary <- olink_summarize_qc_warning(
        df = df,
        qc_warning = "QC_Warning",
        group = c("SampleID", "PlateID")
      )
    )

    expect_identical(
      object = df_summary |>
        dplyr::distinct(
          .data[["SampleID"]], .data[["PlateID"]], .data[["QC_Warning"]]
        ) |>
        dplyr::arrange(.data[["SampleID"]], .data[["PlateID"]]) |>
        dplyr::pull(.data[["QC_Warning"]]),
      expected = c("Warning", "Fail", "Pass", "Fail")
    )
  }
)

test_that(
  "olink_summarize_qc_warning - works - flexible groups and output columns",
  {
    df <- tibble::tibble(
      SampleID = c("S1", "S1", "S1", "S1"),
      Panel = c("P1", "P1", "P2", "P2"),
      QC_Warning = c("Pass", "warn by plate", "Pass", "Pass")
    )

    expect_no_warning(
      object = df_summary <- olink_summarize_qc_warning(
        df = df,
        qc_warning = "QC_Warning",
        group = c("SampleID", "Panel"),
        output_col = "qc_summary"
      )
    )

    expect_identical(
      object = df_summary |>
        dplyr::distinct(.data[["Panel"]], .data[["qc_summary"]]) |>
        dplyr::arrange(.data[["Panel"]]) |>
        dplyr::pull(.data[["qc_summary"]]),
      expected = c("Warning", "Pass")
    )

    expect_identical(
      object = df_summary[["QC_Warning"]],
      expected = df[["QC_Warning"]]
    )
  }
)

test_that(
  "olink_summarize_qc_warning - warns on unknown summaries",
  {
    df <- tibble::tibble(
      SampleID = c("S1", "S1", "S2", "S2"),
      QC_Warning = c(NA_character_, NA_character_, "not checked", NA)
    )

    expect_warning(
      object = df_summary <- olink_summarize_qc_warning(
        df = df,
        qc_warning = "QC_Warning",
        group = "SampleID"
      ),
      regexp = "2 groups were assigned QC status \"Unknown\"",
      fixed = TRUE
    )

    expect_identical(
      object = df_summary |>
        dplyr::distinct(.data[["SampleID"]], .data[["QC_Warning"]]) |>
        dplyr::arrange(.data[["SampleID"]]) |>
        dplyr::pull(.data[["QC_Warning"]]),
      expected = c("Unknown", "Unknown")
    )
  }
)

test_that(
  "olink_summarize_qc_warning - works - ArrowObject",
  {
    skip_if_not_installed("arrow")

    df_arrow <- tibble::tibble(
      SampleID = c("S1", "S1", "S2", "S2", "S3", "S3"),
      Panel = c("P1", "P1", "P1", "P1", "P2", "P2"),
      QC_Warning = c("Pass", "Warn", "pass", "FAIL", "PASS", NA)
    ) |>
      arrow::as_arrow_table()

    expect_no_warning(
      object = df_summary <- olink_summarize_qc_warning(
        df = df_arrow,
        qc_warning = "QC_Warning",
        group = c("SampleID", "Panel")
      )
    )

    expect_true(
      object = check_is_arrow_object(x = df_summary, error = FALSE)
    )

    expect_identical(
      object = df_summary |>
        dplyr::collect() |>
        dplyr::distinct(
          .data[["SampleID"]],
          .data[["Panel"]],
          .data[["QC_Warning"]]
        ) |>
        dplyr::arrange(.data[["SampleID"]]) |>
        dplyr::pull(.data[["QC_Warning"]]),
      expected = c("Warning", "Fail", "Pass")
    )
  }
)
