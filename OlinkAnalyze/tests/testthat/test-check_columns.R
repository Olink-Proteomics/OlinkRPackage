# Test check_columns ----

test_that(
  "check_columns - works - tibble",
  {
    tmp_data <- dplyr::tibble(
      "A" = c(1L, 2L, 3L),
      "B" = c(TRUE, TRUE, FALSE),
      "C" = c("A", "B", "C"),
      "D" = c(FALSE, FALSE, TRUE)
    )

    # both A and B exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "B"))
    )

    # A and (B or C) exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", "C")))
    )

    # (A or D) and (B or C) exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list(c("A", "D"),
                                             c("B", "C")))
    )

    # A exists but E does not
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "E")),
      regexp = "is missing the required columns: \"E\"!"
    )

    # A exists but E and F do not
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "E",
                                             "F")),
      regexp = "is missing the required columns: \"E\" and \"F\"!"
    )

    # A and (B or E) exist -> no error as B exists
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", "E")))
    )

    # A and (F or E) exist -> error as neither E nor F exist
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("F", "E"))),
      regexp = "is missing columns that should be present in at least one"
    )

    # A and (F or E) exist -> error as neither E nor F exist
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("F", "E"),
                                             c("M", "N"))),
      regexp = "is missing columns that should be present in at least one"
    )
  }
)

test_that(
  "check_columns - works - arrow",
  {
    tmp_data <- dplyr::tibble(
      "A" = c(1L, 2L, 3L),
      "B" = c(TRUE, TRUE, FALSE),
      "C" = c("A", "B", "C"),
      "D" = c(FALSE, FALSE, TRUE)
    ) |>
      arrow::as_arrow_table()

    # both A and B exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "B"))
    )

    # A and (B or C) exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", "C")))
    )

    # (A or D) and (B or C) exist
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list(c("A", "D"),
                                             c("B", "C")))
    )

    # A exists but E does not
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "E")),
      regexp = "is missing the required columns: \"E\"!"
    )

    # A exists but E and F do not
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             "E",
                                             "F")),
      regexp = "is missing the required columns: \"E\" and \"F\"!"
    )

    # A and (B or E) exist -> no error as B exists
    expect_no_condition(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", "E")))
    )

    # A and (F or E) exist -> error as neither E nor F exist
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("F", "E"))),
      regexp = "is missing columns that should be present in at least one"
    )

    # A and (F or E) exist -> error as neither E nor F exist
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("F", "E"),
                                             c("M", "N"))),
      regexp = "is missing columns that should be present in at least one"
    )
  }
)

test_that(
  "check_columns - error - incorrect input",
  {
    tmp_data <- dplyr::tibble(
      "A" = c(1L, 2L, 3L),
      "B" = c(TRUE, TRUE, FALSE),
      "C" = c("A", "B", "C"),
      "D" = c(FALSE, FALSE, TRUE)
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             1L)),
      regexp = "contains 1 element that is not character vector"
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             TRUE)),
      regexp = "contains 1 element that is not character vector"
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             1.1)),
      regexp = "contains 1 element that is not character vector"
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             TRUE,
                                             1L,
                                             1.1)),
      regexp = "contains 3 elements that are not character vectors"
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", TRUE),
                                             1L,
                                             1.1)),
      regexp = "contains 2 elements that are not character vectors!"
    )

    # error non-character vector
    expect_error(
      object = check_columns(df = tmp_data,
                             col_list = list("A",
                                             c("B", TRUE),
                                             c(1L, 2L),
                                             1.1)),
      regexp = "contains 2 elements that are not character vectors!"
    )
  }
)
