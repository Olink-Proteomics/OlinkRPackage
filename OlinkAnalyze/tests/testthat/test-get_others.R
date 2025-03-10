# Test get_df_output_print ----

test_that(
  "get_df_output_print - works",
  {
    expect_true(
      object = read_npx_df_output |>
        stringr::str_replace_all("arrow", "ArrowObject") |>
        (\(.) . %in% get_df_output_print())() |>
        all()
    )
  }
)

# Test get_file_ext_summary ----

test_that(
  "get_file_ext_summary - works",
  {
    acc_f_ext_sum <- get_file_ext_summary()

    # file extensions present ----

    all_ext_present <- sapply(accepted_npx_file_ext, grepl, acc_f_ext_sum)

    expect_true(object = all(all_ext_present))

    # file types present ----

    all_type_present <- names(accepted_npx_file_ext) |>
      stringr::str_split(pattern = "_") |>
      lapply(utils::head, 1L) |>
      unlist() |>
      unique() |>
      sapply(grepl, acc_f_ext_sum)

    expect_true(object = all(all_type_present))
  }
)

# Test get_file_formats ----

test_that(
  "get_file_formats - works",
  {
    expect_identical(
      object = get_file_formats(),
      expected = c("excel", "delim", "parquet", "compressed")
    )
  }
)

# Test get_file_ext ----

test_that(
  "get_file_ext - works",
  {
    expect_identical(
      object = get_file_ext(name_sub = NULL) |> unname(),
      expected = c("xls", "xlsx", "csv", "txt", "parquet", "zip")
    )

    expect_identical(
      object = get_file_ext(name_sub = "excel") |> unname(),
      expected = c("xls", "xlsx")
    )

    expect_identical(
      object = get_file_ext(name_sub = "parquet") |> unname(),
      expected = c("parquet")
    )

    expect_identical(
      object = get_file_ext(name_sub = "compressed") |> unname(),
      expected = c("zip")
    )

    expect_identical(
      object = get_file_ext(name_sub = "delim") |> unname(),
      expected = c("csv", "txt")
    )

    expect_identical(
      object = get_file_ext(name_sub = c("excel", "parquet")) |> unname(),
      expected = c("xls", "xlsx", "parquet")
    )

    expect_identical(
      object = get_file_ext(name_sub = c("excel", "delim")) |> unname(),
      expected = c("xls", "xlsx", "csv", "txt")
    )
  }
)

test_that(
  "get_file_ext - error",
  {
    expect_error(
      object = get_file_ext(name_sub = NA_character_),
      regexp = "must be a character vector"
    )

    expect_error(
      object = get_file_ext(name_sub = "zip"),
      regexp = "\"zip\" does not reflect an acceptable file format"
    )

    expect_error(
      object = get_file_ext(name_sub = "text"),
      regexp = "\"text\" does not reflect an acceptable file format"
    )

    expect_error(
      object = get_file_ext(name_sub = c("text", "zip")),
      regexp = "\"text\" and \"zip\" do not reflect an acceptable file format"
    )

    expect_error(
      object = get_file_ext(name_sub = "An_Unacceptable_File_Format"),
      regexp = "\"An_Unacceptable_File_Format\" does not reflect an acceptable"
    )
  }
)
