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

# Test get_accepted_file_ext_summary ----

test_that(
  "get_accepted_file_ext_summary - works",
  {
    acc_f_ext_sum <- get_accepted_file_ext_summary()

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
