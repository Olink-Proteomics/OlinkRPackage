# Test get_df_output ----

test_that(
  "get_df_output - works",
  {
    acc_df_out <- get_df_output()

    all_out_df <- sapply(read_npx_df_output, grepl, acc_df_out)

    expect_true(object = all(all_out_df))
  }
)

# Test get_df_output_print ----

test_that(
  "get_df_output_print - works",
  {
    acc_df_out <- get_df_output_print()

    all_out_df <- read_npx_df_output |>
      stringr::str_replace_all("arrow", "ArrowObject") |>
      sapply(grepl, acc_df_out)

    expect_true(object = all(all_out_df))
  }
)
