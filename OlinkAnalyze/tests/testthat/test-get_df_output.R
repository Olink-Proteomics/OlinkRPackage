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
