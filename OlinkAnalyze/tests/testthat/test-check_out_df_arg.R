test_that(
  "check out df arg works - TRUE",
  {
    expect_true(
      object = check_out_df_arg(out_df = "tibble")
    )

    expect_true(
      object = check_out_df_arg(out_df = "arrow")
    )

  }
)

test_that(
  "check out df arg works - ERROR",
  {

    expect_error(
      object = check_out_df_arg(out_df = "Tibble"),
      regexp = "Acceptable output types"
    )

    expect_error(
      object = check_out_df_arg(out_df = "TIBBLE"),
      regexp = "Acceptable output types"
    )

    expect_error(
      object = check_out_df_arg(out_df = "I_Shall_Not_Pass"),
      regexp = "Acceptable output types"
    )

  }
)
