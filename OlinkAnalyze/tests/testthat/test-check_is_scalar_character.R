test_that(
  "check is scalar character works",
  {
    expect_no_condition(
      check_is_scalar_character(string = "I_Shall_Pass")
    )

    expect_no_condition(
      check_is_scalar_character(string = c("I_Shall_Pass"))
    )

    expect_no_condition(
      check_is_scalar_character(string = c("I_Shall_Pass",
                                           NULL))
    )

    expect_error(
      check_is_scalar_character(string = c("I_Shall_Pass",
                                           "I_Shall_Not_Pass")),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = c("I_Shall_Pass",
                                           NA_character_)),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = NA_character_),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = NULL),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = 1),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = 1L),
      regexp = "must be a string!"
    )

    expect_error(
      check_is_scalar_character(string = TRUE),
      regexp = "must be a string!"
    )

  }
)
