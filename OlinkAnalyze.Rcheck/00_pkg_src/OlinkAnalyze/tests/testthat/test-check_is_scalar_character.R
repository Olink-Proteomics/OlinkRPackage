test_that(
  "check is scalar character works - TRUE",
  {
    expect_true(
      object = check_is_scalar_character(string = "I_Shall_Pass",
                                         error = TRUE)
    )

    expect_true(
      object = check_is_scalar_character(string = "I_Shall_Pass",
                                         error = FALSE)
    )

    expect_true(
      object = check_is_scalar_character(string = c("I_Shall_Pass"),
                                         error = TRUE)
    )

    expect_true(
      object = check_is_scalar_character(string = c("I_Shall_Pass"),
                                         error = FALSE)
    )

    expect_true(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    NULL),
                                         error = TRUE)
    )

    expect_true(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    NULL),
                                         error = FALSE)
    )

  }
)

test_that(
  "check is scalar character works - FALSE",
  {
    expect_false(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    "I_Shall_Not_Pass"),
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    NA_character_),
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = NA_character_,
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = NULL,
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = 1,
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = 1L,
                                         error = FALSE)
    )

    expect_false(
      object = check_is_scalar_character(string = TRUE,
                                         error = FALSE)
    )

  }
)

test_that(
  "check is scalar character works - ERROR",
  {
    expect_error(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    "I_Shall_Not_Pass"),
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = c("I_Shall_Pass",
                                                    NA_character_),
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = NA_character_,
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = NULL,
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = 1,
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = 1L,
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_character(string = TRUE,
                                         error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a string!",
                    fixed = TRUE)
    )

  }
)
