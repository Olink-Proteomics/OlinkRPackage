test_that(
  "check is integer works - TRUE",
  {

    expect_true(
      object = check_is_integer(int = 1L,
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(int = 1L,
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(int = c(1L),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(int = c(1L),
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(int = c(1L,
                                        NULL),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(int = c(1L,
                                        NULL),
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(int = c(1L,
                                        2L),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(int = c(1L,
                                        2L),
                                error = TRUE)
    )

  }
)

test_that(
  "check is integer works - FALSE",
  {

    expect_false(
      object = check_is_integer(int = "I_Shall_Pass",
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(int = NA_character_,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(int = NULL,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(int = TRUE,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(int = 1,
                                error = FALSE)
    )

  }
)

test_that(
  "check is integer works - ERROR",
  {

    expect_error(
      object = check_is_integer(int = "I_Shall_Pass",
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(int = NA_character_,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(int = NULL,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(int = TRUE,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(int = 1,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

  }
)

