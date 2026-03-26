# Test check_is_integer ----

test_that(
  "check is integer works - TRUE",
  {
    expect_true(
      object = check_is_integer(x = 1L,
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(x = 1L,
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(x = c(1L),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(x = c(1L),
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(x = c(1L,
                                      NULL),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(x = c(1L,
                                      NULL),
                                error = TRUE)
    )

    expect_true(
      object = check_is_integer(x = c(1L,
                                      2L),
                                error = FALSE)
    )

    expect_true(
      object = check_is_integer(x = c(1L,
                                      2L),
                                error = TRUE)
    )
  }
)

test_that(
  "check is integer works - FALSE",
  {
    expect_false(
      object = check_is_integer(x = "I_Shall_Pass",
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(x = NA_character_,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(x = NULL,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(x = TRUE,
                                error = FALSE)
    )

    expect_false(
      object = check_is_integer(x = 1,
                                error = FALSE)
    )
  }
)

test_that(
  "check is integer works - ERROR",
  {
    expect_error(
      object = check_is_integer(x = "I_Shall_Pass",
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(x = NA_character_,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(x = NULL,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(x = TRUE,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_integer(x = 1,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be an integer vector!",
                    fixed = TRUE)
    )
  }
)

# Test check_is_scalar_integer ----

test_that(
  "check is scalar integer works - TRUE",
  {
    expect_true(
      object = check_is_scalar_integer(x = 1L,
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_integer(x = 1L,
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_integer(x = c(1L),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_integer(x = c(1L),
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_integer(x = c(1L,
                                             NULL),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_integer(x = c(1L,
                                             NULL),
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar integer works - FALSE",
  {
    expect_false(
      object = check_is_scalar_integer(x = c(1L,
                                             2L),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = c(1L,
                                             NA_integer_),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = NA_integer_,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = NULL,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = 1,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = "I_shall_not_pass",
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_integer(x = TRUE,
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar integer works - ERROR",
  {
    expect_error(
      object = check_is_scalar_integer(x = c(1L,
                                             2L),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = c(1L,
                                             NA_integer_),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = NA_integer_,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = NULL,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = 1,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = "I_shall_not_pass",
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_integer(x = TRUE,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar integer!",
                    fixed = TRUE)
    )
  }
)
