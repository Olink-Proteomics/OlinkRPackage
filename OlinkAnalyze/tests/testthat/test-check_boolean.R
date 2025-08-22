# Test check_is_boolean ----

test_that(
  "check is boolean works - TRUE",
  {
    expect_true(
      object = check_is_boolean(x = TRUE,
                                error = FALSE)
    )

    expect_true(
      object = check_is_boolean(x = TRUE,
                                error = TRUE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE),
                                error = FALSE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE),
                                error = TRUE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE,
                                      NULL),
                                error = FALSE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE,
                                      NULL),
                                error = TRUE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE,
                                      TRUE),
                                error = FALSE)
    )

    expect_true(
      object = check_is_boolean(x = c(TRUE,
                                      TRUE),
                                error = TRUE)
    )

  }
)

test_that(
  "check is boolean works - FALSE",
  {
    expect_false(
      object = check_is_boolean(x = "I_Shall_Pass",
                                error = FALSE)
    )

    expect_false(
      object = check_is_boolean(x = NA_character_,
                                error = FALSE)
    )

    expect_false(
      object = check_is_boolean(x = NULL,
                                error = FALSE)
    )

    expect_false(
      object = check_is_boolean(x = 1L,
                                error = FALSE)
    )

    expect_false(
      object = check_is_boolean(x = 1,
                                error = FALSE)
    )
  }
)

test_that(
  "check is boolean works - ERROR",
  {
    expect_error(
      object = check_is_boolean(x = "I_Shall_Pass",
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a boolean vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_boolean(x = NA_character_,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a boolean vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_boolean(x = NULL,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a boolean vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_boolean(x = 1L,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a boolean vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_boolean(x = 1,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a boolean vector!",
                    fixed = TRUE)
    )
  }
)

# test check_is_scalar_boolean ----

test_that(
  "check is scalar bool works - TRUE",
  {
    expect_true(
      object = check_is_scalar_boolean(x = TRUE,
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_boolean(x = TRUE,
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_boolean(x = c(TRUE),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_boolean(x = c(TRUE),
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             NULL),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             NULL),
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar bool works - FALSE",
  {
    expect_false(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             FALSE),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             NA),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = NA_integer_,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = NULL,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = 1,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = "I_shall_not_pass",
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_boolean(x = 1L,
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar bool works - ERROR",
  {
    expect_error(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             FALSE),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = c(TRUE,
                                             NA),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = NA_integer_,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = NULL,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = 1,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = "I_shall_not_pass",
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_boolean(x = 1L,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar boolean!",
                    fixed = TRUE)
    )
  }
)
