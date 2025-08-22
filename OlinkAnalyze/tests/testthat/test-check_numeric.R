# Test check_is_numeric ----

test_that(
  "check is numeric works - TRUE",
  {
    expect_true(
      object = check_is_numeric(x = 3.14,
                                error = FALSE)
    )

    expect_true(
      object = check_is_numeric(x = 3.14,
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14),
                                error = FALSE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14),
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      NULL),
                                error = FALSE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      NULL),
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      1),
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      1),
                                error = FALSE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      1L),
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3.14,
                                      1L),
                                error = FALSE)
    )

    expect_true(
      object = check_is_numeric(x = c(3L,
                                      1L),
                                error = TRUE)
    )

    expect_true(
      object = check_is_numeric(x = c(3L,
                                      1L),
                                error = FALSE)
    )
  }
)

test_that(
  "check is numeric works - FALSE",
  {
    expect_false(
      object = check_is_numeric(x = c("I_Shall_Pass",
                                      NA_character_),
                                error = FALSE)
    )

    expect_false(
      object = check_is_numeric(x = c("I_Shall_Pass",
                                      "Me_Neither"),
                                error = FALSE)
    )

    expect_false(
      object = check_is_numeric(x = NA_character_,
                                error = FALSE)
    )

    expect_false(
      object = check_is_numeric(x = NULL,
                                error = FALSE)
    )

    expect_false(
      object = check_is_numeric(x = TRUE,
                                error = FALSE)
    )
  }
)

test_that(
  "check is numeric works - ERROR",
  {
    expect_error(
      object = check_is_numeric(x = c("I_Shall_Pass",
                                      NA_character_),
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a numeric vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_numeric(x = c("I_Shall_Pass",
                                      "Me_Neither"),
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a numeric vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_numeric(x = NA_character_,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a numeric vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_numeric(x = NULL,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a numeric vector!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_numeric(x = TRUE,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a numeric vector!",
                    fixed = TRUE)
    )
  }
)

# Test check_is_scalar_numeric ----

test_that(
  "check is scalar numeric works - TRUE",
  {
    expect_true(
      object = check_is_scalar_numeric(x = 3.14,
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = 3.14,
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = 1L,
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = 1L,
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(3.14),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(3.14),
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(1L),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(1L),
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(3.14,
                                             NULL),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(3.14,
                                             NULL),
                                       error = FALSE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(1L,
                                             NULL),
                                       error = TRUE)
    )

    expect_true(
      object = check_is_scalar_numeric(x = c(1L,
                                             NULL),
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar numeric works - FALSE",
  {
    expect_false(
      object = check_is_scalar_numeric(x = c(1,
                                             3.14),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_numeric(x = c(3.14,
                                             NA_character_),
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_numeric(x = NA_character_,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_numeric(x = NULL,
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_numeric(x = "I_shall_not_pass",
                                       error = FALSE)
    )

    expect_false(
      object = check_is_scalar_numeric(x = TRUE,
                                       error = FALSE)
    )
  }
)

test_that(
  "check is scalar numeric works - ERROR",
  {
    expect_error(
      object = check_is_scalar_numeric(x = c(1,
                                             3.14),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_numeric(x = c("3.14",
                                             NA_character_),
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_numeric(x = NA_character_,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_numeric(x = NULL,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_numeric(x = "I_shall_not_pass",
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_scalar_numeric(x = TRUE,
                                       error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "must be a scalar numeric!",
                    fixed = TRUE)
    )
  }
)
