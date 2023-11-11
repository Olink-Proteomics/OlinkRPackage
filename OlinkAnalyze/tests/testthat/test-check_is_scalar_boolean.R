test_that(
  "check is scalar bool works - TRUE",
  {
    expect_true(
      check_is_scalar_boolean(bool = TRUE,
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_boolean(bool = TRUE,
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_boolean(bool = c(TRUE),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_boolean(bool = c(TRUE),
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_boolean(bool = c(TRUE,
                                       NULL),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_boolean(bool = c(TRUE,
                                       NULL),
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar bool works - FALSE",
  {
    expect_false(
      check_is_scalar_boolean(bool = c(TRUE,
                                       FALSE),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = c(TRUE,
                                      NA),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = NA_integer_,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = NULL,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = 1,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = "I_shall_not_pass",
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_boolean(bool = 1L,
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar bool works - ERROR",
  {
    expect_error(
      check_is_scalar_boolean(bool = c(TRUE,
                                       FALSE),
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = c(TRUE,
                                       NA),
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = NA_integer_,
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = NULL,
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = 1,
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = "I_shall_not_pass",
                              error = TRUE),
      regexp = "must be a boolean!"
    )

    expect_error(
      check_is_scalar_boolean(bool = 1L,
                              error = TRUE),
      regexp = "must be a boolean!"
    )

  }
)
