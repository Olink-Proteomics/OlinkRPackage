test_that(
  "check is scalar integer works - TRUE",
  {
    expect_true(
      check_is_scalar_integer(int = 1L,
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_integer(int = 1L,
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_integer(int = c(1L),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_integer(int = c(1L),
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_integer(int = c(1L,
                                      NULL),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_integer(int = c(1L,
                                      NULL),
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar integer works - FALSE",
  {
    expect_false(
      check_is_scalar_integer(int = c(1L,
                                      2L),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = c(1L,
                                      NA_integer_),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = NA_integer_,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = NULL,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = 1,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = "I_shall_not_pass",
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_integer(int = TRUE,
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar integer works - ERROR",
  {
    expect_error(
      check_is_scalar_integer(int = c(1L,
                                      2L),
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = c(1L,
                                      NA_integer_),
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = NA_integer_,
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = NULL,
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = 1,
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = "I_shall_not_pass",
                              error = TRUE),
      regexp = "must be an integer!"
    )

    expect_error(
      check_is_scalar_integer(int = TRUE,
                              error = TRUE),
      regexp = "must be an integer!"
    )

  }
)
