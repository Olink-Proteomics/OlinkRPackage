test_that(
  "check is scalar numeric works - TRUE",
  {
    expect_true(
      check_is_scalar_numeric(num = 3.14,
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = 3.14,
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_numeric(num = 1L,
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = 1L,
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(3.14),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(3.14),
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(1L),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(1L),
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(3.14,
                                      NULL),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(3.14,
                                      NULL),
                              error = FALSE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(1L,
                                      NULL),
                              error = TRUE)
    )

    expect_true(
      check_is_scalar_numeric(num = c(1L,
                                      NULL),
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar numeric works - FALSE",
  {

    expect_false(
      check_is_scalar_numeric(num = c(1,
                                      3.14),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_numeric(num = c("3.14",
                                      NA_character_),
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_numeric(num = NA_character_,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_numeric(num = NULL,
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_numeric(num = "I_shall_not_pass",
                              error = FALSE)
    )

    expect_false(
      check_is_scalar_numeric(num = TRUE,
                              error = FALSE)
    )

  }
)

test_that(
  "check is scalar numeric works - ERROR",
  {

    expect_error(
      check_is_scalar_numeric(num = c(1,
                                      3.14),
                              error = TRUE),
      regexp = "must be a number!"
    )

    expect_error(
      check_is_scalar_numeric(num = c("3.14",
                                      NA_character_),
                              error = TRUE),
      regexp = "must be a number!"
    )

    expect_error(
      check_is_scalar_numeric(num = NA_character_,
                              error = TRUE),
      regexp = "must be a number!"
    )

    expect_error(
      check_is_scalar_numeric(num = NULL,
                              error = TRUE),
      regexp = "must be a number!"
    )

    expect_error(
      check_is_scalar_numeric(num = "I_shall_not_pass",
                              error = TRUE),
      regexp = "must be a number!"
    )

    expect_error(
      check_is_scalar_numeric(num = TRUE,
                              error = TRUE),
      regexp = "must be a number!"
    )

  }
)
