test_that(
  "check is numeric works - TRUE",
  {
    expect_true(
      check_is_numeric(num = 3.14,
                       error = FALSE)
    )

    expect_true(
      check_is_numeric(num = 3.14,
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3.14),
                       error = FALSE)
    )

    expect_true(
      check_is_numeric(num = c(3.14),
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               NULL),
                       error = FALSE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               NULL),
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               1),
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               1),
                       error = FALSE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               1L),
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3.14,
                               1L),
                       error = FALSE)
    )

    expect_true(
      check_is_numeric(num = c(3L,
                               1L),
                       error = TRUE)
    )

    expect_true(
      check_is_numeric(num = c(3L,
                               1L),
                       error = FALSE)
    )

  }
)

test_that(
  "check is numeric works - FALSE",
  {

    expect_false(
      check_is_numeric(num = c("I_Shall_Pass",
                               NA_character_),
                       error = FALSE)
    )

    expect_false(
      check_is_numeric(num = NA_character_,
                       error = FALSE)
    )

    expect_false(
      check_is_numeric(num = NULL,
                       error = FALSE)
    )

    expect_false(
      check_is_numeric(num = TRUE,
                       error = FALSE)
    )

  }
)

test_that(
  "check is numeric works - ERROR",
  {

    expect_error(
      check_is_numeric(num = c("I_Shall_Pass",
                               NA_character_),
                       error = TRUE),
      regexp = "must be a number vector!"
    )

    expect_error(
      check_is_numeric(num = NA_character_,
                       error = TRUE),
      regexp = "must be a number vector!"
    )

    expect_error(
      check_is_numeric(num = NULL,
                       error = TRUE),
      regexp = "must be a number vector!"
    )

    expect_error(
      check_is_numeric(num = TRUE,
                       error = TRUE),
      regexp = "must be a number vector!"
    )

  }
)
