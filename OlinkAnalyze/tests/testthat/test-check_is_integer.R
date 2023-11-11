test_that(
  "check is integer works - TRUE",
  {
    expect_true(
      check_is_integer(int = 1L,
                       error = FALSE)
    )

    expect_true(
      check_is_integer(int = 1L,
                       error = TRUE)
    )

    expect_true(
      check_is_integer(int = c(1L),
                       error = FALSE)
    )

    expect_true(
      check_is_integer(int = c(1L),
                       error = TRUE)
    )

    expect_true(
      check_is_integer(int = c(1L,
                               NULL),
                       error = FALSE)
    )

    expect_true(
      check_is_integer(int = c(1L,
                               NULL),
                       error = TRUE)
    )

    expect_true(
      check_is_integer(int = c(1L,
                               2L),
                       error = FALSE)
    )

    expect_true(
      check_is_integer(int = c(1L,
                               2L),
                       error = TRUE)
    )

  }
)

test_that(
  "check is integer works - FALSE",
  {

    expect_false(
      check_is_integer(int = "I_Shall_Pass",
                       error = FALSE)
    )

    expect_false(
      check_is_integer(int = NA_character_,
                       error = FALSE)
    )

    expect_false(
      check_is_integer(int = NULL,
                       error = FALSE)
    )

    expect_false(
      check_is_integer(int = TRUE,
                       error = FALSE)
    )

    expect_false(
      check_is_integer(int = 1,
                       error = FALSE)
    )

  }
)

test_that(
  "check is integer works - FALSE",
  {

    expect_error(
      check_is_integer(int = "I_Shall_Pass",
                       error = TRUE),
      regexp = "must be an integer vector!"
    )

    expect_error(
      check_is_integer(int = NA_character_,
                       error = TRUE),
      regexp = "must be an integer vector!"
    )

    expect_error(
      check_is_integer(int = NULL,
                       error = TRUE),
      regexp = "must be an integer vector!"
    )

    expect_error(
      check_is_integer(int = TRUE,
                       error = TRUE),
      regexp = "must be an integer vector!"
    )

    expect_error(
      check_is_integer(int = 1,
                       error = TRUE),
      regexp = "must be an integer vector!"
    )

  }
)

