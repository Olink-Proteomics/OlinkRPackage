test_that(
  "check is boolean works - TRUE",
  {
    expect_true(
      check_is_boolean(bool = TRUE,
                       error = FALSE)
    )

    expect_true(
      check_is_boolean(bool = TRUE,
                       error = TRUE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE),
                       error = FALSE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE),
                       error = TRUE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE,
                                NULL),
                       error = FALSE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE,
                                NULL),
                       error = TRUE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE,
                                TRUE),
                       error = FALSE)
    )

    expect_true(
      check_is_boolean(bool = c(TRUE,
                                TRUE),
                       error = TRUE)
    )

  }
)

test_that(
  "check is boolean works - FALSE",
  {

    expect_false(
      check_is_boolean(bool = "I_Shall_Pass",
                       error = FALSE)
    )

    expect_false(
      check_is_boolean(bool = NA_character_,
                       error = FALSE)
    )

    expect_false(
      check_is_boolean(bool = NULL,
                       error = FALSE)
    )

    expect_false(
      check_is_boolean(bool = 1L,
                       error = FALSE)
    )

    expect_false(
      check_is_boolean(bool = 1,
                       error = FALSE)
    )

  }
)

test_that(
  "check is boolean works - ERROR",
  {

    expect_error(
      check_is_boolean(bool = "I_Shall_Pass",
                       error = TRUE),
      regexp = "must be a boolean vector!"
    )

    expect_error(
      check_is_boolean(bool = NA_character_,
                       error = TRUE),
      regexp = "must be a boolean vector!"
    )

    expect_error(
      check_is_boolean(bool = NULL,
                       error = TRUE),
      regexp = "must be a boolean vector!"
    )

    expect_error(
      check_is_boolean(bool = 1L,
                       error = TRUE),
      regexp = "must be a boolean vector!"
    )

    expect_error(
      check_is_boolean(bool = 1,
                       error = TRUE),
      regexp = "must be a boolean vector!"
    )

  }
)

