# Test check_is_list ----

test_that(
  "check is list works - TRUE",
  {
    expect_true(
      object = check_is_list(x = list("I_Shall_Pass"),
                             error = FALSE)
    )

    expect_true(
      object = check_is_list(x = list("I_Shall_Pass"),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass")),
                             error = FALSE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass")),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        NULL)),
                             error = FALSE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        NULL)),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        "I_Shall_Not_Pass")),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        "I_Shall_Not_Pass")),
                             error = FALSE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        1,
                                        1.1,
                                        TRUE)),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list(c("I_Shall_Pass",
                                        1,
                                        1.1,
                                        TRUE)),
                             error = FALSE)
    )

    expect_true(
      object = check_is_list(x = list("A" = c("I_Shall_Pass",
                                              1,
                                              1.1,
                                              TRUE),
                                      "B" = "Im_good"),
                             error = TRUE)
    )

    expect_true(
      object = check_is_list(x = list("A" = c("I_Shall_Pass",
                                              1,
                                              1.1,
                                              TRUE),
                                      "B" = "Im_good"),
                             error = FALSE)
    )
  }
)

test_that(
  "check is list works - FALSE",
  {
    expect_false(
      object = check_is_list(x = c("I_Shall_Pass",
                                   NA_character_),
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(x = NA_character_,
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(x = NULL,
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(x = 1,
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(x = 1L,
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(x = TRUE,
                             error = FALSE)
    )

    expect_false(
      object = check_is_list(
        x = dplyr::tibble(
          "A" = 1L:5L,
          "B" = LETTERS[1L:5L]
        ),
        error = FALSE
      )
    )
  }
)

test_that(
  "check is list works - ERROR",
  {
    expect_error(
      object = check_is_list(x = c("I_Shall_Pass",
                                   NA_character_),
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(x = NA_character_,
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(x = NULL,
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(x = 1,
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(x = 1L,
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(x = TRUE,
                             error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_list(
        x = dplyr::tibble(
          "A" = 1L:5L,
          "B" = LETTERS[1L:5L]
        ),
        error = TRUE
      ),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a list!",
                    fixed = TRUE)
    )
  }
)
