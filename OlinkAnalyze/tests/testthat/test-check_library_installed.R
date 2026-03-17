# Test check_library_installed ----

test_that(
  "check_library_installed - works - TRUE", {

    expect_no_condition(
      object = check_library_installed(
        x = c("tools", "base"),
        error = FALSE
      )
    )

    expect_no_condition(
      object = check_library_installed(
        x = c("tools", "base"),
        error = TRUE
      )
    )

  }
)

# Test that FALSE returns when library is missing
test_that(
  "check_library_installed - works - FALSE",
  {

    expect_false(
      object = check_library_installed(
        x = c("MissingLibraryOne"),
        error = FALSE
      )
    )

    expect_false(
      object = check_library_installed(
        x = c("MissingLibraryOne",
              "MissingLibraryTwo",
              "MissingLibraryThree"),
        error = FALSE
      )
    )

    expect_false(
      object = check_library_installed(
        x = c("MissingLibraryOne",
              "tools"),
        error = FALSE
      )
    )

  }
)

# Test that relevant error is thrown when library is missing
test_that(
  "check_library_installed - works - library missing",
  {

    expect_error(
      object = check_library_installed(
        x = c("MissingLibraryOne"),
        error = TRUE
      ),
      regexp = "Missing library:"
    )

    expect_error(
      object = check_library_installed(
        x = c("MissingLibraryOne",
              "MissingLibraryTwo",
              "MissingLibraryThree"),
        error = TRUE
      ),
      regexp = "Missing libraries:"
    )

    expect_error(
      object = check_library_installed(
        x = c("MissingLibraryOne",
              "tools"),
        error = TRUE
      ),
      regexp = "Missing library:"
    )

  }
)
