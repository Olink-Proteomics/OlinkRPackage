# Test that the function returns TRUE when the packages are present
test_that(
  "check library installed works - TRUE", {

    expect_no_condition(
      object = check_library_installed(
        libraries = c("tools", "base"),
        error = FALSE
      )
    )

    expect_no_condition(
      object = check_library_installed(
        libraries = c("tools", "base"),
        error = TRUE
      )
    )

  }
)

# Test that FALSE returns when library is missing
test_that(
  "check library installed works - FALSE",
  {

    expect_false(
      object = check_library_installed(
        libraries = c("MissingLibraryOne"),
        error = FALSE
      )
    )

    expect_false(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "MissingLibraryTwo",
                      "MissingLibraryThree"),
        error = FALSE
      )
    )

    expect_false(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "tools"),
        error = FALSE
      )
    )

  }
)

# Test that relevant error is thrown when library is missing
test_that(
  "check library installed works - library missing",
  {

    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne"),
        error = TRUE
      ),
      regexp = "One or more missing libraries"
    )

    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "MissingLibraryTwo",
                      "MissingLibraryThree"),
        error = TRUE
      ),
      regexp = "One or more missing libraries"
    )

    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "tools"),
        error = TRUE
      ),
      regexp = "One or more missing libraries"
    )

  }
)
