# Test that the function throws no errors, warning or messages when the packages
# are present
test_that(
  "check library installed works - library present", {
    expect_no_condition(
      object = check_library_installed(
        libraries = c("tools", "base")
      )
    )
  }
)

# Test that relevant error is thrown when library is missing
test_that(
  "check library installed works - library missing", {
    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne")
      ),
      regexp = "One or more missing libraries"
    )

    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "MissingLibraryTwo",
                      "MissingLibraryThree")
      ),
      regexp = "One or more missing libraries"
    )

    expect_error(
      object = check_library_installed(
        libraries = c("MissingLibraryOne",
                      "tools")
      ),
      regexp = "One or more missing libraries"
    )
  }
)
