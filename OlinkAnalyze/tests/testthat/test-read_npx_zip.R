test_that(
  "read NPX zip - zip contains only README.txt",
  {
    readme_file <- file.path(tempdir(),
                             "README.txt")

    writeLines("foo", readme_file)

    expect_true(file.exists(readme_file))

    zip_file <- character()

    withr::with_tempfile(
      new = "zip_test",
      pattern = "zip_test",
      fileext = ".zip",
      code = {
        utils::zip(zipfile = zip_test,
                   files = c(readme_file),
                   flags = "-jq")

        expect_true(file.exists(zip_test))

        expect_error(
          read_npx_zip(
            file = zip_test
          ),
          regexp = "The compressed file does not contain checksum or NPX files!"
        )

        zip_file <<- zip_test
      }
    )

    file.remove(readme_file)

    expect_false(file.exists(zip_file))
    expect_false(file.exists(readme_file))
  }
)
