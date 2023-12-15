# Test read_npx_wide_assay_nrow ----

test_that(
  "read_npx_wide_assay_nrow - works as expecetd",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "NPX", "Panel", "Assay",
                  "Uniprot ID", "OlinkID", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "OID1", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "OID2", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "OID3", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = assay_nrow <-
            read_npx_wide_assay_nrow(file = wide_excel)
        )

        # check that assay_nrow exists
        expect_true(exists("assay_nrow"))

        # check that the correct number of rows is returned
        expect_equal(
          object = assay_nrow,
          expected = 4L
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "Ct", "Panel", "Assay",
                  "Uniprot ID", "OlinkID", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "OID1", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "OID2", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "OID3", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = assay_nrow <-
            read_npx_wide_assay_nrow(file = wide_excel)
        )

        # check that assay_nrow exists
        expect_true(exists("assay_nrow"))

        # check that the correct number of rows is returned
        expect_equal(
          object = assay_nrow,
          expected = 4L
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "Quantified", "Panel", "Assay",
                  "Uniprot ID", "OlinkID", "Unit", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "OID1", "pg/mL", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "OID2", "pg/mL", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "OID3", "pg/mL", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = assay_nrow <-
            read_npx_wide_assay_nrow(file = wide_excel)
        )

        # check that assay_nrow exists
        expect_true(exists("assay_nrow"))

        # check that the correct number of rows is returned
        expect_equal(
          object = assay_nrow,
          expected = 5L
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_assay_nrow - unexpected number of assay rows",
  {
    ## NPX/Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "NPX", "Panel", "Assay",
                  "Uniprot ID", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_assay_nrow(file = wide_excel),
          regexp = "We identified 3 rows containing data about assays in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "Quantified", "Panel", "Assay",
                  "Uniprot ID", "OlinkID", "Unit", "Extracol", NA_character_,
                  "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "OID1", "pg/mL", "A", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "OID2", "pg/mL", "A", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "OID3", "pg/mL", "A", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_assay_nrow(file = wide_excel),
          regexp = "We identified 6 rows containing data about assays in file"
        )

      }
    )


  }
)

test_that(
  "read_npx_wide_assay_nrow - non-matching number of assay rows",
  {
    ## NPX/Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "Quantified", "Panel", "Assay",
                  "Uniprot ID", "OlinkID", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "OID1", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "OID2", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "OID3", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_assay_nrow(file = wide_excel),
          regexp = "while we expected 5"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        dplyr::tibble(
          "A" = c("project_Name", "Quantified", "Panel", "Assay",
                  "Uniprot ID", "Unit", NA_character_, "Sample1"),
          "B" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                  "Uniprot1", "pg/mL", NA_character_, 1.1),
          "C" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                  "Uniprot2", "pg/mL", NA_character_, 2.2),
          "D" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                  "Uniprot3", "pg/mL", NA_character_, 3.3)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_assay_nrow(file = wide_excel),
          regexp = "while we expected 5"
        )

      }
    )


  }
)
