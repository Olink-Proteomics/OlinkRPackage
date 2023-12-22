# Test read_npx_wide_split_row ----

test_that(
  "read_npx_wide_split_row - works as expecetd",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "NPX", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, "Sample1",
                   NA_character_, "LOD"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1, NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2, NA_character_, 1.1),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3, NA_character_, 1.1)
        )

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = "NPX")
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(3L:6L)
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(8L) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(10L) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            )
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "Ct", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, "Sample1"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3)
        )

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = "Ct")
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(3L:6L)
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(8L) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            )
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "Quantified", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", "Unit", NA_character_, "Sample1",
                   NA_character_, "LOD"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", "pg/mL", NA_character_, 1.1,
                   NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", "pg/mL", NA_character_, 2.2,
                   NA_character_, 1.1),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", "pg/mL", NA_character_, 3.3,
                   NA_character_, 1.1)
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = "Quantified")
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(3L:7L)
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(9L) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(11L) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - no or too many all-NA rows",
  {
    ## No all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        dplyr::tibble(
          "V1" = c("project_Name", "NPX", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", "A", "Sample1",
                   "A", "LOD"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1, NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2, NA_character_, 1.1),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3, NA_character_, 1.1)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX"),
          regexp = "We identified 0 rows with all columns `NA` in file"
        )
      }
    )

    ## Too many all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        dplyr::tibble(
          "V1" = c("project_Name", NA_character_, "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, "Sample1",
                   NA_character_, "LOD"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1, NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2, NA_character_, 1.1),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3, NA_character_, 1.1)
        ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX"),
          regexp = "We identified 3 rows with all columns `NA` in file"
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - exactly as many all-NA rows as expected",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "NPX", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, "Sample1"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3)
        )

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX"),
          regexp = "contains 1 row with all columns NA, but based on"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "Ct", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, "Sample1",
                   NA_character_, "LOD"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, 1.1, NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, 2.2, NA_character_, 1.1),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, 3.3, NA_character_, 1.1)
        )

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Ct"),
          regexp = "contains 2 rows with all columns NA, but based on"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "Quantified", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", "Unit", NA_character_, "Sample1"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", "pg/mL", NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", "pg/mL", NA_character_, 2.2),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", "pg/mL", NA_character_, 3.3)
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "contains 1 row with all columns NA, but based on"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - all-NA rows are not consecutive",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("project_Name", "NPX", "Panel", "Assay",
                   "Uniprot ID", "OlinkID", NA_character_, NA_character_,
                   "Sample1"),
          "V2" = c("SW Version", NA_character_, "Olink Target 48", "Assay1",
                   "Uniprot1", "OID1", NA_character_, NA_character_, 1.1),
          "V3" = c(NA_character_, NA_character_, "Olink Target 48", "Assay2",
                   "Uniprot2", "OID2", NA_character_, NA_character_, 2.2),
          "V4" = c(NA_character_, NA_character_, "Olink Target 48", "Assay3",
                   "Uniprot3", "OID3", NA_character_, NA_character_, 3.3)
        )

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX"),
          regexp = "Consecutive rows with all columns NA."
        )

      }
    )

  }
)

# Test read_npx_wide_check_top ----

test_that(
  "read_npx_wide_check_top - works as expecetd",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "NPX")
        )

        # check that function runs for Ct
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Ct")
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1", "pg/mL"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2", "pg/mL"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3", "pg/mL")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified")
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - unexpected number of assay rows",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "NPX"),
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

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit",
                   "ExtraCol"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1", "pg/mL",
                   "A"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2", "pg/mL",
                   "B"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3", "pg/mL",
                   "C")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "We identified 6 rows containing data about assays in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - non-matching number of assay rows",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified"),
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

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "Unit"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "pg/mL"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "pg/mL"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "pg/mL")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "while we expected 5"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - incorrect values in column 1",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID2"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "NPX"),
          regexp = "Column 1 of assay metadata in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit2"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1", "pg/mL"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2", "pg/mL"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3", "pg/mL")
        )

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "Column 1 of assay metadata in file"
        )

      }
    )

  }
)

# Test read_npx_wide_split_col ----

test_that(
  "read_npx_wide_split_col - works",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3"),
          "V5" = c("Olink Target 48", "Plate ID",
                   NA_character_, NA_character_),
          "V6" = c("Olink Target 48", "QC Warning",
                   NA_character_, NA_character_),
          "V7" = c("Olink Target 48", "QC Deviation from median",
                   "Inc Ctrl", NA_character_),
          "V8" = c("Olink Target 48", "QC Deviation from median",
                   "Det Ctrl", NA_character_)
        )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = x_index,
          expected = 5L
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1", "pg/mL"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2", "pg/mL"),
          "V4" = c("Olink Target 48", "Assay3", "Uniprot3", "OID3", "pg/mL"),
          "V5" = c("Olink Target 48", "Assay4", "Uniprot4", "OID4", "pg/mL"),
          "V6" = c("Olink Target 48", "Assay5", "Uniprot5", "OID5", "pg/mL"),
          "V7" = c("Olink Target 48", "Assay6", "Uniprot6", "OID6", "pg/mL"),
          "V8" = c("Olink Target 48", "Plate ID",
                   NA_character_, NA_character_, NA_character_),
          "V9" = c("Olink Target 48", "QC Warning",
                   NA_character_, NA_character_, NA_character_),
          "V10" = c("Olink Target 48", "QC Deviation from median",
                    "Inc Ctrl", NA_character_, NA_character_),
          "V11" = c("Olink Target 48", "QC Deviation from median",
                    "Det Ctrl", NA_character_, NA_character_)
        )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = x_index,
          expected = 8L
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_col - no Assay=\"Plate ID\" columns",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        df <- dplyr::tibble(
          "V1" = c("Panel", "Assay", "Uniprot ID", "OlinkID"),
          "V2" = c("Olink Target 48", "Assay1", "Uniprot1", "OID1"),
          "V3" = c("Olink Target 48", "Assay2", "Uniprot2", "OID2")
        )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_split_col(df = df,
                                           file = wide_excel),
          regexp = "Unexpected format of the top metadata in file"
        )

      }
    )

  }
)

# Test read_npx_wide_top_split ----

test_that(
  "read_npx_wide_top_split - T48 - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- c("Inc Ctrl", "Amp Ctrl", "Ext Ctrl")

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Quantified",
                                                olink_platform = "Target 48")
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_identical(
          object = list_top$df_qc_dev,
          expected = df_tmp_qc_dev
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T48 - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 2L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 2L
        n_assay <- 45L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 2L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- c("Inc Ctrl", "Amp Ctrl", "Ext Ctrl")

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Quantified",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_identical(
          object = list_top$df_qc_dev,
          expected = df_tmp_qc_dev
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T96 - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 1L
        n_assay <- 92L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 1L
        n_assay <- 92L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T96 - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 2L
        n_assay <- 92L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 2L
        n_assay <- 92L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - Flex/Focus - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Flex"
        n_panels <- 1L
        n_assay <- 33L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Flex"
        n_panels <- 1L
        n_assay <- 33L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Flex"
        n_panels <- 1L
        n_assay <- 33L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- c("Inc Ctrl", "Amp Ctrl", "Ext Ctrl")

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Quantified",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_identical(
          object = list_top$df_qc_dev,
          expected = df_tmp_qc_dev
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - Flex/Focus - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Focus"
        n_panels <- 2L
        n_assay <- 33L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "NPX",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Focus"
        n_panels <- 2L
        n_assay <- 33L
        n_qc_warn <- 0L
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Ct",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_true(object = is.null(list_top$df_qc_dev))
      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Focus"
        n_panels <- 2L
        n_assay <- 33L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- c("Inc Ctrl", "Amp Ctrl", "Ext Ctrl")

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object =
            list_top <- read_npx_wide_top_split(df = df_t,
                                                file = wide_excel,
                                                data_type = "Quantified",
                                                olink_platform = o_platform)
        )

        # modify df so that we can test output ----

        colnames(df) <- dplyr::slice_head(df, n = 1L)
        df <- df |>
          dplyr::slice(
            2L:dplyr::n()
          ) |>
          dplyr::mutate(
            col_index = dplyr::row_number() + 1L,
            col_index = paste0("V", .data[["col_index"]])
          )

        ## separate df ----

        df_tmp_oid <- df |>
          dplyr::filter(
            !is.na(.data[["OlinkID"]])
          )

        df_tmp_meta <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
          ) |>
          dplyr::select(
            -dplyr::all_of(c("Uniprot ID", "OlinkID"))
          ) |>
          dplyr::rename(
            "Var" = "Assay"
          )

        df_tmp_qc_dev <- df |>
          dplyr::filter(
            is.na(.data[["OlinkID"]])
            & (.data[["Assay"]] == "QC Deviation from median"
               | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
          ) |>
          dplyr::select(
            -dplyr::all_of(c("OlinkID"))
          )

        # check that tmp df are identical to function output ----

        expect_identical(
          object = list_top$df_oid,
          expected = df_tmp_oid
        )

        expect_identical(
          object = list_top$df_meta,
          expected = df_tmp_meta
        )

        expect_identical(
          object = list_top$df_qc_dev,
          expected = df_tmp_qc_dev
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - unrecognizable tags",
  {

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = 1L),
                         seq_len(1L))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels)),
                   rep(x = "Unknown", times = 1L)),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels),
                   rep(x = NA_character_, times = 1L)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)),
                   rep(x = NA_character_, times = 1L))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - NAs in OlinkID/Uniprot/Assay",
  {
    ## OlinkID = NA 1 instance ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )

        # introduce NAs
        df <- df |>
          dplyr::mutate(
            V3 = dplyr::if_else(.data[["V3"]] %in% "Uniprot1",
                                NA_character_,
                                .data[["V3"]])
          )

        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 1 empty cells in columns"
        )

      }
    )

    ## OlinkID = NA 5 instances ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )

        # introduce NAs
        df <- df |>
          dplyr::mutate(
            V3 = dplyr::if_else(
              .data[["V3"]] %in% paste0("Uniprot", seq_len(2L)),
              NA_character_,
              .data[["V3"]]
            ),
            V2 = dplyr::if_else(
              .data[["V2"]] %in% paste0("Assay", seq_len(3L)),
              NA_character_,
              .data[["V2"]]
            )
          )

        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 5 empty cells in columns"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - wrong number of assays",
  {
    ## T48 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 40L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 40 assays in 1 panels in file"
        )

      }
    )

    ## T48 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 2L
        n_assay <- 32L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 64 assays in 2 panels in file"
        )
      }
    )

    ## T96 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 1L
        n_assay <- 67L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 67 assays in 1 panels in file"
        )

      }
    )

    ## T96 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 96"
        n_panels <- 2L
        n_assay <- 78L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0L)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform),
          regexp = "Detected 156 assays in 2 panels in file"
        )
      }
    )


  }
)

test_that(
  "read_npx_wide_top_split - QC_Warning on Ct data",
  {

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----
        o_platform <- "Target 48"
        n_panels <- 1L
        n_assay <- 45L
        n_qc_warn <- n_panels
        n_plates <- n_panels
        int_ctrl <- character(0)

        df <- dplyr::tibble(
          "V1" = c("Panel",
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = n_assay),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_plates),
                         seq_len(n_plates)),
                   paste(rep(paste("Olink ", o_platform, " Panel"),
                             times = n_qc_warn),
                         seq_len(n_qc_warn)),
                   rep(x = paste("Olink ", o_platform,
                                 " Panel", seq_len(n_panels)),
                       each = length(int_ctrl))),
          "V2" = c("Assay",
                   paste0(rep(x = "Assay", times = (n_panels * n_assay)),
                          seq_len(n_panels * n_assay)),
                   rep(x = "Plate ID", times = n_plates),
                   rep(x = "QC Warning", times = n_qc_warn),
                   rep(x = "QC Deviation from median",
                       times = (length(int_ctrl) * n_panels))),
          "V3" = c("Uniprot ID",
                   paste0(rep(x = "Uniprot", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = int_ctrl, times = n_panels)),
          "V4" = c("OlinkID",
                   paste0(rep(x = "OID", times = (n_panels * n_assay)),
                          seq_len((n_panels * n_assay))),
                   rep(x = NA_character_, times = n_plates),
                   rep(x = NA_character_, times = n_qc_warn),
                   rep(x = NA_character_,
                       times = (length(int_ctrl) * n_panels)))
        )
        df_t <- t(df)
        colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
        rownames(df_t) <- NULL
        df_t <- dplyr::as_tibble(df_t)

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df_t,
                                           file = wide_excel,
                                           data_type = "Ct",
                                           olink_platform = o_platform),
          regexp = "in the right-hand side of the top matrix in file"
        )

      }
    )

  }
)
