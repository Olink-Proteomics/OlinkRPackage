# Test read npx excel ----

test_that(
  "read_npx_excel - correct input - long",
  {
    ## Target 96 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Target 96"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works olink_platform T96
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works olink_platform T96
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 96",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in lo"
        )

      }
    )

    ### Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_ct",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Ct")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Target 96"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with olink_platform T96 and
        # data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

        # check that read_npx_excel works with long_format NULL and
        # olink_platform T96 and data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 96",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in lo"
        )

      }
    )

    ## Target 48 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Target 48"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works olink_platform T48
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works olink_platform T48
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in lo"
        )

      }
    )

    ### Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_ct",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Ct")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Target 48"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with olink_platform T48 and
        # data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format NULL and
        # olink_platform T48 and data_type Ct
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in lo"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Quantified")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Target 48"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # olink_platform T48
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with olink_platform T48 and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

        # check that read_npx_excel works with long_format NULL and
        # olink_platform T48 and data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in lo"
        )

      }
    )

    ## Flex v1 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Flex"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Quantified")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "Olink Flex"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with olink_platform Flex and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format NULL and
        # olink_platform Flex and data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

      }
    )

    ## Flex v2 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "ABCD-ABCD"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works long_format TRUE
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works olink_platform Flex
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in lo"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Quantified")
        colnames(df_in)[7L] <- "Panel"
        df_in$Panel[1] <- "ABCD-ABCD"

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # olink_platform Flex
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format TRUE and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with olink_platform Flex and
        # data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

        # check that read_npx_excel works with long_format NULL and
        # olink_platform Flex and data_type Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_long,
            long_format = TRUE,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in lo"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel - correct input - wide",
  {
    ## Target 96 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX", "Panel"),
          y = c("Signature", NA_character_, "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format FALSE
        # and olink_platform T96
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format FALSE
        # and data_type NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works olink_platform = T96
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = T96 and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 96",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 96\" in wi"
        )

      }
    )

    ### Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_ct",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "Ct", "Panel"),
          y = c("Signature", NA_character_, "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works olink_platform = T96
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = T96
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 96",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works olink_platform = T96
        # and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 96",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = T96 and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 96",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 96\" in wi"
        )

      }
    )

    ## Target 48 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX", "Panel"),
          y = c("Signature", NA_character_, "Olink Target 48")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = T48 and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wi"
        )

      }
    )

    ### Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_ct",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Ct at cell A2
        dplyr::tibble(
          x = c("Olink", "Ct", "Panel"),
          y = c("Signature", NA_character_, "Olink Target 48")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        # and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = T48 and data_type = Ct
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "Ct"
          ),
          regexp = "Detected \"Ct\" data from \"Olink Target 48\" in wi"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Quantified at cell A2
        dplyr::tibble(
          x = c("Olink", "Quantified", "Panel"),
          y = c("Signature", NA_character_, "Olink Target 48")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = T48
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works olink_platform = T48
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = T48 and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Target 48\" in wi"
        )

      }
    )

    ## Flex v1 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX", "Panel"),
          y = c("Signature", NA_character_, "Olink Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = Flex and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Quantified at cell A2
        dplyr::tibble(
          x = c("Olink", "Quantified", "Panel"),
          y = c("Signature", NA_character_, "Olink Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = Flex and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

      }
    )

    ## Flex v2 ----

    ### NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_npx",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX", "Panel"),
          y = c("Signature", NA_character_, "ABCD-ABCD")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        # and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = Flex and data_type = NPX
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = "NPX"
          ),
          regexp = "Detected \"NPX\" data from \"Olink Flex\" in wi"
        )

      }
    )

    ### Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide_quantified",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Quantified at cell A2
        dplyr::tibble(
          x = c("Olink", "Quantified", "Panel"),
          y = c("Signature", NA_character_, "ABCD-ABCD")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel works
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and olink_platform = Flex
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = NULL
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = NULL,
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works olink_platform = Flex
        # and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = NULL,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

        # check that read_npx_excel works long_format = FALSE and
        # olink_platform = Flex and data_type = Quantified
        expect_message(
          object = read_npx_excel(
            file = excel_wide,
            long_format = FALSE,
            olink_platform = "Flex",
            data_type = "Quantified"
          ),
          regexp = "Detected \"Quantified\" data from \"Olink Flex\" in wi"
        )

      }
    )
  }
)

# Test check excel format (wide/long) ----

test_that(
  "read_npx_excel_format - correct input - wide",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_npx_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_wide,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = FALSE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("NPX")
        )

        # check that read_npx_excel_format works with long_format = FALSE
        expect_no_condition(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_wide,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_false,
          expected = df_npx_null
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_ct_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Ct at cell A2
        dplyr::tibble(
          x = c("Olink", "Ct")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_wide,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = FALSE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("Ct")
        )

        # check that read_npx_excel_format works with long_format = FALSE
        expect_no_condition(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_wide,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_false,
          expected = df_npx_null
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_quantified_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Quantified at cell A2
        dplyr::tibble(
          x = c("Olink", "Quantified")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_wide,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = FALSE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("Quantified")
        )

        # check that read_npx_excel_format works with long_format = FALSE
        expect_no_condition(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_wide,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_false,
          expected = df_npx_null
        )

      }
    )
  }
)

test_that(
  "read_npx_excel_format - correct input - long",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_npx_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_long,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("LOD", "UniProt", "Assay", "NPX")
        )

        # check that read_npx_excel_format works with long_format = TRUE
        expect_no_condition(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_long,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_true,
          expected = df_npx_null
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_ct_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Ct at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Ct")

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_long,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("LOD", "UniProt", "Assay", "Ct")
        )

        # check that read_npx_excel_format works with long_format = TRUE
        expect_no_condition(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_long,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_true,
          expected = df_npx_null
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_quantified_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel with Quantified at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Quantified")

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_long,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("LOD", "UniProt", "Assay", "Quantified")
        )

        # check that read_npx_excel_format works with long_format = TRUE
        expect_no_condition(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_long,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that the two runs of read_npx_excel_format return the same
        expect_identical(
          object = df_npx_true,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_format - correct input - difference in detection",
  {
    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "NPX")

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        ### long_format = NULL ----

        # check that read_npx_excel_format throws error with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_long,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("LOD", "UniProt", "Assay", "NPX")
        )

        ### long_format = TRUE ----

        # check that read_npx_excel_format throws error with long_format = NULL
        expect_no_condition(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_long,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_true),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_true$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_true$data_cells,
          expected = c("LOD", "UniProt", "Assay", "NPX")
        )

        ### long_format = FALSE ----

        # check that read_npx_excel_format throws warn with long_format = FALSE
        expect_warning(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_long,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Based on `long_format` we were expecting \"wide\" format"
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = FALSE
        )

        # check that the output string is conatins data from df_in
        expect_equal(
          object = df_npx_false$data_cells |>
            as.numeric(),
          expected = df_in |>
            dplyr::slice(1L) |>
            dplyr::pull(.data[["A"]]),
          tolerance = 1e-8
        )

      }
    )

    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel with NPX at cell A2
        dplyr::tibble(
          x = c("Olink", "NPX")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        ### long_format = NULL ----

        # check that read_npx_excel_format throws error with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_format(
            file = excel_wide,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = FALSE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = "NPX"
        )

        ### long_format = TRUE ----

        # check that read_npx_excel_format throws warn with long_format = TRUE
        expect_warning(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_wide,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Based on `long_format` we were expecting \"long\" format"
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_true),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_true$is_long_format,
          expected = TRUE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_true$data_cells,
          expected = character(0L)
        )

        ### long_format = FALSE ----

        # check that read_npx_excel_format throws warn with long_format = FALSE
        expect_no_condition(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_wide,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = FALSE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_false$data_cells,
          expected = "NPX"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_format - incorrect input - incorrect input file",
  {
    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel with a wrong quantification name at cell O1
        df_in <- matrix(
          data = rnorm(n = 30L),
          nrow = 2L,
          ncol = 15L
        ) |>
          dplyr::as_tibble(
            .name_repair = "minimal"
          )
        colnames(df_in) <- LETTERS[1L:15L]
        colnames(df_in)[12L:15L] <- c("LOD", "UniProt", "Assay", "Wrong_Name")

        writexl::write_xlsx(
          x = df_in,
          path = excel_long,
          col_names = TRUE,
          format_headers = FALSE
        )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        ### long_format = NULL ----

        # check that read_npx_excel_format throws error with long_format = NULL
        expect_error(
          object = read_npx_excel_format(
            file = excel_long,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to recognize the format of the excel file"
        )

        ### long_format = TRUE ----

        # check that read_npx_excel_format throws warn with long_format = TRUE
        expect_warning(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_long,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to confirm the \"long\" format from the excel file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_true),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_true$is_long_format,
          expected = TRUE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_true$data_cells,
          expected = c("LOD", "UniProt", "Assay", "Wrong_Name")
        )

        ### long_format = FALSE ----

        # check that read_npx_excel_format throws warn with long_format = FALSE
        expect_warning(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_long,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to confirm the \"wide\" format from the excel file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = FALSE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_false$data_cells |>
            as.numeric(),
          expected = df_in |>
            dplyr::slice(1L) |>
            dplyr::pull(.data[["A"]]),
          tolerance = 1e-8
        )

      }
    )

    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel with a wrong quantification name at cell A2
        dplyr::tibble(
          x = c("Olink", "Wrong_Name")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        ### long_format = NULL ----

        # check that read_npx_excel_format throws error with long_format = NULL
        expect_error(
          object = read_npx_excel_format(
            file = excel_wide,
            long_format = NULL,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to recognize the format of the excel file"
        )

        ### long_format = TRUE ----

        # check that read_npx_excel_format throws warn with long_format = TRUE
        expect_warning(
          object = df_npx_true <- read_npx_excel_format(
            file = excel_wide,
            long_format = TRUE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to confirm the \"long\" format from the excel file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_true),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_true$is_long_format,
          expected = TRUE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_true$data_cells,
          expected = character(0L)
        )

        ### long_format = FALSE ----

        # check that read_npx_excel_format throws warn with long_format = FALSE
        expect_warning(
          object = df_npx_false <- read_npx_excel_format(
            file = excel_wide,
            long_format = FALSE,
            quant_methods_excel = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to confirm the \"wide\" format from the excel file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = FALSE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_false$data_cells,
          expected = "Wrong_Name"
        )

      }
    )
  }
)

# Test check excel platform (T96, T48, Flex, Focus) ----

test_that(
  "read_npx_excel_platform - correct input - wide",
  {
    ## Target 96 ----

    withr::with_tempfile(
      new = "excel_t96",
      pattern = "test_excel_t96",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 96 in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_t96,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_t96))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_t96,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 96"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_no_condition(
          object = df_npx_t96 <- read_npx_excel_platform(
            file = excel_t96,
            olink_platform = "Target 96",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t96"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t96,
          expected = df_npx_null
        )

      }
    )

    ## Target 48 ----

    withr::with_tempfile(
      new = "excel_t48",
      pattern = "test_excel_t48",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 48 in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Target 48")
        ) |>
          writexl::write_xlsx(
            path = excel_t48,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_t48))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_t48,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 48"
        )

        # check that read_npx_excel_platform works for olink_platform = "T48"
        expect_no_condition(
          object = df_npx_t48 <- read_npx_excel_platform(
            file = excel_t48,
            olink_platform = "Target 48",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t48"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t48,
          expected = df_npx_null
        )

      }
    )

    ## Flex v1 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Flex in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_flex,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # check that read_npx_excel_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = "Flex",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

    ## Flex v2 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Flex (ABCD-ABCD) in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "ABCD-ABCD")
        ) |>
          writexl::write_xlsx(
            path = excel_flex,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # check that read_npx_excel_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = "Flex",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_platform - correct input - long",
  {
    ## Target 96 ----

    withr::with_tempfile(
      new = "excel_t96",
      pattern = "test_excel_t96",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 96 in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_t96,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_t96))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_t96,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 96"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_no_condition(
          object = df_npx_t96 <- read_npx_excel_platform(
            file = excel_t96,
            olink_platform = "Target 96",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t96"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t96,
          expected = df_npx_null
        )

      }
    )

    ## Target 48 ----

    withr::with_tempfile(
      new = "excel_t48",
      pattern = "test_excel_t48",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 48 in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Target 48")
        ) |>
          writexl::write_xlsx(
            path = excel_t48,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_t48))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_t48,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 48"
        )

        # check that read_npx_excel_platform works for olink_platform = "T48"
        expect_no_condition(
          object = df_npx_t48 <- read_npx_excel_platform(
            file = excel_t48,
            olink_platform = "Target 48",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t48"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t48,
          expected = df_npx_null
        )

      }
    )

    ## Flex v1 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Flex in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_flex,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # check that read_npx_excel_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = "Flex",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

    ## Flex v2 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Flex (ABCD-ABCD) in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "ABCD-ABCD")
        ) |>
          writexl::write_xlsx(
            path = excel_flex,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # check that read_npx_excel_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_excel_platform(
            file = excel_flex,
            olink_platform = "Flex",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_platform - incorrect input - difference in detection",
  {
    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 96 in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_wide,
            olink_platform = "Target 48",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Based on `olink_platform` we were expecting Olink Target 48"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 48"
        )

      }
    )

    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Target 96 in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Target 96")
        ) |>
          writexl::write_xlsx(
            path = excel_long,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_long,
            olink_platform = "Target 48",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Based on `olink_platform` we were expecting Olink Target 48"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 48"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_platform - incorrect input - no match",
  {
    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Unknown platform in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Unknown")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_error(
          object = read_npx_excel_platform(
            file = excel_wide,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Expected one of:"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_wide,
            olink_platform = "Target 96",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "No matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with Unknown platform in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Unknown")
        ) |>
          writexl::write_xlsx(
            path = excel_long,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_error(
          object = read_npx_excel_platform(
            file = excel_long,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Expected one of:"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_long,
            olink_platform = "Target 96",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "No matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_platform - incorrect input - multiple matches",
  {
    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with multiple matches in cell B3
        dplyr::tibble(
          "A" = c("Project_Name", "NPX data", "Panel"),
          "B" = c("NPX_Manager", NA_character_, "Olink Target 48 Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_wide,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_error(
          object = read_npx_excel_platform(
            file = excel_wide,
            olink_platform = NULL,
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Too many matches from:"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_wide,
            olink_platform = "Target 96",
            is_long_format = FALSE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Too many matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # write a simple excel file with multiple matches in cell G2
        dplyr::tibble(
          "A" = c("A", "A"),
          "B" = c("B", "B"),
          "C" = c("C", "C"),
          "D" = c("D", "D"),
          "E" = c("E", "E"),
          "F" = c("F", "F"),
          "G" = c("G", "Olink Target 96 Flex")
        ) |>
          writexl::write_xlsx(
            path = excel_long,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_platform works for olink_platform = NULL
        expect_error(
          object = read_npx_excel_platform(
            file = excel_long,
            olink_platform = NULL,
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Too many matches from:"
        )

        # check that read_npx_excel_platform works for olink_platform = "T96"
        expect_warning(
          object = df_npx <- read_npx_excel_platform(
            file = excel_long,
            olink_platform = "Target 96",
            is_long_format = TRUE,
            olink_platforms_excel =  accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Too many matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

  }
)

# Test check excel quantification method (NPX, Ct, Quantified) ----

test_that(
  "read_npx_excel_quant - correct input - wide",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_npx_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "NPX"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_no_condition(
          object = df_npx_npx <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_npx"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_npx,
          expected = df_npx_null
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_ct_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Ct"
        )

        # check that read_npx_excel_quant works for data_type = Ct
        expect_no_condition(
          object = df_npx_ct <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "Ct",
            data_cells = "Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_ct"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_ct,
          expected = df_npx_null
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_quantified_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Quantified",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Quantified"
        )

        # check that read_npx_excel_quant works for data_type = Quantified
        expect_no_condition(
          object = df_npx_quant <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "Quantified",
            data_cells = "Quantified",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_quant"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_quant,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_quant - correct input - long",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_npx_long",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "Assay", "NPX"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "NPX"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_no_condition(
          object = df_npx_npx <- read_npx_excel_quant(
            file = excel_long,
            data_type = "NPX",
            data_cells = c("LOD", "UniProt", "Assay", "NPX"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_npx"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_npx,
          expected = df_npx_null
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_ct_long",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "Assay", "Ct"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Ct"
        )

        # check that read_npx_excel_quant works for data_type = Ct
        expect_no_condition(
          object = df_npx_ct <- read_npx_excel_quant(
            file = excel_long,
            data_type = "Ct",
            data_cells = c("LOD", "UniProt", "Assay", "Ct"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_ct"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_ct,
          expected = df_npx_null
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_quantified_long",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "Assay", "Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Quantified"
        )

        # check that read_npx_excel_quant works for data_type = Quantified
        expect_no_condition(
          object = df_npx_quant <- read_npx_excel_quant(
            file = excel_long,
            data_type = "Quantified",
            data_cells = c("LOD", "UniProt", "Assay", "Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_quant"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_quant,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_quant - incorrect input - difference in detection",
  {
    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = Ct
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "Ct",
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Based on `data_type` we were expecting \"Ct\" format data"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Ct"
        )

      }
    )

    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = Ct
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_long,
            data_type = "Ct",
            data_cells = c("LOD", "UniProt", "Assay", "NPX"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Based on `data_type` we were expecting \"Ct\" format data"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Ct"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_quant - incorrect input - no quant method match",
  {
    ## Long ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_error(
          object = read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "Assay", "Wrong_Name"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Expected one of:"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_long,
            data_type = "NPX",
            data_cells = c("LOD", "UniProt", "Assay", "Wrong_Name"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "No matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_error(
          object = read_npx_excel_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Wrong_Name",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Expected one of:"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "Wrong_Name",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "No matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

  }
)

test_that(
  "read_npx_excel_quant - incorrect input - multiple quant method matches",
  {
    ## Long v1 ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_error(
          object = read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "NPX", "Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many occurrences of:"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_long,
            data_type = "NPX",
            data_cells = c("LOD", "UniProt", "NPX", "Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains NULL
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

    ## Long v2 ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_long)

        # check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_error(
          object = read_npx_excel_quant(
            file = excel_long,
            data_type = NULL,
            data_cells = c("LOD", "UniProt", "Assay", "NPX Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many occurrences of:"
        )

        # check that read_npx_excel_quant works for data_type = NPX
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_long,
            data_type = "NPX",
            data_cells = c("LOD", "UniProt", "Assay", "NPX Quantified"),
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains NULL
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

    ## Wide ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_excel_quant works for data_type = NULL
        expect_error(
          object = read_npx_excel_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "NPX_Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many occurrences of:"
        )

        # check that read_npx_excel_quant works for data_type = "NPX"
        expect_warning(
          object = df_npx <- read_npx_excel_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "NPX_Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many matches!"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains NULL
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

  }
)
