# Test olink_npx S3 class --------------------------------------------------

# Test data ----

df_tbl <- dplyr::tibble(
  SampleID = LETTERS[1L:4L],
  OlinkID = paste0("OID1234", seq(1L:4L)),
  UniProt = LETTERS[1L:4L],
  Assay = LETTERS[1L:4L],
  Panel = LETTERS[1L:4L],
  Panel_Lot_Nr = LETTERS[1L:4L],
  NPX = rnorm(4L),
  PlateID = rep("plate1", 4L),
  QC_Warning = rep("Pass", 4L)
)

check_log_result <- check_npx(df = df_tbl) |>
  suppressWarnings() |>
  suppressMessages()

# Test new_olink_npx ----

test_that(
  "new_olink_npx - works - creates an olink_npx from tibble and check_log",
  {
    result <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    expect_s3_class(object = result, class = "olink_npx")
    expect_s3_class(object = result, class = "tbl_df")
    expect_equal(object = nrow(result), expected = 4L)
    expect_equal(object = ncol(result), expected = 9L)
    expect_identical(
      object = attr(x = result, which = "check_log", exact = TRUE),
      expected = check_log_result
    )
  }
)

test_that(
  "new_olink_npx - error - non-tibble input",
  {
    expect_error(
      object = new_olink_npx(data = as.data.frame(df_tbl),
                             check_log = check_log_result),
      regexp = "must be a tibble"
    )
  }
)

test_that(
  "new_olink_npx - error - invalid check_log",
  {
    expect_error(
      object = new_olink_npx(data = df_tbl,
                             check_log = list(a = 1L)),
      regexp = "missing from"
    )

    expect_error(
      object = new_olink_npx(data = df_tbl,
                             check_log = "not_a_list"),
      regexp = "is not a list"
    )
  }
)

# Test olink_check_log ----

test_that(
  "olink_check_log - works - extracts check_log from olink_npx",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- olink_check_log(x = obj)

    expect_identical(object = result, expected = check_log_result)
  }
)

test_that(
  "olink_check_log - works - returns NULL for plain tibble",
  {
    result <- olink_check_log(x = df_tbl)
    expect_null(object = result)
  }
)

test_that(
  "olink_check_log - works - returns NULL for non-data objects",
  {
    expect_null(object = olink_check_log(x = "string"))
    expect_null(object = olink_check_log(x = 42L))
    expect_null(object = olink_check_log(x = list(a = 1L)))
  }
)

# Test Arrow metadata round-trip ----

test_that(
  "attach_check_log_arrow / olink_check_log - round-trip",
  {
    arrow_tbl <- arrow::as_arrow_table(x = df_tbl)

    result <- attach_check_log_arrow(data = arrow_tbl,
                                     check_log = check_log_result)

    # metadata should be set
    expect_true(object = "olink_check_log" %in% names(result$metadata))

    # round-trip extraction
    extracted <- olink_check_log(x = result)

    expect_identical(object = extracted, expected = check_log_result)
  }
)

test_that(
  "attach_check_log_arrow - error - non-ArrowObject input",
  {
    expect_error(
      object = attach_check_log_arrow(data = df_tbl,
                                      check_log = check_log_result),
      regexp = "must be an ArrowObject"
    )
  }
)

test_that(
  "olink_check_log - works - returns NULL for ArrowObject without metadata",
  {
    arrow_tbl <- arrow::as_arrow_table(x = df_tbl)
    result <- olink_check_log(x = arrow_tbl)
    expect_null(object = result)
  }
)

# Test validate_check_log ----

test_that(
  "validate_check_log - works - valid check_log",
  {
    expect_invisible(
      object = validate_check_log(check_log = check_log_result)
    )
  }
)

test_that(
  "validate_check_log - error - unnamed list",
  {
    expect_error(
      object = validate_check_log(check_log = list(1L, 2L, 3L)),
      regexp = "must be a named list"
    )
  }
)

test_that(
  "validate_check_log - error - missing elements",
  {
    expect_error(
      object = validate_check_log(
        check_log = list(col_names = list())
      ),
      regexp = "missing from"
    )
  }
)

# Test dplyr_reconstruct ----

test_that(
  "dplyr_reconstruct.olink_npx - works - preserves class through filter",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- dplyr::filter(obj, .data[["SampleID"]] == "A")

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
    expect_equal(object = nrow(result), expected = 1L)
  }
)

test_that(
  "dplyr_reconstruct.olink_npx - works - preserves class through mutate",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- dplyr::mutate(obj, new_col = 1L)

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
    expect_true(object = "new_col" %in% names(result))
  }
)

test_that(
  "dplyr_reconstruct.olink_npx - works - preserves class through select",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- dplyr::select(obj, "SampleID", "OlinkID")

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
  }
)

test_that(
  "dplyr_reconstruct.olink_npx - works - preserves class through arrange",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- dplyr::arrange(obj, dplyr::desc(.data[["SampleID"]]))

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
  }
)

# Test tbl_sum ----

test_that(
  "tbl_sum.olink_npx - works - shows check log status",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    header <- tbl_sum(obj)

    expect_true(object = "Check log" %in% names(header))
    expect_equal(object = header[["Check log"]],
                 expected = "attached")
  }
)

# Test base64 round-trip ----

test_that(
  "base64_encode / base64_decode - round-trip",
  {
    # Simple string
    raw_input <- charToRaw("Hello, World!")
    encoded <- base64_encode(raw_bytes = raw_input)
    decoded <- base64_decode(encoded_str = encoded)
    expect_identical(object = decoded, expected = raw_input)

    # Empty input
    raw_empty <- raw(0L)
    encoded_empty <- base64_encode(raw_bytes = raw_empty)
    decoded_empty <- base64_decode(encoded_str = encoded_empty)
    expect_identical(object = decoded_empty, expected = raw_empty)

    # Complex R object
    complex_obj <- list(a = 1L:10L, b = "test", c = list(d = TRUE))
    raw_complex <- serialize(object = complex_obj, connection = NULL)
    encoded_complex <- base64_encode(raw_bytes = raw_complex)
    decoded_complex <- base64_decode(encoded_str = encoded_complex)
    expect_identical(
      object = unserialize(connection = decoded_complex),
      expected = complex_obj
    )
  }
)

# Test serialize_check_log / deserialize_check_log ----

test_that(
  "serialize_check_log / deserialize_check_log - round-trip",
  {
    encoded <- serialize_check_log(check_log = check_log_result)
    expect_type(object = encoded, type = "character")
    expect_length(object = encoded, n = 1L)

    decoded <- deserialize_check_log(encoded_str = encoded)
    expect_identical(object = decoded, expected = check_log_result)
  }
)

# Test run_check_npx auto-extraction ----

test_that(
  "run_check_npx - works - auto-extracts check_log from olink_npx",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    # should NOT produce the "not provided" message
    expect_no_message(
      object = {
        result <- run_check_npx(df = obj, check_log = NULL)
      },
      message = "not provided"
    )

    expect_identical(object = result, expected = check_log_result)
  }
)

test_that(
  "run_check_npx - works - auto-extracts check_log from arrow with metadata",
  {
    arrow_tbl <- arrow::as_arrow_table(x = df_tbl)
    arrow_with_log <- attach_check_log_arrow(data = arrow_tbl,
                                             check_log = check_log_result)

    # should NOT produce the "not provided" message
    expect_no_message(
      object = {
        result <- run_check_npx(df = arrow_with_log, check_log = NULL)
      },
      message = "not provided"
    )

    expect_identical(object = result, expected = check_log_result)
  }
)

test_that(
  "run_check_npx - works - explicit check_log takes precedence",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    # passing check_log explicitly should use the explicit one
    result <- run_check_npx(df = obj, check_log = check_log_result)

    expect_identical(object = result, expected = check_log_result)
  }
)

# Test convert_read_npx_output preserves check_log ----

test_that(
  "convert_read_npx_output - works - preserves check_log on tibble",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- convert_read_npx_output(df = obj, out_df = "tibble")

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
  }
)

test_that(
  "convert_read_npx_output - works - preserves check_log to arrow",
  {
    obj <- new_olink_npx(data = df_tbl, check_log = check_log_result)

    result <- convert_read_npx_output(df = obj, out_df = "arrow")

    expect_true(
      object = inherits(x = result, what = "ArrowObject")
    )
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
  }
)

test_that(
  "convert_read_npx_output - works - preserves check_log from arrow to tibble",
  {
    arrow_tbl <- arrow::as_arrow_table(x = df_tbl)
    arrow_with_log <- attach_check_log_arrow(data = arrow_tbl,
                                             check_log = check_log_result)

    result <- convert_read_npx_output(df = arrow_with_log, out_df = "tibble")

    expect_s3_class(object = result, class = "olink_npx")
    expect_identical(
      object = olink_check_log(x = result),
      expected = check_log_result
    )
  }
)

# Test clean_npx returns olink_npx ----

test_that(
  "clean_npx - works - returns olink_npx object",
  {
    result <- suppressWarnings(suppressMessages(
      clean_npx(df = df_tbl, check_log = check_log_result)
    ))

    expect_s3_class(object = result, class = "olink_npx")
    expect_false(object = is.null(olink_check_log(x = result)))
  }
)

test_that(
  "clean_npx - works - returns ArrowObject with check_log when out_df = arrow",
  {
    result <- suppressWarnings(suppressMessages(
      clean_npx(df = df_tbl, check_log = check_log_result, out_df = "arrow")
    ))

    expect_true(
      object = inherits(x = result, what = "ArrowObject")
    )
    expect_false(object = is.null(olink_check_log(x = result)))
  }
)
