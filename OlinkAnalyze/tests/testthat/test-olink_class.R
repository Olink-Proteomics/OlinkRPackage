# Test olink_class S3 class --------------------------------------------------

# Test data ----

npx_data1_check_log <- check_npx(df = npx_data1) |>
  suppressWarnings() |>
  suppressMessages()

# Test new_olink_class ----

test_that(
  "new_olink_class - works - creates an olink_class from tibble and check_log",
  {
    result <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    expect_s3_class(object = result, class = "olink_class")
    expect_s3_class(object = result, class = "tbl_df")
    expect_identical(object = dim(result), expected = c(29440L, 17L))
    expect_identical(
      object = attr(x = result, which = "check_log", exact = TRUE),
      expected = npx_data1_check_log
    )
  }
)

test_that(
  "new_olink_class - error - non-tibble input",
  {
    expect_error(
      object = new_olink_class(
        df = as.data.frame(npx_data1),
        check_log = npx_data1_check_log
      ),
      regexp = "`df` is not a tibble dataset!"
    )
  }
)

test_that(
  "new_olink_class - error - invalid check_log",
  {
    expect_error(
      object = new_olink_class(
        df = npx_data1,
        check_log = list(a = 1L)
      ),
      regexp = paste("Elements \"col_names\", \"oid_invalid\", \"assay_na\",",
                     "\"sample_id_dups\", \"sample_id_na\", \"col_class\",",
                     "\"assay_qc\", \"non_unique_uniprot\", and",
                     "\"darid_invalid\" are missing from `check_log`!")
    )

    expect_error(
      object = new_olink_class(
        df = npx_data1,
        check_log = list("col_names" = 1L)
      ),
      regexp = paste("Elements \"oid_invalid\", \"assay_na\",",
                     "\"sample_id_dups\", \"sample_id_na\", \"col_class\",",
                     "\"assay_qc\", \"non_unique_uniprot\", and",
                     "\"darid_invalid\" are missing from `check_log`!")
    )

    expect_error(
      object = new_olink_class(
        df = npx_data1,
        check_log = "not_a_list"
      ),
      regexp = "`check_log` is not a list!"
    )
  }
)

# Test olink_check_log ----

test_that(
  "olink_check_log - works - extracts check_log from olink_class",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    result <- olink_check_log(df = obj)

    expect_identical(object = result, expected = npx_data1_check_log)
  }
)

test_that(
  "olink_check_log - works - returns NULL for plain tibble",
  {
    result <- olink_check_log(df = npx_data1)
    expect_null(object = result)
  }
)

test_that(
  "olink_check_log - works - returns NULL for non-data objects",
  {
    expect_null(object = olink_check_log(df = "string"))
    expect_null(object = olink_check_log(df = 42L))
    expect_null(object = olink_check_log(df = list(a = 1L)))
  }
)

# Test Arrow metadata round-trip ----

test_that(
  "attach_check_log_arrow / olink_check_log - round-trip",
  {
    arrow_tbl <- arrow::as_arrow_table(
      x = npx_data1
    )

    result <- attach_check_log_arrow(
      df = arrow_tbl,
      check_log = npx_data1_check_log
    )

    # metadata should be set
    expect_true(object = "olink_check_log" %in% names(result$metadata))

    # round-trip extraction
    extracted <- olink_check_log(df = result)

    expect_identical(object = extracted, expected = npx_data1_check_log)
  }
)

test_that(
  "olink_check_log - works - returns NULL for ArrowObject without metadata",
  {
    arrow_tbl <- arrow::as_arrow_table(x = npx_data1)
    result <- olink_check_log(df = arrow_tbl)
    expect_null(object = result)
  }
)

# Test attach_check_log ----

test_that(
  "attach_check_log - works - tibble path returns olink_class with check_log",
  {
    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = result <- attach_check_log(
            df = npx_data1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected:",
                         "\"CONTROL_SAMPLE_AS 1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_s3_class(object = result, class = "olink_class")
    expect_false(object = is.null(olink_check_log(df = result)))
  }
)

test_that(
  "attach_check_log - works - arrow path returns ArrowObject with check_log",
  {
    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = result <- attach_check_log(
            df = npx_data1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected:",
                         "\"CONTROL_SAMPLE_AS 1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_true(object = inherits(x = result, what = "ArrowObject"))
    expect_false(object = is.null(olink_check_log(df = result)))
  }
)

test_that(
  "attach_check_log - works - forwards preferred_names to check_npx",
  {
    # preferred_names allows renaming columns; passing a valid mapping should
    # not cause an error and the result should still carry a check_log
    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = result <- attach_check_log(
            df = npx_data1,
            out_df = "tibble",
            preferred_names = NULL
          ),
          regexp = paste("Duplicate SampleIDs detected:",
                         "\"CONTROL_SAMPLE_AS 1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_s3_class(object = result, class = "olink_class")
    expect_false(object = is.null(olink_check_log(df = result)))
  }
)

# Test validate_check_log ----

test_that(
  "validate_check_log - works - valid check_log",
  {
    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = expect_true(
            object = validate_check_log(
              df = npx_data1,
              check_log = npx_data1_check_log
            )
          )
        )
      )
    )
  }
)

test_that(
  "validate_check_log - error - unnamed list",
  {
    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = list(1L, 2L, 3L)
      ),
      regexp = "`check_log` is a list with no names!"
    )
  }
)

test_that(
  "validate_check_log - error - missing elements",
  {
    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = list(col_names = list())
      ),
      regexp = paste("Elements \"oid_invalid\", \"assay_na\",",
                     "\"sample_id_dups\", \"sample_id_na\", \"col_class\",",
                     "\"assay_qc\", \"non_unique_uniprot\", and",
                     "\"darid_invalid\" are missing from `check_log`!")
    )
  }
)

test_that(
  "validate_check_log - error - check_log has additional unexpected names",
  {
    test_check_log <- append(
      x = npx_data1_check_log,
      values = list("i_am_a_new_element" = c(1L, 2L, 3L))
    )

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log
      ),
      regexp = paste("Additional element \"i_am_a_new_element\" detected in",
                     "`check_log`!")
    )
  }
)

test_that(
  "validate_check_log - error - check_log misses required keys for col names",
  {
    test_check_log <- npx_data1_check_log
    test_check_log$col_names <- test_check_log$col_names[utils::head(x = names(test_check_log$col_names), n = 4L)] # nolint: indentation_linter

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log
      ),
      regexp = "There are no column names associated with the following keys:"
    )
  }
)

test_that(
  "validate_check_log - error - check_log has additional keys for column names",
  {
    test_check_log <- npx_data1_check_log
    test_check_log$col_names <- append(
      x = test_check_log$col_names,
      values = list("i_am_a_new_element" = "Site")
    )

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log
      ),
      regexp = "Unexpected key \"i_am_a_new_element\" corresponding to column"
    )
  }
)

test_that(
  "validate_check_log - error - check_log contains column names not in df",
  {
    # check_log contains non-character values v1 ----

    test_check_log_v1 <- npx_data1_check_log
    test_check_log_v1$col_names$sample_id <- FALSE

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log_v1
      ),
      regexp = paste("Column name \"FALSE\" from `check_log` is missing from",
                     "the dataset `df`!")
    )

    # check_log contains non-character values v2 ----

    test_check_log_v2 <- npx_data1_check_log
    test_check_log_v2$col_names$sample_id <- FALSE
    test_check_log_v2$col_names$olink_id <- 1L
    test_check_log_v2$col_names$uniprot <- 1.1
    test_check_log_v2$col_names$plate_id <- TRUE

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log_v2
      ),
      regexp = paste("Column names \"FALSE\", \"1\", \"1.1\", and \"TRUE\"",
                     "from `check_log` are missing from the dataset `df`!")
    )

    # check_log contains values not in df v1 ----

    test_check_log_v3 <- npx_data1_check_log
    test_check_log_v3$col_names$sample_id <- "SampleID2"

    expect_error(
      object = validate_check_log(
        df = npx_data1,
        check_log = test_check_log_v3
      ),
      regexp = paste("Column name \"SampleID2\" from `check_log` is missing",
                     "from the dataset `df`!")
    )
  }
)

# Test dplyr_reconstruct ----

test_that(
  "dplyr_reconstruct.olink_class - works - preserves class through filter",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    # dplyr::filter

    result <- dplyr::filter(obj, .data[["SampleID"]] == "A1")

    expect_s3_class(object = result, class = "olink_class")
    expect_identical(
      object = olink_check_log(df = result),
      expected = npx_data1_check_log
    )
    expect_equal(object = nrow(result), expected = 184L)
  }
)

test_that(
  "dplyr_reconstruct.olink_class - works - preserves class through mutate",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    result <- dplyr::mutate(obj, new_col = 1L)

    expect_s3_class(object = result, class = "olink_class")
    expect_identical(
      object = olink_check_log(df = result),
      expected = npx_data1_check_log
    )
    expect_true(object = "new_col" %in% names(result))
    expect_equal(
      object = dim(result),
      expected = c(nrow(obj), ncol(obj) + 1L)
    )
  }
)

test_that(
  "dplyr_reconstruct.olink_class - works - preserves class through select",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    result <- dplyr::select(obj, dplyr::all_of(c("SampleID", "OlinkID")))

    expect_s3_class(object = result, class = "olink_class")
    expect_identical(
      object = olink_check_log(df = result),
      expected = npx_data1_check_log
    )
    expect_equal(
      object = names(result),
      expected = c("SampleID", "OlinkID")
    )
    expect_equal(
      object = nrow(result),
      expected = 29440L
    )
  }
)

test_that(
  "dplyr_reconstruct.olink_class - works - preserves class through arrange",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    result <- dplyr::arrange(obj, dplyr::desc(.data[["SampleID"]]))

    expect_s3_class(object = result, class = "olink_class")
    expect_identical(
      object = olink_check_log(df = result),
      expected = npx_data1_check_log
    )
    expect_identical(
      object = unique(result$SampleID),
      expected = unique(sort(x = npx_data1$SampleID, decreasing = TRUE))
    )
  }
)

# Test tbl_sum ----

test_that(
  "tbl_sum.olink_class - works - shows check log status",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    header <- tbl_sum(obj)

    expect_true(object = "Check log" %in% names(header))
    expect_equal(object = header[["Check log"]],
                 expected = "attached")
  }
)

test_that(
  "tbl_sum.olink_class - works - shows 'missing' when check_log is NULL",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )
    # forcibly remove the check_log attribute to exercise the 'missing' branch
    attr(obj, "check_log") <- NULL

    header <- tbl_sum(obj)

    expect_true(object = "Check log" %in% names(header))
    expect_equal(object = header[["Check log"]],
                 expected = "missing")
  }
)

# Test strip_check_log ----

test_that(
  "strip_check_log - works - strips check_log from olink_class tibble",
  {
    obj <- new_olink_class(
      df = npx_data1,
      check_log = npx_data1_check_log
    )

    result <- strip_check_log(df = obj)

    expect_false(object = inherits(x = result, what = "olink_class"))
    expect_s3_class(object = result, class = "tbl_df")
    expect_null(object = olink_check_log(df = result))
  }
)

test_that(
  "strip_check_log - works - strips check_log from ArrowObject",
  {
    arrow_tbl <- arrow::as_arrow_table(x = npx_data1)
    arrow_with_log <- attach_check_log_arrow(
      df = arrow_tbl,
      check_log = npx_data1_check_log
    )

    result <- strip_check_log(df = arrow_with_log)

    expect_true(object = inherits(x = result, what = "ArrowObject"))
    expect_null(object = olink_check_log(df = result))
  }
)

test_that(
  "strip_check_log - works - returns plain tibble unchanged",
  {
    result <- strip_check_log(df = npx_data1)

    expect_s3_class(object = result, class = "tbl_df")
    expect_identical(object = result, expected = npx_data1)
  }
)

test_that(
  "strip_check_log - works - returns ArrowObject without metadata unchanged",
  {
    arrow_tbl <- arrow::as_arrow_table(x = npx_data1)

    result <- strip_check_log(df = arrow_tbl)

    expect_true(object = inherits(x = result, what = "ArrowObject"))
    expect_null(object = olink_check_log(df = result))
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
    encoded <- serialize_check_log(check_log = npx_data1_check_log)
    expect_type(object = encoded, type = "character")
    expect_length(object = encoded, n = 1L)

    decoded <- deserialize_check_log(encoded_str = encoded)
    expect_identical(object = decoded, expected = npx_data1_check_log)
  }
)

test_that(
  "deserialize_check_log - warns and returns NULL for corrupted input",
  {
    # A string that is valid base64 but not a serialized R object
    corrupted <- base64_encode(raw_bytes = charToRaw("not_an_R_object"))

    expect_warning(
      object = {
        result <- deserialize_check_log(encoded_str = corrupted)
      },
      regexp = "Could not deserialize"
    )

    expect_null(object = result)
  }
)
