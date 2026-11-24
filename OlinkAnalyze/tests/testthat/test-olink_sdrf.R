# Tests for SDRF helpers in R/olink_sdrf.R

test_that(
  "generate_sdrf - works - from manifest with Target 96",
  {
    sdrf <- generate_sdrf(
      metadata = manifest,
      platform = "Target 96",
      panel_name = "Inflammation",
      organism = "homo sapiens",
      organism_part = "blood",
      file = NULL,
      validate = FALSE
    )

    expect_s3_class(sdrf, "tbl_df")
    expect_equal(nrow(sdrf), nrow(dplyr::distinct(manifest, SampleID)))
    expect_true(all(
      c(
        "source name",
        "characteristics[sample type]",
        "characteristics[organism]",
        "characteristics[organism part]",
        "characteristics[biological replicate]",
        "assay name",
        "technology type",
        "comment[technical replicate]",
        "comment[data file]",
        "comment[platform]"
      ) %in% colnames(sdrf)
    ))
    expect_equal(unique(sdrf[["characteristics[organism]"]]), "homo sapiens")
    expect_equal(unique(sdrf[["characteristics[organism part]"]]), "blood")
    expect_equal(unique(sdrf[["comment[platform]"]]), "Olink Target 96")
    expect_equal(unique(sdrf[["comment[panel name]"]]), "Inflammation")
    # leftover manifest columns become characteristics[*]
    expect_true(all(
      c(
        "characteristics[SubjectID]",
        "characteristics[Visit]",
        "characteristics[Site]"
      ) %in% colnames(sdrf)
    ))
    expect_false("characteristics[SampleID]" %in% colnames(sdrf))
    # characteristics before assay name (sdrf-pipelines column-order rule)
    expect_lt(
      match("characteristics[Visit]", colnames(sdrf)),
      match("assay name", colnames(sdrf))
    )
  }
)

test_that(
  "generate_sdrf - works - platform full name and Explore HT",
  {
    sdrf <- generate_sdrf(
      metadata = manifest,
      platform = "Olink Explore HT",
      panel_name = "Explore 1536",
      file = NULL,
      validate = FALSE
    )
    expect_equal(unique(sdrf[["comment[platform]"]]), "Olink Explore HT")
    expect_equal(unique(sdrf[["comment[panel name]"]]), "Explore 1536")
  }
)

test_that(
  "generate_sdrf - error - unknown platform",
  {
    expect_error(
      object = generate_sdrf(
        metadata = manifest,
        platform = "NotAPlatform",
        validate = FALSE
      ),
      regexp = "Unknown Olink"
    )
  }
)

test_that(
  "generate_sdrf - error - missing SampleID column",
  {
    expect_error(
      object = generate_sdrf(
        metadata = data.frame(x = 1L),
        validate = FALSE
      ),
      regexp = "SampleID"
    )
  }
)

test_that(
  "generate_sdrf - works - panel_name from metadata column",
  {
    meta <- manifest |>
      dplyr::mutate(Panel = "Cardiometabolic")
    sdrf <- generate_sdrf(
      metadata = meta,
      platform = "Target 96",
      panel_name = "Panel",
      file = NULL,
      validate = FALSE
    )
    expect_equal(unique(sdrf[["comment[panel name]"]]), "Cardiometabolic")
  }
)

test_that(
  "olink_sdrf - works - builds required columns from npx_data1",
  {
    meta <- dplyr::distinct(
      npx_data1,
      SampleID, Subject, Treatment, Site, Time, Project
    )
    sdrf <- olink_sdrf(
      npx = npx_data1,
      sample_metadata = meta,
      file = NULL,
      validate = FALSE,
      platform = "Olink Target 96"
    )

    expect_s3_class(sdrf, "tbl_df")
    expect_equal(nrow(sdrf), dplyr::n_distinct(npx_data1$SampleID))
    expect_true("source name" %in% colnames(sdrf))
    expect_true("comment[platform]" %in% colnames(sdrf))
    # CONTROL_SAMPLE_* → plate control via SampleID regex
    ctrl_rows <- grepl("CONTROL", sdrf[["source name"]])
    expect_true(any(ctrl_rows))
    expect_true(all(
      sdrf[["characteristics[sample type]"]][ctrl_rows] == "plate control"
    ))
    expect_true(all(
      sdrf[["characteristics[sample type]"]][!ctrl_rows] == "study sample"
    ))
  }
)

test_that(
  "olink_sdrf_validate - error - missing file",
  {
    expect_error(
      object = olink_sdrf_validate(
        sdrf_file = "does-not-exist.sdrf.tsv",
        error = TRUE
      ),
      regexp = "Unable to locate file"
    )
  }
)

test_that(
  "generate_sdrf - works - write and optional CLI validate",
  {
    withr::with_tempfile(
      new = "sdrf_file",
      fileext = ".sdrf.tsv",
      code = {
        sdrf <- generate_sdrf(
          metadata = manifest,
          platform = "Target 96",
          panel_name = "Inflammation",
          file = sdrf_file,
          validate = FALSE
        )
        expect_true(file.exists(sdrf_file))
        expect_equal(nrow(sdrf), 138L)

        skip_if_not(
          nzchar(Sys.which("parse_sdrf")),
          message = "parse_sdrf not on PATH"
        )

        res <- olink_sdrf_validate(
          sdrf_file = sdrf_file,
          template = "affinity-proteomics",
          skip_ontology = TRUE,
          error = FALSE
        )
        expect_true(res$ok)
        expect_equal(res$status, 0L)
      }
    )
  }
)

test_that(
  "find_parse_sdrf - works - soft miss returns NULL",
  {
    expect_null(
      find_parse_sdrf(
        parse_sdrf = "definitely-not-a-real-parse-sdrf-binary",
        error = FALSE
      )
    )
  }
)

test_that(
  "olink_sdrf_validate - warns - skips when parse_sdrf missing",
  {
    withr::with_tempfile(
      "sdrf_file",
      fileext = ".sdrf.tsv",
      code = {
        utils::write.table(
          data.frame(
            `source name` = "s1",
            `assay name` = "a1",
            check.names = FALSE
          ),
          file = sdrf_file,
          sep = "\t",
          quote = FALSE,
          row.names = FALSE
        )

        expect_warning(
          res <- olink_sdrf_validate(
            sdrf_file = sdrf_file,
            parse_sdrf = "definitely-not-a-real-parse-sdrf-binary",
            error = FALSE
          ),
          regexp = "validation skipped"
        )
        expect_false(res$ok)
        expect_true(res$skipped)
        expect_true(is.na(res$status))
      }
    )
  }
)
