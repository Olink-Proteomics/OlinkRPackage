# Test check_npx ----

test_that(
  "check_npx - error - df is not tibble or arrow data frame",
  {
    df <- data.frame(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expect_error(
      object = check_npx(df),
      regexp = "`df` is not a tibble or an ArrowObject dataset!"
    )
  }
)

test_that(
  "check_npx - works - minimum set of columns, results as expected",
  {
    df <- dplyr::tibble(
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

    expected_result <- list(
      col_names = list(sample_id = "SampleID",
                       olink_id = "OlinkID",
                       uniprot = "UniProt",
                       assay = "Assay",
                       panel = "Panel",
                       plate_id = "PlateID",
                       panel_version = "Panel_Lot_Nr",
                       quant = "NPX",
                       qc_warning = "QC_Warning"),
      oid_invalid = character(0L),
      assay_na = character(0L),
      sample_id_dups = character(0L),
      sample_id_na = character(0L),
      col_class = dplyr::tibble(
        "col_name" = character(0L),
        "col_class" = character(0L),
        "col_key" = character(0L),
        "expected_col_class" = character(0L)
      ),
      assay_qc = character(0L),
      non_unique_uniprot = character(0L)
    )

    expect_equal(
      object = check_npx(df = df,
                         preferred_names = NULL),
      expected = expected_result
    )
  }
)

test_that(
  "check_npx - works - full set of columns, results as expected",
  {
    df <- dplyr::tibble(
      SampleID = LETTERS[1L:4L],
      SampleType = LETTERS[1L:4L],
      AssayType = LETTERS[1L:4L],
      OlinkID = c(paste0("OID1234", seq(1L:3L)), "OID12345"),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Block = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      LOD = rnorm(4L),
      NPX = rnorm(4L),
      Count = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      AssayQC = c(rep("Pass", 3L), "Warning"),
      Normalization = LETTERS[1L:4L]
    )

    expected_result <- list(
      col_names = list(sample_id = "SampleID",
                       sample_type = "SampleType",
                       assay_type = "AssayType",
                       olink_id = "OlinkID",
                       uniprot = "UniProt",
                       assay = "Assay",
                       panel = "Panel",
                       block = "Block",
                       plate_id = "PlateID",
                       panel_version = "Panel_Lot_Nr",
                       lod = "LOD",
                       quant = "NPX",
                       count = "Count",
                       qc_warning = "QC_Warning",
                       assay_warn = "AssayQC",
                       normalization = "Normalization"),
      oid_invalid = character(0L),
      assay_na = character(0L),
      sample_id_dups = character(0L),
      sample_id_na = character(0L),
      col_class = dplyr::tibble(
        "col_name" = character(0L),
        "col_class" = character(0L),
        "col_key" = character(0L),
        "expected_col_class" = character(0L)
      ),
      assay_qc = c("OID12345"),
      non_unique_uniprot = character(0L)
    )

    expect_message(
      object = expect_equal(
        object = check_npx(df = df,
                           preferred_names = NULL),
        expected = expected_result
      ),
      regexp = "QC warnings in column `AssayQC` of the dataset: \"OID12345\"."
    )
  }
)

test_that(
  "check_npx - warnings - invalid OlinkID, duplicate SampleID and NPX non-num",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "A", "C", "D"),
      OlinkID = rep("OID123456", 4L),
      UniProt = rep(LETTERS[1L], 4L),
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = as.character(rnorm(4L)),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L)
    )

    expected_result <- list(
      col_names = list(sample_id = "SampleID",
                       olink_id = "OlinkID",
                       uniprot = "UniProt",
                       assay = "Assay",
                       panel = "Panel",
                       plate_id = "PlateID",
                       panel_version = "Panel_Lot_Nr",
                       quant = "NPX",
                       qc_warning = "QC_Warning"),
      oid_invalid = c("OID123456"),
      assay_na = character(0L),
      sample_id_dups = c("A"),
      sample_id_na = character(0L),
      col_class = dplyr::tibble(
        "col_name" = c("NPX"),
        "col_class" = c("character"),
        "col_key" = c("quant"),
        "expected_col_class" = c("numeric")
      ),
      assay_qc = character(0L),
      non_unique_uniprot = character(0L)
    )

    expect_warning(
      object = expect_warning(
        object = expect_warning(
          object = expect_equal(
            object = check_npx(df = df,
                               preferred_names = NULL),
            expected = expected_result
          ),
          regexp = "Unrecognized OlinkID detected"
        ),
        regexp = "Duplicate SampleID detected"
      ),
      regexp = "\"NPX\": Expected \"numeric\". Detected \"character\"."
    )
  }
)

test_that(
  "check_npx - warnings - OlinkIDs mapped with >1 Uniprots",
  {
    df <- dplyr::tibble(
      SampleID = c("Sample1", "Sample1", "Sample2"),
      OlinkID = c("OID00001", "OID00002", "OID00002"),
      UniProt = c("Uniprot_1", "Uniprot_2", "Uniprot_3"),
      SampleType = rep(x = "SAMPLE", times = 3L),
      AssayType = rep(x = "assay", times = 3L),
      SampleQC = rep(x = "PASS", times = 3L),
      AssayQC = rep(x = "PASS", times = 3L),
      NPX = rnorm(n = 3L),
      PlateID = rep(x = "plate1", times = 3L),
      Assay = rep(x = "assay_a", times = 3L),
      Panel = rep(x = "panel_a", times = 3L),
      PanelVersion = rep(x = "panel_version_a", times = 3L),
      LOD = rnorm(n = 3L),
      ExtNPX = rnorm(n = 3L),
      Count = rnorm(n = 3L),
      Normalization = rep(x = "Intensity", times = 3L)
    )

    expected_result <- list(
      col_names = list(
        sample_id = "SampleID",
        sample_type = "SampleType",
        assay_type = "AssayType",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "PanelVersion",
        lod = "LOD",
        quant = "NPX",
        ext_npx = "ExtNPX",
        count = "Count",
        qc_warning = "SampleQC",
        assay_warn = "AssayQC",
        normalization = "Normalization"
      ),
      oid_invalid = character(0L),
      assay_na = character(0L),
      sample_id_dups = character(0L),
      sample_id_na = character(0L),
      col_class = dplyr::tibble(
        "col_name" = character(0L),
        "col_class" = character(0L),
        "col_key" = character(0L),
        "expected_col_class" = character(0L)
      ),
      assay_qc = character(0L),
      non_unique_uniprot = c("OID00002")

    )

    expect_warning(
      object = expect_equal(
        object = check_npx(df = df),
        expected = expected_result
      ),

      regexp = "Multiple UniProt IDs detected for assay: \"OID00002\"."
    )

  }
)

# Test check_npx_col_names ----

test_that(
  "check_npx_col_names - works - all columns are detected",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L)
    )

    expected_result <- list(
      sample_id = "SampleID",
      olink_id = "OlinkID",
      uniprot = "UniProt",
      assay = "Assay",
      panel = "Panel",
      plate_id = "PlateID",
      panel_version = "Panel_Lot_Nr",
      quant = "NPX",
      qc_warning = "QC_Warning"
    )

    expect_equal(
      object = check_npx_col_names(df = df,
                                   preferred_names = NULL),
      expected = expected_result
    )
  }
)

test_that(
  "check_npx_col_names - works - nullable columns are ok",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      SampleType = rep("SAMPLE", 4L),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # missing SampleType ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of("SampleType")
        ) |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "Panel_Lot_Nr",
        lod = "LOD",
        quant = "NPX",
        qc_warning = "QC_Warning"
      )
    )

    # missing LOD ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of("LOD")
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        sample_type = "SampleType",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "Panel_Lot_Nr",
        quant = "NPX",
        qc_warning = "QC_Warning"
      )
    )

    # missing LOD and SampleType ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of(c("SampleType", "LOD"))
        ) |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "Panel_Lot_Nr",
        quant = "NPX",
        qc_warning = "QC_Warning"
      )
    )
  }
)

test_that(
  "check_npx_col_names - works - preferred_names",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      SampleType = rep("SAMPLE", 4L),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L)
    )

    # one column name ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName")
        ),
      expected = list(
        sample_id = "IamSampleName",
        sample_type = "SampleType",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "Panel_Lot_Nr",
        quant = "NPX",
        qc_warning = "QC_Warning"
      )
    )

    # multiple column names ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamSampleType" = "SampleType",
          "IamPlateIdentifier" = "PlateID",
          "IamOlinkIdentifier" = "OlinkID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "sample_type" = "IamSampleType",
                              "plate_id" = "IamPlateIdentifier",
                              "olink_id" = "IamOlinkIdentifier")
        ),
      expected = list(
        sample_id = "IamSampleName",
        sample_type = "IamSampleType",
        olink_id = "IamOlinkIdentifier",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "IamPlateIdentifier",
        panel_version = "Panel_Lot_Nr",
        quant = "NPX",
        qc_warning = "QC_Warning"
      )
    )

    # break ties (multiple matches) ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamSampleType" = "SampleType"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          PlateLOD = rnorm(4L),
          MaxLOD = rnorm(4L),
          Quantified_value = rnorm(4L)
        ) |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "sample_type" = "IamSampleType",
                              "lod" = "PlateLOD",
                              "quant" = "Quantified_value")
        ),
      expected = list(
        sample_id = "IamSampleName",
        sample_type = "IamSampleType",
        olink_id = "OlinkID",
        uniprot = "UniProt",
        assay = "Assay",
        panel = "Panel",
        plate_id = "PlateID",
        panel_version = "Panel_Lot_Nr",
        lod = "PlateLOD",
        quant = "Quantified_value",
        qc_warning = "QC_Warning"
      )
    )
  }
)

test_that(
  "check_npx_col_names - error - preferred_names val not in data frame colname",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      SampleType = rep("SAMPLE", 4L),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one non existing column column name ----

    expect_error(
      object = df |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName")
        ),
      regexp = "Value \"IamSampleName\" from `preferred_names` corresponding to"
    )

    # multiple non existing column column names ----

    expect_error(
      object = df |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "lod" = "PlateLOD",
                              "sample_type" = "IamSampleType")
        ),
      regexp = "Values \"IamSampleName\", \"IamSampleType\", and \"PlateLOD\""
    )
  }
)

test_that(
  "check_npx_col_names - inform - resolve ties with multiple matches",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # two matches ----

    expect_message(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          NPX = rnorm(4L),
          Quantified_value = rnorm(4L)
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = paste("\"NPX\" was selected. Options were \"NPX\" or",
                     "\"Quantified_value\".")
    )

    # 3 matches ----

    expect_message(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          Ct = rnorm(4L),
          Quantified_value = rnorm(4L),
          NPX = rnorm(4L)
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = paste("\"NPX\" was selected. Options were \"NPX\",",
                     "\"Quantified_value\", or \"Ct\".")
    )
  }
)

test_that(
  "check_npx_col_names - error - ties (multiple matches)",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one column with multiple matches ----

    expect_error(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          plate_id = LETTERS[1L:4L]
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There is more than one column names in `df` associated with the"
    )

    # mutiple columns with multiple matches ----

    expect_error(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          plate_id = LETTERS[1L:4L],
          assay = LETTERS[1L:4L]
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There is more than one column names in `df` associated with the"
    )
  }
)

test_that(
  "check_npx_col_names - error - no match for non-nullable columns",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      SampleType = rep("SAMPLE", 4L),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L)
    )

    # one column with no matches ----

    expect_error(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There is no column name associated with the following key"
    )

    # mutiple columns with no matches ----

    expect_error(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamPlateID" = "PlateID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There are no column names associated with the following keys"
    )
  }
)

# Test check_npx_update_col_names ----

# working cases are covered by tests on check_npx_col_names
# here we will check only the errors

test_that(
  "check_npx_update_col_names - error - no match in col_keys",
  {
    # one name not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id_wrong" = "SampleID")
      ),
      regexp = "Unexpected name in"
    )

    # multiple names not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id_wrong" = "SampleID",
                            "wrong_sample_type" = "SampleType",
                            "lod2" = "LOD")
      ),
      regexp = "Unexpected names in"
    )
  }
)

test_that(
  "check_npx_update_col_names - error - duplicated names in array",
  {
    # one name not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id" = "SampleID",
                            "sample_id" = "SampleID2")
      ),
      regexp = "Duplicated name in"
    )

    # multiple names not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id" = "SampleID",
                            "sample_id" = "SampleID2",
                            "sample_type" = "Sample_Type",
                            "lod" = "LOD1",
                            "lod" = "LOD2")
      ),
      regexp = "Duplicated names in"
    )
  }
)

# Test check_npx_olinkid ----

test_that(
  "check_npx_olinkid - warning - returns invalid Olink IDs",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D", "E"),
      OlinkID = c("OID12345",
                  "OID123456",
                  "OID1234",
                  "12345",
                  "NA"),
      UniProt = LETTERS[1L:5L],
      Assay = LETTERS[1L:5L],
      Panel = LETTERS[1L:5L],
      Panel_Lot_Nr = LETTERS[1L:5L],
      NPX = rnorm(5L),
      PlateID = rep("plate1", 5L),
      QC_Warning = rep("Pass", 5L)
    )

    expect_no_condition(
      object = col_names <- check_npx_col_names(df = df,
                                                preferred_names = NULL)
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_olinkid(df = df,
                                   col_names = col_names),
        expected = c("OID123456",
                     "OID1234",
                     "12345",
                     "NA")
      ),
      regexp = "Unrecognized OlinkIDs detected: \"OID123456\", \"OID1234\","
    )
  }
)

test_that(
  "check_npx_olinkid - works - all OlinkID are valid",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L)
    )

    expect_no_condition(
      object = col_names <- check_npx_col_names(df = df,
                                                preferred_names = NULL)
    )

    expect_equal(
      object = check_npx_olinkid(df = df,
                                 col_names = col_names),
      expected = character(0L)
    )
  }
)

# Test check_npx_all_na_assays ----

test_that(
  "check_npx_all_na_assays - warning - all-NA assay captured",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              NA_real_,
              1.2,
              1.3)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_assays(
          df = df,
          col_names = col_names
        ),
        expected = "OID12345"
      ),
      regexp = "\"OID12345\" has \"NPX\" = NA for all samples."
    )
  }
)

test_that(
  "check_npx_all_na_assays - works - no assay has all NAs",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(1.1,
              1.2,
              1.3,
              NA_real_)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_equal(
      object = check_npx_all_na_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

test_that(
  "check_npx_all_na_assays - warning - arrow - all-NA assay captured",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              NA_real_,
              1.2,
              1.3)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_assays(
          df = df,
          col_names = col_names
        ),
        expected = "OID12345"
      ),
      regexp = "\"OID12345\" has \"NPX\" = NA for all samples."
    )
  }
)

test_that(
  "check_npx_all_na_assays - works - arrow - no assay has all NAs",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = rnorm(4L)
    )

    col_names <- list(quant = "NPX",
                      olink_id = "OlinkID")

    expect_equal(
      object = check_npx_all_na_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

# Test check_npx_duplicate_sample_ids ----

test_that(
  "check_npx_duplicate_sample_ids - warning - duplicate SampleID",
  {
    df <- arrow::arrow_table(SampleID = c("A", "B", "A", "C"),
                             OlinkID = rep("OID12345", 4L),
                             NPX = rnorm(4L))

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_duplicate_sample_ids(df = df,
                                                col_names = col_names),
        expected = "A"
      ),
      regexp = "Duplicate SampleID detected: \"A\""
    )
  }
)

test_that(
  "check_npx_duplicate_sample_ids - warning - mutiple duplicate sample IDs",
  {
    df <- arrow::arrow_table(SampleID = c("A", "A", "B", "B", "C"),
                             OlinkID = c(rep("OID12345", 2L),
                                         rep("OID12346", 3L)),
                             NPX = rnorm(5L))

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_duplicate_sample_ids(df = df,
                                                col_names = col_names),
        expected = c("A", "B")
      ),
      regexp = "Duplicate SampleIDs detected: \"A\" and \"B\""
    )
  }
)

test_that(
  "check_npx_duplicate_sample_ids - works - no duplicates",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c(rep("OID12345", 2L),
                  rep("OID12346", 2L)),
      NPX = rnorm(4L)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_equal(
      object = check_npx_duplicate_sample_ids(df = df,
                                              col_names = col_names),
      expected = character(0L)
    )
  }
)

# Test check_npx_all_na_sample ----

test_that(
  "check_npx_all_na_sample - warning - all-NA assays captured",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              1.2,
              NA_real_,
              1.3)
    )

    col_names <-  list(quant = "NPX",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_sample(
          df = df,
          col_names = col_names
        ),
        expected = "A"
      ),
      regexp = "\"A\" has \"NPX\" = NA for all assays."
    )
  }
)

test_that(
  "check_npx_all_na_sample - works - no sample has all NAs",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(1.1,
              1.2,
              1.3,
              NA_real_)
    )

    col_names <-  list(quant = "NPX",
                       sample_id = "SampleID")

    expect_equal(
      object = check_npx_all_na_sample(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

test_that(
  "check_npx_all_na_sample - warning - arrow - all-NA assay captured",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "A", "B", "C"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              1.2,
              NA_real_,
              NA_real_,
              1.3,
              NA_real_)
    )

    col_names <-  list(quant = "NPX",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_sample(
          df = df,
          col_names = col_names
        ),
        expected = c("A", "C")
      ),
      regexp = "\"A\" and \"C\" have \"NPX\" = NA for all assays."
    )
  }
)

test_that(
  "check_npx_all_na_sample - works - arrow - no assay has all NAs",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = rnorm(4L)
    )

    col_names <- list(quant = "NPX",
                      sample_id = "SampleID")

    expect_equal(
      object = check_npx_all_na_sample(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

# Test check_npx_col_class ----

test_that(
  "check_npx_col_class - warning - NPX non-numeric",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = as.character(rnorm(4L))
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID"
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_col_class(df = df,
                                     col_names = col_names),
        expected = dplyr::tibble(
          "col_name" = c("NPX"),
          "col_class" = c("character"),
          "col_key" = c("quant"),
          "expected_col_class" = c("numeric")
        )
      ),
      regexp = "\"NPX\": Expected \"numeric\". Detected \"character\"."
    )
  }
)

test_that(
  "check_npx_col_class - warning - NPX, LOD and PlateLOD non-numeric",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = as.character(rnorm(4L)),
      LOD = as.character(rnorm(4L))
    ) |>
      dplyr::mutate(
        PlateLOD = .data[["LOD"]]
      )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID",
      lod = c("LOD", "PlateLOD")
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_col_class(df = df,
                                     col_names = col_names),
        expected = dplyr::tibble(
          "col_name" = c("NPX", "LOD", "PlateLOD"),
          "col_class" = c("character", "character", "character"),
          "col_key" = c("quant", "lod", "lod"),
          "expected_col_class" = c("numeric", "numeric", "numeric")
        )
      ),
      regexp = "\"PlateLOD\": Expected \"numeric\". Detected \"character\"."
    )
  }
)

# Test check_npx_qcwarn_assays ----

test_that(
  "check_npx_qcwarn_assays - works - no AssayQC or Assay_Warning columns",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = as.character(rnorm(4L))
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID"
    )

    expect_equal(
      object = check_npx_qcwarn_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

test_that(
  "check_npx_qcwarn_assays - works - no assay with warning",
  {
    # AssayQC all pass v1 ----

    df <- arrow::arrow_table(
      SampleID = rep(x = c("A", "B", "C", "D"), times = 4L),
      OlinkID = rep(x = c("OID12345", "OID12346", "OID12347", "OID12348"),
                    each = 4L),
      NPX = as.character(x = rnorm(n = 16L)),
      AssayQC = rep(x = "Pass", times = 16L)
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID",
      assay_warn = "AssayQC"
    )

    expect_equal(
      object = check_npx_qcwarn_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )

    # AssayQC all pass v2 ----

    df <- arrow::arrow_table(
      SampleID = rep(x = c("A", "B", "C", "D"), times = 4L),
      OlinkID = rep(x = c("OID12345", "OID12346", "OID12347", "OID12348"),
                    each = 4L),
      NPX = as.character(x = rnorm(n = 16L)),
      Assay_Warning = rep(x = "PASS", times = 16L)
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID",
      assay_warn = "Assay_Warning"
    )

    expect_equal(
      object = check_npx_qcwarn_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

test_that(
  "check_npx_qcwarn_assays - works - assays with warnings",
  {
    # AssayQC 2 assays with warn ----

    df <- arrow::arrow_table(
      SampleID = rep(x = c("A", "B", "C", "D"), times = 4L),
      OlinkID = rep(x = c("OID12345", "OID12346", "OID12347", "OID12348"),
                    each = 4L),
      NPX = as.character(x = rnorm(n = 16L)),
      AssayQC = c(rep(x = c("PASS", rep(x = "WARN", times = 3L)), times = 2L),
                  rep(x = "PASS", times = 8L))
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID",
      assay_warn = "AssayQC"
    )

    expect_message(
      object = expect_equal(
        object = check_npx_qcwarn_assays(df = df,
                                         col_names = col_names),
        expected = c("OID12345", "OID12346")
      ),
      regexp = "column `AssayQC` of the dataset: \"OID12345\" and \"OID12346\"."
    )


    # AssayQC 4 assays with warn ----

    df <- arrow::arrow_table(
      SampleID = rep(x = c("A", "B", "C", "D"), times = 4L),
      OlinkID = rep(x = c("OID12345", "OID12346", "OID12347", "OID12348"),
                    each = 4L),
      NPX = as.character(x = rnorm(n = 16L)),
      Assay_Warning = rep(x = c("PASS", rep(x = "WARN", times = 3L)),
                          times = 4L)
    )

    col_names <-  list(
      quant = "NPX",
      olink_id = "OlinkID",
      sample_id = "SampleID",
      assay_warn = "Assay_Warning"
    )

    expect_message(
      object = expect_equal(
        object = check_npx_qcwarn_assays(df = df,
                                         col_names = col_names),
        expected = c("OID12345", "OID12346", "OID12347", "OID12348")
      ),
      regexp = "\"OID12345\", \"OID12346\", \"OID12347\", and \"OID12348\"."
    )
  }
)

# Test check_npx_nonunique_uniprot ----

test_that(
  "check_npx_nonunique_uniprot - works - no OlinkID mapped with >1 Uniprot IDs",
  {

    # test tibble ----
    df <- dplyr::tibble(
      SampleID = c("Sample1", "Sample1", "Sample1"),
      OlinkID = c("OID00001", "OID00002", "OID00003"),
      UniProt = c("Uniprot_1", "Uniprot_2", "Uniprot_3")
    )

    col_names <- list(
      sample_id = "SampleID",
      olink_id = "OlinkID",
      uniprot = "UniProt"
    )

    expect_equal(
      object = check_npx_nonunique_uniprot(df = df,
                                           col_names = col_names),
      expected = character(0L)
    )

    # test arrow tibble ----
    arrow_df <- arrow::arrow_table(
      SampleID = c("Sample1", "Sample1", "Sample1"),
      OlinkID = c("OID00001", "OID00002", "OID00003"),
      UniProt = c("Uniprot_1", "Uniprot_2", "Uniprot_3")
    )

    col_names <- list(
      sample_id = "SampleID",
      olink_id = "OlinkID",
      uniprot = "UniProt"
    )

    expect_equal(
      object = check_npx_nonunique_uniprot(df = arrow_df,
                                           col_names = col_names),
      expected = character(0L)
    )

  }
)

test_that(
  "check_npx_nonunique_uniprot - works - 1 OlinkID mapped with >1 Uniprot IDs",
  {

    # test tibble ----
    df <- dplyr::tibble(
      SampleID = c("Sample1", "Sample1", "Sample1"),
      OlinkID = c("OID00001", "OID00002", "OID00002"),
      UniProt = c("Uniprot_1", "Uniprot_2", "Uniprot_3")
    )

    col_names <- list(
      sample_id = "SampleID",
      olink_id = "OlinkID",
      uniprot = "UniProt"
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_nonunique_uniprot(df = df,
                                             col_names = col_names),
        expected = "OID00002"
      ),

      regexp = "Multiple UniProt IDs detected for assay: \"OID00002\"."
    )

    # test arrow tibble ----
    arrow_df <- arrow::arrow_table(
      SampleID = c("Sample1", "Sample1", "Sample1"),
      OlinkID = c("OID00001", "OID00002", "OID00002"),
      UniProt = c("Uniprot_1", "Uniprot_2", "Uniprot_3")
    )

    col_names <- list(
      sample_id = "SampleID",
      olink_id = "OlinkID",
      uniprot = "UniProt"
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_nonunique_uniprot(df = arrow_df,
                                             col_names = col_names),
        expected = "OID00002"
      ),

      regexp = "Multiple UniProt IDs detected for assay: \"OID00002\"."
    )

  }
)
