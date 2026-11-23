# Tests for MSstatstoNPX in R/msstats.R

make_protein_level <- function() {
  data.frame(
    RUN = c(1L, 2L, 1L, 2L),
    Protein = c("P1", "P1", "P2", "P2"),
    LABEL = "L",
    LogIntensities = c(9.1, 9.2, NA_real_, 7.5),
    originalRUN = c("S1", "S2", "S1", "S2"),
    GROUP = c("A", "B", "A", "B"),
    SUBJECT = c("sub1", "sub2", "sub1", "sub2"),
    more50missing = c(FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that(
  "MSstatstoNPX - works - protein-level to NPX columns",
  {
    npx <- MSstatstoNPX(make_protein_level())

    expect_s3_class(npx, "tbl_df")
    expect_true(all(
      c(
        "SampleID", "OlinkID", "UniProt", "Assay", "NPX",
        "MissingFreq", "Panel", "Panel_Version", "PlateID",
        "QC_Warning", "SampleType", "Subject", "Condition"
      ) %in% colnames(npx)
    ))
    expect_equal(nrow(npx), 4L)
    expect_equal(sort(unique(npx$OlinkID)), c("OID00001", "OID00002"))
    expect_equal(unique(npx$Panel), "MSstats/DIANN protein")
    expect_equal(npx$Condition, npx$PlateID)
    expect_true(any(npx$QC_Warning == "Warning"))
  }
)

test_that(
  "MSstatstoNPX - error - missing required columns",
  {
    expect_error(
      object = MSstatstoNPX(data.frame(x = 1L)),
      regexp = "Missing required"
    )
  }
)

test_that(
  "MSstatstoNPX - works - RUN used when originalRUN absent",
  {
    df <- make_protein_level()
    df$originalRUN <- NULL
    npx <- MSstatstoNPX(df)
    expect_equal(sort(unique(npx$SampleID)), c("1", "2"))
  }
)
