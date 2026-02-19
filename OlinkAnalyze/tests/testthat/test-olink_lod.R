# Test check_ht_fixed_lod_version ----

test_that(
  "no message when darid_invalid is empty",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = character(0L),
        "PanelDataArchiveVersion" = character(0L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT",
      "Version" = "7.0.0"
    )

    expect_no_message(
      check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      )
    )
  }
)

test_that(
  "warn when missing Version column",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT"
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Outdated version of Fixed LOD file detected."
    )
  }
)

test_that(
  "warn when multiple version values are presented",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = rep("Explore_HT", 2L),
      "Version" = c("7.0.0", "9.0.0")
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Multiple versions detected: \"7.0.0\" and \"9.0.0\"."
    )
  }
)

test_that(
  "warn when version < 6.0.0",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT",
      "Version" = "5.0.0"
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Outdated version of Fixed LOD file detected."
    )

  }
)
