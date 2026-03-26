# Test olink_normalization_n ----

test_that(
  "olink_normalization_n - works - bridge",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### bridge normalization - no norm column ----

    norm_schema_bridge_nonorm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_norm", "df2_no_norm"),
      data = list(
        "df1_no_norm" = ref_norm_res$lst_df$df1_no_norm,
        "df2_no_norm" = ref_norm_res$lst_df$df2_no_norm
      ),
      samples = list(
        "df1_no_norm" = NA_character_,
        "df2_no_norm" = list(
          "DF1" = ref_norm_res$lst_sample$bridge_samples,
          "DF2" = ref_norm_res$lst_sample$bridge_samples
        )
      ),
      normalization_type = c(NA_character_, "Bridge"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_bridge_nonorm <- olink_normalization_n(
      norm_schema = norm_schema_bridge_nonorm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_bridge_nonorm,
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### bridge normalization - with norm column ----

    norm_schema_bridge_norm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_norm", "df2_norm"),
      data = list(
        "df1_norm" = ref_norm_res$lst_df$df1_norm,
        "df2_norm" = ref_norm_res$lst_df$df2_norm
      ),
      samples = list(
        "df1_norm" = NA_character_,
        "df2_norm" = list(
          "DF1" = ref_norm_res$lst_sample$bridge_samples,
          "DF2" = ref_norm_res$lst_sample$bridge_samples
        )
      ),
      normalization_type = c(NA_character_, "Bridge"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_bridge_norm <- olink_normalization_n(
      norm_schema = norm_schema_bridge_norm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_bridge_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$norm,
      tolerance = 1e-4
    )

    ### bridge normalization - no lod column ----

    norm_schema_bridge_nolod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_lod", "df2_no_lod"),
      data = list(
        "df1_no_lod" = ref_norm_res$lst_df$df1_no_lod,
        "df2_no_lod" = ref_norm_res$lst_df$df2_no_lod
      ),
      samples = list(
        "df1_no_lod" = NA_character_,
        "df2_no_lod" = list(
          "DF1" = ref_norm_res$lst_sample$bridge_samples,
          "DF2" = ref_norm_res$lst_sample$bridge_samples
        )
      ),
      normalization_type = c(NA_character_, "Bridge"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_bridge_nolod <- olink_normalization_n(
      norm_schema = norm_schema_bridge_nolod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_bridge_nolod,
      expected = ref_norm_res$lst_norm$bridge_norm$no_lod,
      tolerance = 1e-4
    )

    ### bridge normalization - multiple lod columns ----

    norm_schema_bridge_multilod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_multiple_lod", "df2_multiple_lod"),
      data = list(
        "df1_multiple_lod" = ref_norm_res$lst_df$df1_multiple_lod,
        "df2_multiple_lod" = ref_norm_res$lst_df$df2_multiple_lod
      ),
      samples = list(
        "df1_multiple_lod" = NA_character_,
        "df2_multiple_lod" = list(
          "DF1" = ref_norm_res$lst_sample$bridge_samples,
          "DF2" = ref_norm_res$lst_sample$bridge_samples
        )
      ),
      normalization_type = c(NA_character_, "Bridge"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_bridge_multilod <- olink_normalization_n(
      norm_schema = norm_schema_bridge_multilod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_bridge_multilod,
      expected = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization_n - works - intensity",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### intensity normalization - no norm column ----

    norm_schema_intensity_nonorm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_norm", "df2_no_norm"),
      data = list(
        "df1_no_norm" = ref_norm_res$lst_df$df1_no_norm,
        "df2_no_norm" = ref_norm_res$lst_df$df2_no_norm
      ),
      samples = list(
        "df1_no_norm" = NA_character_,
        "df2_no_norm" = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_intensity_nonorm <- olink_normalization_n(
      norm_schema = norm_schema_intensity_nonorm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_intensity_nonorm,
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

    ### intensity normalization - with norm column ----

    norm_schema_intensity_norm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_norm", "df2_norm"),
      data = list(
        "df1_norm" = ref_norm_res$lst_df$df1_norm,
        "df2_norm" = ref_norm_res$lst_df$df2_norm
      ),
      samples = list(
        "df1_norm" = NA_character_,
        "df2_norm" = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_intensity_norm <- olink_normalization_n(
      norm_schema = norm_schema_intensity_norm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_intensity_norm,
      expected = ref_norm_res$lst_norm$intensity_norm$norm,
      tolerance = 1e-4
    )

    ### intensity normalization - no lod column ----

    norm_schema_intensity_nolod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_lod", "df2_no_lod"),
      data = list(
        "df1_no_lod" = ref_norm_res$lst_df$df1_no_lod,
        "df2_no_lod" = ref_norm_res$lst_df$df2_no_lod
      ),
      samples = list(
        "df1_no_lod" = NA_character_,
        "df2_no_lod" = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_intensity_nolod <- olink_normalization_n(
      norm_schema = norm_schema_intensity_nolod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_intensity_nolod,
      expected = ref_norm_res$lst_norm$intensity_norm$no_lod,
      tolerance = 1e-4
    )

    ### intensity normalization - multiple lod columns ----

    norm_schema_intensity_multilod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_multiple_lod", "df2_multiple_lod"),
      data = list(
        "df1_multiple_lod" = ref_norm_res$lst_df$df1_multiple_lod,
        "df2_multiple_lod" = ref_norm_res$lst_df$df2_multiple_lod
      ),
      samples = list(
        "df1_multiple_lod" = NA_character_,
        "df2_multiple_lod" = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_res_intensity_multilod <- olink_normalization_n(
      norm_schema = norm_schema_intensity_multilod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_res_intensity_multilod,
      expected = ref_norm_res$lst_norm$intensity_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization_n - works - subset",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### subset normalization - no norm column ----

    norm_schema_subset_nonorm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_norm", "df2_no_norm"),
      data = list(
        "df1_no_norm" = ref_norm_res$lst_df$df1_no_norm,
        "df2_no_norm" = ref_norm_res$lst_df$df2_no_norm
      ),
      samples = list(
        "df1_no_norm" = NA_character_,
        "df2_no_norm" = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_subset_nonorm <- olink_normalization_n(
      norm_schema = norm_schema_subset_nonorm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_subset_nonorm,
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )

    ### subset normalization - with norm column ----

    norm_schema_subset_norm <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_norm", "df2_norm"),
      data = list(
        "df1_norm" = ref_norm_res$lst_df$df1_norm,
        "df2_norm" = ref_norm_res$lst_df$df2_norm
      ),
      samples = list(
        "df1_norm" = NA_character_,
        "df2_norm" = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_subset_norm <- olink_normalization_n(
      norm_schema = norm_schema_subset_norm
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_subset_norm,
      expected = ref_norm_res$lst_norm$subset_norm$norm,
      tolerance = 1e-4
    )

    ### subset normalization - no lod column ----

    norm_schema_subset_nolod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_no_lod", "df2_no_lod"),
      data = list(
        "df1_no_lod" = ref_norm_res$lst_df$df1_no_lod,
        "df2_no_lod" = ref_norm_res$lst_df$df2_no_lod
      ),
      samples = list(
        "df1_no_lod" = NA_character_,
        "df2_no_lod" = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_subset_nolod <- olink_normalization_n(
      norm_schema = norm_schema_subset_nolod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_subset_nolod,
      expected = ref_norm_res$lst_norm$subset_norm$no_lod,
      tolerance = 1e-4
    )

    ### subset normalization - multiple lod columns ----

    norm_schema_subset_multilod <- dplyr::tibble(
      order = c(1L, 2L),
      name = c("df1_multiple_lod", "df2_multiple_lod"),
      data = list(
        "df1_multiple_lod" = ref_norm_res$lst_df$df1_multiple_lod,
        "df2_multiple_lod" = ref_norm_res$lst_df$df2_multiple_lod
      ),
      samples = list(
        "df1_multiple_lod" = NA_character_,
        "df2_multiple_lod" = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        )
      ),
      normalization_type = c(NA_character_, "Subset"),
      normalize_to = c(NA_character_, "1")
    )

    norm_results_subset_multilod <- olink_normalization_n(
      norm_schema = norm_schema_subset_multilod
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::filter(
        .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
      )

    expect_equal(
      object = norm_results_subset_multilod,
      expected = ref_norm_res$lst_norm$subset_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization_n - works - multi-batch",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    ### Simple multi-project normalization example

    npx_multi_df1 <- npx_data1 |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["SampleID"]],
          pattern = "CONTROL_"
        )
      ) |>
      dplyr::select(
        -dplyr::all_of("Project")
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    npx_multi_df2 <- npx_data2 |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["SampleID"]],
          pattern = "CONTROL_"
        )
      ) |>
      dplyr::select(
        -dplyr::all_of("Project")
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    # manipulating the sample NPX datasets to create another two random ones
    npx_multi_df3 <- npx_data2 |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["SampleID"]],
          pattern = "CONTROL_"
        )
      ) |>
      dplyr::mutate(
        SampleID = paste(.data[["SampleID"]], "_mod", sep = ""),
        PlateID = paste(.data[["PlateID"]], "_mod", sep = "")
      ) |>
      dplyr::select(
        -dplyr::all_of("Project")
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    npx_multi_df4 <- npx_data1 |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["SampleID"]],
          pattern = "CONTROL_"
        )
      ) |>
      dplyr::mutate(
        SampleID = paste(.data[["SampleID"]], "_mod2", sep = ""),
        PlateID = paste(.data[["PlateID"]], "_mod2", sep = "")
      ) |>
      dplyr::select(
        -dplyr::all_of("Project")
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    ## samples to use for normalization
    overlap_samples <- dplyr::intersect(
      x = npx_data1$SampleID,
      y = npx_data2$SampleID
    ) |>
      tibble::enframe() |>  # to tibble
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["value"]],
          pattern = "CONTROL_SAMPLE"
        )
      ) |>
      dplyr::pull(
        .data[["value"]]
      )

    # sample subset used to reduce file size of the ref results
    sample_subset <- c("A6", "A38", "B47", "B22", "A43", "D75", "D79", "C66",
                       "B43", "B70", "D52", "A58", "B71", "A50", "D1", "B8")

    # Bridge samples with same identifiers between npx_df1 and npx_df2
    overlap_samples_df1_df2 <- list("DF1" = overlap_samples,
                                    "DF2" = overlap_samples)

    # Bridge samples with different identifiers between npx_df2 and npx_df3
    overlap_samples_df2_df3 <- list(
      "DF1" = c("C19", "C2", "C20", "C21", "C22",
                "C23", "C24", "C25", "C26", "C27"),
      "DF2" = c("C1_mod", "C10_mod", "C11_mod", "C12_mod", "C13_mod",
                "C14_mod", "C15_mod", "C16_mod", "C17_mod", "C18_mod")
    )

    # Samples to use for intensity normalization between npx_df4 and the
    # normalized dataset of npx_df1 and npx_df2
    overlap_samples_df13_df4 <- list(
      "DF1" = c("A1", "A10", "A11", "A12", "A13", "A13_mod", "A14", "A15",
                "A16", "A17", "A18", "A19", "A2", "A20", "A21", "A22", "A23",
                "A24", "A25", "A26", "A27", "A28", "A29", "A29_mod", "A3",
                "A30", "A30_mod", "A31", "A32", "A33", "A34", "A35", "A36",
                "A36_mod", "A37", "A38", "A39", "A4", "A40", "A41", "A42",
                "A43", "A44", "A45", "A45_mod", "A46", "A46_mod", "A47", "A48",
                "A49", "A5", "A50", "A51", "A52", "A52_mod", "A53", "A54",
                "A55", "A56", "A57", "A58", "A59", "A6", "A60", "A61", "A62",
                "A63", "A63_mod", "A64", "A65", "A66", "A67", "A68", "A69",
                "A7", "A70", "A71", "A71_mod", "A72", "A73", "A73_mod", "A74",
                "A75", "A76", "A77", "A8", "A9"),
      "DF2" = c("B1_mod2", "B10_mod2", "B11_mod2", "B12_mod2", "B13_mod2",
                "B14_mod2", "B15_mod2", "B16_mod2", "B17_mod2", "B18_mod2",
                "B19_mod2", "B2_mod2", "B20_mod2", "B21_mod2", "B22_mod2",
                "B23_mod2", "B24_mod2", "B25_mod2", "B26_mod2", "B27_mod2",
                "B28_mod2", "B29_mod2", "B3_mod2", "B30_mod2", "B31_mod2",
                "B32_mod2", "B33_mod2", "B34_mod2", "B35_mod2", "B36_mod2",
                "B37_mod2", "B38_mod2", "B39_mod2", "B4_mod2", "B40_mod2",
                "B41_mod2", "B42_mod2", "B43_mod2", "B44_mod2", "B45_mod2",
                "B46_mod2", "B47_mod2", "B48_mod2", "B49_mod2", "B5_mod2",
                "B50_mod2", "B51_mod2", "B52_mod2", "B53_mod2", "B54_mod2",
                "B55_mod2", "B56_mod2", "B57_mod2", "B58_mod2", "B59_mod2",
                "B6_mod2", "B60_mod2", "B61_mod2", "B62_mod2", "B63_mod2",
                "B64_mod2", "B65_mod2", "B66_mod2", "B67_mod2", "B68_mod2",
                "B69_mod2", "B7_mod2", "B70_mod2", "B71_mod2", "B72_mod2",
                "B73_mod2", "B74_mod2", "B75_mod2", "B76_mod2", "B77_mod2",
                "B78_mod2", "B79_mod2", "B8_mod2", "B9_mod2")
    )

    # create tibble for input
    norm_schema_npx_multi <- dplyr::tibble(
      order = c(1, 2, 3, 4),
      name = c("NPX_DF1", "NPX_DF2", "NPX_DF3", "NPX_DF4"),
      data = list("NPX_DF1" = npx_multi_df1,
                  "NPX_DF2" = npx_multi_df2,
                  "NPX_DF3" = npx_multi_df3,
                  "NPX_DF4" = npx_multi_df4),
      samples = list("NPX_DF1" = NA_character_,
                     "NPX_DF2" = overlap_samples_df1_df2,
                     "NPX_DF3" = overlap_samples_df2_df3,
                     "NPX_DF4" = overlap_samples_df13_df4),
      normalization_type = c(NA_character_, "Bridge", "Bridge", "Subset"),
      normalize_to = c(NA_character_, "1", "2", "1,3")
    )

    normalization_results_multi <- olink_normalization_n(
      norm_schema = norm_schema_npx_multi
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::mutate(
        SampleID_tmp = stringr::str_split(
          string = .data[["SampleID"]],
          pattern = "_"
        ) |>
          lapply(head, 1L) |>
          unlist()
      ) |>
      dplyr::filter(
        .data[["SampleID_tmp"]] %in% .env[["sample_subset"]]
      ) |>
      dplyr::select(
        -dplyr::all_of("SampleID_tmp")
      ) |>
      dplyr::arrange(
        .data[["Project"]],
        .data[["Panel"]],
        .data[["OlinkID"]],
        .data[["SampleID"]]
      )

    expect_equal(
      object = normalization_results_multi,
      expected = reference_results$normalization_multibatch,
      tolerance = 1e-4
    )
  }
)

# Test olink_normmalization_bridge ----

test_that(
  "olink_normmalization_bridge - works",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### bridge normalization - no norm column ----

    expect_message(
      object = expect_warning(
        object = expect_message(
          object = bridge_no_norm <- olink_normalization_bridge(
            project_1_df = ref_norm_res$lst_df$df1_no_norm,
            project_2_df = ref_norm_res$lst_df$df2_no_norm,
            bridge_samples = list(
              "DF1" = ref_norm_res$lst_sample$bridge_samples,
              "DF2" = ref_norm_res$lst_sample$bridge_samples
            ),
            project_1_name = "df1_no_norm",
            project_2_name = "df2_no_norm",
            project_ref_name = "df1_no_norm",
            format = FALSE,
            project_1_check_log = check_npx(
              df = ref_norm_res$lst_df$df1_no_norm
            ) |>
              suppressMessages() |>
              suppressWarnings(),
            project_2_check_log = check_npx(
              df = ref_norm_res$lst_df$df2_no_norm
            ) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                       "contain a column named \"Normalization\"")
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_no_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### bridge normalization - with norm column ----

    expect_message(
      object = expect_message(
        object = bridge_norm <- olink_normalization_bridge(
          project_1_df = ref_norm_res$lst_df$df1_norm,
          project_2_df = ref_norm_res$lst_df$df2_norm,
          bridge_samples = list(
            "DF1" = ref_norm_res$lst_sample$bridge_samples,
            "DF2" = ref_norm_res$lst_sample$bridge_samples
          ),
          project_1_name = "df1_norm",
          project_2_name = "df2_norm",
          project_ref_name = "df1_norm",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_norm
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_norm
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$norm,
      tolerance = 1e-4
    )

    ### bridge normalization - no lod column ----

    expect_message(
      object = expect_message(
        object = bridge_no_lod <- olink_normalization_bridge(
          project_1_df = ref_norm_res$lst_df$df1_no_lod,
          project_2_df = ref_norm_res$lst_df$df2_no_lod,
          bridge_samples = list(
            "DF1" = ref_norm_res$lst_sample$bridge_samples,
            "DF2" = ref_norm_res$lst_sample$bridge_samples
          ),
          project_1_name = "df1_no_lod",
          project_2_name = "df2_no_lod",
          project_ref_name = "df1_no_lod",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_no_lod
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_no_lod
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_no_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$no_lod,
      tolerance = 1e-4
    )

    ### bridge normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = expect_message(
          object = bridge_multiple_lod <- olink_normalization_bridge(
            project_1_df = ref_norm_res$lst_df$df1_multiple_lod,
            project_2_df = ref_norm_res$lst_df$df2_multiple_lod,
            bridge_samples = list(
              "DF1" = ref_norm_res$lst_sample$bridge_samples,
              "DF2" = ref_norm_res$lst_sample$bridge_samples
            ),
            project_1_name = "df1_multiple_lod",
            project_2_name = "df2_multiple_lod",
            project_ref_name = "df1_multiple_lod",
            format = FALSE,
            project_1_check_log = check_npx(
              df = ref_norm_res$lst_df$df1_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings(),
            project_2_check_log = check_npx(
              df = ref_norm_res$lst_df$df2_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\"",
                       "contain multiple columns matching")
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_multiple_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normmalization_bridge - errors - bridge samples not in df",
  {
    expect_error(
      object = olink_normalization_bridge(
        project_1_df = npx_data1 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        project_2_df = npx_data2 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        bridge_samples = list(
          "DF1" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"),
          "DF2" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76")
        )
      ),
      regexp = paste("not all SampleID in bridge_samples$DF2 are present in",
                     "\"project_2_df\""),
      fixed = TRUE
    )
  }
)

test_that(
  "olink_normmalization_bridge - errors - incorrect project name",
  {
    expect_error(
      object = olink_normalization_bridge(
        project_1_df = npx_data1 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        project_2_df = npx_data2 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        bridge_samples = list(
          "DF1" = intersect(npx_data1$SampleID, npx_data2$SampleID) |>
            sort() |>
            head(10L),
          "DF2" = intersect(npx_data1$SampleID, npx_data2$SampleID) |>
            sort() |>
            head(10L)
        ),
        project_1_name = "P1",
        project_2_name = "P1",
        project_ref_name = "P2"
      ),
      regexp = paste("\"project_1_name\" and \"project_2_name\" should differ",
                     "from each other"),
      fixed = TRUE
    )
  }
)

# Test olink_normalization_subset ----

test_that(
  "olink_normalization_subset - works - intensity normalization",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### intensity normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = intensity_no_norm <- olink_normalization_subset(
          project_1_df = ref_norm_res$lst_df$df1_no_norm,
          project_2_df = ref_norm_res$lst_df$df2_no_norm,
          reference_samples = list(
            "DF1" = ref_norm_res$lst_sample$df1_all,
            "DF2" = ref_norm_res$lst_sample$df2_all
          ),
          project_1_name = "df1_no_norm",
          project_2_name = "df2_no_norm",
          project_ref_name = "df1_no_norm",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_no_norm
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_no_norm
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                     "contain a column named \"Normalization\"")
    )

    expect_equal(
      object = intensity_no_norm,
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

    ### intensity normalization - with norm column ----

    expect_message(
      object = intensity_norm <- olink_normalization_subset(
        project_1_df = ref_norm_res$lst_df$df1_norm,
        project_2_df = ref_norm_res$lst_df$df2_norm,
        reference_samples = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        ),
        project_1_name = "df1_norm",
        project_2_name = "df2_norm",
        project_ref_name = "df1_norm",
        format = FALSE,
        project_1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        project_2_check_log = check_npx(df = ref_norm_res$lst_df$df2_norm) |>
          suppressMessages() |>
          suppressWarnings()
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = intensity_norm,
      expected = ref_norm_res$lst_norm$intensity_norm$norm,
      tolerance = 1e-4
    )

    ### intensity normalization - no lod column ----

    expect_message(
      object = intensity_no_lod <- olink_normalization_subset(
        project_1_df = ref_norm_res$lst_df$df1_no_lod,
        project_2_df = ref_norm_res$lst_df$df2_no_lod,
        reference_samples = list(
          "DF1" = ref_norm_res$lst_sample$df1_all,
          "DF2" = ref_norm_res$lst_sample$df2_all
        ),
        project_1_name = "df1_no_lod",
        project_2_name = "df2_no_lod",
        project_ref_name = "df1_no_lod",
        format = FALSE,
        project_1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
          suppressMessages() |>
          suppressWarnings(),
        project_2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_lod) |>
          suppressMessages() |>
          suppressWarnings()
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = intensity_no_lod,
      expected = ref_norm_res$lst_norm$intensity_norm$no_lod,
      tolerance = 1e-4
    )

    ### intensity normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = intensity_multiple_lod <- olink_normalization_subset(
          project_1_df = ref_norm_res$lst_df$df1_multiple_lod,
          project_2_df = ref_norm_res$lst_df$df2_multiple_lod,
          reference_samples = list(
            "DF1" = ref_norm_res$lst_sample$df1_all,
            "DF2" = ref_norm_res$lst_sample$df2_all
          ),
          project_1_name = "df1_multiple_lod",
          project_2_name = "df2_multiple_lod",
          project_ref_name = "df1_multiple_lod",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_multiple_lod
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_multiple_lod
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
    )

    expect_equal(
      object = intensity_multiple_lod,
      expected = ref_norm_res$lst_norm$intensity_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization_subset - works - subset normalization",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### subset normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = subset_no_norm <- olink_normalization_subset(
          project_1_df = ref_norm_res$lst_df$df1_no_norm,
          project_2_df = ref_norm_res$lst_df$df2_no_norm,
          reference_samples = list(
            "DF1" = ref_norm_res$lst_sample$df1_subset,
            "DF2" = ref_norm_res$lst_sample$df2_subset
          ),
          project_1_name = "df1_no_norm",
          project_2_name = "df2_no_norm",
          project_ref_name = "df1_no_norm",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_no_norm
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_no_norm
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                     "contain a column named \"Normalization\"")
    )

    expect_equal(
      object = subset_no_norm,
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )

    ### subset normalization - with norm column ----

    expect_message(
      object = subset_norm <- olink_normalization_subset(
        project_1_df = ref_norm_res$lst_df$df1_norm,
        project_2_df = ref_norm_res$lst_df$df2_norm,
        reference_samples = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        ),
        project_1_name = "df1_norm",
        project_2_name = "df2_norm",
        project_ref_name = "df1_norm",
        format = FALSE,
        project_1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        project_2_check_log = check_npx(df = ref_norm_res$lst_df$df2_norm) |>
          suppressMessages() |>
          suppressWarnings()
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = subset_norm,
      expected = ref_norm_res$lst_norm$subset_norm$norm,
      tolerance = 1e-4
    )

    ### subset normalization - no lod column ----

    expect_message(
      object = subset_no_lod <- olink_normalization_subset(
        project_1_df = ref_norm_res$lst_df$df1_no_lod,
        project_2_df = ref_norm_res$lst_df$df2_no_lod,
        reference_samples = list(
          "DF1" = ref_norm_res$lst_sample$df1_subset,
          "DF2" = ref_norm_res$lst_sample$df2_subset
        ),
        project_1_name = "df1_no_lod",
        project_2_name = "df2_no_lod",
        project_ref_name = "df1_no_lod",
        format = FALSE,
        project_1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
          suppressMessages() |>
          suppressWarnings(),
        project_2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_lod) |>
          suppressMessages() |>
          suppressWarnings()
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = subset_no_lod,
      expected = ref_norm_res$lst_norm$subset_norm$no_lod,
      tolerance = 1e-4
    )

    ### subset normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = subset_multiple_lod <- olink_normalization_subset(
          project_1_df = ref_norm_res$lst_df$df1_multiple_lod,
          project_2_df = ref_norm_res$lst_df$df2_multiple_lod,
          reference_samples = list(
            "DF1" = ref_norm_res$lst_sample$df1_subset,
            "DF2" = ref_norm_res$lst_sample$df2_subset
          ),
          project_1_name = "df1_multiple_lod",
          project_2_name = "df2_multiple_lod",
          project_ref_name = "df1_multiple_lod",
          format = FALSE,
          project_1_check_log = check_npx(
            df = ref_norm_res$lst_df$df1_multiple_lod
          ) |>
            suppressMessages() |>
            suppressWarnings(),
          project_2_check_log = check_npx(
            df = ref_norm_res$lst_df$df2_multiple_lod
          ) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
    )

    expect_equal(
      object = subset_multiple_lod,
      expected = ref_norm_res$lst_norm$subset_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization_subset - errors - bridge samples not in df",
  {
    expect_error(
      object = olink_normalization_subset(
        project_1_df = npx_data1 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        project_2_df = npx_data2 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        reference_samples = list(
          "DF1" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"),
          "DF2" = c("B64", "B36", "A77", "B7", "A24", "A49", "B76")
        )
      ),
      regexp = paste("not all SampleID in reference_samples$DF2 are present in",
                     "\"project_2_df\""),
      fixed = TRUE
    )
  }
)

test_that(
  "olink_normalization_subset - errors - incorrect project name",
  {
    expect_error(
      object = olink_normalization_subset(
        project_1_df = npx_data1 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        project_2_df = npx_data2 |>
          dplyr::select(-dplyr::all_of("Project")) |>
          dplyr::mutate(Normalization = "Intensity"),
        reference_samples = list(
          "DF1" = intersect(npx_data1$SampleID, npx_data2$SampleID) |>
            sort() |>
            head(10L),
          "DF2" = intersect(npx_data1$SampleID, npx_data2$SampleID) |>
            sort() |>
            head(10L)
        ),
        project_1_name = "P1",
        project_2_name = "P1",
        project_ref_name = "P2"
      ),
      regexp = paste("\"project_1_name\" and \"project_2_name\" should differ",
                     "from each other"),
      fixed = TRUE
    )
  }
)
