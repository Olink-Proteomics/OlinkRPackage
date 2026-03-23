# Load reference results
ref_results <- get_example_data(filename = "reference_results.rds")

# Load data with hidden/excluded assays (all NPX = NA)
npx_data_format221010 <- get_example_data(filename = "npx_data_format221010.rds")

# Generate check log

# - npx_data1
check_log <- check_npx(npx_data1) |>
  suppressMessages() |>
  suppressWarnings()
# - npx_data_format221010
check_log_format221010 <- check_npx(npx_data_format221010) |>
  suppressMessages() |>
  suppressWarnings()



# One-way Kruskal-Wallis Test
kruskal_results <- olink_one_non_parametric(
  df = npx_data1,
  check_log = check_log,
  variable = "Site"
  )

# One-way Friedman Test
friedman_results <- olink_one_non_parametric(
  df = npx_data1,
  check_log = check_log,
  variable = "Time",
  subject = "Subject",
  dependence = TRUE
)


#Posthoc test for the results from Kruskal-Wallis Test
if (requireNamespace("FSA", quietly = TRUE) ) {
  kruskal_posthoc_results <- olink_one_non_parametric_posthoc(
    df = npx_data1,
    check_log = check_log,
    variable = "Site",
    test = "kruskal",
    olinkid_list = {
      kruskal_results |>
        dplyr::filter(Threshold == 'Significant') |>
        dplyr::select(OlinkID) |>
        dplyr::distinct() %>%
        dplyr::pull()}
  ) |>
    mutate(id = as.character(OlinkID)) |>
    arrange(id, contrast) |>  #Just for consistency.
    select(-id)
}
#Posthoc test for the results from Friedman Test
friedman_posthoc_results <- olink_one_non_parametric_posthoc(
  df = npx_data1,
  check_log = check_log,
  variable = "Time",
  test = "friedman",
  subject = "Subject",
  olinkid_list = {
    friedman_results |>
      filter(Threshold == 'Significant') |>
      dplyr::select(OlinkID) |>
      distinct() |>
      pull()}
) |>
  mutate(id = as.character(OlinkID)) |>
  arrange(id, contrast) |>  #Just for consistency.
  select(-id)

test_that("olink_one_non_parametric function works", {

  if (requireNamespace("FSA", quietly = TRUE) ){
    expect_equal(kruskal_results, ref_results$kruskal) #result equal to testfile
  }

  expect_equal(friedman_results, ref_results$friedman) #result equal to testfile

  if (requireNamespace("FSA", quietly = TRUE) ){
    expect_equal(nrow(kruskal_results), 184)
    expect_equal(ncol(kruskal_results), 11)
  }

  expect_equal(nrow(friedman_results), 184)
  expect_equal(ncol(friedman_results), 11)

  expect_error(olink_one_non_parametric(npx_data1,)) ##no input data

  # Test for no input data or variable
  expect_error(
    object = olink_one_non_parametric(df = NULL),
    regexp = "`df` and `variable` must be specified."
  )

  expect_error(
    object = olink_one_non_parametric(df = npx_data1,
                                      check_log = check_log),
    regexp = "`df` and `variable` must be specified."
  )

  # --- Data with all NPX=NA for some assays ---

  expect_warning(
    object = olink_one_non_parametric(
      df = npx_data_format221010,
      check_log = NULL,
      variable = "treatment2"
    ) |> suppressMessages(),
    regexp = 'have "NPX" = NA for all samples'
  )

  expect_warning(
    olink_one_non_parametric(
      df = npx_data_format221010,
      check_log = NULL,
      variable = "treatment2"
    ) |> suppressMessages(),
    regexp = 'have \"NPX\" = NA for all samples'
  )

  warning <- capture_warning(
    olink_one_non_parametric(
      df = npx_data_format221010,
      check_log = NULL,
      variable = "treatment2"
    )
  )
  expect_match(
    conditionMessage(warning),
    "\"OID30136\".*\"OID31325\".*have \"NPX\" = NA for all samples"
  )


})

test_that("olink_one_non_parametric_posthoc function works", {
  if (requireNamespace("FSA", quietly = TRUE) ){
    # expect_equal(kruskal_posthoc_results, ref_results$kruskal_posthoc_results) ## result equal to testfile - posthoc
    expect_equal(kruskal_posthoc_results, ref_results$kruskal_posthoc) # result equal to testfile
    expect_equal(nrow(kruskal_posthoc_results), 190) ## check nr of rows
    expect_equal(kruskal_posthoc_results %>%
                   dplyr::select(contrast) %>%
                   unique() %>%
                   nrow(),10)
    expect_error(olink_one_non_parametric_posthoc(npx_data1, 'Site')) ##no olinkid list

    expect_warning(olink_one_non_parametric_posthoc(npx_data_format221010, variable = 'treatment2')) # data with all NPX=NA for some assays

  }
  # expect_equal(friedman_posthoc_results, ref_results$friedman_posthoc_results) ## result equal to testfile - posthoc
  expect_equal(friedman_posthoc_results, ref_results$friedman_posthoc)


  expect_equal(nrow(friedman_posthoc_results), 3) ## check nr of rows


  expect_equal(friedman_posthoc_results %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),3)

})

