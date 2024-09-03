#Load test data
load("../data/qs_normalized_testing.rds")

# Run QS test()
ecdf_transform_npx <- function(data = data) {

  # Outlier removal based on low counts threshold, think the trimodal assays
  model_data_joined <- data |> dplyr::filter(Count > 10)

  # If number of datapoints are < 24, no spline is fitted
  if (nrow(model_data_joined) < 24) {

    preds <- data |>
      dplyr::select(SampleID)

    preds$preds <- NA

    return(preds)
  }

  #Quantiles of 3k mapped to quantiles of HT
  mapped_3k <- stats::quantile(model_data_joined$NPX_ht,
                               sort(
                                 stats::ecdf(model_data_joined$NPX_3k)
                                 (model_data_joined$NPX_3k))
  )
  npx_3k <- sort(unique(model_data_joined$NPX_3k))

  #The quantile points used for adapting the nonelinear spline
  knots_npx3k <- stats::quantile(npx_3k, probs = c(0.05, 0.1, 0.25, 0.5, 0.75,
                                                   0.9, 0.95))

  #The nonlinear model
  spline_model <- lm(mapped_3k ~ splines::ns(npx_3k, knots = knots_npx3k))

  #Output (just making sure that correct points are output)
  newdata <- as.data.frame(c(data$NPX_3k))
  colnames(newdata) <- "npx_3k"
  preds <- as.data.frame(stats::predict(spline_model, newdata = newdata))
  colnames(preds) <- "QSNormalizedNPX"
  preds$SampleID <- data$SampleID

  return(preds)
}

ecdf_transform <- data |>
  dplyr::group_by(OlinkID) |>
  dplyr::reframe(ecdf_transform_npx(pick(everything()))) |>
  dplyr::ungroup() |>
  dplyr::select(OlinkID, SampleID, QSNormalizedNPX)

check_func <- ecdf_transform |>
  dplyr::inner_join(ecdf_transform_verification, by = c("OlinkID", "SampleID"))

testthat::expect_equal(check_func$QSNormalizedNPX,
                       check_func$preds_verification)

