set.seed(10)
#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

pca_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(quiet = TRUE)

pca_plot_treatCol <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment', quiet = TRUE)

pca_plot_treatCol_topLoadings <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment',
                 loadings_list = {ref_results$t.test_results %>%
                     head(5) %>%
                     pull(OlinkID)},
                 quiet = TRUE)

#PCA by panel
pca_plot_byPanel <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = TRUE, quiet = TRUE)

#Label outliers
pca_plot_byPanel_outliers <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = TRUE, outlierDefX = 4, outlierDefY = 2.5, quiet = TRUE)
outliers <- lapply(pca_plot_byPanel_outliers, function(x){x$data}) %>%
  bind_rows() %>%
  filter(Outlier == 1)


test_that("olink_pca_plot works", {

  # Two Warnings thrown: for dropped assays and droppes samples
  expect_warning(
    expect_warning(
      pca_plot_drop <- npx_data1 %>%
      mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
      olink_pca_plot(drop_assays = TRUE, drop_samples = TRUE, quiet = TRUE)
    )
  )

  expect_equal(outliers$SampleID, c("B4_83", "A14_15", "A15_16", "A19_21"))
  expect_equal(outliers$Panel, c("Cardiometabolic", "Inflammation", "Inflammation", "Inflammation"))

  vdiffr::expect_doppelganger('PCA plot', pca_plot[[1]])
  vdiffr::expect_doppelganger('PCA plot color by treatment', pca_plot_treatCol[[1]])
  vdiffr::expect_doppelganger('PCA plot with loadings', pca_plot_treatCol_topLoadings[[1]])
  vdiffr::expect_doppelganger('PCA plot drop_assays and drop_samples', pca_plot_drop[[1]])
  vdiffr::expect_doppelganger('PCA plot panel 1', pca_plot_byPanel[[1]])
  vdiffr::expect_doppelganger('PCA plot panel 2', pca_plot_byPanel[[2]])
})


# PCA plot internal -------------------------------------------------------

test_that("PCA plot internal", {

  pca_p2 <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = FALSE,
                            outlierLines = FALSE)


  vdiffr::expect_doppelganger("PCA plot internal", pca_p2)


  pca_p3 <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE)


  vdiffr::expect_doppelganger("PCA plot internal 2", pca_p3)


  pca_p4 <- list(npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE))

  vdiffr::expect_doppelganger("PCA plot internal 3", pca_p4[[1]])


  pca_p5 <- npx_data1 %>%
    dplyr::filter(stringr::str_detect(OlinkID, "OID[0-9]{5}")) %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE)

  vdiffr::expect_doppelganger("PCA plot internal 4", pca_p5)

})

# PCA calculation ---------------------------------------------------------

## Order of output

### With set locale -----

old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")

pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

locale_outside <- Sys.getlocale (category = "LC_ALL")

Sys.setlocale("LC_COLLATE", old_collate)

test_that("PCA calculation - output order 2", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  locale_inside <- Sys.getlocale (category = "LC_ALL")

  expect_equal(locale_outside, locale_inside)
  expect_equal(rownames(pca$scores), rownames(pca_outside$scores))

  expect_snapshot_value(pca$scores, style = "deparse")
  expect_snapshot_value(pca$loadings, style = "deparse")
})

### Without set locale -----

pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

test_that("PCA calculation - output order 2", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  expect_equal(rownames(pca$scores), rownames(pca_outside$scores))
})


pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

test_that("PCA calculation - output values", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  expect_equal(pca$loadings_scaling_factor, pca_outside$loadings_scaling_factor)
  expect_equal(pca$loadings, pca_outside$loadings)
  expect_equal(pca$PoV, pca_outside$PoV)
  expect_equal(pca$scores, pca_outside$scores)

})



test_that("PCA basic plotting", {
  pca_input <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed()
  pca <- pca_input %>%
    olink_calculate_pca()

  pca_p1 <- ggplot(pca$scores, aes(x = PCX, y = PCY)) +
    geom_point()
  vdiffr::expect_doppelganger('PCA basic plotting', pca_p1)
})



# PCA plot function -------------------------------------------------------

test_that("minimal PCA plot", {
  pca_plot <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot(quiet = TRUE,
                   label_outliers = FALSE)

  vdiffr::expect_doppelganger("PCA plot - not label outliers", pca_plot[[1]])

  pca_plot_outliers <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot(quiet = TRUE, label_outliers = TRUE)

  vdiffr::expect_doppelganger("PCA plot - label outliers", pca_plot_outliers[[1]])

})


# prcomp ------------------------------------------------------------------

C <- chol(S <- toeplitz(.9 ^ (0:31))) # Cov.matrix and its root
set.seed(17)
X <- matrix(rnorm(32000), 1000, 32)
Z <- X %*% C  ## ==>  cov(Z) ~=  C'C = S

pZ_outside <- prcomp(Z, tol = 0.1)

test_that("prcomp", {
  pZ_inside <- prcomp(Z, tol = 0.1)

  expect_equal(pZ_outside, pZ_inside)

})


# locale ------------------------------------------------------------------

old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")

Sys.setlocale("LC_COLLATE", old_collate)

#----

test_that("PCA calculation", {

  locale_inside <- Sys.getlocale (category = "LC_ALL")

  expect_equal(locale_outside, locale_inside)

})
