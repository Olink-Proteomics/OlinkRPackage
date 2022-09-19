#Create an outlier group that should be picked up by the UMAP
outGroup <- c("A1_1", "A10_11", "A12_13", "A17_18", "A25_27", "A69_71", "B2_81", "B31_112", "B66_147", "B71_152")
dat <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  mutate(NPX = ifelse(SampleID %in% outGroup, NPX + 5, NPX),
         group = ifelse(SampleID %in% outGroup, 'A', 'B'))

#Run UMAP and cluster the results
umap_plot <- olink_umap_plot(dat, color_g = 'group') #UMAP
umap_plot.cl <- kmeans(umap_plot[[1]]$data, centers = 2) #Cluster umap results

#Get the samples in the smallest cluster
clusterSizes <- table(umap_plot.cl$cluster)
outGroup.detected <- umap_plot.cl$cluster[as.numeric(names(clusterSizes)[which.min(clusterSizes)]) == umap_plot.cl$cluster] %>%
  names()

test_that("olink_umap_plot works", {
  # Two Warnings thrown: for dropped assays and droppes samples
  expect_warning(
    expect_warning(
      umap_plot_drop <- npx_data1 %>%
        mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
        olink_umap_plot(drop_assays = TRUE, drop_samples = TRUE, quiet = TRUE)
    )
  )

  expect_true(all(outGroup.detected %in% outGroup))
})
