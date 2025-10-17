lod_file <-data.frame(OlinkID = "OID12345",
                      AssayType = "assay",
                      UniProt = "Q12345",
                      Assay = "TestAssay",
                      Panel = "TestPanel",
                      Block = 1,
                      DataAnalysisRefID = 'D10010',
                      LODNPX = 1,
                      LODCount = 1000,
                      LODMethod = "lod_npx")

df <- npx_data1 |> 
  dplyr::mutate(OlinkID = ifelse(OlinkID == npx_data1$OlinkID[1],
                                 "OID12345",
                                 OlinkID)) |> 
  dplyr::mutate(DataAnalysisRefID = "D10010") |> 
  dplyr::mutate(PanelDataArchiveVersion = "1.5")
test_that("LOD DARID message works", {
  expect_message(check_lod_darid(lod_file, df),
                 regexp = "Outdated version")
  
  expect_message(check_lod_darid(lod_file, df),
                 regexp = "Olink.com")
  
  expect_no_message(check_lod_darid(lod_file |> 
                                      mutate(Version = "9.0.0"),
                                    df))
  
  expect_no_message(check_lod_darid(lod_file,
                                    df |> 
                                      mutate(PanelDataArchiveVersion = "1.0")))
})
