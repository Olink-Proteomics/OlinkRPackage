# Data loads correctly with 'read_NPX()'

    Code
      read_NPX(filename = zip_npx_file_success_sha)
    Output
      # A tibble: 3 x 14
        SampleID  Index OlinkID  UniProt Assay MissingFreq Panel  Panel_Lot_Nr PlateID
        <chr>     <int> <chr>    <chr>   <chr>       <dbl> <chr>  <chr>        <chr>  
      1 sample1_1     1 OID20070 O43186  CRX             0 Cardi~ B04405       full_2~
      2 sample2_1     2 OID20070 O43186  CRX             0 Cardi~ B04405       full_2~
      3 sample3_1     3 OID20070 O43186  CRX             0 Cardi~ B04405       full_2~
      # ... with 5 more variables: QC_Warning <chr>, LOD <dbl>, NPX <dbl>,
      #   Normalization <chr>, Assay_Warning <chr>

