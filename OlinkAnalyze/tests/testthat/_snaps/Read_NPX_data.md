# Data loads correctly with 'read_NPX()'

    Code
      read_NPX(filename = zip_npx_file_success_sha)
    Output
      # A tibble: 3 x 14
        SampleID  Index OlinkID  UniProt Assay Missing~1 Panel Panel~2 PlateID QC_Wa~3
        <chr>     <int> <chr>    <chr>   <chr>     <dbl> <chr> <chr>   <chr>   <chr>  
      1 sample1_1     1 OID20070 O43186  CRX           0 Card~ B04405  full_2~ PASS   
      2 sample2_1     2 OID20070 O43186  CRX           0 Card~ B04405  full_2~ PASS   
      3 sample3_1     3 OID20070 O43186  CRX           0 Card~ B04405  full_2~ PASS   
      # ... with 4 more variables: LOD <dbl>, NPX <dbl>, Normalization <chr>,
      #   Assay_Warning <chr>, and abbreviated variable names 1: MissingFreq,
      #   2: Panel_Lot_Nr, 3: QC_Warning
