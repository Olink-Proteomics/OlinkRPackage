library(testthat)
library(OlinkAnalyze)

if (length(strsplit(as.character(packageVersion("OlinkAnalyze")), "\\.")) > 3){
  test_check("OlinkAnalyze")
}


