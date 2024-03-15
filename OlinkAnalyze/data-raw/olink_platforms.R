## code to prepare internal dataset goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata

## Acceptable Olink platforms ----

# this tibble contains generic information about the Olink platforms that
# Olink Analyze accepts, their names, regular expressions to determine them from
# the data, quantification methods etc.
#
# it is used by various functions and tests internally.
accepted_olink_platforms <- dplyr::tibble(
  full_name = c(
    "Olink Target 48",
    "Olink Flex",
    "Olink Target 96",
    "Olink Explore 3072",
    "Olink Explore HT",
    "Olink Focus"
  ),
  name = c(
    "Target 48",
    "Flex",
    "Target 96",
    "Explore 3072",
    "Explore HT",
    "Focus"
  ),
  code_friendly_name = c(
    "Target_48",
    "Flex",
    "Target_96",
    "Explore_3072",
    "Explore_HT",
    "Focus"
  ),
  short_name = c(
    "T48",
    "Flex",
    "T96",
    "3k",
    "HT",
    "Focus"
  ),
  broader_platform = c(
    "qPCR",
    "qPCR",
    "qPCR",
    "NGS",
    "NGS",
    "qPCR"
  ),
  regexp = c(
    "Target 48",
    "[A-Z]{4}-[A-Z]{4}|Flex",
    "Target 96",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  quant_method = list(
    c("NPX", "Quantified", "Ct"),
    c("NPX", "Quantified", "Ct"),
    c("NPX", "Ct"),
    c("NPX"),
    c("NPX"),
    c("NPX", "Quantified", "Ct")
  ),
  quant_type = list(
    c("relative", "absolute", "relative"),
    c("relative", "absolute"),
    c("relative", "relative"),
    c("relative"),
    c("relative"),
    c("relative", "absolute")
  ),
  base_index = c(
    NA_integer_,
    NA_integer_,
    92L,
    NA_integer_,
    NA_integer_,
    NA_integer_
  ),
  wide_format_plate_info = c(
    TRUE,
    TRUE,
    FALSE,
    NA,
    NA,
    TRUE
  )
)
