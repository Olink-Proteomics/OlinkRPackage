# Calculate current coverage
coverage <- covr::package_coverage(path = "OlinkAnalyze")
coverage_value <- covr::percent_coverage(x = coverage) |>
  ceiling()
if (coverage_value > 0L && coverage_value < 50L) {
  coverage_badge_color <- "red"
} else if (coverage_value >= 50L && coverage_value < 70L) {
  coverage_badge_color <- "orange"
} else if (coverage_value >= 70L && coverage_value < 85L) {
  coverage_badge_color <- "yellow"
} else if (coverage_value >= 85L && coverage_value <= 100L) {
  coverage_badge_color <- "green"
} else {
  coverage_badge_color <- "grey"
}
# https://img.shields.io/badge/Coverage-50%25-green
coverage_badge_url <- paste0(
  "https://img.shields.io/badge/Coverage-",
  coverage_value,
  "%25-",
  coverage_badge_color
)

# Read the README file
readme <- readLines("README.md")

# Extract the current badge value
badge_line <- which(grepl(pattern = "^\\!\\[Coverage\\]", x = readme))
current_badge_url <- stringr::str_extract(string = readme[badge_line],
                                          pattern = "(?<=\\().+?(?=\\))")

# Update the badge if the coverage value has changed
if (current_badge_url != coverage_badge_url) {
  print("Changing test coverage badge!")
  new_badge <- paste0("![Coverage](", coverage_badge_url, ")")
  readme <- c(
    head(x = readme, n = badge_line - 1L),
    new_badge,
    tail(x = readme, n = length(readme) - badge_line)
  )
  writeLines(readme, "README.md")
} else {
  print("Test coverage is identical!")
}
