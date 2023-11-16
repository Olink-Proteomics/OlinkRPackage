#' Function to read NPX data into long format
#'
#' Imports an NPX or QUANT file exported from Olink Software.
#' No alterations to the output format is allowed.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt;
#'   Christoffer Cambronero;
#'   Boxi Zhang;
#'   Olof Mansson;
#'   Marianne Sandin
#'
#' @param filename Path to Olink Software output file.
#'
#' @return A "tibble" in long format.
#'
#' @keywords NPX parquet csv zip xlsx Olink Explore Target96 Target48 Flex HT
#' 1536 3072 3k
#'
#' @export
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#'

read_npx <- function(filename) {

  # If the file is csv or txt read_NPX assumes Explore NPX data in long format
  if (tools::file_ext(filename) %in% c("csv", "txt", "zip", "parquet")) {

    read_NPX_explore(filename = filename)

  } else {

    stop("Unrecognized input file extension!")

  }
}

#' @rdname read_npx
#' @export
read_NPX <- read_npx
#' @rdname read_npx
