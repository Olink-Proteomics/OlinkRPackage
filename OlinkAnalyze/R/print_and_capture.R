#' Capture the output of printing an object
#'
#' @param x printable object
#'
#' @return string representation of the provided object
#'
#' @examples
#'
#' OlinkAnalyze:::print_and_capture(npx_data1)
#'
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}
