
#' Title
#'
#' @param x a number
#'
#' @return a number
#' @export
#'
#' @examples
#' t <- test(1)
test<-function(x){
  y <- x +1
  if (x == 1){
    y <- y + 2
  }
  if (y == 3){
    y <- y -2
  }


  return(y)
}
