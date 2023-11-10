#' Title
#'
#' @param x
#' @param a
#' @param b
#' @param c
#'
#' @return
#' @export
#'
#' @examples
f_gauss<- function(x,a,b,c){

  (a*exp(-(x-b)^2/(2*c^2)))

}
