#' @title function sin(x)
#' @description function sin(x)
#' @param x the value of the independent variable
#' @return the value of sin(x)
#' @examples
#' \dontrun{
#' sin(2)
#' }
#' @export
fun<-function(x){
  sin(x)}

#' @title calculate the integral of the function
#' @description calculate the integral of the function
#' @param down the value of the lower bound
#' @param up the value of the higher bound
#' @param n the number of intervals divided
#' @return the integral
#' @examples
#' \dontrun{
#'     LeftRect(1,2,100)
#' }
#' @export
LeftRect<-function(down, up, n){
  h = (up-down)/n
  s = fun(down)*h
  for (i in 1:n)
  {
    s = s + fun(down+i*h)*h
  } 
  
  s
}

#' @title Rcpp functions.
#' @name Rcpp
#' @description Rcpp package
#' @useDynLib SA23204184
#' @import Rcpp
NULL