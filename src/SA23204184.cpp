#include <Rcpp.h>
using namespace Rcpp;

//' @title function sin(x)
 //' @description function sin(x)
 //' @param x the value of the independent variable
 //' @return the value of sin(x)
 //' @examples
 //' \dontrun{
 //'sin(2)
 //' }
 //' @export
 // [[Rcpp::export]]
 double fun(double x)
 {
   double y;
   y = sin(x);
   return y;
 }

//' @title calculate the integral of the function
 //' @description calculate the integral of the function
 //' @param down the value of the lower bound
 //' @param up the value of the higher bound
 //' @param n the number of intervals divided
 //' @return the integral
 //' @examples
 //' \dontrun{
 //' LeftRect(1,2,100)
 //' }
 //' @export
 // [[Rcpp::export]]
 double LeftRect(double down, double up, int n)
 {
   double h, s;
   int i;
   h = (up-down)/n;
   s = fun(down)*h;
   for (i=1; i<n; i++)
   {
     s = s + fun(down+i*h)*h;	
   } 
   
   return s;
 }