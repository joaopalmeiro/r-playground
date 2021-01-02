#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame createCliffordTrajectory(int n, double x0, double y0, double a,
                                   double b, double c, double d) {
  NumericVector x(n);
  NumericVector y(n);

  x[0] = x0;
  y[0] = y0;

  for (int i = 1; i < n; ++i) {
    x[i] = sin(a * y[i - 1]) + c * cos(a * x[i - 1]);
    y[i] = sin(b * x[i - 1]) + d * cos(b * y[i - 1]);
  }

  return DataFrame::create(_["x"] = x, _["y"] = y);
}
