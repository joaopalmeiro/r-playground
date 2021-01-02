#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame createMapTrajectory(int n, double x0, double y0, double a1, double a2,
                              double a3, double a4, double a5, double a6,
                              double a7, double a8, double a9, double a10,
                              double a11, double a12, double a13, double a14) {
  NumericVector x(n);
  NumericVector y(n);

  x[0] = x0;
  y[0] = y0;

  for (int i = 1; i < n; ++i) {
    x[i] = a1 + a2 * x[i - 1] + a3 * y[i - 1] + a4 * pow(fabs(x[i - 1]), a5) +
           a6 * pow(fabs(y[i - 1]), a7);
    y[i] = a8 + a9 * x[i - 1] + a10 * y[i - 1] +
           a11 * pow(fabs(x[i - 1]), a12) + a13 * pow(fabs(y[i - 1]), a14);
  }

  return DataFrame::create(_["x"] = x, _["y"] = y);
}
