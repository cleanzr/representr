#include <Rcpp.h>

#ifndef DIST_COL_TYPE_H
#define DIST_COL_TYPE_H
double dist_col_type(Rcpp::DataFrame a, Rcpp::DataFrame b, Rcpp::CharacterVector col_type, Rcpp::NumericVector weights, Rcpp::List orders);
#endif
