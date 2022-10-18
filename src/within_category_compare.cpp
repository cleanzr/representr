#include <Rcpp.h>
#include "dist_col_type.h"
using namespace Rcpp;

DataFrame subset(DataFrame df, int idx, CharacterVector col_type) {
  int p = df.length();
  CharacterVector names = df.names();

  List vectors(p);

  for(int i = 0; i < p; i++) {
    if(col_type[i] == "numeric") {
      NumericVector vec = df(i);
      NumericVector vec_idx(0);
      vec_idx.push_front(vec(idx));
      vectors[i] = vec_idx;
    } else {
      CharacterVector vec = df(i);
      CharacterVector vec_idx(0);
      vec_idx.push_front(vec(idx));
      vectors[i] = vec_idx;
    }
  }

  DataFrame result(vectors);
  result.attr("names") = names;
  return(result);
}

//' within_category_compare_cpp
//' Inner column type record distance function
//'
//' @param ties A data frame of the records that are tied
//' @param not_cluster A data frame of the records outside the cluster
//' @param col_type A vector encoding the column type for each column in the dataset. Can take values
//' in "categorical", "ordinal", "string", or "numeric"
//' @param weights A vector of weights for each column for making some column distances more important. Must sum to 1.
//' Defaults to equal weights.
//' @param orders A named list containing the order of the levels in each ordinal column. Defaults to NULL,
//' which corresponds to no ordinal variables.
//' @param distance function that does nothing right now, but must be supplied to not break other code.
//' @export
// [[Rcpp::export]]
int within_category_compare_cpp(DataFrame ties, DataFrame not_cluster, CharacterVector col_type, NumericVector weights, List orders, Function distance) {
  int n = ties.nrow(); //nrow
  int p = ties.length(); //ncol
  int m = not_cluster.nrow();

  // which columns are character or ordinal to match on
  NumericVector which_char(p);
  for(int i = 0; i < p; i++) {
    which_char[i] = (col_type[i] == "categorical") || (col_type[i] == "ordinal");
  }

  // how many fields to compare on
  int num_compare_fields = sum(which_char);

  // get comparison vectors for each tie if applicable
  NumericMatrix dists(n, m);
  for(int i = 0; i < n; i++) {

    // figure out which records match categorical records of each tie
    IntegerMatrix which_subset_i(p, m);
    for(int j = 0; j < m; j++) {
      for(int k = 0; k < p; k++) {
        if(which_char[i] == 1) {
          std::string tip = Rcpp::as<std::string>(ties(i, k));
          std::string ncjp = Rcpp::as<std::string>(not_cluster(j, k));
          if(tip == ncjp) which_subset_i(k, j) = 1; else which_subset_i(k, j) = 0;
        }
      }
    }

    // store the appropriate distances
    IntegerVector which_subset_match = rowSums(which_subset_i);
    for(int j = 0; j < m; j++) {
      if(which_subset_match[j] == num_compare_fields) {
        DataFrame a = subset(ties, i, col_type); // TODO: write data frame subset function
        DataFrame b = subset(not_cluster, j, col_type);

        // compare to ties
        double dist_ij = dist_col_type(a, b, col_type, weights, orders);
        dists(i, j) = dist_ij;
      }
    }
  }

  // for each tie, find the max distance
  NumericVector max_dist(n);
  for(int i = 0; i < n; i++) {
    NumericVector di = dists(i,_);
    max_dist[i] = max(di);
  }

  // return the index of the min/max tie
  // double min_max_dist = min(max_dist);
  // int res = 0;
  // for(int i = 0; i < n; i++) {
  //   if(max_dist[i] == min_max_dist) res = i + 1;
  // }
  return(which_min(max_dist));
}




