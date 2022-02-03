#include <Rcpp.h>
#include "string_distance.h"
using namespace Rcpp;


//' dist_col_type
//' Inner column type record distance function
//'
//' @param a record a
//' @param b record b
//' @param col_type A vector encoding the column type for each column in the dataset. Can take values
//' in "categorical", "ordinal", "string", or "numeric"
//' @param weights A vector of weights for each column for making some column distances more important. Must sum to 1.
//' Defaults to equal weights.
//' @param orders A named list containing the order of the levels in each ordinal column. Defaults to NULL,
//' which corresponds to no ordinal variables.
//' @export
// [[Rcpp::export]]
double dist_col_type(DataFrame a, DataFrame b, CharacterVector col_type, NumericVector weights, List orders) {
  double dist = 0; //storage

  int p = a.length();

  for(int j = 0; j < p; j++) {

    if(col_type[j] == "numeric") {
      // numeric: weighted absolute difference in values
      NumericVector a_val = a[j]; NumericVector b_val = b[j];
      double a_num = a_val[0]; double b_num = b_val[0];

      dist +=  weights[j] * fabs(a_num - b_num);

    } else if(col_type[j] == "categorical") {
      // categorical: weighted binary distance
      CharacterVector a_val = a[j]; CharacterVector b_val = b[j];
      std::string a_string = Rcpp::as<std::string>(a_val[0]); std::string b_string = Rcpp::as<std::string>(b_val[0]);
      bool not_equal = a_string != b_string;

      dist += weights[j] * not_equal;

    } else if(col_type[j] == "ordinal") {
      // ordinal: look up absolute level distance
      CharacterVector a_val = a[j]; CharacterVector b_val = b[j];
      std::string a_string = Rcpp::as<std::string>(a_val[0]); std::string b_string = Rcpp::as<std::string>(b_val[0]);

      CharacterVector col_names = a.names();
      std::string col_name = Rcpp::as<std::string>(col_names[j]);
      CharacterVector ord = orders[col_name];
      int k = ord.length();

      int which_a, which_b = 0;
      for(int i = 0; i < k; i++) {
        std::string ord_i = Rcpp::as<std::string>(ord[i]);
        if(a_string == ord_i) which_a = i;
        if(b_string == ord_i) which_b = i;
      }

      dist += weights[j] * abs(which_a - which_b);

    } else {
      // string: weighted string distance
      CharacterVector a_val = a[j]; CharacterVector b_val = b[j];
      std::string a_string = Rcpp::as<std::string>(a_val[0]); std::string b_string = Rcpp::as<std::string>(b_val[0]);
      int a_b_dist = levenshtein(a_string, b_string);

      dist += weights[j] * a_b_dist;
    }
  }

    return(dist);
}
