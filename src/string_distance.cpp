#include <Rcpp.h>
using namespace Rcpp;

//' levenshtein
//' Internal String distance functions
//'
//' @param str1 string 1
//' @param str2 string 2.

//'
//' @references {
//'   M. E., Macherki (2016): Algorithm:Levenshtein Rcpp. figshare. Software. https://doi.org/10.6084/m9.figshare.3386308.v1
//' }
// [[Rcpp::export]]
int levenshtein(std::string str1, std::string str2) {
  int lenStr1 = str1.size();
  int lenStr2 = str2.size();
  NumericMatrix d(lenStr1 + 1, lenStr2 + 1);

  for(int i = 0; i < lenStr1 + 1; i++) {
    d(i, 0) = i;
  }

  for(int j = 0; j < lenStr2 + 1; j++){
    d(0, j) = j;
  }

  int cost=0;
  NumericVector tmp(3);
  NumericVector tmp1(2);

  for(int i = 1; i < lenStr1 + 1; i++) {
    for(int j = 1; j < lenStr2 + 1; j++){
      if(str1[i - 1] == str2[j - 1]) cost = 0;
      else cost=1;

      tmp[0] = d(i - 1, j) + 1;
      tmp[1] = d(i, j - 1) + 1;
      tmp[2] = d(i - 1, j - 1) + cost;
      d(i, j) = min(tmp);
    }
  }

  int res = d(lenStr1, lenStr2);
  return res;
}




