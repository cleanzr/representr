#include <Rcpp.h>
using namespace Rcpp;

DataFrame rbind_cpp(List dfs, CharacterVector col_type) {
  int k = dfs.length();
  DataFrame df1 = DataFrame(dfs(0));
  CharacterVector names = df1.names();
  int p = df1.length();

  List vectors(p);
  for(int i = 0; i < p; i++) {
    if(col_type[i] == "numeric") {
      NumericVector vec(0);
      vectors(i) = vec;
    } else {
      CharacterVector vec(0);
      vectors(i) = vec;
    }
  }

  for(int j = 0; j < k; j++) {
    DataFrame df = DataFrame(dfs(j));
    for(int i = 0; i < p; i++) {
      if(col_type[i] == "numeric") {
        NumericVector vectors_i = vectors(i);
        NumericVector vec = df(i);
        int n = vec.length();
        for(int m = 0; m < n; m++) {
          vectors_i.push_back(vec[m]);
        }
        vectors(i) = vectors_i;
      } else {
        CharacterVector vectors_i = vectors(i);
        CharacterVector vec = df(i);
        int n = vec.length();
        for(int m = 0; m < n; m++) {
          vectors_i.push_back(vec[m]);
        }
        vectors(i) = vectors_i;
      }
    }
  }

  // turn list back into df
  DataFrame result(vectors);
  result.attr("names") = names;
  return(result);
}

// [[Rcpp::export]]
List compute_not_clusters(List clusters, CharacterVector col_type) {
  int k = clusters.length();
  List not_clusters(k);


  for(int i = 0; i < k; i++) {
    List not_i(0);
    DataFrame df = DataFrame(clusters(i));
    int n = df.nrow();
    if(n > 1) {
      for(int j = 0; j < k; j++) {
        if(j != i) {
          DataFrame df2 = DataFrame(clusters(j));
          not_i.push_back(df2);
        }
      }
    }

    if(not_i.length() > 0) {
      DataFrame result = rbind_cpp(not_i, col_type);
      not_clusters(i) = result;
    } else {
      not_clusters(i) = R_NilValue;
    }
  }
  return(not_clusters);
}



