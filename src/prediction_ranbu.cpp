#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;


//' @export
 // [[Rcpp::export]]
arma::colvec prediction_ranbu(arma::mat nodes1, arma::colvec y_base, arma::mat nodes2, double h) {

  int n_predictions = nodes2.n_rows;
  int n_base = nodes1.n_rows;
  int n_tree = nodes1.n_cols;

  arma::colvec y_pred(n_predictions);

  arma::colvec distances(n_base); distances.fill(0.0);

  for (int i = 0; i < n_predictions; i++) {

    for (int j = 0; j < n_base; j++) {

      for (int t = 0; t < n_tree; t++){

        if (nodes1.at(j, t) != nodes2.at(i, t)) distances(j) = distances(j) + 1;

      }

    }

    distances = distances/n_tree;

    // arma::colvec K = exp(-square(distances/h));

    arma::colvec K = -square(distances/h);

    K = exp(K - max(K) - log(sum(exp(K - max(K)))));

    // Rcpp::Rcout << "w\n" << std::fixed << K << "\n" << std::endl;

    y_pred(i) = dot(K, y_base);

    distances.fill(0.0);

  }

  return y_pred;

}



