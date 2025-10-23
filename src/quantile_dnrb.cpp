#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' @export
 // [[Rcpp::export]]
 NumericVector stl_sort(NumericVector x) {
   NumericVector y = clone(x);
   std::sort(y.begin(), y.end());
   return y;
 }



//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix quantile_dnrb_aux(Rcpp::NumericVector quantis, Rcpp::NumericVector y_unique, Rcpp::NumericVector y, Rcpp::NumericVector weights) {

  int n_obs = weights.length();
  int n_quantis = quantis.length();
  int ny_unique = y_unique.length();

  Rcpp::NumericMatrix results(1, n_quantis); results.fill(0.0);
  Rcpp::NumericVector indicator_pos(n_obs); indicator_pos.fill(0.0);
  Rcpp::NumericVector indicator_pos_minus_one(n_obs); indicator_pos_minus_one.fill(0.0);

  int quantile_position = 0;

  double w_sum_pos = 0.0;
  double w_sum_pos_minus_one = 0.0;

  indicator_pos = ifelse(y <= y_unique(0), 1.0, 0.0);
  w_sum_pos = sum(weights * indicator_pos);


  while (quantis(quantile_position) <= w_sum_pos) {

    results(0, quantile_position) = y_unique(0);
    quantile_position = quantile_position + 1;

    if (quantile_position == n_quantis) break;

  }

  if (quantile_position == n_quantis) { return results; }

  int start = 0;
  int end = ny_unique - 1;
  int position = 0;

  position = floor((start + end)/2);

  for (int q_i = quantile_position; q_i < n_quantis; q_i++) {

    indicator_pos = ifelse(y <= y_unique(position), 1.0, 0.0);
    indicator_pos_minus_one = ifelse(y <= y_unique(position - 1), 1.0, 0.0);

    w_sum_pos = sum(weights * indicator_pos);
    w_sum_pos_minus_one = sum(weights * indicator_pos_minus_one);

    while (!((w_sum_pos >= quantis(q_i)) & (w_sum_pos_minus_one < quantis(q_i)))) {

      if (w_sum_pos >= quantis(q_i)) {

        start = start;
        end = position;

      } else {

        start = position;
        end = end;

      }

      // Rcpp::Rcout << start << std::endl;
      // Rcpp::Rcout << end << std::endl;

      position = floor((start + end)/2);

      if (start == ny_unique - 2) position = end;

      indicator_pos = ifelse(y <= y_unique(position), 1.0, 0.0);
      indicator_pos_minus_one = ifelse(y <= y_unique(position - 1), 1.0, 0.0);

      w_sum_pos = sum(weights * indicator_pos);
      w_sum_pos_minus_one = sum(weights * indicator_pos_minus_one);

    }

    results(0, q_i) = y_unique(position);

    start = position;
    end = ny_unique - 1;

  }

  return results;

}





//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix quantile_dnrb(Rcpp::NumericVector quantis, Rcpp::NumericVector y_unique, Rcpp::NumericVector y, Rcpp::NumericMatrix nodes1, Rcpp::NumericMatrix nodes2, double h) {


  int n_train = nodes1.nrow();
  int n_test = nodes2.nrow();
  int n_tree = nodes1.ncol();
  int n_quantis = quantis.length();
  int ny_unique = y_unique.length();
  int start = 0;
  int end = ny_unique;
  int position = 0;

  double w_sum_pos = 0.0;
  double w_sum_pos_minus_one = 0.0;

  Rcpp::NumericMatrix results(n_test, n_quantis); results.fill(0.0);
  Rcpp::NumericVector weights(n_train); weights.fill(0.0);
  Rcpp::NumericVector indicator_pos(n_train); indicator_pos.fill(0.0);
  Rcpp::NumericVector indicator_pos_minus_one(n_train); indicator_pos_minus_one.fill(0.0);


  //Rcpp::NumericMatrix weights2(n_train, 1); results.fill(0.0);
  //Rcpp::NumericVector weights_temp(n_train); weights_temp.fill(0.0);

  for (int i = 0; i < n_test; i++) {

    weights.fill(0.0);
    start = 0;
    end = ny_unique - 1;
    position = floor((start + end)/2);

    for (int j = 0; j < n_train; j++){

      weights(j) = sum(nodes1(j, _) == nodes2(i, _));
      weights(j) = 1 - weights(j) / n_tree;

    }

    // weights = exp(-pow(weights/h, 2)) / sum(exp(-pow(weights/h, 2)));

    weights = exp( -pow(weights/h, 2) - max(-pow(weights/h, 2)) - log(sum(exp(-pow(weights/h, 2) - max(-pow(weights/h, 2))))));
    // weights = exp(-pow(weights, 2)/h) / sum(exp(-pow(weights, 2)/h));
    // weights = exp(-pow(weights, 2)/h) / sum(exp(-pow(weights, 2)/h));


    //weights_temp = weights;
    //weights_temp = stl_sort(weights_temp);
    //weights2(_, 0) = weights_temp;

    //Rcpp::Rcout.precision(7);
    //Rcpp::Rcout << "w\n" << std::fixed << weights2 << "\n" << std::endl;

    //Rcpp::Rcout.precision(7);
    //Rcpp::Rcout << "w\n" << std::fixed << weights << std::endl;

    //for(int t=0; t<weights.length(); ++t){
    //  Rprintf("the value of w[%i] : %f \n", t, weights[t]);
    //}

    results(i, _) = quantile_dnrb_aux(quantis, y_unique, y, weights);

  }

  return results;
  //return weights2;

}


