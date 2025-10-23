#' RanBu - Random Bushes
#'
#' @description
#' Generates predictions using the RanBu (Random Bushes) approach for conditional mean or quantiles.
#' This method performs post-hoc kernel weighting over Random Forest terminal nodes, leveraging
#' tree-based distances to compute similarity weights. The weighting is controlled by the bandwidth
#' parameter `h`, applied to Breiman’s distance (1 – RF proximity).
#'
#' @param rf A Random Forest object created using the `ranger` package. The model must be trained with
#' `write.forest = TRUE` for terminal node extraction.
#' @param df1 A data frame representing the training dataset used to fit the Random Forest model.
#' @param y A numeric vector of responses corresponding to the training dataset (`df1`).
#' @param df2 A data frame containing new observations for which predictions are to be made.
#' @param h A numeric value for the RanBu hyperparameter, which controls the weighting of distances
#' between nodes. Default is `0.1`.
#' @param type A character string specifying the type of prediction. Can be `"mean"` for mean predictions
#' or `"quantile"` for quantile predictions.
#' @param quantiles A numeric vector specifying the quantiles to predict. Required only when `type = "quantile"`.
#' Values must be between 0 and 1. Default is `c(0.25, 0.5, 0.75)`.
#'
#' @return
#' A tibble:
#' - For `type = "mean"`, a single column tibble with predicted mean values for `df2`.
#' - For `type = "quantile"`, a tibble with one column per quantile, containing the predicted quantile values for `df2`.
#'
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' df1 <- tibble::tibble(
#'   x1 = stats::rnorm(300, 0, 1),
#'   x2 = stats::rnorm(300, 0, 1),
#'   x3 = stats::rnorm(300, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + x3 + stats::rnorm(300, 0, 1)
#' )
#'
#' df2 <- tibble::tibble(
#'   x1 = stats::rnorm(5, 0, 1),
#'   x2 = stats::rnorm(5, 0, 1),
#'   x3 = stats::rnorm(5, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + x3 + stats::rnorm(5, 0, 1)
#' )
#'
#' # Train a Random Forest model
#' rf <- ranger::ranger(
#'   y ~ .,
#'   data = df1,
#'   max.depth = 5,
#'   num.trees = 10,
#'   write.forest = TRUE
#' )
#'
#' # Mean predictions
#' predictions_mean <- ranbu(
#'   rf = rf,
#'   df1 = df1,
#'   y = df1$y,
#'   df2 = df2,
#'   h = 0.1,
#'   type = "mean"
#' )
#' print(predictions_mean)
#'
#' # Quantile predictions
#' rf_quantile <- ranger::ranger(
#'   y ~ .,
#'   data = df1,
#'   max.depth = 5,
#'   num.trees = 10,
#'   write.forest = TRUE,
#'   quantreg = TRUE
#' )
#'
#' predictions_quantile <- ranbu(
#'   rf = rf_quantile,
#'   df1 = df1,
#'   y = df1$y,
#'   df2 = df2,
#'   h = 0.1,
#'   type = "quantile",
#'   quantiles = c(0.025, 0.5, 0.975)
#' )
#' print(predictions_quantile)
#'
ranbu <- function(rf,
                  df1,
                  y,
                  df2,
                  h = 0.1,
                  type = "mean",
                  quantiles = c(.25, .5, .75)) {
  if (type == "mean") {
    predicted1 <- predict(rf, df1, type = "terminalNodes")$predictions
    predicted2 <- predict(rf, df2, type = "terminalNodes")$predictions

    resultados <- tibble::tibble(element = 1:nrow(predicted2))

    predictions <- tibble::tibble(
      element = 1:nrow(predicted2),
      predicted = prediction_ranbu(predicted1, y, predicted2, h = h)[, 1]
    )
    return(predictions[, 2])

  }


  if (type == "quantile") {
    if (any(quantiles <= 0) |
        any(quantiles >= 1))
      return("choose quantiles between 0 and 1.")

    results <- matrix(NA, nrow = nrow(df2), ncol = length(quantiles))

    colnames(results) <- paste0("q_", quantiles)
    rank <- order(y)
    y <- y[rank]
    df1 <- df1[rank, ]
    y_unique <- base::unique(y)

    predicted1 <- predict(rf, df1, type = "terminalNodes")$predictions
    predicted2 <- predict(rf, df2, type = "terminalNodes")$predictions

    dist <- tibble::tibble(y_unique = y_unique)

    results <- quantile_dnrb(quantiles, y_unique, y, predicted1, predicted2, h)

    colnames(results) <- paste0("q_", quantiles)

    return(tibble::as_tibble(results))

  }

}
