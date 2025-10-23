#' DiNo Predictions
#'
#' @description
#' Computes DiNo (Distance with Nodes) predictions for the mean or quantiles of the response variable
#' based on a Random Forest model. This method leverages the distances between nodes in the forest to produce
#' weighted predictions.
#'
#' @param rf Fitted `ranger` object (typically shallow: 50 trees, max depth 5). Must be trained on the `df1` dataset.
#' @param df1 A data frame used as the training dataset.
#' @param y A numeric vector representing the response variable in the training dataset.
#' @param df2 A data frame containing new data for which predictions are made.
#' @param h A positive numeric value representing the DiNo hyperparameter, controlling the influence of node distances.
#' @param nodes_dist A data frame of node distances by tree, obtained using `dirf::distances`. If `NULL`,
#' the function automatically calculates the node distances based on the input Random Forest model and training data.
#' @param type A character string indicating the type of prediction: `"mean"` for mean predictions or `"quantile"` for quantile predictions.
#' @param quantiles A numeric vector specifying the quantiles to predict (e.g., `c(0.025, 0.5, 0.975)`). Only used when `type = "quantile"`.
#'
#' @return A numeric vector or matrix of predicted values, depending on the type of prediction:
#' - If `type = "mean"`, returns a numeric vector of mean predictions.
#' - If `type = "quantile"`, returns a numeric matrix with rows corresponding to observations and columns to quantiles.
#'
#' @export
#'
#' @examples
#'
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
#' # mean
#'
#' rf <- ranger::ranger(
#'   y ~ .,
#'   max.depth = 5,
#'   write.forest = TRUE,
#'   data = df1,
#'   num.trees = 10
#' )
#'
#' dino(
#'   rf = rf,
#'   df1 = df1,
#'   y = df1$y,
#'   df = df2,
#'   h = 0.1,
#'   type = "mean"
#' )
#'
#' # quantiles
#' rf <- ranger::ranger(
#'   y ~ .,
#'   max.depth = 5,
#'   write.forest = TRUE,
#'   quantreg = TRUE,
#'   data = df1,
#'   num.trees = 10
#' )
#'
#' dino(
#'   rf = rf,
#'   df1 = df1,
#'   y = df1$y,
#'   df = df2,
#'   h = 0.1,
#'   type = "quantile",
#'   quantiles = c(.025, .5, .975)
#' )
#'
dino <- function(rf,
                 df1,
                 y,
                 df2,
                 h = 0.1,
                 nodes_dist = NULL,
                 type = "mean",
                 quantiles = c(.25, .5, .75)) {
  if (type == "mean") {
    predicted1 <- predict(rf, df1, type = "terminalNodes")$predictions
    predicted2 <- predict(rf, df2, type = "terminalNodes")$predictions

    if (is.null(nodes_dist))
      nodes_dist <- distances(rf)

    base_prediction <- data.table::data.table(
      obs = rep(1:nrow(predicted1), each = rf$num.trees),
      tree = rep(1:rf$num.trees, nrow(df1)),
      node1 = as.vector(t(predicted1))
    )

    results <- tibble::tibble(element = 1:nrow(df2))
    results$predicted <- apply(results, 1, function(x)
      prediction_nodes(predicted2[x, ], base_prediction, y, nodes_dist, h = h))

    return(results[, 2])

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
    dist <- tibble::tibble(y_unique = y_unique)

    predicted1 <- predict(rf, df1, type = "terminalNodes")$predictions
    predicted2 <- predict(rf, df2, type = "terminalNodes")$predictions

    if (is.null(nodes_dist))
      nodes_dist <- distances(rf)

    base_prediction <- data.table::data.table(
      obs = rep(1:nrow(predicted1), each = rf$num.trees),
      tree = rep(1:rf$num.trees, nrow(df1)),
      node1 = as.vector(t(predicted1))
    )

    for (i in 1:nrow(df2)) {
      results[i, ] <- quantile_dnrb_aux(
        quantiles,
        dist$y_unique,
        y,
        weights(predicted2[i, ], base_prediction, nodes_dist, h = h)
      )
    }

    return(tibble::as_tibble(results))

  }

}
