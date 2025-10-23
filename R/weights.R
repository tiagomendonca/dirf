#' weights
#'
#' @description
#' Calculates the standardized weights (K values) for predictions based on the distance between nodes.
#' This function normalizes the distances, applies a Gaussian kernel weighting scheme, and ensures
#' the weights sum to 1.
#'
#' @param node A numeric vector of terminal nodes from observations for which weights are to be calculated,
#' with one value per tree.
#' @param nodes_base A `data.table` containing the terminal nodes of the training dataset for each tree in the Random Forest.
#' Must include the columns:
#' - `obs`: Observation index from the training dataset.
#' - `tree`: Tree index.
#' - `node1`: Terminal node for the observation in the respective tree.
#' @param nodes_dist A `data.table` containing the distances between terminal nodes, typically generated using the `distances` function.
#' Must include the columns:
#' - `node1`: The first node in the pair.
#' - `node2`: The second node in the pair.
#' - `dist`: Distance between the two nodes.
#' @param h A numeric value representing the weighting parameter, controlling the influence of distances
#' on the calculation. Default is `0.01`.
#'
#' @return
#' A numeric vector of standardized weights (K values), summing to 1. The weights reflect the relative influence
#' of training observations on the given prediction, based on their proximity in the Random Forest structure.
#'
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' df1 <- tibble::tibble(
#'   x1 = runif(20, 0, 1),
#'   x2 = rnorm(20, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + rnorm(20, 0, 1)
#' )
#'
#' df2 <- tibble::tibble(
#'   x1 = runif(20, 0, 1),
#'   x2 = rnorm(20, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + rnorm(20, 0, 1)
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
#' # Get terminal nodes for training and new data
#' predicted1 <- predict(rf, df1, type = 'terminalNodes')$predictions
#' predicted2 <- predict(rf, df2, type = 'terminalNodes')$predictions
#'
#' # Compute distances between nodes
#' nodes_distances_rf <- distances(rf)
#'
#' # Prepare base data for prediction
#' base_prediction <- data.table::data.table(
#'   obs = rep(1:nrow(predicted1), each = rf$num.trees),
#'   tree = rep(1:rf$num.trees, nrow(df1)),
#'   node1 = as.vector(t(predicted1))
#' )
#'
#' # Calculate weights for the first observation in df2
#' weight_values <- weights(
#'   node = predicted2[1, ],
#'   nodes_base = base_prediction,
#'   nodes_dist = nodes_distances_rf,
#'   h = 0.1
#' )
#' print(weight_values)
#'
weights <- function(node, nodes_base, nodes_dist, h = 0.01) {
  n1 <- nodes_base$node1
  n2 <- rep(node, dim(nodes_base)[1] / length(node))
  nodes_base$node1 <- pmin(n1, n2)
  nodes_base$node2 <- pmax(n1, n2)
  nodes_dist[, `:=`(dist, dist / max(dist))]
  aux <- merge(nodes_base, nodes_dist, all.x = TRUE)
  w <- aux[, .(dist = mean(dist)), by = .(obs)][order(obs)]$dist
  K <- -(w / h) ^ 2
  return(exp(K - max(K) - log(sum(exp(
    K - max(K)
  )))))

}
