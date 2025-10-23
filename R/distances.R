#' distances
#'
#' @description
#' Calculates the distances between nodes for all trees in a Random Forest model. These distances can be
#' used for advanced analyses, such as weighting predictions based on proximity in the tree structure.
#'
#' @param rf A Random Forest object created using the `ranger` package. The model must have the `write.forest`
#' parameter set to `TRUE` during training to enable access to tree structures.
#'
#' @return A data table with the following columns:
#' - `tree`: The tree index (integer).
#' - `node1`: The first node in the distance pair (0-based index).
#' - `node2`: The second node in the distance pair (0-based index).
#' - `distance`: The computed distance between `node1` and `node2` for the given tree.
#'
#' @export
#'
#' @examples
#'
#' # Generate example data
#' set.seed(123)
#' df1 <- tibble::tibble(
#'   x1 = stats::runif(20, 0, 1),
#'   x2 = stats::rnorm(20, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + stats::rnorm(20, 0, 1)
#' )
#'
#' # Fit a Random Forest model
#' rf <- ranger::ranger(
#'   y ~ .,
#'   data = df1,
#'   max.depth = 5,
#'   num.trees = 10,
#'   write.forest = TRUE
#' )
#'
#' # Calculate distances between nodes
#' node_distances <- distances(rf)
#' print(node_distances)
#'
distances <- function(rf) {
  aux <- tibble::tibble(tree = 1:rf$num.trees) %>%
    dplyr::mutate(value = purrr::map(tree, ~ distances_rf(rf = rf, n_tree = .x[1]))) %>%
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(node1 = node1 - 1, node2 = node2 - 1)

  return(data.table::data.table(aux))

}
