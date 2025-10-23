#' distances_rf
#'
#' @description
#' Calculates the pairwise distances between the nodes of a specific tree in a Random Forest model.
#' The distances are computed based on the tree's structure and are useful for advanced analyses,
#' such as measuring relationships or proximity between terminal nodes.
#'
#' @param rf A Random Forest object created using the `ranger` package. The model must be trained with `write.forest = TRUE`.
#' @param n_tree An integer specifying the index of the tree within the Random Forest for which to calculate node distances.
#'
#' @return A tibble with the following columns:
#' - `node1`: The first node in the pair (integer).
#' - `node2`: The second node in the pair (integer).
#' - `dist`: The calculated distance between `node1` and `node2`.
#'
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' df1 <- tibble::tibble(
#'   x1 = stats::runif(20, 0, 1),
#'   x2 = stats::rnorm(20, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + stats::rnorm(20, 0, 1)
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
#' # Calculate distances between nodes in the first tree
#' node_distances <- distances_rf(rf, n_tree = 1)
#' print(node_distances)
#'
distances_rf <- function(rf, n_tree) {
  info <- ranger::treeInfo(rf, tree = n_tree)

  if (nrow(info) == 1) return(tibble(node1 = 0, node2 = 0, dist = 0))

  info$nodeID     <- info$nodeID + 1
  info$leftChild  <- info$leftChild + 1
  info$rightChild <- info$rightChild + 1

  tree <- ape::read.tree(text = paste0(convert_par(info), ";"))

  nodes_distances <- ape::dist.nodes(tree)

  text <- paste0(convert_par(info), ";")

  codes <- tibble::tibble(
    label = stringr::str_split(stringr::str_remove_all(text, "\\(|\\)|:1|\\;"), ",")[[1]],
    number = 1:nrow(info[info$terminal, ])
  )

  combinations <- RcppAlgos::comboGeneral(tree$tip.label, m = 2, repetition = TRUE)

  combinations <- tibble::tibble(
    node1 = pmin(as.numeric(combinations[, 1]), as.numeric(combinations[, 2])),
    node2 = pmax(as.numeric(combinations[, 1]), as.numeric(combinations[, 2]))
  )

  combinations <- combinations %>%
    dplyr::mutate(dist = purrr::map2_dbl(
      node1,
      node2,
      ~ distances_mrca(
        .x,
        .y,
        tree = tree,
        nodes_distances = nodes_distances,
        codes = codes
      )
    ))

  return(combinations)

}
