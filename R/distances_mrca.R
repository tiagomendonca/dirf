#' distances_mrca
#'
#' @description
#' Calculates the distance to the Most Recent Common Ancestor (MRCA) between two nodes in a phylogenetic tree.
#' This is useful for analyzing relationships and distances between nodes in a hierarchical structure.
#'
#' @param node1 An integer or character indicating the first node's label or identifier.
#' @param node2 An integer or character indicating the second node's label or identifier.
#' @param tree A phylogenetic tree object created using `ape::read.tree`.
#' @param nodes_distances A matrix of distances between all nodes in the tree, typically obtained using `ape::dist.nodes`.
#' @param codes A data frame or tibble with tree labels and their corresponding node numbers. Must contain columns:
#' - `label`: Node labels as character strings.
#' - `number`: Node numbers corresponding to the labels.
#'
#'
#' @return A numeric value representing the distance from the MRCA to the more distant of the two nodes (`node1` or `node2`).
#' If the nodes are the same, the function returns 0.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' # Generate example data
#' df1 <- tibble::tibble(
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
#' # Extract tree information and adjust node indices
#' info <- ranger::treeInfo(rf, tree = 1)
#' info$nodeID <- info$nodeID + 1
#' info$leftChild <- info$leftChild + 1
#' info$rightChild <- info$rightChild + 1
#'
#' # Convert tree to Newick format and create phylogenetic tree
#' tree <- ape::read.tree(text = paste0(convert_par(info), ";"))
#'
#' # Compute node distances
#' nodes_distances <- ape::dist.nodes(tree)
#'
#' # Create codes for tree labels
#' codes <- tibble::tibble(
#'   label = stringr::str_split(
#'     stringr::str_remove_all(paste0(convert_par(info), ";"), "\\(|\\)|:1|;"),
#'     ","
#'   )[[1]],
#'   number = 1:nrow(info[info$terminal, ])
#' )
#'
#' # Calculate MRCA distance for a pair of nodes
#' distances_mrca(
#'   node1 = tree$tip.label[1],
#'   node2 = tree$tip.label[2],
#'   tree = tree,
#'   nodes_distances = nodes_distances,
#'   codes = codes
#' )
#'
distances_mrca <- function(node1, node2, tree, nodes_distances, codes) {
  if (node1 != node2) {
    mrca <- ape::getMRCA(tree, c(as.character(node1), as.character(node2)))

    dist <- max(nodes_distances[mrca, codes$number[codes$label == node1]], nodes_distances[mrca, codes$number[codes$label == node2]])

  } else {
    dist <- 0
  }

  return(dist)

}

