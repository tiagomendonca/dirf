#' convert_par
#'
#' @description Converts a decision tree structure into a text format compatible with the `ape::read.tree` function.
#' This can be used to represent trees as text for further manipulation or visualization.
#'
#' @param tree A data frame containing tree information, typically obtained from `ranger::treeInfo`.
#' @param row An integer indicating the current row (node) to process. Defaults to 1 (the root node).
#'
#' @return A character string representing the tree in Newick format.
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' df1 <- tibble::tibble(
#'   x1 = stats::runif(20, 0, 1),
#'   x2 = stats::rnorm(20, 0, 1),
#'   y = 2 * x1 + 1.5 * x2 + stats::rnorm(20, 0, 1)
#' )
#'
#' # Fit a random forest model
#' rf <- ranger::ranger(
#'   y ~ .,
#'   max.depth = 5,
#'   write.forest = TRUE,
#'   data = df1,
#'   num.trees = 10
#' )
#'
#'
#' # Extract tree information for the first tree
#' info <- ranger::treeInfo(rf, tree = 1)
#'
#' # Adjust node indices (1-based indexing)
#' info$nodeID     <- info$nodeID + 1
#' info$leftChild  <- info$leftChild + 1
#' info$rightChild <- info$rightChild + 1
#'
#' # Convert the tree to text format
#' convert_par(info)
#'
#' # Parse the tree into a phylogenetic object
#' ape_tree <- ape::read.tree(text = paste0(convert_par(info), ";"))
#' print(ape_tree)
#'
#'
convert_par <- function(tree, row = 1) {
  if (tree$terminal[row] == TRUE)
    return(paste0(row, ":1"))

  return(paste0(
    "(",
    convert_par(tree = tree, row = tree$leftChild[row]),
    ",",
    convert_par(tree = tree, row = tree$rightChild[row]),
    "):1"
  ))

}
