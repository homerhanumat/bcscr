#' @title Building Random Trees

#' @description Utility function to make small directed acyclic graphs, using
#' the data.tree package. Nodes are
#' provided with randomly-selected numerical values.
#'
#' @rdname make_val_tree
#' @usage make_val_tree(min_children = 1,
#'                      max_children = 4,
#'                      min_depth = 3,
#'                      values = 1:5,
#'                      fillcolor = "palegreen",
#'                      fontcolor = "black",
#'                      seed = NULL)
#' @param min_children Minimum number of children when node will not be a leaf.
#' @param max_children Maximum number of children when node will not be a leaf.
#' @param min_depth Nodes at less than this specified depth will not be leaves.
#' @param values Numerical vector of possible values to associate with each node.
#' @param fillcolor Fill color for nodes.
#' @param fontcolor Fonot color for text showing node value and node name.
#' @param seed Option to set the random seed.
#' @return A tree object of class "Node" and "R6"
#' @import data.tree
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' \dontrun{
#' tr <- make_val_tree(
#'   values = 1:10,
#'   seed = 4040,
#'   fillcolor = "lightblue"
#' )
#' library(DiagrammeR)
#' plot(tr)
#' }
make_val_tree <- function(min_children = 1,
                          max_children = 4,
                          min_depth = 3,
                          values = 1:5,
                          fillcolor = "palegreen",
                          fontcolor = "black",
                          seed = NULL) {

  if (max_children < min_children) {
    stop("max childre should be at least min children")
  }

  if (max(values) == min(values)) {
    stop("more than one node-weight should be possible")
  }

  if (!is.null(seed)) set.seed(seed)
  rv <- sample(values, size = 1)
  rt <- list(value = rv)
  tr <- complete_tree(
    rt = rt,
    min = min_children,
    max = max_children,
    min_depth = min_depth,
    depth = 0,
    values = values
  )
  tr$name <- paste0("R: ", rv)
  tr_n <- data.tree::as.Node(
    tr,
    mode = "explicit",
    childrenName = "children"
  )
  data.tree::SetGraphStyle(tr_n, rankdir = "TB")
  data.tree::SetEdgeStyle(tr_n, arrowhead = "vee",
               color = "grey35", penwidth = 2)
  data.tree::SetNodeStyle(tr_n, style = "filled",
               fillcolor = fillcolor,
               fontcolor = fontcolor)
  tr_n
}



complete_tree <- function(rt, min = 1, max = 4,
                          min_depth = 3,
                          depth = 0,
                          values = 1:10) {
  prob_children <- 1 / max
  have_kids <-
    (depth < min_depth) || (runif(1) <= prob_children)
  if (have_kids) {
    kids <- ifelse(
      min == max,
      min,
      sample(min:max, size = 1)
    )
    lst <- vector(mode = "list", length = kids)
    vals <- sample(values, size = kids, replace = FALSE)
    for (i in 1:kids) {
      child <- Recall(
        rt = rt,
        min = min,
        max = max,
        min_depth = min_depth,
        depth = depth + 1,
        values = values
      )
      child$value <- vals[i]
      lst[[i]] <- child
    }
    names(lst) <- paste0(letters[1:kids], ": ", vals)
    rt$children <- lst
    return(rt)
  } else {
    return(list())
  }
}
