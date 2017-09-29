#' @title Chance of a Triangle

#' @description Break a unit length at two random points:  what's the chance
#' that the three segments produced can form a triangle?
#'
#' @rdname triangleSim
#' @usage triangleSim(reps = 10000, table = FALSE, seed = NULL)
#' @param reps number of simulations to perform
#' @param table Does the user want a table of the results?
#' @param seed The user may provide a seed-value for random-number
#' generation.
#' @return side effects
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' triangleSim(seed = 3030)
triangleSim <- function(reps = 10000, table = FALSE, seed = NULL) {

  # utility functions
  isTriangle <- function(x, y, z) {
    (x + y > z) & (x +z > y) & (y + z > x)
  }
  makesTriangle <- function(x, y) {
    a <- pmin(x, y)
    b <- pmax(x, y)
    side1 <- a
    side2 <- b - a
    side3 <- 1 - b
    isTriangle(x = side1, y = side2, z = side3)
  }

  # is there a seed?
  if ( !is.null(seed) ) {
    set.seed(seed)
  }

  # simulate!
  cut1 <- runif(reps)
  cut2 <- runif(reps)
  triangle <- makesTriangle(cut1, cut2)

  # report!
  if ( table ) {
    cat("Here is a table of the results:\n\n")
    print(table(triangle))
    cat("\n")
  }
  cat("The proportion of triangles was ", mean(triangle), ".\n", sep = "")
}
