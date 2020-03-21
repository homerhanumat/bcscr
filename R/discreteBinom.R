#' @title Drunken-Turtle Simulation

#' @description A drunken turtle starts at the origin and takes unit steps,
#' turning through a randdom angle after each step.  We are interested in
#' the distribution of the number of close-returns to the origin, for a fixed
#' number of steps and a fixed measure of closeness.
#'
#' @rdname drunkenSim
#' @usage drunkenSim(steps = 1000, reps = 10000, close = 0.5,
#'                        seed = NULL, table = FALSE)
#' @param steps the number of steps the turtle will take
#' @param close the distance from the origin that counts as "close"
#' @param reps number of simulations to perform
#' @param table Does the user want a table of the results?
#' @param seed The user may provide a seed-value for random-number
#' generation.
#' @return side effects
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' drunkenSim(seed = 3030)
drunkenSim <- function(steps = 1000, reps = 10000, close = 0.5,
                       seed = NULL, table = FALSE) {
  if ( !is.null(seed) ) {
    set.seed(seed)
  }

  # set up returns vector to store the number of
  # close returns in each repetition:
  returns <- numeric(reps)

  for (i in 1:reps) {
    angle <- runif(steps, 0 , 2*pi)
    xSteps <- cos(angle)
    ySteps <- sin(angle)

    x <- cumsum(xSteps)
    y <- cumsum(ySteps)

    dist <- sqrt(x^2 + y^2)
    closeReturn <- (dist < 0.5)
    returns[i] <- sum(closeReturn)
  }

  if ( table ) {
    cat("Here is a table of the number of close returns:\n\n")
    tab <- prop.table(table(returns))
    print(tab)
    cat("\n")
  }
  cat("The average number of close returns was:  ",
      mean(returns), ".", sep = "")
}
