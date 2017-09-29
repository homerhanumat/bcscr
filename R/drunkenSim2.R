#' @title Drunken-Turtle Simulation (Graph Version)

#' @description A drunken turtle starts at the origin and takes unit steps,
#' turning through a randdom angle after each step.  We graph the distance
#' from the origin as a function of step-number.
#'
#' @rdname drunkenSim2
#' @usage drunkenSim2(steps = 1000, seed = NULL)
#' @param steps the number of steps the turtle will take
#' @param seed The user may provide a seed-value for random-number
#' generation.
#' @return side effects
#' @import ggplot2
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' \dontrun{
#' drunkenSim2(seed = 3030)
#' }
drunkenSim2 <- function(steps = 1000, seed = NULL) {
  if ( !is.null(seed) ) {
    set.seed(seed)
  }

  angle <- runif(steps, 0 , 2*pi)
  xSteps <- cos(angle)
  ySteps <- sin(angle)
  x <- cumsum(xSteps)
  y <- cumsum(ySteps)
  dist <- sqrt(x^2 + y^2)
  plotTitle <- paste0("First ", steps, " Steps of the Drunken Turtle")
  p <- ggplot(mapping = aes(x = 1:steps, y=dist)) + geom_line() +
    labs(x = "Step", y = "Distance From Origin",
         title = plotTitle)
  print(p)
}
