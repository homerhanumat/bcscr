#' @title Bouncing Turtle (Turtle Graphics)

#' @description A turtle walks randomly, but bounces back from the edge
#' of its containing field.
#'
#' @rdname turtle_bounce
#' @usage turtle_bounce(side = 60, step= 10)
#' @param side side-lengths of the containing square
#' @param step length of one turtle step (side-length/2 must be
#' a multiple of step).
#' @return side effects
#' @import TurtleGraphics
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' \dontrun{
#' turtle_bounce(side = 80, step = 10)
#' }
turtle_bounce <- function(side = 60, step= 10) {
  if ( (side/2) %% step != 0 ) {
    stop("Side-length divided by two must be a multiple of step.")
  }
  bounds <- c(0, side)
  turtle_init(side, side, mode = "clip")
  origin <- turtle_getpos()
  cp <- turtle_getpos()
  repeat {
    move <- readline(prompt = "Go Again? (enter q to quit):  ")
    if ( move == "q") break
    x <- cp["x"]
    y <- cp["y"]
    if (x %in% bounds | y %in% bounds) {
      angle <- 180
    } else {
      angle <- sample(c(0,90,180,270), 1)
    }
    turtle_right(angle)
    turtle_forward(step)
    cp <- round(turtle_getpos(), 0)
    print(cp)
  }
  cat("All done!")
}
