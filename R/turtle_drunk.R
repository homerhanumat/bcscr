#' @title Drunken Turtle (Turtle Graphics)

#' @description A turtle takes steps of a fixed length, but turns at a
#' random angle after each step.
#'
#' @rdname turtle_drunk
#' @usage turtle_drunk(side, step)
#' @param side side-lengths of the containing square
#' @param step length of one turtle step
#' @return side effects
#' @import TurtleGraphics
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' \dontrun{
#' turtle_drunk(side = 100, step = 10)
#' }
turtle_drunk <- function(side, step) {
  turtle_init(side, side, mode = "clip")
  # save (side/2, side/2), the turtle's initial position:
  initial <- turtle_getpos()
  repeat {
    move <- readline(prompt = "Go Again? (enter q to quit):  ")
    if ( move == "q") break
    # pick a random angle to turn by:
    angle <- runif(1, min = 0, max = 360)
    turtle_left(angle)
    turtle_forward(step)
    # get new position, make it the current position:
    cp <- turtle_getpos()
    # print to console:
    print(cp)
    # determine distnce from initial position (round to 3 decimals):
    distance <- round(sqrt((cp[1] - initial[1])^2 + (cp[2] - initial[2])^2),3)
    # prepare message to console,and print it:
    message <- paste0("Distance from starting point is: ", distance)
    cat(message)
  }
  cat("All done!")
}
