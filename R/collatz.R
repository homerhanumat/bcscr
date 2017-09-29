#' @title Collatz Numbers

#' @description Find and graph the Collatz sequence from a given initial number.
#'
#' @rdname collatz
#' @usage collatz(n, limit = 10000)
#' @param n the initial integer
#' @param limit maximum number of members of the Collatz sequence to compute
#' @return side effects
#' @import ggplot2
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' \dontrun{
#' collatz(1757)
#' }
collatz <- function(n, limit = 10000) {
  # validation:
  n <- suppressWarnings(as.integer(n))
  isValid <- !is.na(n) && n > 1
  if (!isValid ) {
    stop("Need an integer bigger than 1.  Try again.")
  }
  # define collatzRule:
  collatzRule <- function(m) {
    if ( m %% 2 == 0) {
      return(m/2)
    } else {
      return(3*m + 1)
    }
  }

  # On with the show!
  # Record initial number because we will change it:
  initial <- n
  numbers <- numeric(limit)
  counter <- 0
  while ( n > 1 & counter < limit) {
    counter <- counter + 1
    numbers[counter] <- n
    n <- collatzRule(n)
  }
  howMany <- min(counter, limit)
  cat("The Collatz sequence has ", howMany, " elements.\n", sep = "")
  show <- readline("Do you want to see it (y/n)?  ")
  if ( show == "y" ) {
    print(numbers[1:howMany])
  }
  plotTitle <- paste0("Collatz Sequence for n = ", initial)
  steps <- 1:howMany
  p <- ggplot(mapping = aes(x = steps, y = numbers[1:howMany])) +
    geom_point() + geom_line() +
    labs( x = "Step", y = "Collatz Value at Step",
          title = plotTitle)
  print(p)
}
