#' @title Number-Needed Simulation

#' @description You pick random numbers from 0 to 1, until their sum exceeds
#' some target number.  What's the expected value of the number of numbers
#' you have to pick?
#'
#' @rdname numberNeededSim
#' @usage numberNeededSim(target = 1, reps = 1000,
#'                 seed = NULL, table = TRUE)
#' @param target the target number
#' @param reps number of simulations to perform
#' @param table Does the user want a table of the results?
#' @param seed The user may provide a seed-value for random-number
#' generation.
#' @return side effects
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' numberNeededSim(seed = 3030)
numberNeededSim <- function(target = 1, reps = 1000,
                            seed = NULL, table = TRUE) {

  if ( !is.null(seed) ) {
    set.seed(seed)
  }

  # define helper function
  numberNeeded <- function(target) {
    mySum <- 0
    count <- 0
    while( mySum < target ) {
      number <- runif(1)
      mySum <- mySum + number
      count <- count + 1
    }
    count
  }

  #perform simulation
  needed <- numeric(reps)
  for (i in 1:reps ) {
    needed[i] <- numberNeeded(target)
  }

  # report results
  if ( table ) {
    print(prop.table(table(needed)))
    cat("\n")
  }
  cat("The expected number needed is about ",
      mean(needed), ".\n", sep = "")
}
