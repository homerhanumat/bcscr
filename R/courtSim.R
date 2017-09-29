#' @title Appeals Court Simulation

#' @description In this version all five judges vote independently.
#'
#' @rdname courtSim
#' @usage courtSim(reps = 10000,
#'                 seed = NULL,
#'                 table = FALSE,
#'                 probs = c(0.95, 0.94, 0.90, 0.90, 0.80))
#' @param reps number of simulations to perform
#' @param table Does the user want a table of the results?
#' @param seed The user may provide a seed-value for random-number
#' generation.
#' @param probs Chance for each judge to make the right decision on any
#' given case.
#' @return side effects
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' courtSim(seed = 3030)
courtSim <- function(reps = 10000,
                     seed = NULL,
                     table = FALSE,
                     probs = c(0.95, 0.94, 0.90, 0.90, 0.80)) {

  if ( !is.null(seed) ) {
    set.seed(seed)
  }

  # get the probabilities
  aProb <- probs[1]
  bProb <- probs[2]
  cProb <- probs[3]
  dProb <- probs[4]
  eProb <- probs[5]

  # simulate decisions of each judge:
  a <- rbinom(n = reps, size = 1, prob = aProb)
  b <- rbinom(n = reps, size = 1, prob = bProb)
  c <- rbinom(n = reps, size = 1, prob = cProb)
  d <- rbinom(n = reps, size = 1, prob = dProb)
  e <- rbinom(n = reps, size = 1, prob = eProb)

  # count the number of correct votes in each case:
  correctVotes <- a + b + c + d + e

  # determine whether court decided correctly, in each case:
  courtCorrect <- (correctVotes >= 3)

  # record results
  if ( table ) {
    cat("Here is a table of the results:\n\n")
    print(table(courtCorrect))
    cat("\n")
  }
  cat("The proportion of times the court was correct was ",
      mean(courtCorrect), ".\n", sep = "")
}
