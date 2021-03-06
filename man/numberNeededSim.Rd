% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numberNeededSim.R
\name{numberNeededSim}
\alias{numberNeededSim}
\title{Number-Needed Simulation}
\usage{
numberNeededSim(target = 1, reps = 1000,
                seed = NULL, table = TRUE)
}
\arguments{
\item{target}{the target number}

\item{reps}{number of simulations to perform}

\item{seed}{The user may provide a seed-value for random-number
generation.}

\item{table}{Does the user want a table of the results?}
}
\value{
side effects
}
\description{
You pick random numbers from 0 to 1, until their sum exceeds
some target number.  What's the expected value of the number of numbers
you have to pick?
}
\examples{
numberNeededSim(seed = 3030)
}
\author{
Homer White \email{homerhanumat@gmail.com}
}
