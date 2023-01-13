#' bcscr
#'
#' Functions and datasets to accompany the text Beginning Computer Science with R
#'
#'
#' @name bcscr-package
#' @aliases bcscr bcscr-package
#' @docType package
#' @title Beginning Computer Science with R
#' @author Homer White (\email{homerhanumat@gmail.com})
#' @references
#' \url{https://homerhanumat.github.io/r-notes}
#'
#' @keywords package internal
#' @import ggplot2 TurtleGraphics R6 shiny shinydashboard data.tree
#' @importFrom ggExtra ggMarginal
#' @importFrom stats rbinom
#' @importFrom stats runif
#' @importFrom stats dnorm sd qnorm
#' @importFrom purrr map map_dfr
#' @importFrom htmltools tagAppendAttributes
NULL

## quiets concerns of R CMD check kd and qq Explore apps:
if(getRversion() >= "2.15.1")  utils::globalVariables(c("x", "y"))
