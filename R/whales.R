#' @title Whales in an Ocean

#' @description R6 Objects for modelling a whale.  Female and Male
#' inherit from whale.  Use Female and Male  if you want to
#' provide custom lists of male and female whales when you instantiate
#' an ocean.
#' @rdname Whale
#' @name Whale
#' @aliases Female Male
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for simulation.
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @section Methods:
#' \itemize{
#' \item{\code{new}: }{Instantiates an Whale object.  Parameters are:}
#' \itemize{
#' \item{\code{position}: }{A numeric vector of length 2 giving the initial
#' position.  (Make sure that it's within the dimensions of the ocean.)}
#' \item{\code{age}: }{Initial age of the whale.}
#' \item{\code{lifespan}: }{Lifespan of the whale.}
#' \item{\code{range}: }{Distance at which a female can detect an
#' eligible male.}
#' \item{\code{maturity}: }{Age of whale at which reproduction is possible.}
#' \item{\code{stepSize}: }{Number of units the whale move sin each gneration.}
#' }
#' }
#' @examples
#' \dontrun{
#' initialMales <- vector(mode = "list", length = 10)
#' ages <- c(rep(3, 5), c(rep(10, 5)))
#' for (i in 1:10) {
#'   initialMales[[i]] <- Male$new(
#'     position = runif(2, min = 0, max = 100),
#'     age = ages[i],
#'     lifespan = 40,
#'     range = 12,
#'     maturity = 10,
#'     stepSize = 7
#'   )
#' }
#' initialFemales <- vector(mode = "list", length = 10)
#' for (i in 1:10) {
#'   initialFemales[[i]] <- Female$new(
#'     position = runif(2, min = 0, max = 100),
#'     age = ages[i],
#'     lifespan = 40,
#'     maturity = 10,
#'     range = 12,
#'     stepSize = 3
#'   )
#' }
#'
#' library(ggplot2)
#' oceanSim <- function(
#'   steps = 100,
#'   males = 10,
#'   females = 10,
#'   starve = 5,
#'   animate = TRUE,
#'   seed = NULL
#'   ) {
#'  if ( !is.null(seed) ) {
#'    set.seed(seed)
#'   }
#'   ocean <- Ocean$new(dims = c(100, 100), males = males,
#'                      females = females, starve = starve)
#'   population <-numeric(steps)
#'   for ( i in 1:steps ) {
#'     population[i] <- ocean$malePop + ocean$femalePop
#'     if ( animate ) ocean$plot()
#'     if ( population[i] == 0 ) break
#'     ocean$advance()
#'     if ( animate ) {
#'         ocean$plot()
#'         Sys.sleep(0.5)
#'       }
#'   }
#'   pop <- population[1:i]
#'   df <- data.frame(time = 1:length(pop),
#'                   pop)
#'   ggplot(df, aes(x = time, y = pop)) + geom_line() +
#'     labs(x = "Time", y = "Whale Population")
#' }
#' oceanSim(males = initialMales, females = initialFemales)
#'}
Whale <- R6::R6Class("Whale",
                 public = list(
                   position = NULL,
                   age = NULL,
                   lifespan = NULL,
                   range = NULL,
                   maturity = NULL,
                   stepSize = NULL,
                   initialize = function(position = NA, age = 3,
                                         lifespan = 40, range = 5,
                                         maturity = 10, stepSize = 5) {
                     self$position <- position
                     self$age <- age
                     self$lifespan <- lifespan
                     self$range <- range
                     self$maturity <- maturity
                     self$stepSize <- stepSize
                   }
                 ))

Whale$set("public",
          "move",
          function(dims, r = self$stepSize) {
            xMax <- dims[1]
            yMax <- dims[2]
            repeat {
              theta <- runif(1, min = 0, max = 2*pi)
              p <- self$position + r * c(cos(theta), sin(theta))
              within <- (p[1] > 0 && p[1] < xMax) && (p[2] > 0 && p[2] < yMax)
              if ( within ) {
                self$position <- p
                break
              }
            }
          }, overwrite = TRUE)

#' @rdname Whale
#' @name Male
#' @export
Male <- R6::R6Class("Male",
                inherit = Whale,
                public = list(
                  sex = "male"
                ))
#' @rdname Whale
#' @name Female
#' @export
Female <- R6::R6Class("Female",
                  inherit = Whale,
                  public = list(
                    sex = "female",
                    timeToFertility = 0,
                    infertilityPeriod = 5
                  ))

Female$set("public",
           "maleNear",
           function(males, dist) {
             foundOne <- FALSE
             for ( male in males ) {
               near <- dist(male$position, self$position) < self$range
               mature <- (male$age >= male$maturity)
               if ( near && mature ) {
                 foundOne <- TRUE
                 break
               }
             }
             foundOne
           }, overwrite = TRUE)

Female$set("public",
           "mate",
           function() {
             babySex <- sample(c("female", "male"), size = 1)
             self$timeToFertility <- self$infertilityPeriod
             return(babySex)
           }, overwrite = TRUE)

