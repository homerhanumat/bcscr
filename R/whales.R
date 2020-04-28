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

#' @title Whales in an Ocean

#' @description R6 Object for simulating a population of whales.
#'
#' @rdname Ocean
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for simulation.
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @section Properties:
#' \itemize{
#' \item{\code{dimensions}: }{A vector of length two giving the
#' dimensions of the ocean.}
#' \item{\code{males}: }{A list of R6 objects of class Male
#' containing the current population of male whales.}
#' \item{\code{females}: }{A list of R6 objects of class Female
#' containing the current population of female whales.}
#' \item{\code{malePop}: }{Current number of males in the population.}
#' \item{\code{femalePop}: }{Current number of females in the population.}
#' \item{\code{starveParameter}: }{Helps determine probability for each
#' whale to die by starvation in the current generation.}
#' \item{\code{distance}: }{Computes distance between any two whales.}
#' }
#' @section Methods:
#' \itemize{
#' \item{\code{new}: }{Instantiates an Ocean object.  Parameters are:}
#' \itemize{
#' \item{\code{dims}: }{A numeric vector of length 2 setting the length
#' and width
#'  of the ocean.}
#' \item{\code{males}: }{An integer giving the number of males (to be
#' created with
#' defaults) or a list of \code{\link{Male}} whale objects.}
#' \item{\code{females}: }{An integer giving the number of females (to be
#' created with
#' defaults) or a list of \code{\link{Female}} whale objects.}
#' \item{\code{starve}: }{A non-negative number, used to determine
#' probability that an individual starves in a given
#' generation. The larger the
#' value, the lower the carrying-capacity of the population
#' will be.}
#' }
#' \item{\code{advance}: }{Advances the simulation by one generation.
#' Takes no arguments.}
#' \item{\code{plot}: }{Plots the current population.  Takes no arguments.}
#' }
#' @examples
#' \dontrun{
#' library(ggplot2)
#' oceanSim <- function(
#'   steps = 100, males = 10,
#'   females = 10, starve = 5,
#'   animate = TRUE, seed = NULL
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
#'   pop <- population[1:i]
#'   df <- data.frame(time = 1:length(pop),
#'                   pop)
#'   ggplot(df, aes(x = time, y = pop)) + geom_line() +
#'     labs(x = "Time", y = "Whale Population")
#' }
#' oceanSim(males = initialMales, females = initialFemales)
#'}
Ocean <- R6::R6Class("Ocean",
                 public = list(
                   dimensions = NULL,
                   males = NULL,
                   females = NULL,
                   malePop = NULL,
                   femalePop = NULL,
                   starveParameter = NULL,
                   distance = function(a, b) {
                     sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
                   },
                   initialize = function(dims = c(100, 100),
                                         males = 10,
                                         females = 10,
                                         starve = 5) {
                     self$dimensions <- dims
                     xMax <- dims[1]
                     yMax <- dims[2]
                     if (mode(males) == "list") {
                       maleWhales <- purrr::map(males, function(x) x$clone())
                       self$malePop <- length(males)
                     } else {
                       maleWhales <- replicate(
                         males,
                         Male$new(age = 10,
                                  position = c(runif(1, 0, xMax),
                                               runif(1, 0, yMax))))
                       self$malePop <- males
                     }
                     if (mode(females) == "list") {
                       femaleWhales <- purrr::map(females, function(x) x$clone())
                       self$femalePop <- length(females)
                     } else {
                       femaleWhales <- replicate(
                         females,
                         Female$new(age = 10,
                                    position = c(runif(1, 0, xMax),
                                                 runif(1, 0, yMax))))
                       self$femalePop <- females
                     }
                     self$males <- maleWhales
                     self$females <- femaleWhales
                     self$starveParameter <- starve
                   },
                   starvationProbability = function(popDensity) {
                     self$starveParameter * popDensity
                   }
                 ))


Ocean$set("public",
          "advance",
          function() {
            malePop <- self$malePop
            femalePop <- self$femalePop
            population <- malePop + femalePop
            if ( population == 0 ) {
              return(NULL)
            }
            males <- self$males
            females <- self$females
            babyMales <- list()
            babyFemales <- list()
            if ( malePop > 0 && femalePop > 0 ) {
              for ( female in females ) {
                if ( female$age >= female$maturity &&
                     female$timeToFertility <= 0 &&
                     female$maleNear(males = males,
                                     dist = self$distance)) {
                  outcome <- female$mate()
                  if ( outcome == "male" ) {
                    baby <- Male$new(age = 0, position = female$position)
                    babyMales <- c(babyMales, baby)
                  } else {
                    baby <- Female$new(age = 0, position = female$position)
                    babyFemales <- c(babyFemales, baby)
                  }
                }
              }
            }

            # augment the male and female lists if needed:
            lmb <- length(babyMales); lfb <- length(babyFemales);

            # throw in the babies:
            if ( lmb > 0 ) {
              males <- c(males, babyMales)
            }
            if ( lfb > 0 ) {
              females <- c(females, babyFemales)
            }

            # revise population for new births:
            population <- length(males) + length(females)

            # starve some of them, maybe:
            popDen <- population/prod(self$dimensions)
            starveProb <- self$starvationProbability(popDensity = popDen)
            maleDead <- logical(length(males))
            femaleDead <- logical(length(females))
            # starve some males
            for ( i in seq_along(maleDead) ) {
              male <- males[[i]]
              maleDead[i] <- (runif(1) <= starveProb)
              male$age <- male$age + 1
              if ( male$age >= male$lifespan ) maleDead[i] <- TRUE
              if ( maleDead[i] ) next
              # if whale is not dead, he should move:
              male$move(dims = self$dimensions)
            }
            # starve some females
            for ( i in seq_along(femaleDead) ) {
              female <- females[[i]]
              femaleDead[i] <- (runif(1) <= starveProb)
              female$age <- female$age + 1
              if ( female$age >= female$lifespan ) femaleDead[i] <- TRUE
              if ( femaleDead[i] ) next
              if ( female$sex == "female" ) {
                female$timeToFertility <- female$timeToFertility - 1
              }
              # if female is not dead, she should move:
              female$move(dims = self$dimensions)
            }

            # revise male and female whale lists:
            malePop <- sum(!maleDead)
            self$malePop <- malePop
            femalePop <- sum(!femaleDead)
            self$femalePop <- femalePop
            if ( malePop > 0 ) {
              self$males <- males[!maleDead]
            } else {
              self$males <- list()
            }
            if ( femalePop > 0 ) {
              self$females <- females[!femaleDead]
            } else {
              self$females <- list()
            }
          }, overwrite = TRUE)

Ocean$set("public",
          "plot",
          function() {
            males <- self$males
            females <- self$females
            whales <- c(males, females)
            if ( length(whales) == 0) {
              plot(0,0, type = "n", main = "All Gone!")
              box(lwd = 2)
              return(NULL)
            }
            len <- length(whales)
            df <- purrr::map_dfr(whales, function(x) {
              list(x = x$position[1],
                   y = x$position[2],
                   sex = as.numeric(x$sex == "male"),
                   mature = as.numeric(x$age >= x$maturity))
            })
            df$color <- ifelse(df$sex == 1, "red", "green")
            # mature whales have cex = 3, immature whales cex 0.7
            df$size <- ifelse(df$mature == 1, 1.3, 0.7)
            with(df,
                 plot(x, y, xlim = c(0, self$dimensions[1]),
                      ylim = c(0, self$dimensions[1]), pch = 19, xlab = "",
                      ylab = "", axes = FALSE, col = color, cex = size,
                      main = paste0("Population = ", nrow(df))))
            box(lwd = 2)
          }, overwrite = TRUE)
