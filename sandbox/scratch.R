library(bcscr)
library(ggplot2)
initialMales <- vector(mode = "list", length = 10)
ages <- c(rep(3, 5), c(rep(10, 5)))
for (i in 1:10) {
  initialMales[[i]] <- Male$new(
    position = runif(2, min = 0, max = 100),
    age = ages[i],
    lifespan = 40,
    range = 12,
    maturity = 10,
    stepSize = 7
  )
}
initialFemales <- vector(mode = "list", length = 10)
for (i in 1:10) {
  initialFemales[[i]] <- Female$new(
    position = runif(2, min = 0, max = 100),
    age = ages[i],
    lifespan = 40,
    range = 12,
    maturity = 10,
    stepSize = 3
  )
}

oceanSim <- function(steps = 100, males = initialMales,
                     females = initialFemales,
                     starve = 5, animate = TRUE, seed = NULL) {
  if ( !is.null(seed) ) {
    set.seed(seed)
  }
  ocean <- Ocean$new(dims = c(100, 100), males = males,
                     females = females, starve = starve)
  population <-numeric(steps)
  for ( i in 1:steps ) {
    population[i] <- ocean$malePop + ocean$femalePop
    if ( animate ) ocean$plot()
    if ( population[i] == 0 ) break
    ocean$advance()
    if ( animate ) Sys.sleep(0.5)
  }
  pop <- population[1:i]
  df <- data.frame(time = 1:length(pop),
                   pop)
  ggplot(df, aes(x = time, y = pop)) + geom_line() +
    labs(x = "Time", y = "Whale Population")
}
oceanSim(steps = 10, males = 5, females = 5, animate = TRUE)
