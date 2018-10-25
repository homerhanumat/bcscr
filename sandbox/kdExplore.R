## A Funvtion to Explore Kernal Density Estimation

## Load the necessary packages  ----------------
library(ggplot2)
library(manipulate)


## Define the function     ----------------

kdExplore <- function(data) {
  data <- sort(data)
  n <- length(data)
  roughBW <- 1.06 * sd(data) * n^(-0.2)
  sliderMax <- roughBW * 3
  sliderMin <- min(data[2:n] - data[1:(n - 1)]) / 10
  slopOver <- (max(data) - min(data))/20
  xAxis <- c(data[1] - slopOver, max(data) + slopOver)  
  manipulate(
    {
      kern <- function(x, mean, a, n) dnorm((x - mean)/a)/(a * n)
      kernalDen <- function(x, data, a) {
        val <- numeric(length(x))
        for (i in 1:length(data)) {
          val <- val + kern(x, mean = data[i], a = a, n = length(data))
        }
        val
      }
      p <- ggplot(data = data.frame(x = xAxis), mapping = aes(x = x))
      for (datum in data) {
        p <- p + stat_function(
          fun = kern,
          args = list(mean = datum, a = a, n = length(data)),
          color = "gray"
        )
      }
      p <- p + stat_function(fun = kernalDen, 
                             args = list(data = data, a = a), n = 1001) +
        geom_rug(data = data.frame(x = data), aes(x = x))
      if (ggden) {
        p <- p + geom_density(data = data.frame(x = data), 
                              aes(x = x), color = "blue")
      }
      p
    },
    a = slider(min = sliderMin, max = sliderMax,
              initial = roughBW,
              label = "bandwidth"),
    ggden = checkbox(label = "Show ggplot2's default density curve:")
  )
}


## Try it on a small data set:  ----------

myData <- c(1, 3, 5, 6, 6.2, 7, 9)

## The following function call starts the app:
kdExplore(myData)
## You'll see the kernals, one for each data point,
## in light gray along the bottom.
## Their sum is the kernal density estimator, in black.
## Use the slider to adjust the bandwidth of the kernals.


## Try on some random normal data -----------

normalStuff <- rnorm(50, 5, 2)
kdExplore(normalStuff)


# Try on some random exponential data:  ------------
expStuff <- rexp(50, rate = 0.2)

kdExplore(expStuff)

## Density Curves vs. "True" Distribution ----------

## How does ggplot's kernal density estimator comapre to the
## underlying distribution?
## Run this to see (pdf for exponential is in red)
ggplot(data = data.frame(x = expStuff), aes(x = x)) +
  geom_density(color = "blue") +
  geom_rug() +
  stat_function(fun = dexp, args = list(rate = 0.2), n = 1001,
                color = "red", lwd = 1.5)
  



          