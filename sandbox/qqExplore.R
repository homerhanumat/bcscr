library(ggplot2)
library(ggExtra)
n <- 300
data <- sort(rexp(n, rate = 0.2))
n <- length(data)
qs <- 1:n/(n + 1)
normals <- qnorm(qs)
xvals <- seq(-3, 3, length.out = length(data))
i <- 200
less <- ifelse(1:n <= i, "less", "more")
df <- data.frame(x = normals, y = data, less = less)

qqfn <- function(x) {
  mean(data) + sd(data) * x
}
p <-
  ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.5, size = 0.5, aes(color = less)) + 
  geom_point(x = normals[i], y= data[i]) +
  geom_segment(x = normals[i], y = data[i],
               xend = normals[i], yend = 1.5* max(data),
               lty = 3) +
  geom_rug(color = "steelblue", size = 0.5,sides = "tr") +
  geom_segment(x = normals[i], y = data[i],
               xend = 1.5 * max(normals), yend = data[i],
               lty = 3) + 
  stat_function(fun = qqfn) +
  labs(x = "theoretical", y = "sample") +
  theme(legend.position = "none")
ggMarginal(p, fill = "burlywood")

