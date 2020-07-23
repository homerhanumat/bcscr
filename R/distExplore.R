#' @title Exploring Major Probability Distributions

#' @description Manipulate parameters of a chosen distribution.
#'
#' @rdname distExplore
#' @usage distExplore(options = NULL)
#' @param options Options that will be passed to \code{shiny::shinyApp}.
#' @return side effects
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' ## distExplore()
distExplore <- function(options = NULL) {

  options(width = 900)

  sideWidth <- 350

  alignCenter <- function(el) {
    htmltools::tagAppendAttributes(
      el,
      style="margin-left:auto;margin-right:auto;width:75%"
      )
  }

  findLimits <- function(qdist, qparams) {
    xlim_opts <- do.call(qdist, c(list(p = c(0, 0.001, 0.999,
                                             1)), qparams))
    dxlim_opts <- diff(xlim_opts)
    xlim <- xlim_opts[2:3]
    if (dxlim_opts[1] < dxlim_opts[2]) {
      xlim[1] <- xlim_opts[1]
    }
    if (dxlim_opts[3] < dxlim_opts[2]) {
      xlim[2] <- xlim_opts[4]
    }
    xlim
  }

  desc <- c(
    "bernoulli" = "One trial; count the number of successes.  Chance of success is p.",
    "binom" = "n trials, count the number of successes. Chance of success is p.",
    "hyper" = paste0("N1 black balls and N2 white balls in an urn.  ",
                     "Draw n balls without replacement and count the ",
                     "number of black balls."),
    "geom" = paste0("Waiting for the first success in a Bernoulli process. ",
                    "Count the number of trials needed."),
    "nbinom" = paste0("Waiting for s successes in a Bernoulli process. ",
                      "Count the number of trials needed."),
    "exp" = paste0("Waiting for the first event in a Poisson process with ",
                   "a rate of lambda events per unit time, ",
                   "theta = 1/lambda average wait between events. ",
                   "Measure how long you wait."),
    "pois" = paste0("Count the number of events in a Poisson pocess with ",
                    "an average of lambda events per unit time. "),
    "gamma" = paste0("Waiting for alpha events in a Poisson process with ",
                     "a rate of lambda events per unit time, ",
                     "theta = 1/lambda average wait between events. ",
                     "Measure how long you wait."),
    "unif" = "Any real number between the min and the max is equally likely.",
    "norm" = "Due to the Central Limit Theorem we meet this distribution a lot! ",
    "beta" = "Quite a flexible distribution."
  )

  ev <- c(
    "bernoulli" = "Expected Value:$$p = ",
    "binom" = "Expected Value:$$np = ",
    "hyper" =  "Expected Value:$$n\\frac{N_1}{N_1+N_2} = ",
    "geom" =  "Expected Value:$$\\frac{1}{p} = ",
    "nbinom" = "Expected Value:$$\\frac{s}{p} = ",
    "exp" = "Expected Value:$$\\theta = ",
    "pois" = "Expected Value:$$\\lambda = ",
    "gamma" = "Expected Value:$$\\alpha \\theta = ",
    "unif" = "Expected Value:$$\\frac{a+b}{2} = ",
    "norm" = "Expected Value:$$\\mu = ",
    "beta" = "Expected Value:$$\\frac{\\alpha}{\\alpha + \\beta} = "
  )

  var <- c(
    "bernoulli" = "Variance:$$p(1- p) = ",
    "binom" = "Variance:$$np(1- p) = ",
    "hyper" =  "Variance:$$np(1- p)\\frac{N_1+N_2 - n}{N_1+N_2-1} = ",
    "geom" =  "Variance:$$\\frac{1-p}{p^2} = ",
    "nbinom" = "Variance:$$\\frac{s(1-p)}{p^2} = ",
    "exp" = "Variance:$$\\theta^2 = ",
    "pois" = "Variance:$$\\lambda = ",
    "gamma" = "Variance:$$\\alpha \\theta^2 = ",
    "unif" = "Variance:$$\\frac{(b - a)^2}{12} = ",
    "norm" = "Variance:$$\\sigma^2 = ",
    "beta" = paste0("Variance:$$\\frac{\\alpha\\beta}","
                    {(\\alpha + \\beta)^2(\\alpha+\\beta+1)} = ")
  )


  pdf <- c(
    "bernoulli" = "pmf:$$f(x)=p^x(1-p)^{1-x},\\quad x=0,1 ",
    "binom" = "pmf:$$f(x)=\\binom{n}{x}p^x(1-p)^{1-x},\\quad x=0,1,\\ldots,n",
    "hyper" = paste0("pmf:$$f(x)=\\frac{\\binom{N_1}{x}\\binom{N_2}{n-x}}",
                     "{\\binom{N_1+N_2}{n}},\\quad x=0,1,\\ldots,n "),
    "geom" =  "pmf:$$f(x)=(1-p)^{x-1}p,\\quad x = 1,2,\\ldots",
    "nbinom" = "pmf:$$f(x)=\\binom{x-1}{s-1}p^s(1-p)^x,\\quad x = s,s+1,\\ldots",
    "exp" = "pdf:$$f(x)=\\frac{1}{\\theta}\\mathrm{e}^{-x/\\theta},\\quad x > 0",
    "pois" = paste0("pmf:$$f(x)=\\mathrm{e}^{-\\lambda}",
                    "\\frac{\\lambda^x}{x!},\\quad x = 0, 1, \\ldots"),
    "gamma" = paste0("pdf:$$f(x)=\\frac{x^{\\alpha - 1}",
                     "\\mathrm{e}^{-\\lambda x}}{\\Gamma(\\alpha)\\theta^{\\alpha}},\\quad ",
                     "x > 0"),
    "unif" = "pdf:$$f(x) = \\frac{1}{b-a},\\quad a < x < b",
    "norm" = paste0("pdf:$$f(x)=\\frac{1}{\\sqrt{2\\pi}\\sigma}",
                    "\\mathrm{e}^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}",
                    "\\quad -\\infty < x < \\infty"),
    "beta" = paste0("pdf:$$f(x)=\\frac{\\Gamma(\\alpha+\\beta)}",
                    "{\\Gamma(\\alpha)\\Gamma(\\beta)}",
                    "x^{\\alpha-1}(1-x)^{\\beta - 1},",
                    "\\quad 0 < x < 1")
  )

  cdf <- c(
    "geom" =  "cdf:$$F(x)=1 - (1-p)^{\\left \\lfloor x\\right \\rfloor}",
    "exp" = "cdf:$$F(x)=1-\\mathrm{e}^{-x/\\theta}"
  )

  mgf <- c(
    "bernoulli" = "mgf:$$M(t)=(1-p)+p\\mathrm{e}^t,\\quad -\\infty < t < \\infty",
    "binom" = paste0("mgf:$$M(t)=\\left((1-p)+p\\mathrm{e}^t\\right)^n,",
                     "\\quad -\\infty < t < \\infty"),
    "geom" =  paste0("mgf:$$M(t)=\\frac{p\\mathrm{e}^t}{1-(1-p)\\mathrm{e}^t},",
                     "\\quad t < -\\ln p"),
    "nbinom" = paste0("mgf:$$M(t)=\\left(\\frac{p\\mathrm{e}^t}",
                      "{1-(1-p)\\mathrm{e}^t}\\right)^s,",
                      "\\quad t < -\\ln p"),
    "exp" = paste0("mgf:$$M(t)=\\frac{1}{1-\\theta t},",
                   "\\quad \\lvert t\\rvert < \\frac{1}{\\theta}"),
    "pois" = paste0("mgf:$$M(t)=\\mathrm{e}^{\\lambda \\mathrm{e}^{\\mathrm{e}^t - 1}},",
                    "\\quad -\\infty < t < \\infty"),
    "gamma" = paste0("mgf:$$M(t)=\\left(\\frac{1}",
                     "{1 - \\theta t}\\right)^{\\alpha},",
                     "\\quad \\lvert t\\rvert < \\frac{1}{\\theta}"),
    "unif" = paste0("mgf:$$M(t) =\\frac{\\mathrm{e}^{bt}-\\mathrm{e}^{at}",
                    "}{t(b-a)},",
                    "\\quad -\\infty < t < \\infty"),
    "norm" = paste0("mgf:$$M(t)=\\mathrm{e}^{\\mu t + \\sigma^2 t^2 / 2},",
                    "\\quad -\\infty < t < \\infty")
  )

  ui <- shinyUI(
    dashboardPage(
      skin = "blue",
      dashboardHeader(title = "Probability Distributions", titleWidth = sideWidth),
      dashboardSidebar(
      #   tags$head(tags$style(HTML('
      #   #description {
      #     margin-left: 5px;
      #     margin-right: 5px
      #   }
      # '))),
        width = sideWidth,
        selectInput(inputId = "dist",
                    label = "Choose a Distribution:",
                    selected = "binom",
                    choices = c(
                      "Bernoulli" = "bernoulli",
                      "Binomial" = "binom",
                      "Hypergeometric" = "hyper",
                      "Geometric" = "geom",
                      "Negative Binomial" = "nbinom",
                      "Poisson" = "pois",
                      "Exponential" = "exp",
                      "Gamma" = "gamma",
                      "Normal" = "norm",
                      "Uniform" = "unif",
                      "Beta" = "beta"
                    ),
                    selectize = FALSE,
                    size = 11, width = sideWidth),
        tags$head(
          tags$style(HTML("

      #description {
        margin-left: 10px;
        margin-right: 10px;
      }

    "))
        ),
        uiOutput("description")
      ),
      dashboardBody(
        fluidRow(
          column(width = 6, plotOutput("p")),
          column(width = 6, plotOutput("c"))
          ),
        br(),
        fluidRow(uiOutput("params"))
      ) # end dashboard body
    ) # end dashboard page
  )

  server <- shinyServer(function(input, output, session) {
    rv <- reactiveValues(
      type = NULL,
      dist = NULL,
      params = NULL,
      ev = NULL,
      var = NULL
    )
    observeEvent(input$dist, {
      dist <- input$dist
      rv$dist <- dist
      if (dist == "binom") {
        rv$params  <- list(size = 100, prob = 0.50)
        rv$type <- "discrete"
        rv$ev <- 100 * 0.50
        rv$var <- 100 * 0.50 * 0.50
      }
      if (dist == "nbinom") {
        rv$params  <- list(size = 3, prob = 0.2)
        rv$type <- "discrete"
        rv$ev <- 3 / 0.2 - 3
        rv$var <- (3 * (1 - 0.2)) / 0.2^2
      }
      if (dist == "geom") {
        rv$params  <- list(prob = 0.2)
        rv$type <- "discrete"
        rv$ev <- 1 / 0.2 - 1
        rv$var <- (1 - 0.2) / 0.2^2
      }
      if (dist == "bernoulli") {
        rv$params  <- list(size = 1, prob = 0.5)
        rv$type <- "discrete"
        rv$ev <- 0.5
        rv$var <- 0.5 * 0.5
      }
      if (dist == "hyper") {
        rv$params  <- list(m = 50, n = 50, k = 20)
        rv$type <- "discrete"
        rv$ev <- 20 * (50 / (50 + 50))
        rv$var <- 20 * 0.5 * 0.5 * (100 - 20)/ 99
      }
      if (dist == "pois") {
        rv$params  <- list(lambda = 3)
        rv$type <- "discrete"
        rv$ev <- 3
        rv$var <- 3
      }
      if (dist == "exp") {
        rv$params  <- list(rate = 0.20)
        rv$type <- "continuous"
        rv$ev <- 1 / 0.20
        rv$var <-  1 / 0.20^2
      }
      if (dist == "gamma") {
        rv$params  <- list(shape = 3, rate = 0.20)
        rv$type <- "continuous"
        rv$ev <- 3 * 1 / 0.20
        rv$var <- 3 * 1 / 0.20^2
      }
      if (dist == "beta") {
        rv$params  <- list(shape1 = 3, shape2 = 1)
        rv$type <- "continuous"
        rv$ev <- 3 / (3 + 1)
        rv$var <- 3/(4^2*5)
      }
      if (dist == "norm") {
        rv$params  <- list(mean = 0, sd = 1)
        rv$type <- "continuous"
        rv$ev <- 0
        rv$var <- 1^2
      }
      if (dist == "unif") {
        rv$params  <- list(min = 0, max = 1)
        rv$type <- "continuous"
        rv$ev <- (0 + 1) / 2
        rv$var <- (1 - 0)^2 / 12
      }
    })
    observe({
      input$size; input$shape; input$m; input$n; input$k
      input$rate; input$prob; input$minmax;
      input$shape1; input$shape2; input$mean; input$sd
      input$lambda
      if (isolate(rv$dist) == "binom") {
        rv$params  <- list(size = input$size, prob = input$prob)
        rv$ev <- input$size * input$prob
        rv$var <- input$size * input$prob * (1 - input$prob)
      }
      if (isolate(rv$dist) == "nbinom") {
        rv$params  <- list(size = input$size, prob = input$prob)
        rv$ev <- input$size  / input$prob
        rv$var <- (input$size*(1 - input$prob))/ input$prob^2
      }
      if (isolate(rv$dist)  == "geom") {
        rv$params  <- list(prob = input$prob)
        rv$ev <- 1 / input$prob
        rv$var <- (1 - input$prob)/ input$prob^2
      }
      if (isolate(rv$dist)  == "bernoulli") {
        rv$params  <- list(size = 1, prob = input$prob)
        rv$ev <- input$prob
        rv$var <- input$prob * (1 - input$prob)
      }
      if (isolate(rv$dist)  == "exp") {
        rv$params  <- list(rate = input$rate)
        rv$ev <- 1 / input$rate
        rv$var <- 1 / input$rate^2
      }
      if (isolate(rv$dist)  == "gamma") {
        rv$params  <- list(shape = input$shape, rate = input$rate)
        rv$ev <- input$shape * 1 / input$rate
        rv$var <- input$shape / input$rate^2
      }
      if (isolate(rv$dist)  == "norm") {
        rv$params  <- list(mean = input$mean, sd = input$sd)
        rv$ev <- input$mean
        rv$var <- input$sd^2
      }
      if (isolate(rv$dist)  == "unif") {
        rv$params  <- list(min= input$minmax[1], max = input$minmax[2])
        rv$ev <- (input$minmax[1] + input$minmax[2]) / 2
        rv$var <- (input$minmax[2] - input$minmax[1])^2 / 12
      }
      if (isolate(rv$dist)  == "beta") {
        s1 <- input$shape1
        s2 <- input$shape2
        rv$params  <- list(shape1 = s1, shape2 = s2)
        rv$ev <- s1 / (s1 + s2)
        rv$var <- s1*s2 / ((s1 + s2)^2*(s1 + s2 + 1))
      }
      if (isolate(rv$dist)  == "pois") {
        rv$params  <- list(lambda = input$lambda)
        rv$ev <- input$lambda
        rv$var <- input$lambda
      }
      if (isolate(rv$dist)  == "hyper") {
        pi <- input$m / (input$m + input$n)
        correction <-  (input$m + input$n - input$k) /  (input$m + input$n - 1)
        rv$params  <- list(m = input$m, n = input$n, k = input$k)
        rv$ev <- input$k * pi
        rv$var <- input$k * pi * (1 - pi) * correction
      }
    })
    output$params <- renderUI({
      dist <- rv$dist
      if (dist == "nbinom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "size", label = "Number of Successes (s, size)",
                          min = 1, max = 100, value = 3, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success (p, prob)",
                          min = 0.01, max = 0.99, value = 0.20, step = 0.01)
            )
          )
        )

      }
      if (dist == "binom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "size", label = "Number of Trials (n, size)",
                          min = 1, max = 1000, value = 100, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success (p, prob)",
                          min = 0.01, max = 0.99, value = 0.50, step = 0.01)
            )
          )
        )
      }
      if (dist == "geom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success (p, prob)",
                          min = 0.01, max = 0.99, value = 0.20, step = 0.01)
            )
          )
        )
      }
      if (dist == "bernoulli") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success (p)",
                          min = 0.01, max = 0.99, value = 0.50, step = 0.01)
            )
          )
        )
      }
      if (dist == "gamma") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "shape", label = "Alpha (shape)",
                          min = 1, max = 100, value = 3, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "rate", label = "Lambda (rate)",
                          min = 0.01, max = 3, value = 0.20, step = 0.01)
            )
          )
        )
      }
      if (dist == "exp") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "rate", label = "Lambda (rate)",
                          min = 0.01, max = 3, value = 0.20, step = 0.01)
            )
          )
        )
      }
      if (dist == "pois") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "lambda", label = "Rate (lambda)",
                          min = 0.1, max = 50, value = 3, step = 0.1)
            )
          )
        )
      }
      if (dist == "unif") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "minmax",
                          label = "Minimum (a, min) and Maximim (b. max)",
                          min = 0, max = 10, value = c(0, 1), step = 0.05)
            )
          )
        )
      }
      if (dist == "norm") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "mean", label = "Mean (mu. mean)",
                          min = -10, max = 10, value = 0, step = 0.1)
            ),
            alignCenter(
              sliderInput(inputId = "sd", label = "SD (sigma, sd)",
                          min = 0.05, max = 5, value = 1, step = 0.05)
            )
          )
        )
      }
      if (dist == "beta") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "shape1", label = "Shape 1 (alpha, shape1)",
                          min = 1, max = 20, value = 3, step = 0.1)
            ),
            alignCenter(
              sliderInput(inputId = "shape2", label = "Shape 2 (beta, shape2)",
                          min = 1, max = 20, value = 1, step = 0.1)
            )
          )
        )
      }
      if (dist == "hyper") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "m", label = "Black Balls (N1)",
                          min = 1, max = 100, value = 50, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "n", label = "White Balls (N2)",
                          min = 1, max = 100, value = 50, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "k", label = "Balls to Draw (n)",
                          min = 1, max = 100, value = 20, step = 1)
            )
          )
        )
      }
    })
    observe({
      input$m; input$n
      if (!(is.null(input$k) | is.null(input$m) | is.null(input$n))) {
        updateSliderInput(session = session, inputId = "k",
                          max = input$m + input$n,
                          value = min(input$k, input$m + input$n))
      }
    })
    output$description <- renderUI({
      tagList(
        p(desc[input$dist]),
        withMathJax(p(paste0(ev[input$dist], round(rv$ev, 3), "$$"))),
        paste0(var[input$dist], round(rv$var, 3), "$$"),
        paste0(pdf[input$dist], "$$"),
        ifelse(!is.na(cdf[input$dist]), paste0(cdf[input$dist],"$$"), ""),
        ifelse(!is.na(mgf[input$dist]), paste0(mgf[input$dist],"$$"), "")
      )
    })
    output$p <- renderPlot({
      params <- rv$params
      n <- length(params)
      assigned <- logical(n)
      for (i in 1:n) {
        assigned[i] <- !is.null(params[[i]])
      }
      if (all(assigned)) {
        dist <- rv$dist
        dist <- ifelse(dist == "bernoulli", "binom", dist)
        params <- rv$params
        xlims <- findLimits(paste0("q", dist), params)
        if (rv$type == "discrete") {
          title <- "Probability Mass Function"
          yLab = "probability"
          if (dist == "geom") {
            x <- xlims[1]:xlims[2] + 1
          } else if (dist == "nbinom") {
            x <- xlims[1]:xlims[2] + input$size
          } else {
            x <- xlims[1]:xlims[2]
          }
        } else {
          title <- "Probability Density Function"
          yLab <- "density"
          x <- seq(xlims[1], xlims[2], length.out = 1001)
        }
        y <- do.call(paste0("d", dist), c(list(x = x), params))
        df <- data.frame(x = x, y = y)
        p <- ggplot(df, aes(x = x, y = y)) +
          labs(y = yLab,
               title = title)
        if (rv$type == "discrete") {
          p <- p +
            geom_point() +
            geom_segment(aes(x = x, xend = x, y = 0, yend = y))
        } else {
          p <- p +
            geom_line()
        }
        p
      }
    })
    output$c <- renderPlot({
      params <- rv$params
      n <- length(params)
      assigned <- logical(n)
      for (i in 1:n) {
        assigned[i] <- !is.null(params[[i]])
      }
      if (all(assigned)) {
        dist <- rv$dist
        dist <- ifelse(dist == "bernoulli", "binom", dist)
        params <- rv$params
        title <- "Cumulative Distribution Function"
        yLab = "probability"
        xlims <- findLimits(paste0("q", dist), params)
        x <- seq(xlims[1], xlims[2], length.out = 1001)
        if (dist == "geom") {
          x <- x + 1
        } else if (dist == "nbinom") {
          x <- x + input$size
        }
        y <- do.call(paste0("p", dist), c(list(q = x), params))
        df <- data.frame(x = x, y = y)
        p <- ggplot(df, aes(x = x, y = y)) +
          geom_line() +
          labs(y = yLab,
               title = title)
        p
      }
    })
  })

  shiny::runApp(list(ui = ui, server = server), launch.browser = TRUE)
}
