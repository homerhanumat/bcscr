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
#' \dontrun{
#' distExplore()
#' }
distExplore <- function(options = NULL) {

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
    "bernoulli" = "One trial; count the number of successes.",
    "binom" = "n trials, count the number of successes.",
    "hyper" = paste0("m white balls and n black balls in an urn.  ",
                     "Draw k balls without replacement and count the ",
                     "number of white balls."),
    "geom" = paste0("Waiting for the first success in a Bernoulli process. ",
                    "Count the number of failures until then."),
    "nbinom" = paste0("Waiting for s successes in a Bernoulli process. ",
                      "Count the number of failures along the way."),
    "exp" = paste0("Waiting for the first event in a Poisson process with ",
                   "an average of lambda events per unit time. ",
                   "Measure how long you wait."),
    "pois" = paste0("Count the number of events in a Poisson pocess with ",
                    "an average of lambda events per unit time. "),
    "gamma" = paste0("Waiting for the alpha events in a Poisson process with ",
                     "an average of lambda events per unit time. ",
                     "Measure how long you wait."),
    "unif" = "Any real number betwen the min and the max is equally likely.",
    "norm" = "Due to the Central Limit Theorem we meet this distribution a lot! ",
    "beta" = "Quite a flexible distribution."
  )

  ui <- shinyUI(
    dashboardPage(
      skin = "blue",
      dashboardHeader(title = "Distributions"),
      dashboardSidebar(
      #   tags$head(tags$style(HTML('
      #   #description {
      #     margin-left: 5px;
      #     margin-right: 5px
      #   }
      # '))),
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
                    size = 11),
        textOutput("description")
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
      params = NULL
    )
    observeEvent(input$dist, {
      dist <- input$dist
      rv$dist <- dist
      if (dist == "binom") {
        rv$params  <- list(size = 100, prob = 0.50)
        rv$type <- "discrete"
      }
      if (dist == "nbinom") {
        rv$params  <- list(size = 3, prob = 0.2)
        rv$type <- "discrete"
      }
      if (dist == "geom") {
        rv$params  <- list(prob = 0.2)
        rv$type <- "discrete"
      }
      if (dist == "bernoulli") {
        rv$params  <- list(size = 1, prob = 0.5)
        rv$type <- "discrete"
      }
      if (dist == "pois") {
        rv$params  <- list(lambda = 4)
        rv$type <- "discrete"
      }
      if (dist == "hyper") {
        rv$params  <- list(m = 50, n = 50, k = 20)
        rv$type <- "discrete"
      }
      if (dist == "pois") {
        rv$params  <- list(lambda = 3)
        rv$type <- "discrete"
      }
      if (dist == "exp") {
        rv$params  <- list(rate = 0.20)
        rv$type <- "continuous"
      }
      if (dist == "gamma") {
        rv$params  <- list(shape = 3, rate = 0.20)
        rv$type <- "continuous"
      }
      if (dist == "beta") {
        rv$params  <- list(shape1 = 3, shape2 = 1)
        rv$type <- "continuous"
      }
      if (dist == "norm") {
        rv$params  <- list(mean = 0, sd = 1)
        rv$type <- "continuous"
      }
      if (dist == "unif") {
        rv$params  <- list(min = 0, max = 1)
        rv$type <- "continuous"
      }
    })
    observe({
      input$size; input$shape; input$m; input$n; input$k
      input$rate; input$prob; input$minmax;
      input$shape1; input$shape2; input$mean; input$sd
      input$lambda
      if (isolate(rv$dist)  %in% c("binom", "nbinom")) {
        rv$params  <- list(size = input$size, prob = input$prob)
      }
      if (isolate(rv$dist)  == "geom") {
        rv$params  <- list(prob = input$prob)
      }
      if (isolate(rv$dist)  == "bernoulli") {
        rv$params  <- list(size = 1, prob = input$prob)
      }
      if (isolate(rv$dist)  == "exp") {
        rv$params  <- list(rate = input$rate)
      }
      if (isolate(rv$dist)  == "gamma") {
        rv$params  <- list(shape = input$shape, rate = input$rate)
      }
      if (isolate(rv$dist)  == "norm") {
        rv$params  <- list(mean = input$mean, sd = input$sd)
      }
      if (isolate(rv$dist)  == "unif") {
        rv$params  <- list(min= input$minmax[1], max = input$minmax[2])
      }
      if (isolate(rv$dist)  == "beta") {
        rv$params  <- list(shape1 = input$shape1, shape2 = input$shape2)
      }
      if (isolate(rv$dist)  == "pois") {
        rv$params  <- list(lambda = input$lambda)
      }
      if (isolate(rv$dist)  == "hyper") {
        rv$params  <- list(m = input$m, n = input$n, k = input$k)
      }
    })
    output$params <- renderUI({
      dist <- rv$dist
      if (dist == "nbinom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "size", label = "Number of Successes",
                          min = 1, max = 100, value = 3, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success",
                          min = 0.01, max = 0.99, value = 0.20, step = 0.01)
            )
          )
        )

      }
      if (dist == "binom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "size", label = "Number of Trials",
                          min = 1, max = 1000, value = 100, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success",
                          min = 0.01, max = 0.99, value = 0.50, step = 0.01)
            )
          )
        )
      }
      if (dist == "geom") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success",
                          min = 0.01, max = 0.99, value = 0.20, step = 0.01)
            )
          )
        )
      }
      if (dist == "bernoulli") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "prob", label = "Probability of Success",
                          min = 0.01, max = 0.99, value = 0.50, step = 0.01)
            )
          )
        )
      }
      if (dist == "gamma") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "shape", label = "Alpha",
                          min = 1, max = 100, value = 3, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "rate", label = "Rate (lambda)",
                          min = 0.01, max = 3, value = 0.20, step = 0.01)
            )
          )
        )
      }
      if (dist == "exp") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "rate", label = "Rate (lambda)",
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
              sliderInput(inputId = "minmax", label = "Min (a) and Max (b)",
                          min = 0, max = 10, value = c(0, 1), step = 0.05)
            )
          )
        )
      }
      if (dist == "norm") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "mean", label = "Mean (mu)",
                          min = -10, max = 10, value = 0, step = 0.1)
            ),
            alignCenter(
              sliderInput(inputId = "sd", label = "SD (sigma)",
                          min = 0.05, max = 5, value = 1, step = 0.05)
            )
          )
        )
      }
      if (dist == "beta") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "shape1", label = "Shape 1 (alpha)",
                          min = 1, max = 20, value = 3, step = 0.1)
            ),
            alignCenter(
              sliderInput(inputId = "shape2", label = "Shape 2 (beta)",
                          min = 1, max = 20, value = 1, step = 0.1)
            )
          )
        )
      }
      if (dist == "hyper") {
        return(
          tagList(
            alignCenter(
              sliderInput(inputId = "m", label = "White Balls (m)",
                          min = 1, max = 100, value = 50, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "n", label = "Black Balls (n)",
                          min = 1, max = 100, value = 50, step = 1)
            ),
            alignCenter(
              sliderInput(inputId = "k", label = "Balls to Draw (k)",
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
    output$description <- renderText({
      desc[input$dist]
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
          x <- xlims[1]:xlims[2]
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

  shiny::shinyApp(ui = ui, server = server)
}
