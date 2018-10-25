#' @title Exploring Kernel Density Estimation

#' @description See how density plots are built from kernels..
#'
#' @rdname kdExplore
#' @usage kdExplore(data, options = NULL,...)
#' @param data A vector of numerical values from which to form the density plot.
#' @param options Options that will be passed to \code{shiny::shinyApp}.
#' @return side effects
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' ## smal cusotm dataset:
#' myData <- c(1, 3, 5, 6, 6.2, 7, 9)
#' kdExplore(myData)
#'
#' ## random exponential data:
#' kdExplore(rexp(50, rate = 0.2))
#' }
kdExplore <-
  function(data, options = NULL) {
    data <- sort(data)
    n <- length(data)
    roughBW <- 1.06 * sd(data) * n^(-0.2)
    sliderMax <- round(roughBW * 3, 3)
    sliderMin <- round(min(data[2:n] - data[1:(n - 1)]) / 10, 3)
    slopOver <- (max(data) - min(data)) / 20
    xAxis <- c(data[1] - slopOver, max(data) + slopOver)

    kernel.factor <- function(kernel) {
      # This function returns the factor c such that
      #              h = c * sigma
      # where sigma is the standard deviation of the kernel, and
      # h is the corresponding bandwidth parameter as conventionally defined.

      # Conventionally h is defined as a scale factor
      # relative to the `standard form' of the kernel, namely the
      # form with support [-1,1], except in the Gaussian case where
      # the standard form is N(0,1).

      # Thus the standard form of the kernel (h=1) has standard deviation 1/c.

      # The kernel with standard deviation 1 has support [-c,c]
      # except for gaussian case.

      switch(kernel,
        gaussian = 1,
        rectangular = sqrt(3),
        triangular = sqrt(6),
        epanechnikov = sqrt(5),
        biweight = sqrt(7),
        cosine = 1 / sqrt(1 / 3 - 2 / pi^2),
        optcosine = 1 / sqrt(1 - 8 / pi^2)
      )
    }

    dkernel <- function(x, kernel, mean, a, n) {
      stopifnot(is.numeric(x))
      y <- abs(x - mean) / a
      dens <-
        switch(kernel,
          gaussian = {
            dnorm(y)
          },
          rectangular = {
            ifelse(y < 1, 1 / 2, 0)
          },
          triangular = {
            ifelse(y < 1, (1 - y), 0)
          },
          epanechnikov = {
            ifelse(y < 1, (3 / 4) * (1 - y^2), 0)
          },
          biweight = {
            ifelse(y < 1, (15 / 16) * (1 - y^2)^2, 0)
          },
          cosine = {
            ifelse(y < 1, (1 + cos(pi * y)) / 2, 0)
          },
          optcosine = {
            ifelse(y < 1, (pi / 4) * cos(pi * y / 2), 0)
          }
        )
      dens / (a * n)
    }
    ## end borrowed code


    kernelDen <- function(x, data, bw, kernel, n) {
      val <- numeric(length(x))
      for (i in 1:length(data)) {
        val <- val + dkernel(x,
          mean = data[i], a = bw * kernel.factor(kernel),
          n = length(data),
          kernel = kernel
        )
      }
      val
    }

    # Define server logic for app
    server <- shinyServer(function(input, output, session) {

      rv <- reactiveValues(
        equation = paste0("f(x) = \\frac{1}{2\\pi}\\mathrm{e}^{-x^2/2}",
                          "\\text{ for }-\\infty < x < \\infty")
      )

      observeEvent(input$kernel, {
        kernel <- input$kernel
        if (kernel == "gaussian") {
          rv$equation <- paste0("f(x) = \\frac{1}{2\\pi}\\mathrm{e}^{-x^2/2}",
                                "\\text{ for }-\\infty < x < \\infty")
        }
        if (kernel == "rectangular") {
          rv$equation <- "f(x) = \\frac{1}{2} \\text{ for } \\vert x \\vert < 1"
        }
        if (kernel == "triangular") {
          rv$equation <- paste0("f(x) = 1- \\vert x \\vert",
                                "\\text{ for } \\vert x \\vert < 1")
        }
        if (kernel == "epanechnikov") {
          rv$equation <- paste0("f(x) = \\frac{3}{4}(1- x^2)",
                                "\\text{ for } \\vert x \\vert < 1")
        }
        if (kernel == "biweight") {
          rv$equation <- paste0("f(x) = \\frac{15}{16}\\left(1- x^2\\right)^2",
                                "\\text{ for } \\vert x \\vert < 1")
        }
        if (kernel == "cosine") {
          rv$equation <- paste0("f(x) = \\frac{1 +\\cos \\pi x}{2}",
                                "\\text{ for } \\vert x \\vert < 1")
        }
        if (kernel == "optcosine") {
          rv$equation <- paste0("f(x) = \\frac{\\pi}{4} \\cos",
                                "\\left(\\frac{\\pi x}{2} \\right)",
                                "\\text{ for } \\vert x \\vert < 1")
        }
      })

      output$density <- renderPlot({
        bw <- input$bw
        ggden <- input$ggden
        kernel <- input$kernel
        p <- ggplot2::ggplot(
          data = data.frame(x = xAxis),
          mapping = ggplot2::aes(x = x)
        )
        for (datum in data) {
          p <- p + ggplot2::stat_function(
            fun = dkernel,
            args = list(
              mean = datum, a = bw * kernel.factor(kernel),
              n = length(data),
              kernel = kernel
            ),
            color = "gray"
          )
        }
        p <- p + ggplot2::stat_function(
          fun = kernelDen,
          args = list(data = data, bw = bw, kernel = kernel),
          n = 1001
        ) +
          ggplot2::geom_rug(data = data.frame(x = data), ggplot2::aes(x = x))
        if (ggden) {
          p <- p + ggplot2::geom_density(
            data = data.frame(x = data),
            ggplot2::aes(x = x), color = "blue",
            kernel = kernel
          )
        }
        p
      })
      output$equation <- renderUI({
        withMathJax(helpText(paste0("The kernal function is:$$",
                                    rv$equation,
                                    "$$")))
      })
    })


    # Define ui for app
    ui <- shinyUI(fluidPage(

      #  Application title
      title = "Kernel Density Estimator",
      titlePanel("Kernel Density Estimator"),

      fluidRow(
        column(
          3,
          sliderInput(
            inputId = "bw", label = "Bandwidth",
            min = sliderMin, max = sliderMax,
            width = "300px",
            value = roughBW,
            round = -3,
            ticks = FALSE
          ),
          selectInput(
            inputId = "kernel", label = "kernel",
            selected = "gaussian",
            choices = c(
              "gaussian" = "gaussian",
              "rectangular" = "rectangular",
              "triangular" = "triangular",
              "epanechnikov" = "epanechnikov",
              "biweight" = "biweight",
              "cosine" = "cosine",
              "optcosine" = "optcosine"
            )
          ),
          checkboxInput(
            inputId = "ggden",
            label = "Show ggplot2 default density curve",
            value = FALSE, width = "300px"
          )
        ),
        column(
          9,
          plotOutput("density"),
          uiOutput("equation")
        )
      )
    )) # end shinyUI

    shiny::shinyApp(ui = ui, server = server)
  } # end kdExplore
