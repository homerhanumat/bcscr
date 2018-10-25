#' @title Exploring Quantile-Quantile Plots

#' @description Understand why qq-plots bend the way they do.
#'
#' @rdname qqExplore
#' @usage qqExplore(data, options = NULL)
#' @param data A vector of numerical values from which to form the qq-plot.
#' @param options Options that will be passed to \code{shiny::shinyApp}.
#' @return side effects
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' ## bimodal data:
#' bimodal <- c(rnorm(50, 5, 1), rnorm(50, 10, 1))
#' qqExplore(bimodal)
#'
#' ## random exponential data:
#' qqExplore(rexp(300, rate = 0.2))
#' }
qqExplore <-
  function(data, options = NULL) {
    data <- sort(data)
    n <- length(data)
    qs <- 1:n/(n + 1)
    normals <- qnorm(qs)

    qqfn <- function(x) {
      mean(data) + stats::sd(data) * x
    }

    alphaLevel <- function(x) {
      if ( x <= 520 ) {
        return(exp( 0.01 * (300 - x)) / (exp(0.01 * (300 - x)) + 1))
      } else {
        return(0.1)
      }
    }

    # Define server logic for app
    server <- shinyServer(function(input, output, session) {

      output$qqplot <- renderPlot({
        if (is.null(input$i)) {
          i <- floor(n/2)
        } else {
          i <- input$i
        }
        less <- ifelse(1:n <= i, "less", "more")
        df <- data.frame(x = normals, y = data, less = less)
        p <-
          ggplot2::ggplot(df, aes(x, y)) +
          ggplot2::geom_point(alpha = alphaLevel(n),
                              ggplot2::aes(color = less)) +
          ggplot2::geom_point(x = normals[i], y= data[i]) +
          ggplot2::geom_segment(x = normals[i], y = data[i],
                       xend = normals[i], yend = 1.5* max(data),
                       lty = 3) +
          ggplot2::geom_rug(color = "steelblue",
                            size = 0.5, sides = "tr",
                            alpha = 0.5) +
          ggplot2::geom_segment(x = normals[i], y = data[i],
                       xend = 1.5 * max(normals), yend = data[i],
                       lty = 3) +
          ggplot2::stat_function(fun = qqfn) +
          ggplot2::labs(x = "theoretical", y = "sample") +
          ggplot2::theme(legend.position = "none")
        ggExtra::ggMarginal(p, fill = "burlywood")
      })
      output$help <- renderUI({
        denom <- as.character(n + 1)
        withMathJax(helpText(paste0("Choose i for the i/",
                                    denom,
                                    " -th quantile:")))
      })
      output$i <- renderUI({
        sliderInput(inputId = "i", label = "i",
                    min = 1, max = n, step = 1, value = floor(n/2))
      })
    })


    # Define ui for app
    ui <- shinyUI(fluidPage(

      #  Application title
      title = "QQ-Plot Explorer",
      titlePanel("QQ-Plot Explorer"),

      fluidRow(
        column(
          3,
          uiOutput("help"),
          uiOutput("i")
        ),
        column(
          9,
          plotOutput("qqplot")
        )
      )
    )) # end shinyUI

    shiny::shinyApp(ui = ui, server = server)
  } # end qqExplore