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

  ui <- shinyUI(
    dashboardPage(
      skin = "blue",
      dashboardHeader(title = "Major Probability Distributions"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Bernoulli", tabName = "bernoulli"),
          menuItem("Binomial", tabName = "binom"),
          menuItem("Hypergeometric", tabName = "hyper"),
          menuItem("Geometric", tabName = "geom"),
          menuItem("Negative Binomial", tabName = "nbinom"),
          menuItem("Poisson", tabName = "pois"),
          menuItem("Exponential", tabName = "exp"),
          menuItem("Gamma", tabName = "gamma"),
          menuItem("Normal", tabName = "norm")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "bernoulli",
            fluidRow(
              column(
                width = 3,
                sliderInput(
                  inputId = "bernoullipi", label = "Success Probability",
                  min = 0.01, max = 0.99, value = 0.50, step = 0.01
                )
              ), # end first column
              column(
                width = 9,
                plotOutput("bernoulliplot")
              ) # end second column
            ) # end row
          ), # end bernoulli tab
          tabItem(
            tabname = "binom",
            fluidRow()
          ),
          tabItem(
            tabname = "hyper",
            fluidRow()
          ),
          tabItem(
            tabname = "geom",
            fluidRow()
          ),
          tabItem(
            tabname = "nbinom",
            fluidRow()
          ),
          tabItem(
            tabname = "pois",
            fluidRow()
          ),
          tabItem(
            tabname = "exp",
            fluidRow()
          ),
          tabItem(
            tabname = "gamma",
            fluidRow()
          ),
          tabItem(
            tabname = "norm",
            fluidRow()
          )
        ) # end tabitems
      ) # end dashboard body
    ) # end dashboard page
  )

  server <- shinyServer(function(input, output) {
    output$bernoulliplot <- renderPlot({
      prob <- input$bernoullipi
      print(class(prob))
      ggformula::gf_dist(dist = "binom", params = list(size = 1, prob = prob))
    })
  })

  shiny::shinyApp(ui = ui, server = server)
}
