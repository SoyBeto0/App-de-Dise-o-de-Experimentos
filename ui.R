ui <- fluidPage(
  titlePanel("Factorial Diseño de Experimento (2^k)"),
  titlePanel("Diseño Factorial (2^K)"),

  sidebarLayout(
    sidebarPanel(
      numericInput("rep", "Number of Replications:", 2, min = 1, max = 10),
      selectInput("factorA", "Factor A Levels:", choices = c("Low" = -1, "High" = 1)),
      selectInput("factorB", "Factor B Levels:", choices = c("Low" = -1, "High" = 1)),
      actionButton("run", "Run Experiment")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Design", tableOutput("designTable")),
        tabPanel("Results", tableOutput("resultsTable")),
        tabPanel("Analysis", verbatimTextOutput("anovaOutput")),
        tabPanel("Plot", plotOutput("interactionPlot"))
      )
    )
  )
)
