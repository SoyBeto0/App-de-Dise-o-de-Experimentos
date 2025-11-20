
server <- function(input, output) {

  factorial_design <- reactive({
    expand.grid(
      A = c(-1, 1),
      B = c(-1, 1),
      rep = 1:input$rep
    )
  })

  simulate_results <- reactive({
    data <- factorial_design()
    set.seed(123)
    data$Y <- with(data, 10 + 5*A + 3*B + 2*A*B + rnorm(nrow(data), 0, 1))
    return(data)
  })

  perform_anova <- reactive({
    data <- simulate_results()
    model <- aov(Y ~ A * B, data = data)
    summary(model)
  })

  render_interaction_plot <- function(data) {
    with(data, interaction.plot(A, B, Y, 
      main = "Interaction Plot: A x B",
      xlab = "Factor A", ylab = "Response (Y)", col = c("blue","red")
    ))
  }

  output$designTable <- renderTable({ factorial_design() })
  output$resultsTable <- renderTable({ simulate_results() })
  output$anovaOutput <- renderPrint({ perform_anova() })
  output$interactionPlot <- renderPlot({ render_interaction_plot(simulate_results()) })
}
