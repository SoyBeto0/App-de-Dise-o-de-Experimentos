library(shiny)

# ================================
# Función para calcular un efecto
# ================================
calcular_efecto <- function(df, vars, respuesta = "Y") {
  # vars: c("A"), c("B"), c("A","B"), etc.
  # producto de los signos (+1, -1)
  signo <- apply(df[, vars, drop = FALSE], 1, prod)

  # medias cuando el producto de signos es +1 y -1
  medias <- tapply(df[[respuesta]], signo, mean)

  data.frame(
    Termino     = paste(vars, collapse = ""),
    Media_mas   = as.numeric(medias["1"]),
    Media_menos = as.numeric(medias["-1"]),
    Efecto      = as.numeric(medias["1"] - medias["-1"])
  )
}

server <- function(input, output, session) {

  # ------------------------------
  # Diseño factorial 2^2 con rep
  # ------------------------------
  factorial_design <- reactive({
    expand.grid(
      A   = c(-1, 1),
      B   = c(-1, 1),
      rep = 1:input$rep
    )
  })

  # ------------------------------
  # Simular resultados Y
  # ------------------------------
  simulate_results <- reactive({
    data <- factorial_design()
    set.seed(123)
    data$Y <- with(
      data,
      10 + 5*A + 3*B + 2*A*B + rnorm(nrow(data), 0, 1)
    )
    data
  })

  # ------------------------------
  # ANOVA
  # ------------------------------
  perform_anova <- reactive({
    data  <- simulate_results()
    model <- aov(Y ~ A * B, data = data)
    summary(model)
  })

  # ------------------------------
  # Gráfica de interacción
  # ------------------------------
  render_interaction_plot <- function(data) {
    with(
      data,
      interaction.plot(
        A, B, Y,
        main = "Interaction Plot: A x B",
        xlab = "Factor A",
        ylab = "Response (Y)",
        col = c("blue", "red")
      )
    )
  }

  # ------------------------------
  # Efectos principales e interacción AB
  # ------------------------------
  efectos_df <- reactive({
    d <- simulate_results()
    req(d)

    lista_efectos <- list(
      calcular_efecto(d, "A"),          # efecto de A
      calcular_efecto(d, "B"),          # efecto de B
      calcular_efecto(d, c("A", "B"))   # interacción AB
    )

    do.call(rbind, lista_efectos)
  })

  # ------------------------------
  # Salidas a la UI
  # ------------------------------
  output$designTable <- renderTable({
    factorial_design()
  })

  output$resultsTable <- renderTable({
    simulate_results()
  })

  output$anovaOutput <- renderPrint({
    perform_anova()
  })

  output$interactionPlot <- renderPlot({
    render_interaction_plot(simulate_results())
  })

  # pestaña nueva: efectos
  output$tabla_efectos <- renderTable({
    efectos_df()
  }, digits = 3)

  output$graf_efectos <- renderPlot({
    ef <- efectos_df()
    barplot(
      ef$Efecto,
      names.arg = ef$Termino,
      main = "Efectos principales e interacción",
      ylab = "Efecto (media nivel +  -  media nivel -)"
    )
  })
}
