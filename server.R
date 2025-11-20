library(shiny)

# ================================
# Función general para calcular un efecto
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

  # =========================================================
  # PARTE 1: Diseño factorial 2^2 simulado (A, B, rep, Y)
  # =========================================================

  # Diseño 2^2 con número de replicaciones
  factorial_design <- reactive({
    expand.grid(
      A   = c(-1, 1),
      B   = c(-1, 1),
      rep = 1:input$rep
    )
  })

  # Simulación de resultados Y
  simulate_results <- reactive({
    data <- factorial_design()
    set.seed(123)
    data$Y <- with(
      data,
      10 + 5*A + 3*B + 2*A*B + rnorm(nrow(data), 0, 1)
    )
    data
  })

  # ANOVA para el modelo 2^2
  perform_anova <- reactive({
    data  <- simulate_results()
    model <- aov(Y ~ A * B, data = data)
    summary(model)
  })

  # Gráfica de interacción A x B
  render_interaction_plot <- function(data) {
    with(
      data,
      interaction.plot(
        A, B, Y,
        main = "Interaction Plot: A x B",
        xlab = "Factor A",
        ylab = "Response (Y)",
        col  = c("blue", "red")
      )
    )
  }

  # Efectos principales de A, B e interacción AB (modelo 2^2)
  efectos_df <- reactive({
    d <- simulate_results()
    req(d)

    lista_efectos <- list(
      calcular_efecto(d, "A"),          # efecto principal de A
      calcular_efecto(d, "B"),          # efecto principal de B
      calcular_efecto(d, c("A", "B"))   # interacción AB
    )

    do.call(rbind, lista_efectos)
  })

  # Salidas a la interfaz para la parte 2^2
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

  output$tabla_efectos <- renderTable({
    efectos_df()
  }, digits = 3)

  output$graf_efectos <- renderPlot({
    ef <- efectos_df()
    barplot(
      ef$Efecto,
      names.arg = ef$Termino,
      main = "Efectos principales e interacción (modelo 2^2)",
      ylab = "Efecto (media nivel +  -  media nivel -)"
    )
  })

  # =========================================================
  # PARTE 2: Actividad 2^4 (A, B, C, D, IF ingresado por el usuario)
  # =========================================================

  # Diseño 2^4 fijo (A,B,C,D = -1,+1)
  design_2k4 <- reactive({
    expand.grid(
      A = c(-1, 1),
      B = c(-1, 1),
      C = c(-1, 1),
      D = c(-1, 1)
    )
  })

  # UI dinámico para capturar los 16 valores de IF
  output$inputs_if_2k4 <- renderUI({
    d <- design_2k4()

    tagList(
      lapply(1:nrow(d), function(i) {
        fluidRow(
          column(
            7,
            tags$p(
              sprintf("Corrida %02d:  A=%2d  B=%2d  C=%2d  D=%2d",
                      i, d$A[i], d$B[i], d$C[i], d$D[i])
            )
          ),
          column(
            5,
            numericInput(
              inputId = paste0("if4_", i),
              label   = "IF:",
              value   = NA
            )
          )
        )
      })
    )
  })

  # Data.frame 2^4 con la columna IF que escribe el usuario
  datos_2k4 <- eventReactive(input$calcular_2k4, {
    d <- design_2k4()
    d$IF <- sapply(1:nrow(d), function(i) input[[paste0("if4_", i)]])
    d
  })

  # Mostrar el diseño 2^4 con IF
  output$tabla_diseno_2k4 <- renderTable({
    req(datos_2k4())
    datos_2k4()
  })

  # Efectos principales e interacciones AB, CD, BCD, ABCD
  efectos_2k4 <- reactive({
    d <- datos_2k4()
    req(d)
    req(!any(is.na(d$IF)))  # asegúrate de que no falte ningún IF

    lista <- list(
      calcular_efecto(d, "A",  respuesta = "IF"),          # A
      calcular_efecto(d, "B",  respuesta = "IF"),          # B
      calcular_efecto(d, "C",  respuesta = "IF"),          # C
      calcular_efecto(d, "D",  respuesta = "IF"),          # D
      calcular_efecto(d, c("A","B"),        "IF"),         # AB
      calcular_efecto(d, c("C","D"),        "IF"),         # CD
      calcular_efecto(d, c("B","C","D"),    "IF"),         # BCD
      calcular_efecto(d, c("A","B","C","D"),"IF")          # ABCD
    )

    do.call(rbind, lista)
  })

  output$tabla_efectos_2k4 <- renderTable({
    efectos_2k4()
  }, digits = 3)
}
