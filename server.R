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

  # Efectos principales de A, B e interacción AB (modelo 2^2 simulado)
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
  # PARTE 2: Tabla editable A,B,C,D (+/-) e IF (tipo actividad)
  # =========================================================

  # Dibujamos 16 corridas: inputs de signos y IF
  output$tabla_signos_if <- renderUI({
    n <- 16  # como el ejemplo 2^4 de la actividad

    tagList(
      lapply(1:n, function(i) {
        fluidRow(
          column(1, tags$p(sprintf("%02d", i))),
          column(2, selectInput(
            paste0("A_tab_", i), "A",
            choices = c("-" = -1, "+" = 1)
          )),
          column(2, selectInput(
            paste0("B_tab_", i), "B",
            choices = c("-" = -1, "+" = 1)
          )),
          column(2, selectInput(
            paste0("C_tab_", i), "C",
            choices = c("-" = -1, "+" = 1)
          )),
          column(2, selectInput(
            paste0("D_tab_", i), "D",
            choices = c("-" = -1, "+" = 1)
          )),
          column(3, numericInput(
            paste0("IF_tab_", i), "IF",
            value = NA
          ))
        )
      })
    )
  })

  # Construimos el data.frame a partir de lo que escribió el usuario
  datos_tabla <- eventReactive(input$calcular_tabla, {
    n <- 16

    data.frame(
      A  = sapply(1:n, function(i) as.numeric(input[[paste0("A_tab_", i)]])),
      B  = sapply(1:n, function(i) as.numeric(input[[paste0("B_tab_", i)]])),
      C  = sapply(1:n, function(i) as.numeric(input[[paste0("C_tab_", i)]])),
      D  = sapply(1:n, function(i) as.numeric(input[[paste0("D_tab_", i)]])),
      IF = sapply(1:n, function(i) as.numeric(input[[paste0("IF_tab_", i)]]))
    )
  })

  # Efectos principales A,B,C,D y las interacciones AB, CD, BCD, ABCD
  # usando SOLO las filas que sí tienen IF
  efectos_tabla <- reactive({
    d <- datos_tabla()
    req(d)

    # nos quedamos solo con las filas que sí tengan IF
    d <- d[!is.na(d$IF), ]

    # si no hay ninguna fila con IF, no calculamos nada
    if (nrow(d) == 0) {
      return(NULL)
    }

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

  output$tabla_efectos_tabla <- renderTable({
    req(efectos_tabla())   # solo muestra si hay al menos un IF lleno
    efectos_tabla()
  }, digits = 3)
}
