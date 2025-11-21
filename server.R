library(shiny)

# ================================
# Función general para calcular un efecto
# ================================
calcular_efecto <- function(df, vars, respuesta = "Y") {
  # vars: "A" o c("A","B"), etc.
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
  # PARTE 2: Tabla editable 2^k (A,B,C,D,..., IF)
  # =========================================================

  # Generar la tabla de signos e IF para 2^k corridas
  output$tabla_signos_if <- renderUI({
    # número de factores k (por defecto 4 si aún no hay input)
    k <- input$k_tabla
    if (is.null(k)) k <- 4

    # nombres de factores: A, B, C, ...
    factores <- LETTERS[1:k]

    # número de corridas: 2^k
    n <- 2^k

    # diseño base 2^k en -1 / +1 (por si quieres usarlo luego)
    # aquí no lo usamos directamente, solo generamos los inputs
    tagList(
      lapply(1:n, function(i) {
        fluidRow(
          column(1, tags$p(sprintf("%02d", i))),
          # columnas de factores A, B, C, ...
          lapply(seq_along(factores), function(j) {
            f <- factores[j]
            column( floor(8 / k),  # ancho aproximado para que quepan
                    selectInput(
                      inputId  = paste0(f, "_tab_", i),
                      label    = f,
                      choices  = c("-" = -1, "+" = 1)
                    )
            )
          }),
          # columna de IF
          column(3,
                 numericInput(
                   inputId = paste0("IF_tab_", i),
                   label   = "IF",
                   value   = NA
                 )
          )
        )
      })
    )
  })

  # Construir el data.frame a partir de lo que escribió el usuario
  datos_tabla <- eventReactive(input$calcular_tabla, {
    k <- input$k_tabla
    if (is.null(k)) k <- 4
    factores <- LETTERS[1:k]
    n <- 2^k

    # construir columnas de factores dinámicamente
    cols_factores <- lapply(factores, function(f) {
      sapply(1:n, function(i) as.numeric(input[[paste0(f, "_tab_", i)]]))
    })
    names(cols_factores) <- factores

    # columna IF
    col_IF <- sapply(1:n, function(i) as.numeric(input[[paste0("IF_tab_", i)]]))

    df <- as.data.frame(cols_factores)
    df$IF <- col_IF
    df
  })

  # Efectos principales para cada factor (A, B, C, ..., según k)
  efectos_tabla <- reactive({
    d <- datos_tabla()
    req(d)

    # usar solo filas con IF válido
    d <- d[!is.na(d$IF), ]
    if (nrow(d) == 0) {
      return(NULL)
    }

    factores <- setdiff(names(d), "IF")  # nombres de factores presentes

    lista <- lapply(factores, function(f) {
      calcular_efecto(d, f, respuesta = "IF")
    })

    do.call(rbind, lista)
  })

  output$tabla_efectos_tabla <- renderTable({
    req(efectos_tabla())
    efectos_tabla()
  }, digits = 3)
}
