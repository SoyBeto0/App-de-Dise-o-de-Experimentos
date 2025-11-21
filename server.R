library(shiny)

# ================================
# Función general para calcular un efecto (modo 2^k)
# ================================
calcular_efecto <- function(df, vars, respuesta = "Y") {
  # vars: "A" o c("A","B"), etc.
  # producto de los signos (+1, -1)
  signo <- apply(df[, vars, drop = FALSE], 1, prod)

  # medias cuando el producto de signos es +1 y -1
  medias <- tapply(df[[respuesta]], signo, mean, na.rm = TRUE)

  data.frame(
    Termino     = paste(vars, collapse = ""),
    Media_mas   = as.numeric(medias["1"]),
    Media_menos = as.numeric(medias["-1"]),
    Efecto      = as.numeric(medias["1"] - medias["-1"])
  )
}

# ================================
# Función para generar TODAS las combinaciones de interacciones (modo 2^k)
# ================================
generar_todas_interacciones <- function(factores) {
  k <- length(factores)
  todas <- list()

  # Efectos principales (orden 1)
  for (i in 1:k) {
    todas[[length(todas) + 1]] <- factores[i]
  }

  # Interacciones de orden 2, 3, ..., k
  if (k >= 2) {
    for (orden in 2:k) {
      combinaciones <- combn(factores, orden, simplify = FALSE)
      todas <- c(todas, combinaciones)
    }
  }

  return(todas)
}

server <- function(input, output, session) {

  # =========================================================
  # MODO 1: DISEÑO 2^K CON SIGNOS
  # =========================================================

  # Generar automáticamente el diseño 2^k estándar (numérico -1 / +1)
  diseno_2k <- reactive({
    if (input$modo_analisis == "regresion_general") return(NULL)

    k <- input$k_factorial
    req(k)

    factores <- LETTERS[1:k]

    # Generar todas las combinaciones de -1 y 1
    combinaciones <- expand.grid(rep(list(c(-1, 1)), k))
    names(combinaciones) <- factores

    # Agregar columna de corrida
    combinaciones$Corrida <- 1:nrow(combinaciones)

    # Agregar columna Y vacía
    combinaciones$Y <- NA
    combinaciones <- combinaciones[, c("Corrida", factores, "Y")]

    return(combinaciones)
  })

  # Mostrar tabla de diseño 2^k con signos como + y -
  output$tabla_diseno <- renderTable({
    if (input$modo_analisis == "regresion_general") return(NULL)

    d <- diseno_2k()
    req(d)

    k <- input$k_factorial
    factores <- LETTERS[1:k]

    d_mostrar <- d
    for (f in factores) {
      d_mostrar[[f]] <- ifelse(d_mostrar[[f]] == 1, "+", "-")
    }

    d_mostrar
  })

  # Inputs para Y en diseño 2^k
  output$inputs_respuesta <- renderUI({
    if (input$modo_analisis == "regresion_general") return(NULL)

    d <- diseno_2k()
    req(d)

    n_corridas <- nrow(d)

    tagList(
      h5("Ingresa los valores de respuesta (Y/IF) para cada corrida:"),
      lapply(1:n_corridas, function(i) {
        fluidRow(
          column(2, tags$p(paste("Corrida", i, ":"))),
          column(4, numericInput(
            inputId = paste0("RESP_", i),
            label   = NULL,
            value   = NA,
            width   = "100%"
          ))
        )
      })
    )
  })

  # =========================================================
  # MODO 2: REGRESIÓN LINEAL MÚLTIPLE GENERAL
  # =========================================================

  # Interfaz para ingresar datos manualmente (modo regresión general)
  output$input_regresion_general <- renderUI({
    if (input$modo_analisis != "regresion_general") return(NULL)

    tagList(
      h4("Configuración de Variables"),

      numericInput("num_observaciones",
                   "Número de observaciones (filas de datos):",
                   value = 10, min = 3, max = 100),

      numericInput("num_variables",
                   "Número de variables explicativas (X):",
                   value = 3, min = 1, max = 10),

      textInput("nombre_y",
                "Nombre de la variable dependiente (Y):",
                value = "Y"),

      textInput("nombres_x",
                "Nombres de las variables independientes (separadas por coma):",
                value = "X1, X2, X3"),

      hr(),

      h4("Ingresa los Datos"),
      p("Completa la tabla con tus datos (una fila por observación):"),

      uiOutput("tabla_datos_regresion")
    )
  })

  # Generar inputs para la tabla de datos de regresión general
  output$tabla_datos_regresion <- renderUI({
    if (input$modo_analisis != "regresion_general") return(NULL)

    n_obs  <- input$num_observaciones
    n_vars <- input$num_variables

    req(n_obs, n_vars)

    nombres_x <- strsplit(input$nombres_x, ",")[[1]]
    nombres_x <- trimws(nombres_x)

    if (length(nombres_x) != n_vars) {
      nombres_x <- paste0("X", 1:n_vars)
    }

    tagList(
      lapply(1:n_obs, function(i) {
        fluidRow(
          column(1, tags$p(paste0(i, ":"))),
          column(2, numericInput(paste0("Y_reg_", i),
                                 label = if (i == 1) input$nombre_y else NULL,
                                 value = NA)),
          lapply(1:n_vars, function(j) {
            column(floor(9 / n_vars),
                   numericInput(paste0("X", j, "_reg_", i),
                               label = if (i == 1) nombres_x[j] else NULL,
                               value = NA))
          })
        )
      })
    )
  })

  # =========================================================
  # CONSTRUIR DATA.FRAME CON DATOS INGRESADOS (COMÚN)
  # =========================================================

  datos_completos <- eventReactive(input$ejecutar_analisis, {

    # --------- MODO: Regresión General ---------
    if (input$modo_analisis == "regresion_general") {
      n_obs  <- input$num_observaciones
      n_vars <- input$num_variables
      req(n_obs, n_vars)

      # Recoger Y
      y_valores <- sapply(1:n_obs, function(i) {
        val <- input[[paste0("Y_reg_", i)]]
        if (is.null(val)) return(NA)
        return(val)
      })

      # Recoger X's
      x_data <- lapply(1:n_vars, function(j) {
        sapply(1:n_obs, function(i) {
          val <- input[[paste0("X", j, "_reg_", i)]]
          if (is.null(val)) return(NA)
          return(val)
        })
      })

      nombres_x <- strsplit(input$nombres_x, ",")[[1]]
      nombres_x <- trimws(nombres_x)

      if (length(nombres_x) != n_vars) {
        nombres_x <- paste0("X", 1:n_vars)
      }

      df <- data.frame(y_valores)
      names(df) <- input$nombre_y

      for (j in 1:n_vars) {
        df[[nombres_x[j]]] <- x_data[[j]]
      }

      df <- df[complete.cases(df), ]
      if (nrow(df) == 0) return(NULL)

      return(df)
    }

    # --------- MODO: Diseño 2^k ---------
    d <- diseno_2k()
    req(d)

    n_corridas <- nrow(d)

    resp_valores <- sapply(1:n_corridas, function(i) {
      val <- input[[paste0("RESP_", i)]]
      if (is.null(val)) return(NA)
      return(val)
    })

    d$Y <- resp_valores
    d <- d[!is.na(d$Y), ]

    if (nrow(d) == 0) return(NULL)

    return(d)
  })

  # =========================================================
  # MODELO LINEAL UNIFICADO (lm) PARA AMBOS MODOS
  # =========================================================

  modelo_unificado <- reactive({
    d <- datos_completos()
    req(d)
    validate(need(nrow(d) > 0, "No hay datos suficientes para ajustar el modelo."))

    if (input$modo_analisis == "regresion_general") {
      # ---- Regresión general: Y ~ X1 + X2 + ... ----
      nombre_y  <- names(d)[1]
      nombres_x <- names(d)[-1]

      formula_str <- paste(nombre_y, "~", paste(nombres_x, collapse = " + "))
      formula_obj <- as.formula(formula_str)

      lm(formula_obj, data = d)

    } else {
      # ---- Diseño 2^k: Y ~ (A + B + C + ...)^k ----
      k        <- input$k_factorial
      factores <- LETTERS[1:k]

      formula_str <- paste0("Y ~ (", paste(factores, collapse = " + "), ")^", k)
      formula_obj <- as.formula(formula_str)

      lm(formula_obj, data = d)
    }
  })

  # =========================================================
  # SALIDA: TABLA DE DATOS COMPLETA (VISUAL)
  # =========================================================

  output$tabla_resultados <- renderTable({
    d <- datos_completos()
    req(d)

    if (input$modo_analisis == "regresion_general") {
      return(d)
    }

    # En diseño 2^k: mostrar factores como + / -
    k        <- input$k_factorial
    factores <- LETTERS[1:k]

    resultado <- d
    for (f in factores) {
      resultado[[f]] <- ifelse(resultado[[f]] == 1, "+", "-")
    }

    resultado
  })

  # =========================================================
  # ANOVA DEL MODELO (UNIFICADO)
  # =========================================================

  output$anova_output <- renderPrint({
    d <- datos_completos()
    req(d)

    m <- modelo_unificado()
    req(m)

    print(anova(m))
  })

  # =========================================================
  # GRÁFICA DE INTERACCIÓN (solo 2^k)
  # =========================================================

  output$grafica_interaccion <- renderPlot({
    if (input$modo_analisis == "regresion_general") {
      plot.new()
      text(0.5, 0.5, "Gráfica de interacción no disponible para regresión general", cex = 1.2)
      return()
    }

    d <- datos_completos()
    req(d)

    k <- input$k_factorial

    if (k < 2) {
      plot.new()
      text(0.5, 0.5, "Se necesitan al menos 2 factores para graficar interacción", cex = 1.2)
      return()
    }

    # Interacción de los dos primeros factores (A y B)
    with(
      d,
      interaction.plot(
        A, B, Y,
        main = "Gráfica de Interacción: A x B",
        xlab = "Factor A",
        ylab = "Respuesta (Y)",
        col  = c("blue", "red"),
        lwd  = 2
      )
    )
  })

  # =========================================================
  # EFECTOS (2^k) Y COEFICIENTES (REGRESIÓN) PARA TABLA/GRAF
  # =========================================================

  # Efectos del diseño 2^k
  todos_efectos_signos <- reactive({
    if (input$modo_analisis != "signos") return(NULL)

    d <- datos_completos()
    req(d)
    if (is.null(d) || nrow(d) == 0) return(NULL)

    k        <- input$k_factorial
    factores <- LETTERS[1:k]

    todas_interacciones <- generar_todas_interacciones(factores)

    lista_efectos <- lapply(todas_interacciones, function(vars) {
      calcular_efecto(d, vars, respuesta = "Y")
    })

    resultado <- do.call(rbind, lista_efectos)
    resultado$Efecto_Abs <- abs(resultado$Efecto)
    resultado <- resultado[order(-resultado$Efecto_Abs), ]

    resultado
  })

  # Coeficientes del modelo unificado (modo regresión general)
  coeficientes_modelo <- reactive({
    if (input$modo_analisis != "regresion_general") return(NULL)

    m <- modelo_unificado()
    req(m)

    s <- summary(m)

    data.frame(
      Variable    = rownames(s$coefficients),
      Coeficiente = s$coefficients[, "Estimate"],
      Error_Std   = s$coefficients[, "Std. Error"],
      t_value     = s$coefficients[, "t value"],
      p_value     = s$coefficients[, "Pr(>|t|)"],
      row.names   = NULL
    )
  })

  # Tabla de efectos/coeficientes (según modo)
  output$tabla_efectos <- renderTable({
    if (input$modo_analisis == "signos") {
      ef <- todos_efectos_signos()
      req(ef)
      ef$Efecto_Abs <- NULL
      return(ef)
    } else {
      ef <- coeficientes_modelo()
      req(ef)
      return(ef)
    }
  }, digits = 4)

  # Gráfica de efectos/coeficientes (según modo)
  output$grafica_efectos <- renderPlot({
    if (input$modo_analisis == "signos") {
      ef <- todos_efectos_signos()
      req(ef)

      barplot(
        ef$Efecto,
        names.arg = ef$Termino,
        main      = "Efectos principales e interacciones (diseño 2^k)",
        ylab      = "Efecto",
        xlab      = "Término",
        las       = 2,
        col       = "steelblue",
        border    = "white"
      )
      abline(h = 0, col = "red", lty = 2, lwd = 2)

    } else {
      ef <- coeficientes_modelo()
      req(ef)

      ef_plot <- ef[ef$Variable != "(Intercept)", ]

      barplot(
        ef_plot$Coeficiente,
        names.arg = ef_plot$Variable,
        main      = "Coeficientes del modelo de regresión múltiple",
        ylab      = "Coeficiente",
        xlab      = "Variable",
        las       = 2,
        col       = "darkorange",
        border    = "white"
      )
      abline(h = 0, col = "red", lty = 2, lwd = 2)
    }
  })
}
