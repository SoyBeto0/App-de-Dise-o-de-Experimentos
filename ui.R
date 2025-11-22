library(shiny)

ui <- fluidPage(

  # ---- ESTILOS GLOBALES ----
  tags$head(
    tags$style(HTML('
      body {
        background-color: #eef2f7;
        font-family: "Segoe UI", sans-serif;
      }

      /* Tarjeta general */
      .card {
        background: white;
        padding: 22px;
        border-radius: 16px;
        border-left: 6px solid #3498db;
        box-shadow: 0 4px 12px rgba(0,0,0,0.10);
        margin-bottom: 25px;
        transition: 0.2s;
      }
      .card:hover {
        transform: scale(1.01);
        box-shadow: 0 6px 18px rgba(0,0,0,0.15);
      }

      /* Títulos */
      .main-title {
        font-size: 36px;
        font-weight: 800;
        text-align: center;
        margin-bottom: 10px;
        color: #2c3e50;
        text-shadow: 1px 1px 1px rgba(0,0,0,0.2);
      }
      .main-subtitle {
        font-size: 20px;
        text-align: center;
        margin-bottom: 25px;
        color: #5d6d7e;
      }

      /* Pestañas */
      .nav-tabs > li > a {
        font-weight: 600;
        color: #2c3e50;
      }
      .nav-tabs > li.active > a {
        background-color: #3498db !important;
        color: white !important;
        border-radius: 6px;
      }

      /* Botón principal */
      .btn-primary {
        background-color: #3498db !important;
        border: none !important;
        padding: 10px;
        width: 100%;
        font-weight: 600;
        border-radius: 10px;
      }
      .btn-primary:hover {
        background-color: #2d83bb !important;
      }

      /* Instrucciones */
      .instruccion {
        background-color: #ecf0f1;
        padding: 12px;
        border-radius: 8px;
        margin-bottom: 15px;
        border-left: 4px solid #3498db;
      }

      /* Radio buttons mejorados */
      .radio {
        margin-bottom: 10px;
      }
    '))
  ),

  # ---- TÍTULOS ----
  div(class = "main-title", "Diseño de Experimentos y Regresión"),
  div(class = "main-subtitle", "Equipo 1"),

  # ---- LAYOUT ----
  sidebarLayout(

    # =============== PANEL LATERAL ===============
    sidebarPanel(
      div(class="card",
          h4("Configuración del Análisis", style="font-weight:700; color:#2c3e50;"),
          
          div(class="instruccion",
              p(style="margin:0; font-size:14px;", 
                "🔬 Selecciona el tipo de problema que deseas resolver.")
          ),
          
          # Selector de modo de análisis
          h5("Tipo de Análisis:", style="font-weight:600; margin-top:10px;"),
          radioButtons("modo_analisis",
                       label = NULL,
                       choices = list(
                         "Diseño 2^k" = "signos",
                         "Regresión Lineal Múltiple" = "regresion_general"
                       ),
                       selected = "signos"),
          
          hr(),
          
          # Número de factores (solo para diseños 2^k)
          conditionalPanel(
            condition = "input.modo_analisis == 'signos'",
            numericInput("k_factorial", 
                         "Número de Factores (k):",
                         value = 3, min = 2, max = 6)
          ),
          
          # Número de réplicas (solo para diseños 2^k)
          conditionalPanel(
            condition = "input.modo_analisis == 'signos'",
            numericInput("num_replicas",
                         "Número de Réplicas:",
                         value = 1, min = 1, max = 10)
          ),
          
          hr(),
          
          actionButton("ejecutar_analisis", 
                       "Ejecutar Análisis",
                       class = "btn btn-primary",
                       style = "margin-top:15px; font-size:16px;")
      )
    ),

    # =============== PANEL PRINCIPAL ===============
    mainPanel(
      div(class="card",
          tabsetPanel(

            # ------------------- PESTAÑA 1: DISEÑO/DATOS --------------------
            tabPanel("📋 Datos de Entrada",
                     
                     # Para diseños 2^k
                     conditionalPanel(
                       condition = "input.modo_analisis == 'signos'",
                       h3("Matriz de Diseño 2^k"),
                       
                       div(class="instruccion",
                           p("Diseño factorial generado automáticamente. Ingresa los valores de respuesta.")
                       ),
                       
                       tableOutput("tabla_diseno"),
                       hr(),
                       h4("Valores de Respuesta"),
                       uiOutput("inputs_respuesta")
                     ),
                     
                     # Para regresión general
                     conditionalPanel(
                       condition = "input.modo_analisis == 'regresion_general'",
                       h3("Regresión Lineal Múltiple"),
                       
                       div(class="instruccion",
                           p("Configura las variables y completa los datos de tu problema de regresión.")
                       ),
                       
                       uiOutput("input_regresion_general")
                     )
            ),

            # ------------------- PESTAÑA 2: RESULTADOS --------------------
            tabPanel("📊 Datos Completos",
                     h3("Tabla de Datos Completa"),
                     
                     div(class="instruccion",
                         p("Presiona 'Ejecutar Análisis' para ver tus datos completos.")
                     ),
                     
                     tableOutput("tabla_resultados")
            ),

            # ------------------- PESTAÑA 3: ANOVA --------------------
            tabPanel("📈 ANOVA",
                     h3("Análisis de Varianza"),
                     
                     div(class="instruccion",
                         p("Significancia estadística de factores y/o variables.",
                           "Pr(>F) < 0.05 indica efectos significativos.")
                     ),
                     
                     verbatimTextOutput("anova_output")
            ),

            # ------------------- PESTAÑA 4: INTERACCIÓN --------------------
            tabPanel("📌 Gráfica de Interacción",
                     h3("Interacción entre Factores"),
                     
                     div(class="instruccion",
                         p("Solo disponible para diseños 2^k con al menos 2 factores.")
                     ),
                     
                     plotOutput("grafica_interaccion", height = "450px")
            ),

            # ------------------- PESTAÑA 5: EFECTOS/COEFICIENTES --------------------
            tabPanel("✨ Efectos y Coeficientes",
                     h3("Resultados del Modelo"),
                     
                     div(class="instruccion",
                         conditionalPanel(
                           condition = "input.modo_analisis == 'signos'",
                           p("Efectos principales e interacciones del diseño 2^k.")
                         ),
                         conditionalPanel(
                           condition = "input.modo_analisis == 'regresion_general'",
                           p("Coeficientes del modelo de regresión lineal múltiple con estadísticas de significancia.")
                         )
                     ),
                     
                     tableOutput("tabla_efectos"),
                     
                     hr(),
                     
                     h3("Visualización Gráfica"),
                     plotOutput("grafica_efectos", height = "450px")
            )
          )
      )
    )
  )
)