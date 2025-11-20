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
    '))
  ),

  # ---- TÍTULOS ----
  div(class = "main-title", "Factorial Diseño de Experimento (2^k)"),
  div(class = "main-subtitle", "Equipo 1"),

  # ---- LAYOUT ----
  sidebarLayout(

    sidebarPanel(
      div(class="card",
          h4("Parámetros del Experimento",
             style="font-weight:700; color:#2c3e50;"),

          numericInput("rep", "Número de Replicaciones:",
                       value = 2, min = 1, max = 10),

          selectInput("factorA", "Niveles del Factor A:",
                      choices = c("Low" = -1, "High" = 1)),

          selectInput("factorB", "Niveles del Factor B:",
                      choices = c("Low" = -1, "High" = 1)),

          actionButton("run", "Ejecutar Experimento",
                       class = "btn btn-primary",
                       style = "margin-top:15px;")
      )
    ),

    mainPanel(
      div(class="card",
          tabsetPanel(
            tabPanel("📋 Diseño",
                     tableOutput("designTable")),

            tabPanel("📊 Resultados",
                     tableOutput("resultsTable")),

            tabPanel("📈 Análisis ANOVA",
                     verbatimTextOutput("anovaOutput")),

            tabPanel("📌 Gráfica de Interacción",
                     plotOutput("interactionPlot")),

            # 🔹 PESTAÑA: efectos del modelo 2^2 simulado
            tabPanel("✨ Efectos principales",
                     h4("Efectos principales e interacción (modelo 2^2)"),
                     tableOutput("tabla_efectos"),
                     br(),
                     plotOutput("graf_efectos")),

            # 🔹 PESTAÑA: actividad 2^4 con A, B, C, D e IF
            tabPanel("📄 2^4 A, B, C, D",
                     h4("Introduce los valores de IF para cada combinación de niveles"),
                     uiOutput("inputs_if_2k4"),
                     br(),
                     actionButton("calcular_2k4", "Calcular efectos 2^4",
                                  class = "btn btn-primary"),
                     br(), br(),
                     h4("Diseño 2^4 con IF"),
                     tableOutput("tabla_diseno_2k4"),
                     br(),
                     h4("Efectos principales e interacciones (AB, CD, BCD, ABCD)"),
                     tableOutput("tabla_efectos_2k4"))
          )
      )
    )
  )
)

