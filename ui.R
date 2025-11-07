#library(shiny)

fluidPage(
  
  # Application title
  titlePanel(
    fluidRow(
      column(3,
             tags$img(src = "logotipo.png", width = "80px", height = "80px", 
                      style = "display: block; margin-left: auto; margin-right: auto;"),
             p("Aldás Ana, Iniquinga Mateo, Torres Emely",
               style = "text-align:left; color:#132b60;
                        font-family: 'Times New Roman'; font-size:0.6em; margin-top:5px;")),
      column(9, h1("Aplicativo Grupal", 
                   style = "text-align:center; color:#132b60; 
                         font-family: 'Times New Roman'; font-weight: bold; margin-bottom: 0;")
      )         
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargar archivo Excel", accept = ".xlsx"),
      uiOutput("sheet_ui"),
      actionButton("analizar", "Carga datos"),
      hr(),
      selectInput("colx", "Seleccione el color:", c("Naranja" = "#EE9572", "Turqueza" = "#7AC5CD", "Rosa" = "#CD8C95")),
      hr(),
      downloadButton("descargar_pdf", "Informe PDF")
    ),
    
    # Tablas y gráficas
    mainPanel(
      tags$style("h1 {color: #132b60; font-family: roman; font-size:1.7em}"),
      navbarPage("Análisis",
                 tabPanel("Datos",
                          h1("Registros de la base de datos"),
                          tableOutput("TablaVariableANITA"),
                          hr(),
                          h1("Tabla en Rango de Edades"),
                          tableOutput("TablaRangoEdad"),
                          hr(),
                          h1("Número de cada tipo de crédito otorgado a cada provincia por nivel de educación"),
                          div(
                            style = "text-align:center; margin-bottom:10px;",
                            tags$div(
                              style = "font-size:0.8em;",  # tamaño reducido
                              fluidRow(
                                column(6, uiOutput("SelectorTipoCredito")),
                                column(6, uiOutput("SelectorProvincia"))
                              )
                            )
                          ),
                          tableOutput("TiposXProvincia"),
                 ),
                 tabPanel("Gráficos",
                          fluidRow(
                            column(12, h1("Gráfico Dispersión")),
                            column(12,
                                   plotOutput("Grafico02")
                            ),
                            column(12, h1("Gráfico de Pastel"),
                                   uiOutput("ProvinciaUI"),
                                   fluidRow(
                                     column(8, plotOutput("Grafico03")),
                                     column(4, uiOutput("LeyendaPastel"))
                                   )
                                   
                            )
                          ),
                          h1("Histograma Nivel de Educación"),
                          div(
                            style = "text-align:center; margin-bottom:10px;",
                            tags$div(
                              style = "font-size:0.8em;",  # tamaño pequeño
                              fluidRow(
                                column(6, uiOutput("SelectorTipoCreditoGrafico")),
                                column(6, uiOutput("SelectorProvinciaGrafico"))
                              )
                            )
                          ),
                          
                          # Gráfico
                          plotOutput("Graficohistograma")),
                 tabPanel("Informe")
      )
    )
  )
  
)



