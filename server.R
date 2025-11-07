library(shiny)
library(dplyr)
#library(tidyverse)
library(readxl)
library(kableExtra)
library(formattable)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(rmarkdown)
#library(quarto)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # ==== EMELY  
  #======GRÁFICO DE DISPERSIÓN: Edad vs. Monto Otorgado ====
  output$Grafico02 <- renderPlot({
    
    req("FECHANACIMIENTO" %in% names(datos()), "MONTO_OTORGADO" %in% names(datos()))
    
    t02 <- datos() %>%
      mutate(EDAD = as.numeric(difftime(Sys.Date(), FECHANACIMIENTO, units = "days")) / 365.25) %>%
      filter(!is.na(EDAD), !is.na(MONTO_OTORGADO))
    
    # Gráfico de dispersión
    ggplot(t02, aes(x = EDAD, y = MONTO_OTORGADO)) +
      geom_point(color = input$colx, alpha = 0.6, size = 3) +
      labs(title = "Relación entre Edad del Solicitante y Monto Otorgado",
           x = "Edad (años)",
           y = "Monto otorgado") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5, color = "#132b60", face = "bold"))
  })
  
  # === SELECCIONAR PROVINCIA ===
  output$ProvinciaUI <- renderUI({
    req(datos())
    provincias <- sort(unique(datos()$PROVINCIA_DOMICILIO))
    selectInput("provincia_sel", "Seleccione la provincia:",
                choices = provincias,
                selected = provincias[1])
  })
  
  # === GRÁFICO DE PASTEL POR PROVINCIA ===
  output$Grafico03 <- renderPlot({
    req(input$provincia_sel)
    
    datos_filtrados <- datos() %>%
      filter(PROVINCIA_DOMICILIO == input$provincia_sel)
    
    tabla_tipos <- table(datos_filtrados$TIPO)
    
    validate(
      need(length(tabla_tipos) > 0, "No existen registros para la provincia seleccionada.")
    )
    
    # Calcular porcentajes
    porcentajes <- round(100 * tabla_tipos / sum(tabla_tipos), 1)
    etiquetas <- paste0(porcentajes, "%")  
    
    colores_pastel <- brewer.pal(n = length(tabla_tipos), "Set3")
    
    
    par(mar = c(1, 1, 2, 1))
    
    # Crear gráfico de pastel base
    pie(tabla_tipos,
        labels = etiquetas,
        col = colores_pastel[seq_along(tabla_tipos)],
        main = paste("Distribución de tipos de crédito en", toupper(input$provincia_sel)),
        cex = 0.9,
        border = "black")
  })
  
  output$LeyendaPastel <- renderUI({
    req(input$provincia_sel)
    
    datos_filtrados <- datos() %>%
      filter(PROVINCIA_DOMICILIO == input$provincia_sel)
    
    tabla_tipos <- table(datos_filtrados$TIPO)
    colores_pastel <- brewer.pal(n = length(tabla_tipos), "Set3")
    
    df_leyenda <- data.frame(
      Tipo = names(tabla_tipos),
      Color = colores_pastel[seq_along(tabla_tipos)]
    )
    
    tags$div(
      style = "font-size:0.85em;",
      tags$h4("Tipo de crédito", style = "text-align:center; color:#132b60;"),
      lapply(1:nrow(df_leyenda), function(i) {
        tags$div(
          style = "display:flex; align-items:center; margin-bottom:4px;",
          tags$div(
            style = paste("width:20px; height:20px; background-color:", df_leyenda$Color[i],
                          "; margin-right:8px; border:1px solid #aaa;")
          ),
          tags$span(df_leyenda$Tipo[i], style = "font-size:0.9em;")
        )
      })
    )
  })
  
  
  #====== Ana
  
  #------------------------ PRIMERA TABLA ANI--------------------- 
  output$TablaVariableANITA <- renderUI({
    req(datos())
    
    res01 <- datos() %>%
      dplyr::mutate(
        FECHANACIMIENTO = as.Date(FECHANACIMIENTO),
        EDAD = floor(as.numeric(difftime(Sys.Date(), FECHANACIMIENTO, units = "days")) / 365.25)
      ) %>%
      dplyr::select(IDENTIFICACION, EDAD, TIPO, NIVEL_EDUCACION) %>%
      dplyr::rename(NUM_ID = IDENTIFICACION)
    
    tabla_html <- res01 %>%
      knitr::kable(
        align = "c",
        caption = "Tabla de Edad, Tipo y Nivel Educativo por Identificación"
      ) %>%
      kableExtra::kable_styling(font_size = 11, full_width = FALSE) %>%
      kableExtra::row_spec(0, background = "#132b60", color = "#ffffff") %>%
      kableExtra::scroll_box(width = "700px", height = "300px")
    
    HTML(tabla_html)
  })
  
  #----------------RANGO DE EDADES ANI----------
  output$TablaRangoEdad <- function(){
    req(datos())
    res <- datos() %>%
      mutate(
        EDAD = floor(as.numeric(difftime(Sys.Date(), FECHANACIMIENTO, units = "days")) / 365.25),
        RANGO_EDAD = cut(
          EDAD,
          breaks = c(18, 25, 32, 40, 50, 60, 100),
          right = FALSE,
          include.lowest = TRUE,
          labels = c("[18-25)", "[25-32)", "[32-40)", "[40-50)", "[50-60)", "[60-100)")
        )
      ) %>%
      group_by(RANGO_EDAD, TIPO) %>%
      summarise(CONTEO = n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = TIPO,
        values_from = CONTEO,
        values_fill = 0
      ) %>%
      
      mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
      relocate(TOTAL, .after = RANGO_EDAD) %>%
      arrange(RANGO_EDAD)
    
    res %>%
      kable(
        align = "c",
        caption = "Distribución de Créditos por Rango de Edad y Tipo de Crédito"
      ) %>%
      kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff") %>%
      scroll_box(width = "900px", height = "400px")
  }
  
  #======== Mateo
  #SELECTORES DE CREDITO Y PROVINCIA PARA GRAFICO Y TABLA ********************************************************************
  output$SelectorTipoCredito <- renderUI({
    req(datos())
    tipos_credito <- unique(datos()$TIPO_CREDITO_OTORGADO)
    selectInput("tipo_credito", "Seleccione el tipo de crédito:", 
                choices = c("Todos", tipos_credito), selected = "Todos")
  })
  
  output$SelectorProvincia <- renderUI({
    req(datos())
    provincias <- unique(datos()$PROVINCIA_DOMICILIO)
    selectInput("provincia", "Seleccione la provincia:", 
                choices = c("Todas", provincias), selected = "Todas")
  })
  
  output$SelectorTipoCreditoGrafico <- renderUI({
    req(datos())
    tipos_credito <- unique(datos()$TIPO_CREDITO_OTORGADO)
    selectInput("tipo_creditoGrafico", "Seleccione el tipo de crédito:", 
                choices = c("Todos", tipos_credito), selected = "Todos")
  })
  
  output$SelectorProvinciaGrafico <- renderUI({
    req(datos())
    provincias <- unique(datos()$PROVINCIA_DOMICILIO)
    selectInput("provinciaGrafico", "Seleccione la provincia:", 
                choices = c("Todas", provincias), selected = "Todas")
  })
  
  
  # -----------HISTOGRAMA FILTRANDO TIPOS DE CREDITO Y PROVINCIA
  output$Graficohistograma <- renderPlot({
    req(datos())
    
    # Filtrar datos según selecciones
    datos_filtrados <- datos()
    
    if (!is.null(input$tipo_creditoGrafico) && input$tipo_creditoGrafico != "Todos") {
      datos_filtrados <- datos_filtrados %>% 
        filter(TIPO_CREDITO_OTORGADO == input$tipo_creditoGrafico)
    }
    
    if (!is.null(input$provinciaGrafico) && input$provinciaGrafico != "Todas") {
      datos_filtrados <- datos_filtrados %>% 
        filter(PROVINCIA_DOMICILIO == input$provinciaGrafico)
    }
    
    t03 <- datos_filtrados %>%
      group_by(NIVEL_EDUCACION) %>%
      summarise(Cantidad = n(), .groups = 'drop')
    
    t03 <- t03 %>% ggplot(aes(x=NIVEL_EDUCACION, y=Cantidad)) + geom_col(fill = input$colx) +
      geom_text(aes(label = Cantidad), vjust = -0.5, size = 4) +
      labs(title = "Distribución por Nivel de Educación",
           x = "Nivel de Educación",
           y = "Cantidad de Créditos")
    return(t03)
  })
  
  ##-------------CAMBIOS DE FUNCION GENERADORA DE TABLA  
  output$TiposXProvincia <- function(){
    req(datos())
    
    datos_filtrados <- datos()
    
    if (!is.null(input$tipo_credito) && input$tipo_credito != "Todos") {
      datos_filtrados <- datos_filtrados %>% 
        filter(TIPO_CREDITO_OTORGADO == input$tipo_credito)
    }
    
    if (!is.null(input$provincia) && input$provincia != "Todas") {
      datos_filtrados <- datos_filtrados %>% 
        filter(PROVINCIA_DOMICILIO == input$provincia)
    }
    
    res03 <- datos_filtrados %>% 
      group_by(PROVINCIA_DOMICILIO, TIPO_CREDITO_OTORGADO, NIVEL_EDUCACION) %>% 
      summarise(Cantidad = n(), .groups = 'drop') %>%
      mutate(PORCENTAJE=percent(Cantidad/sum(Cantidad))) %>% 
      arrange(PROVINCIA_DOMICILIO, TIPO_CREDITO_OTORGADO,NIVEL_EDUCACION, desc(Cantidad))
    
    colnames(res03) <- c("PROVINCIA", "TIPO_CRÉDITO", "NIVEL_EDUCACIÓN", "CANTIDAD","PORCENTAJE")
    
    res03$PORCENTAJE <- color_bar("lightgreen")(res03$PORCENTAJE)
    
    tab03 <- res03 %>% 
      kable("html", escape = F, booktabs = TRUE) %>% 
      kable_styling(font_size = 10, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff") %>%
      scroll_box(width = "950px", height = "600px")
    
    HTML(tab03)
  }
  ## ------------------------------
  
  archivo <- reactive({
    req(input$file)
    path <- input$file$datapath
    excel_sheets(path)
  })
  
  output$sheet_ui <- renderUI({
    req(archivo())
    selectInput("hoja", "Seleccionar hoja:", choices = archivo())
  })
  
  datos <- eventReactive(input$analizar, {
    req(input$file, input$hoja)
    read_excel(input$file$datapath, sheet = input$hoja)
  })
  
  
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("Informe_Estadistico_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(datos())  
      
      temp_data <- tempfile(fileext = ".rds")
      saveRDS(datos(), temp_data)
      
      qmd_path <- normalizePath("reporte.qmd")
      
      rmarkdown::render(
        input = qmd_path,
        output_file = file,
        params = list(
          data_path = temp_data,
          var_cual_param = input$VarCual,
          var_cuan1_param = input$VarCuan,
          var_cuan2_param = input$VarCuan2,
          color_param = input$colx
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}
