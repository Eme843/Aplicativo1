library(shiny)
library(tidyverse)
library(readxl)
library(kableExtra)
library(formattable)
library(lubridate)
library(MASS)
library(RColorBrewer)
library(quarto)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$VarCualitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "character") %>% pull(Variable)
    selectInput("VarCual", "Seleccione la variable:", choices = vars)
  })
  
  output$VarCuantitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan", "Seleccione la variable:", choices = vars)
  })
  output$VarCuantitativa2 <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan2", "Seleccione la variable:", choices = vars)
  })
  
  output$Grafico01 <- renderPlot({
    t01 <- datos() %>% select(input$VarCual)
    colnames(t01) <- "Variable"
    
    t01 <- t01 %>% group_by(Variable) %>% summarise(Registros = n()) %>% 
      ggplot(aes(x=Variable, y=Registros)) + geom_col(fill = input$colx)
    return(t01)
  })
  
  # ==== EMELY  
  #======GRÁFICO DE DISPERSIÓN: Edad vs. Monto Otorgado ====
  output$Grafico02 <- renderPlot({
  
    # Verificamos que las variables existan
    req("FECHANACIMIENTO" %in% names(datos()), "MONTO_OTORGADO" %in% names(datos()))
    
    # Crear variable edad (en años)
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
    
    # Filtrar por provincia seleccionada
    datos_filtrados <- datos() %>%
      filter(PROVINCIA_DOMICILIO == input$provincia_sel)
    
    # Crear tabla de frecuencias por tipo de crédito
    tabla_tipos <- table(datos_filtrados$TIPO)
    
    # Validar que existan datos
    validate(
      need(length(tabla_tipos) > 0, "No existen registros para la provincia seleccionada.")
    )
    
    # Calcular porcentajes
    porcentajes <- round(100 * tabla_tipos / sum(tabla_tipos), 1)
    etiquetas <- paste0(porcentajes, "%")  # ← solo el porcentajeetas <- paste0(names(tabla_tipos), " (", porcentajes, "%)")
    
    # Colores suaves tipo pastel
    colores_pastel <- brewer.pal(n = length(tabla_tipos), "Set3")
  
  # Ajustar márgenes para dejar espacio a la derecha
     par(mar = c(1, 1, 2, 1))  # margen derecho grande para la leyenda
    
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
  
  
  #-------------

  #------------------------ PRIMERA TABLA ANI--------------------- 
 output$TablaVariableANITA <- function(){
    req(datos())  
    res01 <- datos() %>%
      mutate(
        EDAD = floor(as.numeric(difftime(Sys.Date(), FECHANACIMIENTO, units = "days")) / 365.25)
      ) %>%
      select(
        NUM_ID,
        EDAD,
        TIPO,
        NIVEL_EDUCACION
      )
    
    res01 %>%
      kable(
        align = "c",
        caption = "Tabla de Edad, Tipo y Nivel Educativo por NUM_ID"
      ) %>%
      kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff") %>%
      scroll_box(width = "700px", height = "300px")
  }


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

  
  output$CreditosXProv <- function(){
    res02 <- datos() %>% group_by(PROVINCIA_DOMICILIO) %>% summarise(Registros = n()) %>% 
      mutate(PORCENTAJE = percent(Registros/sum(Registros))) %>% arrange(desc(Registros))
    colnames(res02) <- c("PROVINCIA", "CREDITOS", "PORCENTAJE")
    res02$CREDITOS = cell_spec(res02$CREDITOS, color = ifelse(res02$CREDITOS <= 100, "red", "blue"))
    res02$PORCENTAJE <- color_bar("lightgreen")(res02$PORCENTAJE)
    
    tab02 <- res02 %>% kable("html", escape = F, booktabs = TRUE) %>% kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff")
    HTML(tab02)
  }
  
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
      temp_data <- tempfile(fileext = ".rds")
      saveRDS(datos(), temp_data)
      
      # Obtener las variables de entrada que definen los gráficos
      var_cual <- input$VarCual
      var_cuan1 <- input$VarCuan
      var_cuan2 <- input$VarCuan2
      color <- input$colx
      
      qmd_path <- normalizePath("reporte.qmd")
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Convertir QMD a RMD temporal
      file.copy(qmd_path, temp_rmd, , overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        params = list(data_path = temp_data,
                      var_cual_param = var_cual, # Variable cualitativa
                      var_cuan1_param = var_cuan1, # Variable cuantitativa 1
                      var_cuan2_param = var_cuan2, # Variable cuantitativa 2
                      color_param = color # Color
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  

}

