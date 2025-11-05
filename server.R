library(shiny)
library(tidyverse)
library(readxl)
library(kableExtra)
library(formattable)
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
  
  output$Grafico02 <- renderPlot({
    t02 <- datos() %>% select(input$VarCuan, input$VarCuan2)
    colnames(t02) <- c("Variable1", "Variable2")
    
    t02 <- t02 %>% ggplot(aes(x=Variable1, y=Variable2)) + geom_point(fill = input$colx)
    return(t02)
  })
  
  output$TablaVariable <- function(){
    res01 <- datos() %>% select(IDENTIFICACION, FECHAADJUDICACION, ESTADO_CIVIL, MONTO_OTORGADO)
    res01 %>% kable() %>% kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff",) %>% 
      scroll_box(width = "700px", height = "300px")
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
      paste0("Informe_Estadistico_", Sys.Date(), ".pdf")
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