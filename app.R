library(shiny) #Libreria shiny para funciones generales
library(shinyBS) #Para poder trabajar con funciones especiales de shiny como agregar popupscon comentarios
library(shinydashboard) #Para poder agregar iconos
library(tibble)
library(lubridate) #Para hacer calculos de fechas
library(devtools) #Esto es para cargar el paquete que aun esta en GITHUB
#devtools::install_github("ecosistemaglobal/inversion")
library(inversion) #Este es el paquete que he creado con todas las funciones para calcular la inversion
library("jsonlite")
library("httr")
library("data.table")
library("RJDBC") #para leer MDB
#Datos de regiones, provincias, localidades de  CNIG
#http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE#:~:text=Nomencl%C3%A1tor%20Geogr%C3%A1fico%20B%C3%A1sico%20de%20Espa%C3%B1a%20Descripci%C3%B3n%3A%20relaci%C3%B3n%20de,y%20latitud%20y%20UTM%20en%20su%20huso%20correspondiente.
#title: "Idealista_API"
#output: html_notebook
#Author: Antonio Martin-Cobos | Data Analytics Manager
#References
#https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
library("tidyverse")
library("shiny")
library("leaflet")
library("osmdata")
#Inicializa BBDD
library(RSQLite) #PAra trabajar con una base de datos
library("dotenv") #para proteccion archivo con contrseña de Idealiasta
library("kableExtra") #Para dar formato html a la salida de resultados
library("DT")  #Para poder ordenar la tabla de salida de inmuebles de Idealista

# Carga de catos de regiones, provincias y municipios
    #chequeo si existe la BBDD
        dbname <- "idealistaBBDD.sqlite"
        if (file.exists(dbname)) {
          # Si el archivo existe, simplemente cargar la base de datos
          con <- dbConnect(RSQLite::SQLite(), dbname)
        } else {
          # Si el archivo no existe, crear una nueva conexión y cargar los datos desde los archivos
            dbname <- "idealistaBBDD.sqlite"
            # Crea una conexión a la base de datos (esto también creará la base de datos si no existe)
            con <- dbConnect(RSQLite::SQLite(), dbname)
            #Llama a funcion para cargar localidades solo si la BBDD esta vacia
                  
                  # Comprobar si la tabla "provincias" está vacía
                  query <- "SELECT COUNT(*) FROM provincias"
                  num_registros_provincias <- dbGetQuery(con, query)
                  
                  # Comprobar si la tabla "municipios" está vacía
                  query <- "SELECT COUNT(*) FROM municipios"
                  num_registros_municipios <- dbGetQuery(con, query)
                  
                  # Ejecutar carga_datos_localidades si ambas tablas están vacías
                  if (num_registros_provincias == 0 && num_registros_municipios == 0) {
                      # Cargar datos de provincias y municipios (supongamos que tienes los marcos de datos)
                      provincias <- read.csv("./PROVINCIAS.csv", sep = ";", encoding = "UTF-8")
                      municipios <- read.csv("./MUNICIPIOS.csv", sep = ";", encoding = "UTF-8")
                      # Escribir datos de provincias en la base de datos
                      dbWriteTable(con, "provincias", provincias, overwrite = TRUE)
                      # Escribir datos de municipios en la base de datos
                      dbWriteTable(con, "municipios", municipios, overwrite = TRUE)
                  }
          }
        #Selecciona los datos de la base de atos
        query <- "SELECT * FROM provincias"
        provincias <- dbGetQuery(con, query)
        query <- "SELECT * FROM municipios"
        municipios <- dbGetQuery(con, query)
        dbDisconnect(con)
        




  
#Variables iniciales
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")

#Dataframe para poner la tabla de amortizacion
tabla_amortizacion <- data.frame(
  Contador = "Introduzca datos y pulse calcular",
  Fecha = 0,
  Cuota = 0,
  Interes = 0,
  Financiacion = 0,
  Deuda = 0,
  stringsAsFactors = FALSE
)

#Dataframe para poner la tabla de ROI
tabla_roi <- data.frame(
  Contador = "Introduzca datos y pulse calcular",
  Fecha = 0,
  Cuota = 0,
  Interes = 0,
  Financiacion = 0,
  Deuda = 0,
  stringsAsFactors = FALSE
)

#Dataframe datos generales
tabla_resultado <- data.frame(
  Descripcion = c("Entrada", "Ingresos","Deudas","Capacidad de pago","Préstamo solicitado"),
  Valor = c("Entrada que da como aportaciC3n inicial para cubrir gastos", "Ingresos netos mensuales de los solicitantes",
            "Deudas mensuales como otras hipotecas o préstamos","Máxima cantidad que puede pagar durante laduraciC3n escogida en bruto sin decontar comisiones",
            "Propuesta de préstamo no superior al 90% de la tasación"),
  Resultado = c(" "," "," "," "," ")
)
html_resultados <- knitr::kable(tabla_resultado, format = "html")

#Dataframe con datos de alquiler iniciales
tabla_alquiler<- data.frame(
  Descripcion = c("Gastos anuales","Mantenimientos", "Ingresos", "Rentabilidad total", "Rentabilidad mensual"),
  Valor = c("   ...   ","   ...   ","   ...   ","   ...   ","   ...   "),
  Resultado = c("   ...   ","   ...   ","   ...   ","   ...   ","   ...   ")
)
html_alquiler <- knitr::kable(tabla_alquiler, format = "html")

#Dataframe para poner la tabla de ROI
# ia_inversion <- data.frame(
#   Descripcion = c("Gastos anuales","Mantenimientos", "Ingresos", "Rentabilidad total", "Rentabilidad mensual"),
#   Valor = c("   ...   ","   ...   ","   ...   ","   ...   ","   ...   "),
#   Resultado = c("   ...   ","   ...   ","   ...   ","   ...   ","   ...   ")
# )
# html_ia_inversion <- knitr::kable(ia_inversion, format = "html")

#Dataframe para descargar datos de idealista y que sean uniformes
columnas_deseadas <- c(
  "propertyCode", "price", "propertyType", "operation", "size",
  "rooms", "bathrooms", "province", "municipality",
  "status", "url", "hasLift", "floor", "newDevelopment",
  "priceByArea", "district", "exterior", "highlight", "parkingSpace"
)
tabla_idealista_data <- data.frame(matrix(NA, ncol = length(columnas_deseadas), dimnames = list(NULL, columnas_deseadas)))


moneda="euro"

# Definimos una clase CSS personalizada para el sidebarMenu
tags$head(
  tags$style(HTML("
        .menu-text {
          font-size: 12px; /* Ajusta el tamaño de la fuente del menú aquí */
          margin-bottom: 1px;
          margin-top: 1px;
          color: white;
          display: flex;
          align-items: center;
        }
      "))
)

ui <- dashboardPage(
  dashboardHeader(title = "Calculadora de Hipoteca"),
  dashboardSidebar(
    sidebarMenu( 
      # #Inicio menu de IA-inversion
      menuItem("IA inversion", tabName = "IA-inversion",
               fluidRow(class="menu-text",column(width = 12,
                   selectInput("region", "Selecciona una Región:", choices = unique(provincias$COMUNIDAD_AUTONOMA), selected = "Comunidad de Madrid"),
                   selectInput("provincia", "Selecciona una Provincia:", choices = NULL),
                   selectInput("municipio", "Selecciona un Municipio:", choices = NULL)
               )),
               fluidRow(class="menu-text",column(width = 12,
                    selectInput("tipo_inmueble", "Tipo de inmueble:", choices = c("homes","offices", "local", "garages", "penthouse","flat","bedrooms")
               ))),
               fluidRow(class="menu-text",column(width = 12,
                 selectInput("tipo_inversion", "Tipo de inmueble:", choices = c("sale", "rent")
                 ))),                        
               fluidRow(class="menu-text",column(width = 12,
                  sliderInput("metros_inmueble", "Tamaño del inmueble:", min = 0, max = 500, value = c(10, 499))
               )),
               fluidRow(class="menu-text",column(width = 12,
               sliderInput("precio_inmueble", "Precio del inmueble:", min = 0, max = 5000000, value = c(150000, 1000000)),
               )),
              fluidRow(class="menu-text",
                column(width = 6,
                    tags$h6("Busca inmuebles"),
                    actionButton("boton_compra_alquiler", icon("calculator"),
                    title = "Busca inmuebles en BBDD con info extraida de Idealista. Los datos losa ctualizamos pero es posible que no sea la última versión")
                ),
                column(width = 6,
                    tags$h6("Busqueda IA", title = "Busqueda IA"),
                    actionButton("boton_busqueda_inmuebles", icon("calculator"),
                    title = "Esta aplicación busca en la Base de datos los inmuebles con la caractarística sque menciona comparando compra-venta y alquiler para poder extraer la mejor rentabilidad")
                )
              )     

      ),#Fin del menu IA-inversion      
      
      #Inicio Menu Capacidad de Pago
      menuItem("Capacidad de Pago", tabName = "menu_capacidad_pago",
               fluidRow(class="menu-text",column(width = 12,
                   tags$h6("Entrada", title = "Entrada aportada para los gastos"),
                   numericInput("entrada", "", value = 15000))
               ),
               fluidRow(class="menu-text",column(width = 12,
                   tags$h6("Ingresos", title = "Ingresos mensuales de los cuales solo se usará el 33%"),
                   numericInput("ingresos", "", value = 3300)),
               ),
               fluidRow(class="menu-text",column(width = 12,
                   tags$h6("Deudas", title = "Deudas mensuales que restan a los ingresos"),
                   numericInput("deudas", "", value = 560)),
               ),
               fluidRow(class = "menu-text",column(width = 4,
                   tags$h6("Capacidad de pago", title = "Capacidad de pago anual"),
                   actionButton("boton_capacidad_pago", icon("calculator"),
                                title = "Calcula la capacidad de pago en función de los ingresos y los gastos")),
                    column(width = 8,numericInput("capacidad_de_pago", "", value = 0))
               ),
               fluidRow(class="menu-text",
                    column(width = 12,tags$h6("Plazo deseado (anual y mensual)", title = "Plazo en años que desea pagar la hipoteca")),
               ),
               fluidRow(class="menu-text",
                    column(width = 6,numericInput("plazo", "", value = 26)),
                    column(width = 6,numericInput("plazo_meses", "", value = 312))
               )
      ),#Fin Menu Capacidad de Pago
      
      # #Inicio menu de vivienda
      menuItem("Vivienda", tabName = "menu_vivienda",
               fluidRow(class="menu-text",column(width = 12,
                   tags$h6("Precio venta", title = "Precio de venta del inmueble"),
                   numericInput("precio_venta", "", value = 180000))
               ),
               fluidRow(class="menu-text",column(width = 12,
                   tags$h6("Tasación", title = "Tasación oficial o propuesta"),
                   numericInput("tasacion", "", value = 200000))
               )
      ),#Fin del menu vivienda


      #Inicio del menu prestamo      
      menuItem("Prestamo", tabName = "menu_prestamo",
               fluidRow(class="menu-text",column(width = 12,
                 tags$h6("Prestamo solicitado", title = "Prestamo solicitado"),
                 numericInput("prestamo", "", value=180000))
               ),
               fluidRow(class="menu-text",column(width = 12,
                 tags$h6("Fecha inicial", title = "Fecha inicial"),
                 dateInput("fecha_inicial", "", value = floor_date(Sys.Date(), "month")))
               ),
               
               fluidRow(
                 class = "menu-text",
                 column(width = 6,
                        tags$h6("Calculo de plazos", title = "Calculo de plazos automatico")),
                 column(width = 2,
                        actionButton("auto_plazomeses", icon("calculator"),
                                     title = "Calculo del tiempo de hipoteca ajustado segun las condiciones dadas")),
                 column(width = 6,tags$h6("Plazos (Años)", title = "Plazos (Años y meses)")),
                 column(width = 6,tags$h6("Plazos (Meses)", title = "Plazos (Años y meses)")),
                 column(width = 6, numericInput("plazo_ingresos", "", value = 26)),
                 column(width = 6, numericInput("plazo_meses_ingresos", "", value = 312))
               ),
               
               fluidRow(class = "menu-text",column(width = 4,
               tags$h6("Entrada mínima", title = "Calcula la entrada mínima para cubrir el 90% máximo hipotecable"),
               actionButton("boton_calcular_entrada", icon("calculator"),
                            title = "Calcula la entrada mínima")),
                column(width = 8,numericInput("entrada_minima", "", value = 0))
               ),                 
               
               fluidRow(class = "menu-text",column(width = 4,
               tags$h6("Cuota óptima", title = "Calculame la cuota óptima segun las condicions dadas"),
               actionButton("auto_cuota", icon("calculator"),
                            title = "Calculame la cuota óptima segun las condiciones dadas")),
                column(width = 8,numericInput("cuota", "", value = 800))
               )
               
      ),#Fin del menu prestamo
      
      # #Inicio menu de condiciones bancarias
      menuItem("Condiciones bancarias", tabName = "tabla_condiciones",
               fluidRow(class="menu-text",column(width = 12,
               tags$h6("Años"),numericInput("plazo", "", value = 26),
               tags$h6("Meses"),numericInput("plazo_meses", "", value = 312))
               ),
               fluidRow(class = "menu-text",
                column(width = 6,tags$h6("Interés anual", title = "Interés anual")),
                column(width = 6,tags$h6("% Max hipoteca", title = "Porcentaje máximo de la hipoteca que concede el banco")),
                column(width = 6,numericInput("tasa", "", value = 4.2)),
                column(width = 6,numericInput("maxima_hipoteca", "", value = 90))
               ),          
               
               fluidRow(class = "menu-text",
                column(width = 6,tags$h6("% gestión", title = "Porcentaje de gastos de gestion (inmobiliaria + tasación) respecto el precio de venta")),
                column(width = 6,tags$h6("% banco", title = "Porcentaje de gastos bancarios, notariales,registro respecto el precio de venta")),
                column(width = 6,numericInput("comisiones", "", value = 7)),
                column(width = 6,numericInput("gastos_hipotecarios", "", value = 7))
               )
      ),#Fin del menu condiciones bancaria
      
      
      # #Inicio menu de alquiler
      menuItem("Alquiler", tabName = "tabla_alquiler",
               fluidRow(class = "menu-text",column(width = 12,tags$h6("Seguros + IBI(anual)", title = "Seguros + IBI(anual)"))),
               fluidRow(class = "menu-text",column(width = 12,numericInput("gastos_alquiler", "", value = 1000))),
               fluidRow(class = "menu-text",column(width = 12,tags$h6("Comunidad + Reserva(anual)", title = "Comunidad + Reserva(anual)"))),
               fluidRow(class = "menu-text",column(width = 12,numericInput("mantenimientos_alquiler", "", value = 1000))),
               fluidRow(class = "menu-text",column(width = 12,tags$h6("Ingresos anuales alquiler", title = "Ingresos anuales alquiler(anual)"))),
               fluidRow(class = "menu-text",column(width = 12,numericInput("ingresos_alquiler", "", value = 10000))),
               fluidRow(class = "menu-text",column(width = 4,tags$h6("Rendimiento alquiler", title = "Calculo del rendimiento de alquiler"),
                  actionButton("calcular_rendimiento_alquiler", icon("calculator"),
                  title = "Calculame la cuota óptima segun las condiciones dadas")),
                  column(width = 8,numericInput("rentabilidad_alquiler", "", value = 0)))
      )#Fin de menu de alquiler

      
    )#Fin del sidebarMenu
  ), #Fin de DashboardSideBar
  dashboardBody(
    tabsetPanel(
      # Creación de pestañas
      tabPanel("Cuadro datos", tableOutput("resultados")),
      tabPanel("Tabla de amortizacion", tableOutput("tabla_amortizacion")),
      tabPanel("ROI", tableOutput("tabla_roi")),
      tabPanel("Inmuebles", tableOutput("inmuebles"))
      )
    )#Fin de dashboardBody
) #Fin de dashboardPage

# Define la lC3gica de la aplicaciC3n
server <- function(input, output, session) {

  #salida de resultados tabla amortizacion
  # Mostrar la tabla HTML en la salida
  output$resultados <- renderUI({
    div(
      HTML(html_resultados),
      # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
      HTML(html_alquiler)
    )
  })
  
  
  #salida de resultados tabla amortizacion
  output$tabla_amortizacion <- renderTable({
    tabla_amortizacion
  })
  
  #salida de resultados tabla amortizacion
  output$tabla_roi <- renderTable({
    tabla_roi
  })
 


  
  
  #Recupera datos acerca la rentabilidad del alquiler
  observeEvent(input$calcular_rendimiento_alquiler,{
    gastos_alquiler<-input$gastos_alquiler
    mantenimientos_alquiler<-input$mantenimientos_alquiler
    ingresos_alquiler<-input$ingresos_alquiler
    datos_html_alquiler<-inversion::recolecta_datos_de_alquiler(gastos_alquiler,mantenimientos_alquiler,ingresos_alquiler)
    #html_alquiler<-datos_html_alquiler[[1]] #esto es la tabla
    
    html_alquiler<-paste0(datos_html_alquiler[[1]],datos_html_alquiler[[2]])
    
    rentabilidad_alquiler <- as.integer(datos_html_alquiler[[2]][[1]]) #Rentabilidad alquiler
    updateNumericInput(session, "rentabilidad_alquiler", value = rentabilidad_alquiler)

    #salida de resultados tabla amortizacion
    # Mostrar la tabla HTML en la salida
    output$resultados <- renderUI({
      div(
        #HTML(html_resultados),
        # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
        HTML(html_alquiler)
      )
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_amortizacion <- renderTable({
      tabla_amortizacion
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_roi <- renderTable({
      tabla_roi
    })
  })

  
  
  
  
  
  #Observa si cambian los valores iniciales
  observeEvent(c(input$entrada, input$gastos_hipotecarios, input$comisiones, input$precio_venta), {
    
    #Calculo del préstamo en funcion del precio del piso y los gastos
    entrada<-input$entrada; gastos_hipotecarios<-input$gastos_hipotecarios; tasacion<-input$tasacion
    comisiones<-input$comisiones; precio_venta<- input$precio_venta;
    
    nuevo_prestamo <- recalcular_prestamo(tasacion, entrada, gastos_hipotecarios, comisiones, precio_venta)
    updateNumericInput(session, "prestamo", value = nuevo_prestamo)
    
    #Calculo de todos los costes
    nuevo_prestamo <- input$precio_venta + (input$precio_venta * (input$gastos_hipotecarios / 100)) + (input$precio_venta * (input$comisiones / 100)) - input$entrada
    updateNumericInput(session, "prestamo", value = nuevo_prestamo)
    
  })
  
  #Observa si cambian los valores iniciales
  observeEvent(c(input$prestamo), {#, input$tasa, input$plazo, input$cuota,input$prestamo,input$tasacion
    tasacion <- input$tasacion
    tasa <- input$tasa 
    cuota <- input$cuota          
    prestamo <- input$prestamo
    plazo_meses <- input$plazo_meses
    fecha_inicial <- input$fecha_inicial
    ingresos<-input$ingresos
    deudas<-input$deudas
    entrada<-input$entrada
    entrada_minima<-input$entrada_minima
    
    tabla_amortizacion<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    tabla_roi<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    
    html_resultados<-inversion::recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deudas,entrada,entrada_minima)
  })  
  
  #Pulsamos boton calcular  
  observeEvent(input$calcular, {
    tasacion <- input$tasacion
    tasa <- input$tasa 
    cuota <- input$cuota          
    prestamo <- input$prestamo
    plazo_meses <- input$plazo_meses
    plazo_anios <- input$plazo
    fecha_inicial <- input$fecha_inicial
    ingresos<-input$ingresos
    deudas<-input$deudas
    entrada<-input$entrada
    entrada_minima<-input$entrada_minima
    
    tabla_amortizacion<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    tabla_roi<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    html_resultados<-inversion::recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deudas,entrada,entrada_minima)
    
    #salida de resultados tabla amortizacion
    # Mostrar la tabla HTML en la salida
    output$resultados <- renderUI({
      div(
        HTML(html_resultados),
        # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
        HTML(html_alquiler)
      )
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_amortizacion <- renderTable({
      tabla_amortizacion
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_roi <- renderTable({
      tabla_roi
    })
  })
  

  
  #Para calcular la cuota automaticamente
  observeEvent(input$auto_cuota, {
    # Realizar cC!lculo de la cuota
    tasacion <- input$tasacion
    tasa <- input$tasa
    cuota <- input$cuota          
    prestamo <- input$prestamo
    plazo_meses <- input$plazo_meses
    plazo_anios <- input$plazo
    fecha_inicial <- input$fecha_inicial
    entrada<-input$entrada
    entrada_minima<-input$entrada_minima
    
    tasa_mensual <- input$tasa / 100 / 12
    
    #Calcula la nueva cuota, redondeando solo 2 decimales
    nueva_cuota <- round(prestamo * (tasa_mensual) / (1 - (1 + tasa_mensual)^(-plazo_anios*12)),2)
    
    # Actualizar el valor de la cuota en la interfaz
    updateNumericInput(session, "cuota", value = nueva_cuota)
    cuota<-nueva_cuota
    ingresos<-input$ingresos
    deudas<-input$deudas
    tabla_amortizacion<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    tabla_roi<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    html_resultados<-inversion::recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deudas,entrada,entrada_minima)
    
    #salida de resultados tabla amortizacion
    # Mostrar la tabla HTML en la salida
    output$resultados <- renderUI({
      div(
        HTML(html_resultados),
        # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
        HTML(html_alquiler)
      )
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_amortizacion <- renderTable({
      tabla_amortizacion
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_roi <- renderTable({
      tabla_roi
    })
 
    
  })
  
  #Para calcular los meses automaticamente
  observeEvent(input$auto_plazomeses,{
    # CC!lculo del tiempo necesario para cancelar la hipoteca
    tasacion <- input$tasacion  
    tasa <- input$tasa
    cuota <- input$cuota          
    prestamo <- input$prestamo
    plazo_meses <- input$plazo_meses
    plazo_anios <- input$plazo
    fecha_inicial <- input$fecha_inicial
    entrada<-input$entrada
    entrada_minima<-input$entrada_minima
    
    tasa_mensual <- input$tasa / 100 / 12 
    nuevo_tiempo <- log(cuota / (cuota - prestamo * tasa_mensual)) / log(1 + tasa_mensual)
    
    nuevo_tiempo<-round(nuevo_tiempo,0)
    
    
    # Actualizar el valor de la cuota en la interfaz
    updateNumericInput(session, "plazo_meses", value = nuevo_tiempo)
    plazo_meses<-nuevo_tiempo
    ingresos<-input$ingresos
    deudas<-input$deudas
    
    tabla_amortizacion<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    tabla_roi<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    html_resultados<-inversion::recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deudas,entrada,entrada_minima)
    
    #salida de resultados tabla amortizacion
    # Mostrar la tabla HTML en la salida
    output$resultados <- renderUI({
      div(
        HTML(html_resultados),
        # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
        HTML(html_alquiler)
      )
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_amortizacion <- renderTable({
      tabla_amortizacion
    })
    
    #salida de resultados tabla amortizacion
    output$tabla_roi <- renderTable({
      tabla_roi
    })
 
    
    
  })
  
  #Para calcular la entada automaticamente
  observeEvent(input$boton_calcular_entrada,{
    tasacion<-input$tasacion
    gastos_hipotecarios<-input$gastos_hipotecarios
    comisiones<-input$comisiones
    precio_venta<-input$precio_venta
    entrada<-input$entrada
    entrada_minima<-input$entrada_minima
    
    tasa<-input$tasa
    cuota<-input$cuota
    prestamo<-input$prestamo
    plazo<-input$plazo
    plazo_meses<-input$plazo_meses
    plazo_ingresos<-input$plazo_ingresos
    plazo_meses_ingresos<-input$plazo_meses_ingresos
    fecha_inicial<-input$fecha_inicial
    
    maxima_hipoteca <-input$maxima_hipoteca
    
    ingresos<-input$ingresos
    deudas<-input$deudas
    
    entrada_minima <-round(calcular_entrada (tasacion, gastos_hipotecarios, comisiones,precio_venta,maxima_hipoteca),0)
    updateNumericInput(session, "entrada_minima", value = entrada_minima)
    
    
    tabla_amortizacion<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    tabla_roi<-inversion::crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
    
    html_resultados<-inversion::recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deudas,entrada, entrada_minima)
    
    #salida de resultados tabla amortizacion
    # Mostrar la tabla HTML en la salida
    output$resultados <- renderUI({
      div(
        HTML(html_resultados),
        # AquC- puedes agregar la segunda tabla HTML o cualquier otro contenido adicional
        HTML(html_alquiler)
      )
    })
  })  
  
  #Estas funciones actualizan los plazos de los campos aC1o y meses para que siempre este igual
  observeEvent(c(input$plazo,input$plazo_ingresos), {
    # Actualizar el valor de "Plazo (meses)" al modificar "Plazo (años)"
    updateNumericInput(session, "plazo_meses", value = round(input$plazo * 12,0))
    # Actualizar el valor de "Plazo (meses)" al modificar "Plazo (años)"
    updateNumericInput(session, "plazo_meses_ingresos", value = round(input$plazo_ingresos * 12,0))
  })
  observeEvent(c(input$plazo_meses,input$plazo_meses_ingresos), {
    # Actualizar el valor de "Plazo (años)" al modificar "Plazo (meses)"
    updateNumericInput(session, "plazo", value = round(input$plazo_meses / 12,1))
    # Actualizar el valor de "Plazo (años)" al modificar "Plazo (meses)"
    updateNumericInput(session, "plazo_ingresos", value = round(input$plazo_meses_ingresos / 12,1))
  })
  
  
  
  #Calculame el maximo prestamo segun mis ingresos, gastos, entrada, plazo e interes
  observeEvent(input$boton_capacidad_pago, {
    ingresos<-input$ingresos
    deudas<-input$deudas
    tasa<-input$tasa
    plazo_meses<-input$plazo_ingresos*12
    entrada<-input$entrada
    gastos_hipotecarios<-input$gastos_hipotecarios
    comisiones<-input$comisiones
    
    capacidad_pago<-calcular_capacidad_pago(ingresos, deudas, tasa, plazo_meses, entrada,gastos_hipotecarios,comisiones)
    capacidad_pago<-as.integer(capacidad_pago)
    
    updateNumericInput(session, "capacidad_de_pago", value = capacidad_pago)
  }) 
  

  
  # # Filtra las provincias y municipios según la región seleccionada
  observeEvent(input$region, {
    # Filtra las provincias según la región seleccionada
    provincias_filtradas <- filter(provincias, COMUNIDAD_AUTONOMA == input$region)
    updateSelectInput(session, "provincia", choices = provincias_filtradas$PROVINCIA)

  })

  #Filtra los municipios según la provincia seleccionada
  observeEvent(input$provincia, {
    if (!is.null(input$provincia) && input$provincia != "") {
      municipios_filtrados <- filter(municipios, PROVINCIA == input$provincia)
      municipios_filtrados <- subset(municipios_filtrados, select = c("PROVINCIA", "NOMBRE_ACTUAL","POBLACION_MUNI","CAPITAL","LONGITUD_ETRS89","LATITUD_ETRS89"))

      #Seleccion temporal de 10 primeras municipios msa grandes
      # Ordena los municipios por POBLACION_MUNI de mayor a menor
      municipios_ordenados <- municipios_filtrados[order(-municipios_filtrados$POBLACION_MUNI), ]
      # Selecciona los primeros 10 municipios
      primeros_10_municipios <- head(municipios_ordenados, 10)
      updateSelectInput(session, "municipio", choices = primeros_10_municipios$NOMBRE_ACTUAL)
    }
  })

  
# Evento a ejecutar cuando tenemos la localidad y buscar en Idealista
  observeEvent(input$boton_busqueda_inmuebles, {
    if (!is.null(input$provincia) && input$provincia != "" && !is.null(input$provincia) && input$municipio != "") {
      Print("este boton aun no tiene utilidad")
    }})
  
  # Evento a ejecutar cuando tenemos la localidad y buscar en Idealista
  observeEvent(input$boton_compra_alquiler, {
    if (!is.null(input$provincia) && input$provincia != "" && !is.null(input$provincia) && input$municipio != "") {
      region<-input$region
      provincia<-input$provincia
      municipio<-input$municipio
      coordenadas<-busca_coordenadas_municipios(municipios,municipio)
      longitud <- coordenadas["longitud"]
      latitud <- coordenadas["latitud"]
      tipo_inmueble <-input$tipo_inmueble
      metros_inmueble_min<-input$metros_inmueble[1]
      metros_inmueble_max<-input$metros_inmueble[2]
      precio_inmueble_min<-input$precio_inmueble[1]
      precio_inmueble_max<-input$precio_inmueble[2]
      tipo_inversion<-input$tipo_inversion
      #Salida por pestaña de datos ia_inversion
      tabla_filtro_busqueda <- data.frame(
        Variable = c("Región: ", "Provincia: ", "Municipio: ","Tipo de inversion", "Latitud: ", "Longitud: ","Tipo vivienda: ","Metros cuadrados desde: ","Metros cuadrados hasta: ","Precio inmueble desde: ","Precio inmueble hasta: "),
        Valor = c(region, provincia, municipio,tipo_inversion, latitud, longitud,tipo_inmueble,metros_inmueble_min,metros_inmueble_max,precio_inmueble_min,precio_inmueble_max)
      )
      tabla_filtro_busqueda_html <- knitr::kable(tabla_filtro_busqueda, format = "html")
      
      
      #Tengo que ver si estos datos son de idealista o de mi BBDD
      tabla_idealista_data<-extrae_info_bbdd_idealista() #Conectate a la BBDD y extrae el contenido
      
      
      output$inmuebles <- renderUI({
        fluidRow(
          column(width = 6, tabla_filtro_busqueda_html),
          column(width = 6,
                 # Muestra resultados de la búsqueda
                 # Obtén los nombres de las columnas
                 column_names <- colnames(tabla_idealista_data),
                 # Crea la tabla de datos
                 datatable(tabla_idealista_data, options = list(pageLength = 10, colnames = column_names))
          )
        )

    })}})
  

#Busca datos de coordenada segun municipio
busca_coordenadas_municipios <- function(municipios, Nombre) {
    # Obtiene y muestra las coordenadas cuando se selecciona un municipio
      municipio_seleccionado <- filter(municipios, NOMBRE_ACTUAL == input$municipio)
      latitud <- as.numeric(gsub(",", ".", municipio_seleccionado$LATITUD_ETRS89))
      longitud <- as.numeric(gsub(",", ".", municipio_seleccionado$LONGITUD_ETRS89))
      return(c(longitud = longitud, latitud = latitud))
  }


#Filtra datos que no intresan o son erroneos  
filtra_datos_de_idealista <- function(idealista_data) {
  idealista_data <- idealista_data
  #Transforma resultado en formato html
  # Quito columnas para facilitar por ahora
browser()
  idealista_data[,suggestedTexts:=NULL]
  idealista_data[,detailedType:=NULL]
  idealista_data <- distinct(idealista_data)
  idealista_data <- idealista_data[order(idealista_data[["propertyCode"]])]
  
  

  #tabla_idealista_data <- idealista_data %>% select(-thumbnail,-externalReference,-numPhotos)
  #tabla_idealista_data <- idealista_data %>% select(-country,-neighborhood,-latitude,-longitude,-showAddress,-distance,-hasVideo,-newDevelopment,-priceByArea,-has3DTour,-has360,-hasStaging,-superTopHighlight,-topNewDevelopment,-description)
 

  
  # Verificar si las columnas existen en los datos descargados
  columnas_faltantes <- setdiff(columnas_deseadas, colnames(idealista_data))
  
  # Si faltan columnas, agrégalas con valores NA
  if (length(columnas_faltantes) > 0) {
    for (col in columnas_faltantes) {
      idealista_data[[col]] <- NA
    }
  }
  
  
  # Combinar los datos descargados con el DataFrame vacío
  tabla_idealista_data <- rbind(tabla_idealista_data, tabla_idealista_data)
  
  # Asegurarse de que las columnas tengan el tipo de dato adecuado
  
  browser()
  #Convierte los nombres segun me interesa
  tabla_idealista_data$price <- as.numeric(tabla_idealista_data$price)
  tabla_idealista_data$size <- as.numeric(tabla_idealista_data$size)
  tabla_idealista_data$propertyType <- ifelse(tabla_idealista_data$propertyType == "flat", "apartamento", tabla_idealista_data$propertyType)
  tabla_idealista_data <- tabla_idealista_data %>%mutate(propertyType = ifelse(propertyType == "penthouse", "ático", propertyType))
  tabla_idealista_data <- tabla_idealista_data %>%mutate(operation = ifelse(operation == "sale", "venta", operation))
  tabla_idealista_data <- tabla_idealista_data %>% rename(Tamaño = size)
  #tabla_idealista_data$price <- format(tabla_idealista_data$price,big.mark = ",", decimal.mark = "")
  tabla_idealista_data <- tabla_idealista_data %>% rename(Precio = price)
  tabla_idealista_data <- tabla_idealista_data %>% rename("Tipo de vivienda" = propertyType)
  tabla_idealista_data <- tabla_idealista_data %>% rename(Operacion = operation)
  tabla_idealista_data <- tabla_idealista_data %>% rename(Habitaciones = rooms)
  tabla_idealista_data <- tabla_idealista_data %>% rename(Baños = bathrooms)
  tabla_idealista_data <- tabla_idealista_data %>% rename(Municipio = municipality)
  tabla_idealista_data <- tabla_idealista_data %>% rename(Barrio = district)
  
  #tabla_idealista_data <- tabla_idealista_data %>% rename("exterior" = "exterior.1")
  #tabla_idealista_data <- tabla_idealista_data %>% mutate(exterior = ifelse(exterior == NA, "NO", "SI"))
  
  
  tabla_idealista_data <- tabla_idealista_data %>% rename("ID_inmueble" = propertyCode)
  tabla_idealista_data$priceByArea[is.na(tabla_idealista_data$priceByArea)] <- 0
  tabla_idealista_data <- tabla_idealista_data %>% mutate(parkingSpace = ifelse(isTRUE(parkingSpace), TRUE, FALSE))
  tabla_idealista_data <- tabla_idealista_data %>% mutate(hasLift = ifelse(is.na(hasLift), 0, hasLift))
  tabla_idealista_data <- tabla_idealista_data %>% mutate(floor = ifelse(is.na(floor), 0, floor))
  tabla_idealista_data <- tabla_idealista_data %>% mutate(Barrio  = ifelse(is.na(Barrio ), " ", Barrio ))

  return(tabla_idealista_data)
}

extrae_info_bbdd_idealista <- function() {
  #Leer contenido de la BBDD
  # Conecta con la base de datos SQLite
  
  con <- dbConnect(RSQLite::SQLite(), dbname = "idealistaBBDD.sqlite")
  # Obtiene la lista de tablas en la base de datos
  #tables <- dbListTables(con)
  # Muestra la lista de tablas
  #print(tables)
  
  #Leer contenido de la tabla. Un dia aqui haré antes un filtro para saber si tengo datos en local o los ,tengo quie descagrar de Idealista
  idealista_conexion() #Llama a funcion para conectar con Idelista. Pendiente definir los datos a enviar
  
  #Conexion a la base da datos local
  idealista_data <- dbReadTable(con, "idealista_data")
  
  dbDisconnect(con)
  return(tabla_idealista_data)
  



}


#Programita para descargar info de la conexion Idealista  
idealista_conexion <- function(jsonlite, base64_enc, httr, POST, add_headers, content, suggestedTexts, detailedType) {
  # Aqui la parte de idealista
  dotenv::load_dot_env("./acceso.env") #leo d forma secreta usuario y contraseña de acceso
  # Accede a tus credenciales
  #consumer_key <- Sys.getenv("CONSUMER_KEY")
  #consumer_secret <- Sys.getenv("CONSUMER_SECRET")
  consumer_key <- "udkfigsvweox8jf07ww92b2545fy1r4i"
  consumer_secret <- "ZvDnMFh709uY"
  
  #Use basic authentication
  secret <- jsonlite::base64_enc(paste(consumer_key, consumer_secret, sep = ":"))
  req <- httr::POST("https://api.idealista.com/oauth/token",
                    httr::add_headers(
                      #"Authorization" = paste("Basic", gsub("n", "", secret)),
                      "Authorization" = paste("Basic", secret, sep = " "),
                      "Content-Type" = "application/x-www-form-urlencoded;charset=utf-8"
                    ),
                    body = "grant_type=client_credentials"
  )
  token <- paste("Bearer", httr::content(req)$access_token)
  
  region<-input$region
  provincia<-input$provincia
  municipio<-input$municipio
  coordenadas<-busca_coordenadas_municipios(municipios,municipio)

  y <- coordenadas["longitud"] #lineas verticales. meridianos
  y <- coordenadas["latitud"] #lineas horizontales a diversas alrutas, tropicos, ecuador..
  #maxItems = '50' #numero maximo de items a descargar maximo 50
  # distance = '3000'  #distandia desde el centro de las coordenada
  type <- input$tipo_inmueble #tipo de vivienda homes, offices, premises, garages, bedrooms
  op <- input$tipo_inversion  #tipo de intercambio puede ser sale o rent
  minprice <- input$precio_inmueble[1]  #precio minimo
  maxprice <- input$precio_inmueble[2] #precio maximo
  minsize <- input$metros_inmueble[1]  #Metroscuadrados minimos
  maxsize <- input$metros_inmueble[2] #metroscuadrados maximos
  tipo_inversion<-input$tipo_inversion
 

  #url user parameters
  x = '40.47344' #Majadahonda
  y = '-3.87233' #Majadahonda
  maxItems = '50' #numero maximo de items a descargar maximo 50
  distance = '3000'  #distandia desde el centro de las coordenada
  #type = 'homes' #tipo de vivienda homes, offices, premises, garages, bedrooms
  #op = 'sale'  #tipo de intercambio puede ser sale o rent
  #minprice = '0'  #precio minimo
  #maxprice = '300000' #precio maximo
  #minsize = '0'  #Metroscuadrados minimos
  #maxsize = '200' #metroscuadrados maximos
  
  
  #url fixed parameters
  site = 'https://api.idealista.com/3.5/es/search?'
  loc = 'center='
  country = '&country=es'
  maxitems = '&maxItems=50'
  pages = '&numPage=1'
  dist = '&distance='
  property = '&propertyType='
  operation = '&operation='
  pricefrom = '&minPrice='
  priceto = '&maxPrice='
  misize = '&minSize='
  masize = '&maxSize='
  chalet = '&chalet=0'
  
  
  url <- paste(site, loc, x, ',', y, country, maxitems, pages, dist, distance, property, type, operation, op, pricefrom, minprice, priceto, maxprice, misize, minsize, masize, maxsize, sep = "")
  
  res <- httr::POST(url, httr::add_headers("Authorization" = token))
  cont_raw <- httr::content(res)
  browser()

  
  #Este programita es para que estandaricen los datos. haydatos que llegan con diferente numero de columnas
      # Obtén la lista de elementos
      elementList <- cont_raw[["elementList"]]
      
      # Encuentra la longitud máxima de las sublistas
      max_length <- max(sapply(elementList, length))
      
      # Completa las sublistas para que tengan la misma longitud
      elementList <- lapply(elementList, function(x) {
        if (length(x) < max_length) {
          missing_cols <- max_length - length(x)
          x <- c(x, rep(NA, missing_cols))
        }
        return(x)
      })
      
      # Convierte la lista de elementos en un marco de datos
      library(data.table)
      data <- rbindlist(elementList, use.names = TRUE, fill = TRUE, idcol = NULL)
  
  
  
  
  

    
  data<-filtra_datos_de_idealista(data)
  browser()

  #Si tras consultar a Idealista en internet ha traido datos, que los agregue a la BBDD interna
  if (!is.null(cont_raw[["elementList"]])) {

        #Almacena datos en base de datos
        # Conecta con la base de datos SQLite
        con <- dbConnect(RSQLite::SQLite(), dbname = "idealistaBBDD.sqlite")
             
        # Crea una nueva tabla llamada "idealista_data" y escribe los datos en ella
        dbWriteTable(con, name = "idealista_data2", value = data, row.names = FALSE, append = TRUE)
        
  }
  else{
    print("No hay datos con estas consulta")
  }

  #Leer contenido de la BBDD
  # Conecta con la base de datos SQLite
  con <- dbConnect(RSQLite::SQLite(), dbname = "idealistaBBDD.sqlite")
  
  # Obtiene la lista de tablas en la base de datos
  tables <- dbListTables(con)
  # Muestra la lista de tablas
  print(tables)
  
  #Leer contenido de la tabla
  provincias <- dbReadTable(con, "provincias")
  municipios <- dbReadTable(con, "municipios")
  idealista_data <- dbReadTable(con, "idealista_data")

  # Cierra la conexión con la base de datos
  dbDisconnect(con)
  
}

  
}#fin de backlog server

# Crea la aplicaciC3n Shiny
shinyApp(ui = ui, server = server)


#Subir paquete a GITHUB
# git init
# git add README.md
# git config --global user.email "ecosistemaglobal@gmail.com"
# git config --global user.name "ecosistemaglobal"
# git add inversion.Rproj project.Rproj
# git commit -m "first commit"
# git branch -M main
# 
# git remote set-url origin https://github.com/ecosistemaglobal/inversion.git
# git remote add origin https://github.com/ecosistemaglobal/inversion.git
# 
# git push -u origin main

#Subir paquetes a CRAN
# install.packages("devtools")
# library(devtools)
# devtools::check() 
# devtools::build()
# devtools::release()










