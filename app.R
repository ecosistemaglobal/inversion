library(shiny) #Libreria shiny para funciones generales
library(shinyBS) #Para poder trabajar con funciones especiales de shiny como agregar popupscon comentarios
library(shinydashboard) #Para poder agregar iconos
library(tibble)
library(lubridate) #Para hacer calculos de fechas
library(inversion) #este es el paquete que he creado a mano
#Variables iniciales
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")

#Dataframe para poner la tabla de amortizacion
tabla_amortizacion <- data.frame(
  Contador = "Introduzca datos y pulse calcular",
  Fecha = 0,
  Cuota = 0,
  Interes = 0,
  Financiacien = 0,
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


moneda="euro"


# Define la interfaz de la aplicaciC3n
ui <- fluidPage(
  titlePanel("Calculadora de Hipoteca"),
  sidebarLayout(
    sidebarPanel(
      #DIV de capacidad de pago      
      div(
        tags$style(HTML("
            h5 {margin-bottom: 2px;margin-top: 2px;
              color: blue; /* Color azul para los encabezados h5 */
            }
            .campos_formulario_h6 {
              margin-bottom: 1px;
              margin-top: 1px;
              color: black;
              display: flex;
              align-items: center;
            }
            divider {
              border-top: 2px solid black;
              margin: 10px 0;
            }
            resultado-error {
              color: red;
            }
            .campo-rojo {
              color: red;
            }
            #Este estilo es para quitar las flechas del campo y ganar un poco de espacio
            .shiny-input-container > .form-control {
                -webkit-appearance: none;
                -moz-appearance: textfield;
              }
              .shiny-input-container > .form-control::-webkit-inner-spin-button,
              .shiny-input-container > .form-control::-webkit-outer-spin-button {
                -webkit-appearance: none;
              }
          ")),
        fluidRow(
          column(width = 12,
                 h5("Ingresos")
          )),
        fluidRow(
          column(width = 4,
                 class = "campos_formulario_h6",
                 tags$h6("Entrada", title = "Entrada aportada para los gastos"),
                 box(
                   width = NULL,  # Deja que el ancho del box se ajuste automáticamente al contenido
                   solidHeader = TRUE,
                   numericInput("entrada", "", value = 15000)
                 )
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Ingresos", title = "Ingresos mensuales de los cuales solo se usará el 33%"),
                 numericInput("ingresos", "", value = 3300)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Deudas", title = "Deudas mensuales que restan a los ingresos"),
                 numericInput("deudas", "", value = 560)
          )),

        fluidRow(        
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Capacidad", title = "Capacidad de pago anual"),
                 box(
                   width = 1,
                   solidHeader = TRUE,
                   actionButton("boton_capacidad_pago",
                                icon("calculator"),
                                title = "Calcula la capacidad de pago en función de los ingresos y los gastos")
                 )),
          column(width = 8,
                 class="campos_formulario_h6",
                 tags$h6("Capacidad de pago", title = "Capacidad de pago en función de los ingresos y los gastos"),
                 tags$input(id = "capacidad_de_pago",
                            type = "number",
                            value = 0,
                            style = "pointer-events: none;  border: none;")
          ),
        ),
        fluidRow(          
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Plazo deseado (anual) ", title = "Plazo en años que desea pagar la hipoteca"),
                 numericInput("plazo_ingresos", "", value = 26)
          ),
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Plazo deseado (meses)", title = "Plazo en meses que desea pagar la hipoteca"),
                 numericInput("plazo_meses_ingresos", "", value = 312)
          ))
      ),#Fin del div capacidad de pago
      div(class = "divider", style = "border-top: 4px solid darkgray; margin: 10px ;"),
      #DIV de vivienda
      div(
        fluidRow(
          column(width = 12,
                 h5("Vivienda")
          )),
        fluidRow(
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Precio venta", title = "Precio de venta del inmueble"),
                 numericInput("precio_venta", "", value = 180000)
          ),
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Tasación", title = "Tasación oficial o propuesta"),
                 numericInput("tasacion", "", value = 200000)
          ),
        )
      ),#Fin del div vivienda 
      #DIV de prestamo      
      div(class = "divider", style = "border-top: 4px solid darkgray; margin: 10px 0;"),
      div(
        fluidRow(
          column(width = 12,
                 h5("Préstamo")
          )),
        fluidRow(
          column(width = 4,
                 tags$h6("Interés anual"),
                 class="campos_formulario_h6",
                 numericInput("tasa", "", value = 4.2)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Plazo (anual)"),
                 numericInput("plazo", "", value = 26)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Plazo (meses)"),
                 numericInput("plazo_meses", "", value = 312)
          )
        ),
        fluidRow(
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("% banco", title = "Porcentaje de gastos respecto el precio de venta"),
                 numericInput("gastos_hipotecarios", "", value = 7)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("% gestión", title = "Porcentaje de gastos de gestion respecto el precio de venta"),
                 numericInput("comisiones", "", value = 5)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("% Max hipoteca", title = "Porcentaje máximo de la hipoteca que concede el banco"),
                 numericInput("maxima_hipoteca", "", value = 90)
          ),
        ),
       fluidRow(
        column(width = 5,
               class="campos_formulario_h6",
               tags$h6("Préstamo solicitado"),
               numericInput("prestamo", "", value = 158000)
        ),
       ),
       fluidRow(
         column(width = 4,
                class="campos_formulario_h6",
                tags$h6("Entrada mínima", title = "Calcula la entrada mínima para cubrir el 90% máximo hipotecable"),
                box(
                  width = 1,
                  solidHeader = TRUE,
                  actionButton("boton_calcular_entrada",
                               icon("calculator"),
                               title = "Calcula la entrada mínima")
                )),
         column(width = 8,
                class="campos_formulario_h6",
                tags$h6("Entrada minima", title = "Entrada mínima para cubrir gastos de compraventa y 10% de tasacion o precio de venta"),
                tags$input(id = "entrada_minima",
                           type = "number",
                           value = 0,
                           style = "pointer-events: none;  border: none;")
         ),
       )
      ), #Fin del div prestamo
      #DIV de pago hipoteca      
      div(class = "divider", style = "border-top: 4px solid darkgray; margin: 10px 0;"),
      div(
        fluidRow(
          column(width = 12,
                 h5("Pago de hipoteca")
          )),
        fluidRow(
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Cuota mensual"),
                 numericInput("cuota", "", value = 800)
          ),
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Fecha inicial"),
                 dateInput("fecha_inicial", "", value = floor_date(Sys.Date(), "month"))
          ),
        ),
      ),#Fin del div pago de hipoteca
      #DIV de alquiler      
      div(class = "divider", style = "border-top: 4px solid darkgray; margin: 10px 0;"),
      div(
        fluidRow(
          column(width = 12,
                 h5("Alquiler")
          )),
        fluidRow(
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Seguros + IBI(anual) "),
                 numericInput("gastos_alquiler", "", value = 1000)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Comunidad + Reserva(anual)"),
                 numericInput("mantenimientos_alquiler", "", value = 1000)
          ),
          column(width = 4,
                 class="campos_formulario_h6",
                 tags$h6("Ingresos anuales alquiler"),
                 numericInput("ingresos_alquiler", "", value = 15000)
          )),
        fluidRow(
          column(width = 6,
                 actionButton("calcular_rendimiento_alquiler", "Calcula rendimiento alquiler")
          ),
          column(width = 6,
                 class="campos_formulario_h6",
                 tags$h6("Rentabilidad alquiler mensual"),
                 numericInput("rentabilidad_alquiler", "", value = 0)
          )),
      ),#Fin del div alquiler
      div(class = "divider", style = "border-top: 4px solid darkgray; margin: 10px 0;"),
      #DIV de capacidad de pago
      div(
        fluidRow(
          column(width = 12,
                 actionButton("auto_plazomeses", "Calculame los meses"),
                 actionButton("auto_cuota", "Calculame la cuota")
          )
          ),
      )#Fin del div botones
    ),#Fin de sidebarPanel
    mainPanel(
      tabsetPanel(
        # Creacion de pestañas
        tabPanel("Cuadro datos",column(width = 12,),
                 tableOutput("resultados"),
        ),
        tabPanel("Tabla de amortizacion",column(width = 12, ),
                 tableOutput("tabla_amortizacion")
        ) ,
        tabPanel("ROI",column(width = 12, ),
                 tableOutput("tabla_roi")
        )
        
     )#Fin de tabsetPanel
   )#Fin de mainPanel
 )#Fin de sidebarLayout
)#Fin de fluidPage

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
  
  
  #Recupera datos accerca la rentabilidad del alquiler
  observeEvent(input$calcular_rendimiento_alquiler,{
    
    gastos_alquiler<-input$gastos_alquiler
    mantenimientos_alquiler<-input$mantenimientos_alquiler
    ingresos_alquiler<-input$ingresos_alquiler
    datos_html_alquiler<-inversion::recolecta_datos_de_alquiler(gastos_alquiler,mantenimientos_alquiler,ingresos_alquiler)
    html_alquiler<-datos_html_alquiler[1]
    updateNumericInput(session, "rentabilidad_alquiler", value = datos_html_alquiler[2])
    
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
  
  
  
  
  
  
  #Observa si cambian los valores iniciales
  observeEvent(c(input$entrada, input$gastos_hipotecarios, input$comisiones, input$precio_venta), {
    #Calculo del préstamo en funcion del precio del piso y los gastos
    entrada<-input$entrada; gastos_hipotecarios<-input$gastos_hipotecarios
    comisiones<-input$comisiones; precio_venta<- input$precio_venta
    nuevo_prestamo <- recalcular_prestamo(tasacion, entrada, gastos_hipotecarios, comisiones, precio_venta)
    updateNumericInput(session, "prestamo", value = nuevo_prestamo)
    
    #Calculo de todos los costes
    prestamo <- input$precio_venta + (input$precio_venta * (input$gastos_hipotecarios / 100)) +
      (input$precio_venta * (input$comisiones / 100)) - input$entrada
    updateNumericInput(session, "prestamo", value = prestamo)
    
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
  observeEvent(input$plazo, {
    # Actualizar el valor de "Plazo (meses)" al modificar "Plazo (años)"
    updateNumericInput(session, "plazo_meses", value = round(input$plazo * 12,0))
  })
  observeEvent(input$plazo_meses, {
    # Actualizar el valor de "Plazo (años)" al modificar "Plazo (meses)"
    updateNumericInput(session, "plazo", value = round(input$plazo_meses / 12,1))
  })
  observeEvent(input$plazo_ingresos, {
    # Actualizar el valor de "Plazo (meses)" al modificar "Plazo (años)"
    updateNumericInput(session, "plazo_meses_ingresos", value = round(input$plazo_ingresos * 12,0))
  })
  observeEvent(input$plazo_meses_ingresos, {
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
  
  
}#fin de backlog server

# Crea la aplicaciC3n Shiny
shinyApp(ui = ui, server = server)
