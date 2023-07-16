#' Recopila los datos mas importantes y lo devuelve en formato HTML con colores
#' Create output result with all the core values
#'
#' @param tasacion  Value of the building, flat, house
#' @param tasa  Anual tax
#' @param cuota Monthly quota to pay
#' @param prestamo Import mortgage loan (prestamo)
#' @param plazo_meses mortgage term in months
#' @param fecha_inicial Start of mortgage
#' @param ingresos earning monthly
#' @param deuda Debt monthly
#' @param entrada Down payment contributed to cover initial expenses and 90% of the sale price.
#' @param entrada_minima Minimum apportation to be abble to have the mortgage accorging earning and debt
#' @param deuda_final Final value of all the mortgage
#' @param intereses_pagados All the interests to pay during all the mortgage
#' @return Return table with amortizacion. there are 5 columns
#' @export
#'
#' @examples
# recolecta_datos_de_resultado(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deuda,entrada,entrada_minima)
#' \dontrun{
#' recolecta_datos_de_resultado(180000,4.2,1200,200000,300,"2024-12-31",4000,500,15000,0)
#' }
#'
# Salida con datos generales respecto hipoteca
recolecta_datos_de_resultado <- function(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deuda,entrada,entrada_minima) {
  tasacion <- tasacion
  tasa <- tasa
  tasa_mensual <- tasa / 100 / 12
  cuota_mensual <- cuota
  prestamo <- prestamo
  plazo_meses <- plazo_meses
  fecha_inicial <- fecha_inicial
  ingresos <- ingresos
  deuda <- deuda
  entrada <- entrada
  entrada_minima <- entrada_minima
  deuda_final <- round(inversion::calcular_deuda_final(tasa, cuota, prestamo, plazo_meses, fecha_inicial),2)
  intereses_pagados<- round(inversion::calcular_intereses_pagados(tasa, cuota, prestamo, plazo_meses, fecha_inicial),2)

  tabla_resultado <- data.frame(
    Descripcion = c("Tasación","Préstamo", "Interés anual", "Inicio de la hipoteca", "Meses necesarios para cancelar la hipoteca",
                    "Años necesarios para cancelar la hipoteca", "Cuota mensual a pagar", "Deuda al final de la hipoteca",
                    "Ingresos", "Deuda", "Intereses pagados", "Entrada", "Entrada minima"),
    Valor = c(as.character(tasacion),as.character(prestamo), as.character(tasa), as.character(fecha_inicial),
              as.character(plazo_meses), as.character(round(plazo_meses/12, 2)), as.character(cuota_mensual),
              as.character(deuda_final),as.character(ingresos),as.character(deuda),as.character(intereses_pagados),as.character(entrada),as.character(entrada_minima)),
    Resultado = c("OK","OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK")
  )

  tabla_resultado <- formatea_resultados(tabla_resultado)

 
  # Obtener el dataframe en formato HTML
  html_resultados <- tabla_resultado
  # Convertir dataframe a tabla HTML con kableExtra
  tabla_html <- knitr::kable(html_resultados, format = "html") %>%  kableExtra::kable_styling()
  # Aplicar estilo CSS para resaltar filas con Col2 igual a 2
  html_table_resultados2 <- tabla_html %>%
    row_spec(which(html_resultados$Resultado != "OK"), color = "red")
  return(html_table_resultados2)
}

#Pone formato a la salida tabla_resultado paraque aparezca en rojosi hay datos destacables
formatea_resultados<-function(tabla_resultado){

  tabla_resultado<-tabla_resultado
  # Obt??n el valor de la columna "Deuda al final de la hipoteca" restando ultima cuota 1 mes
  deuda_final <- as.numeric(tabla_resultado$Valor[8])
  cuota <- as.numeric(tabla_resultado$Valor[7])
  deuda_final<-deuda_final-cuota
  # Modifica casilla de resultado en caso que la deuda sea mayor que 0 al acabar la hipoteca

  if (length(deuda_final) > 0 && deuda_final > 0) {
    tabla_resultado$Resultado[8] <- "KO. Acaba la hipoteca debiendo dinero"
  }
  else
  {tabla_resultado$Valor[8]=0;tabla_resultado$Resultado[8]="OK"}

  # Modifica casilla de tasacion en caso que el prestamo sea mayor al 90% de la tasacion
  tasacion<-as.numeric(tabla_resultado$Valor[1])
  prestamo<-as.numeric(tabla_resultado$Valor[2])
  if (length(tasacion) > 0 && ((prestamo*100)/tasacion) > 90) {
    tabla_resultado$Resultado[2] <- "KO. El pr??stamo es mayor  al 90% de la tasaci??n "
  }

  # Modifica casilla de a??os y mesesen caso que sea superior a 30 a??os 360 meses
  meses<-as.numeric(tabla_resultado$Valor[5])

  if (meses > 360) {
    tabla_resultado$Resultado[5] <- "KO. El pr??stamo tiene una duraci??n muy larga"
    tabla_resultado$Resultado[6] <- "KO. El pr??stamo tiene una duraci??n muy larga"
  }  else{tabla_resultado$Resultado[5]<-"OK";tabla_resultado$Resultado[6]<-"OK"}

  #Revisa si la cuota a pagar es menor al 33% de los ingresos limpios

  ingreso_neto =  as.numeric(tabla_resultado$Valor[9])-as.numeric(tabla_resultado$Valor[10])
  ingreso_neto = ingreso_neto * 0.33
  if(ingreso_neto>as.numeric(tabla_resultado$Valor[7])){
    tabla_resultado$Resultado[7] <- "OK"
    tabla_resultado$Resultado[9] <- "OK"}
  else{

    tabla_resultado$Resultado[7] <- sprintf("KO, Puedes subirlos ingresos, aumentar la entrada, cancelar deudas o aumentar los plazos")
    tabla_resultado$Resultado[9] <- sprintf("KO, 33 porciento de  %s es el pago m??ximo.",ingreso_neto)
  }
  

  # Revisa si la entrada que se da(entrada) es suficiente respecto el calculo de la entrada minima (entrada_minima)
  if (tabla_resultado$Valor[12] < tabla_resultado$Valor[13]) {
    tabla_resultado$Resultado[12] <- "KO. Entrada minima necesaria"
    tabla_resultado$Resultado[13] <- "KO. Entrada minima necesaria"
  }  else{tabla_resultado$Resultado[12]<-"OK";tabla_resultado$Resultado[13]<-"OK"}
  
  
  return(tabla_resultado)
}


