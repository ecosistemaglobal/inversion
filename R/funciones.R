#Calcula hipoteca y devuelve tabla de amortizacion
calcula_hipoteca_basica <- function(tasa,cuota,prestamo,plazo_meses,fecha_inicial) {

  # Cálculo del tiempo necesario para cancelar la hipoteca
  tasa_mensual <- tasa / 100 / 12
  cuota_mensual <- cuota
  prestamo <- prestamo
  plazo_meses <- plazo_meses
  fecha_inicial <- fecha_inicial

  tiempo <- log((cuota_mensual / tasa_mensual) / (cuota_mensual / tasa_mensual - prestamo)) / log(1 + tasa_mensual)

}




# Salida con datos generales respecto hipoteca
  recolecta_datos_de_resultado <- function(tasacion,tasa,cuota,prestamo,plazo_meses,fecha_inicial,ingresos,deuda) {
    tasacion <- tasacion
    tasa <- tasa
    tasa_mensual <- tasa / 100 / 12
    cuota_mensual <- cuota
    prestamo <- prestamo
    plazo_meses <- plazo_meses
    fecha_inicial <- fecha_inicial
    ingresos <- ingresos
    deuda <- deuda
    deuda_final <- round(calcular_deuda_final(tasa, cuota, prestamo, plazo_meses, fecha_inicial),2)
    intereses_pagados<- round(calcular_intereses_pagados(tasa, cuota, prestamo, plazo_meses, fecha_inicial),2)

    tabla_resultado <- data.frame(
      Descripcion = c("Tasación","Préstamo", "Interés anual", "Inicio de la hipoteca", "Meses necesarios para cancelar la hipoteca",
                      "Años necesarios para cancelar la hipoteca", "Cuota mensual a pagar", "Deuda al final de la hipoteca",
                      "Ingresos", "Deuda", "Intereses pagados"),
      Valor = c(as.character(tasacion),as.character(prestamo), as.character(tasa), as.character(fecha_inicial),
                as.character(plazo_meses), as.character(round(plazo_meses/12, 2)), as.character(cuota_mensual),
                as.character(deuda_final),as.character(ingresos),as.character(deuda),as.character(intereses_pagados)),
      Resultado = c("OK","OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK")
    )

  tabla_resultado <- formatea_resultados(tabla_resultado)



    # Obtener el dataframe en formato HTML
    library(knitr);library(kableExtra)

    html_resultados <- tabla_resultado
    # Convertir dataframe a tabla HTML con kableExtra
    tabla_html <- kable(html_resultados, format = "html") %>%  kable_styling()
    # Aplicar estilo CSS para resaltar filas con Col2 igual a 2
    html_table_resultados2 <- tabla_html %>%
      row_spec(which(html_resultados$Resultado != "OK"), color = "red")
    return(html_table_resultados2)
  }

  # Salida con datos generales respecto alquiler
  recolecta_datos_de_alquiler <- function(gastos_alquiler,mantenimientos_alquiler,ingresos_alquiler) {

    gastos_alquiler <- gastos_alquiler
    mantenimientos_alquiler <- mantenimientos_alquiler
    ingresos_alquiler <- ingresos_alquiler
    rentabilidad_total <- ingresos_alquiler - gastos_alquiler - mantenimientos_alquiler
    rentabilidad_mensual <- round(rentabilidad_total/12,2)

    tabla_resultado_alquiler <- data.frame(
      Descripcion = c("Gastos anuales","Mantenimientos", "Ingresos", "Beneficios limpios","Rentabilidad mensual"),
      Valor = c(gastos_alquiler,mantenimientos_alquiler,ingresos_alquiler,rentabilidad_total,rentabilidad_mensual)
    )

    # Obtener el dataframe en formato HTML
    library(knitr);library(kableExtra)

    html_resultado_alquiler <- tabla_resultado_alquiler
    # Convertir dataframe a tabla HTML con kableExtra
    tabla_html_alquiler <- kable(html_resultado_alquiler, format = "html")

    # Aplicar estilo CSS cuando el alquiler es negativo
    # Verificar si el valor en la celda es mayor que 0 y cambiar el color
    if (!is.na(as.numeric(html_resultado_alquiler[4, 2])) && as.numeric(html_resultado_alquiler[4, 2]) < 0) {
      html_resultado_alquiler <- tabla_html_alquiler %>%
        row_spec(row = 4, color = "red") %>%
        row_spec(row = 5, color = "red")
    }
    else{
      html_resultado_alquiler<- kable(html_resultado_alquiler, format = "html") %>%  kable_styling()
    }
    return(c(html_resultado_alquiler, rentabilidad_mensual))
  }



#Pone formato a la salida tabla_resultado paraque aparezca en rojosi hay datos destacables
formatea_resultados<-function(tabla_resultado){

  tabla_resultado<-tabla_resultado
  # Obtén el valor de la columna "Deuda al final de la hipoteca" restando ultima cuota 1 mes
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
    tabla_resultado$Resultado[2] <- "KO. El préstamo es mayor  al 90% de la tasación "
  }

  # Modifica casilla de años y mesesen caso que sea superior a 30 años 360 meses
  meses<-as.numeric(tabla_resultado$Valor[5])

  if (meses > 360) {
    tabla_resultado$Resultado[5] <- "KO. El préstamo tiene una duración muy larga"
    tabla_resultado$Resultado[6] <- "KO. El préstamo tiene una duración muy larga"
  }  else{tabla_resultado$Resultado[5]<-"OK";tabla_resultado$Resultado[6]<-"OK"}

  #Revisa si la cuota a pagar es menor al 33% de los ingresos limpios

  ingreso_neto =  as.numeric(tabla_resultado$Valor[9])-as.numeric(tabla_resultado$Valor[10])
  ingreso_neto = ingreso_neto * 0.33
  if(ingreso_neto>as.numeric(tabla_resultado$Valor[7])){
    tabla_resultado$Resultado[7] <- "OK"
    tabla_resultado$Resultado[9] <- "OK"}
   else{

    tabla_resultado$Resultado[7] <- sprintf("KO, Puedes subirlos ingresos, aumentar la entrada, cancelar deudas o aumentar los plazos")
    tabla_resultado$Resultado[9] <- sprintf("KO, 33 porciento de  %s es el pago máximo.",ingreso_neto)
    }
  return(tabla_resultado)
  }




#Con esta funcion devuelve el último valor de la deuda tras finalizar el periodo. Idóneo en calculo manual
  calcular_deuda_final <- function(tasa, cuota, prestamo, plazo_meses, fecha_inicial) {
    tabla_amortizacion <- crea_tabla_amortizacion(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
    deuda_final <- tail(tabla_amortizacion$Deuda, n = 1)
    if (deuda_final<cuota){deuda_final=0}else{deuda_final=deuda_final-cuota}
    return(deuda_final)
  }

#Con esta funcion devuelve los interesess pagados
  calcular_intereses_pagados <- function(tasa, cuota, prestamo, plazo_meses, fecha_inicial) {
    tabla_amortizacion <- crea_tabla_amortizacion(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
    intereses_pagados <- round(sum(tabla_amortizacion[4]),2)
    if (intereses_pagados<1){deuda_final=0}else{intereses_pagados=intereses_pagados-cuota}
    return(intereses_pagados)
}

  # Función para recalcular el préstamo en funcino del precio de venta, comisiones..etc..
  recalcular_prestamo <- function(tasacion, entrada, gastos_hipotecarios, comisiones, precio_venta) {
    prestamo <- precio_venta + (precio_venta * (gastos_hipotecarios / 100)) + (precio_venta * (comisiones / 100)) - entrada
    return(prestamo)
  }

  calcular_entrada <- function(tasacion, gastos_hipotecarios, comisiones,precio_venta,maxima_hipoteca) {
    entrada <- precio_venta * ((100-maxima_hipoteca)/100) + (precio_venta/gastos_hipotecarios) + (precio_venta/comisiones) #Equivale a 90%
    return(entrada)
  }

# Función para calcular el préstamo máximo en funcion de los ingresos
  calcular_capacidad_pago <- function(ingresos, deudas, tasa, plazo_meses, entrada, gastos_hipotecarios, comisiones) {
    tasa_mensual <- (tasa / 12)  # Conversión de tasa anual a mensual
    ingresos_disponibles <- ingresos - deudas
    cuota_mensual <- ingresos_disponibles * 0.33  # Pago mensual máximo permitido (33% de ingresos disponibles)
    capacidad_pago <- (cuota_mensual * plazo_meses) + entrada

    return(capacidad_pago)
  }







