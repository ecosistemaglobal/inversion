library(lubridate) #PAra hacer calculos de fechas
# CC!lculo del tiempo necesario para cancelar la hipoteca
#' Create amortizacion table with basic infomration accordind input valuees
#'
#' @param tasa  Annual tax
#' @param cuota Monthly quota to pay
#' @param prestamo Import mortgage loan (prestamo)
#' @param plazo_meses mortgage term in months
#' @param fecha_inicial Start of mortgage
#'
#' @return Return table with amortizacion. there are 5 columns
#' @export
#'
#' @examples
# crea_tabla_amortizacion(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
#' \dontrun{
#' crea_tabla_amortizacion(4.2,1200,300,200000,"2024-12-31")
#' }
#'
crea_tabla_amortizacion <- function(tasa,cuota,prestamo,plazo_meses,fecha_inicial) {
  #Dataframe para poner la tabla de amortizacion
  tabla_resultado_amortizacion <- data.frame(
    Contador = "Introduzca datos y pulse calcular",
    Fecha = 0,
    Cuota = 0,
    Interes = 0,
    Financiacion = 0,
    Deuda = 0,
    stringsAsFactors = FALSE
  )
  resultados <- data.frame(
    Descripcion = "Introduzca datos y pulse calcular",
    Resultado = "OK"
  )
  tabla_resultado <- data.frame(
    Descripcion = c("Prestamo", "Interes anual", "Inicio de la hipoteca", "Meses necesarios para cancelar la hipoteca",
                    "AC1os necesarios para cancelar la hipoteca", "Cuota mensual a pagar", "Deuda al final de la hipoteca"),
    Valor = c("pendiente", "pendiente", "pendiente","pendiente", "pendiente", "pendiente","pendiente"),
    Resultado = c("pendiente", "pendiente", "pendiente","pendiente", "pendiente", "pendiente","pendiente")
  )


  tasa <- tasa
  tasa_mensual <- tasa / 100 / 12
  cuota_mensual <- cuota
  prestamo <- prestamo
  plazo_meses <- plazo_meses
  fecha_inicial <- fecha_inicial <- as.Date(fecha_inicial, format = "%d-%m-%Y")
  # Crear tabla de amortizacion
  fechas <- seq(fecha_inicial %m+% months(1), by = "months", length.out = plazo_meses)
  fechas <- format(fechas, "%d-%m-%Y")
  cuotas <- rep(cuota_mensual, plazo_meses)

  intereses <- numeric(plazo_meses)
  deuda <- numeric(plazo_meses)
  deuda[1] <- prestamo

  for (i in 1:plazo_meses) {
    intereses[i] <- deuda[i] * tasa_mensual
    deuda[i+1] <- deuda[i] - (cuotas[i] - intereses[i])
  }
  tabla_resultado_amortizacion <- data.frame(
    Contador = seq_along(fechas),
    Fecha = fechas,
    Cuota = cuotas,
    Interes = intereses,
    Financiacion = cuotas - intereses,
    Deuda = deuda[1:plazo_meses]
  )
  return(tabla_resultado_amortizacion)

}


