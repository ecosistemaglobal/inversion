# Calculo del tiempo necesario para roi
#' Create ROI table
#'
#' @param tasa  Anual tax
#' @param cuota Monthly quota to pay
#' @param prestamo Import mortgage loan (prestamo)
#' @param plazo_meses mortgage term in months
#' @param fecha_inicial Start of mortgage
#'
#' @return Return table with amortizacion. there are 5 columns
#' @export
#'
#' @examples
# crea_tabla_roi(tasa,cuota,prestamo,plazo_meses,fecha_inicial)
#' \dontrun{
#' crea_tabla_roi(4.2,1200,300,200000,"2024-12-31")
#' }
crea_tabla_roi <- function(tasa,cuota,prestamo,plazo_meses,fecha_inicial) {
  #Dataframe para poner la tabla de amortizacion
  tabla_resultado_roi <- data.frame(
    Descripcion = c("Prestamo", "Interes anual", "Inicio de la hipoteca", "Meses necesarios para cancelar la hipoteca",
                    "Tiempo anual necesario para cancelar la hipoteca", "Cuota mensual a pagar", "Deuda al final de la hipoteca"),
    Valor = c("pendiente", "pendiente", "pendiente","pendiente", "pendiente", "pendiente","pendiente"),
    Resultado = c("pendiente", "pendiente", "pendiente","pendiente", "pendiente", "pendiente","pendiente")
  )
  tasa <- tasa
  tasa_mensual <- tasa / 100 / 12
  cuota_mensual <- cuota
  prestamo <- prestamo
  plazo_meses <- plazo_meses
  
  fecha_inicial <- fecha_inicial <- as.Date(fecha_inicial, format = "%Y-%m-%d")
  # Crear tabla de amortizacion
  fecha_inicial<-lubridate::date(fecha_inicial)
  fechas <- seq(lubridate::`%m+%`(fecha_inicial, months(1)), by = "months", length.out = plazo_meses)
  fechas <- format(fechas, "%d-%m-%Y")
  cuotas <- rep(cuota_mensual, plazo_meses)

  intereses <- numeric(plazo_meses)
  deuda <- numeric(plazo_meses)
  deuda[1] <- prestamo

  for (i in 1:plazo_meses) {
    intereses[i] <- deuda[i] * tasa_mensual
    deuda[i+1] <- deuda[i] - (cuotas[i] - intereses[i])
  }
  tabla_resultado_roi <- data.frame(
    Contador = seq_along(fechas),
    Fecha = fechas,
    Cuota = cuotas,
    Interes = intereses,
    Financiacion = cuotas - intereses,
    Deuda = deuda[1:plazo_meses]
  )

  return(tabla_resultado_roi)

}

