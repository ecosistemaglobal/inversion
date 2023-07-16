# Funci??n para calcular la capacidad de pago mensual
#' monthly payment capacity
#'
#' @param ingresos
#' @param deudas
#' @param tasa
#' @param plazo_meses
#' @param entrada
#' @param gastos_hipotecarios
#' @param comisiones
#'
#' @return
#' @export
#'
#' @examples
#' calcular_capacidad_pago(3000,500,4.2,300,15000,7.5,7.5)
#' #262500
calcular_capacidad_pago <- function(ingresos, deudas, tasa, plazo_meses, entrada, gastos_hipotecarios, comisiones) {
  tasa_mensual <- (tasa / 12)  # ConversiC3n de tasa anual a mensual
  ingresos_disponibles <- ingresos - deudas
  cuota_mensual <- ingresos_disponibles * 0.33  # Pago mensual maximo permitido (33% de ingresos disponibles)
  capacidad_pago <- (cuota_mensual * plazo_meses) + entrada

  return(capacidad_pago)
}
