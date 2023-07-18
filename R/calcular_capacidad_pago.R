# Funci??n para calcular la capacidad de pago mensual
#' monthly payment capacity
#'
#' @param ingresos earning monthly
#' @param deudas Debt monthly
#' @param tasa  Anual tax
#' @param plazo_meses mortgage term in months
#' @param entrada Debt monthly
#' @param gastos_hipotecarios #Son los de gestion
#' @param comisiones #Comisiones bancarias
#' 
#' @return capacidad_pago #Devuelve la capacidad de pago
#' @export
#'
calcular_capacidad_pago <- function(ingresos, deudas, tasa, plazo_meses, entrada, gastos_hipotecarios, comisiones) {
  tasa_mensual <- (tasa / 12)  # ConversiC3n de tasa anual a mensual
  ingresos_disponibles <- ingresos - deudas
  cuota_mensual <- ingresos_disponibles * 0.33  # Pago mensual maximo permitido (33% de ingresos disponibles)
  capacidad_pago <- (cuota_mensual * plazo_meses) + entrada

  return(capacidad_pago)
}
