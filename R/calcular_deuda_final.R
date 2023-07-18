#' Calcula el coste de toda la hipoteca
#' Calculate the total cost of the mortgage.
#'
#' @param tasa  Anual tax
#' @param cuota Monthly quota to pay
#' @param prestamo Import mortgage loan (prestamo)
#' @param plazo_meses mortgage term in months
#' @param fecha_inicial Start of mortgage
#' @return Return total cost of the mortgage
#' @export
#'
#' @examples
# calcular_deuda_final(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
#' \dontrun{
#' calcular_deuda_final(4.2,1200,200000,300,"2024-12-31")
#' }
#'
# Salida con datos generales respecto hipoteca
#Con esta funcion devuelve el C:ltimo valor de la deuda tras finalizar el periodo. IdC3neo en calculo manual
calcular_deuda_final <- function(tasa, cuota, prestamo, plazo_meses, fecha_inicial) {
  tabla_amortizacion <- crea_tabla_amortizacion(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
  deuda_final <- utils::tail(tabla_amortizacion$Deuda, n = 1)
  if (deuda_final<cuota){deuda_final=0}else{deuda_final=deuda_final-cuota}
    return(deuda_final)
}
