#' calcular_entrada
#'
#' @param tasacion
#' @param gastos_hipotecarios
#' @param comisiones
#' @param precio_venta
#' @param maxima_hipoteca
#'
#' @return
#' @export
#'
#' @examples
calcular_entrada <- function(tasacion, gastos_hipotecarios, comisiones,precio_venta,maxima_hipoteca) {
  entrada <- precio_venta * ((100-maxima_hipoteca)/100) + (precio_venta/gastos_hipotecarios) + (precio_venta/comisiones) #Equivale a 90%
  return(entrada)
}
