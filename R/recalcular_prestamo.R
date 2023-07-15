# Funci??n para recalcular el prestamo en funcion del precio de venta, comisiones..etc..
#' Title
#'
#' @param tasacion
#' @param entrada
#' @param gastos_hipotecarios
#' @param comisiones
#' @param precio_venta
#'
#' @return
#' @export
#'
#' @examples
recalcular_prestamo <- function(tasacion, entrada, gastos_hipotecarios, comisiones, precio_venta) {
  prestamo <- precio_venta + (precio_venta * (gastos_hipotecarios / 100)) + (precio_venta * (comisiones / 100)) - entrada
  return(prestamo)
}
