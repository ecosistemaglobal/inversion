#' Title Funci??n para recalcular el prestamo en funcion del precio de venta, comisiones..etc..
#'
#' @param tasacion #tasacion
#' @param entrada #entrada
#' @param gastos_hipotecarios #gastos_hipotecarios
#' @param comisiones #comisiones
#' @param precio_venta #precio_venta
#' @export
#' @return prestamo #Devuelve el calor del prestamo necesario
#' @examples
#' recalcular_prestamo (200000,45000,7,7,18000)
recalcular_prestamo <- function(tasacion, entrada, gastos_hipotecarios, comisiones, precio_venta) {
  prestamo <- precio_venta + (precio_venta * (gastos_hipotecarios / 100)) + (precio_venta * (comisiones / 100)) - entrada
  return(prestamo)
}
