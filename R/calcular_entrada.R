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
  gastos_hipotecarios<- 100-gastos_hipotecarios
  comisiones<-100-comisiones
  entrada <- (precio_venta * (1 - (maxima_hipoteca / 100))) + (precio_venta *(1 - (gastos_hipotecarios / 100))) + (precio_venta * (1 - (comisiones / 100)))
  return(entrada)
}
