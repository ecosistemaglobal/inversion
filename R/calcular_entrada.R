#' calcular_entrada
#' @param tasacion Value of the building, flat, house
#' @param gastos_hipotecarios #Gastos hipotecarios
#' @param comisiones #Comisiones de gestores
#' @param precio_venta #Precio de venta
#' @param maxima_hipoteca #hipoteca maxima
#' @return entrada Cantidad de dinero a aportar como entrada
#' @export
#'
#' @examples
#' #calcular_entrada (tasacion, gastos_hipotecarios, comisiones,precio_venta,maxima_hipoteca)
#' calcular_entrada (200000, 7, 7,180000,90)
calcular_entrada <- function(tasacion, gastos_hipotecarios, comisiones,precio_venta,maxima_hipoteca) {
  gastos_hipotecarios<- 100-gastos_hipotecarios
  comisiones<-100-comisiones
  entrada <- (precio_venta * (1 - (maxima_hipoteca / 100))) + (precio_venta *(1 - (gastos_hipotecarios / 100))) + (precio_venta * (1 - (comisiones / 100)))
  return(entrada)
}
