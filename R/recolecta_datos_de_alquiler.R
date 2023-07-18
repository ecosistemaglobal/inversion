#' Calculate profits based on the expenses and income from a rental
#'
#' @param gastos_alquiler Annual expenses
#' @param mantenimientos_alquiler Annual expenses
#' @param ingresos_alquiler Annual earnings
#'
#' @return Return table with all the information and final profit
#' @export
#'
#' @examples
#' gastos_alquiler <- 1000
#' mantenimientos_alquiler <- 1000
#' ingresos_alquiler <- 15000
#' recolecta_datos_de_alquiler(gastos_alquiler, mantenimientos_alquiler, ingresos_alquiler)
#' # Output: 1083.33
#'
#' @import knitr
#' @import kableExtra
#' @import magrittr
recolecta_datos_de_alquiler <- function(gastos_alquiler, mantenimientos_alquiler, ingresos_alquiler) {
  gastos_alquiler <- gastos_alquiler
  mantenimientos_alquiler <- mantenimientos_alquiler
  ingresos_alquiler <- ingresos_alquiler
  rentabilidad_total <- ingresos_alquiler - gastos_alquiler - mantenimientos_alquiler
  rentabilidad_mensual <- round(rentabilidad_total/12,2)
  tabla_resultado_alquiler <- data.frame(
    Descripcion = c("Gastos anuales","Mantenimientos", "Ingresos", "Beneficios limpios","Rentabilidad mensual"),
    Valor = c(gastos_alquiler,mantenimientos_alquiler,ingresos_alquiler,rentabilidad_total,rentabilidad_mensual)
  )
  html_resultado_alquiler <- tabla_resultado_alquiler
  # Convertir dataframe a tabla HTML con kableExtra
  tabla_html_alquiler <- knitr::kable(html_resultado_alquiler, format = "html")

  # Aplicar estilo CSS cuando el alquiler es negativo
  # Verificar si el valor en la celda es mayor que 0 y cambiar el color
  if ((!is.na(as.numeric(html_resultado_alquiler[4, 2]))) && (as.numeric(html_resultado_alquiler[4, 2]) < 0)) {
    html_resultado_alquiler <- tabla_html_alquiler %>% row_spec(row = 4, color = "red") %>% row_spec(row = 5, color = "red")
  }
  return(c(html_resultado_alquiler, rentabilidad_mensual))
}

