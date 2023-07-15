#' Calcula los intereses que se van a pagar durante toda la hipoteca
#' Calculate the interest to be paid over the entire mortgage.
#'
#' @param tasacion  Value of the building, flat, house
#' @param tasa  Anual tax
#' @param cuota Monthly quota to pay
#' @param prestamo Import mortgage loan (prestamo)
#' @param plazo_meses mortgage term in months
#' @param fecha_inicial Start of mortgage

#' @return Return interest to be paid over the entire mortgag
#' @export
#'
#' @examples
# recolecta_datos_de_resultado(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
#' \dontrun{
#' recolecta_datos_de_resultado(4.2,1200,200000,300,"2024-12-31")
#' }
#'
# Salida con datos generales respecto hipoteca
#Con esta funcion devuelve el C:ltimo valor de la deuda tras finalizar el periodo. IdC3neo en calculo manual
#Con esta funcion devuelve los interesess pagados
calcular_intereses_pagados <- function(tasa, cuota, prestamo, plazo_meses, fecha_inicial) {
  tabla_amortizacion <- crea_tabla_amortizacion(tasa, cuota, prestamo, plazo_meses, fecha_inicial)
  intereses_pagados <- round(sum(tabla_amortizacion[4]),2)
  if (intereses_pagados<1){deuda_final=0}else{intereses_pagados=intereses_pagados-cuota}
  return(intereses_pagados)
}
