#' Tittle Load database from sql file or load files if is first time ever you start.
#'
#' @param provincias Dataframe con datos de las provincias que ademas contiene las regiones
#' @param municipios Dataframe con datos de los municipios
#' @param idealista_data Datos de los inmuebles que se cargaron en el pasado de Idealista. BBDD interna para explotacion de historico
#'
#' @return c(provincias, municipios,idealista_data))Return 3 tables with provincias, municipios and idealista_data
#'
#' @examples
#' carga_bbdd_inmuebles()
#'
#' @export
#' 
#' 
#' 
#' 
carga_bbdd_inmuebles <- function() {
#Inicializa BBDD
  library("RSQLite") #PAra trabajar con una base de datos
  library("dotenv") #para proteccion archivo con contrseña de Idealiasta
  # Carga de catos de regiones, provincias y municipios
  #chequeo si existe la BBDD
  # Obtén la ruta al archivo MUNICIPIOS.csv en la carpeta inst
    ruta_sqlite <- system.file("inst", "idealistaBBDD.sqlite", package = "inversion")
    
    # Si el archivo no existe, crear una nueva conexión y cargar los datos desde los archivos        
    if (file.exists(ruta_sqlite))
   {
     # Si el archivo existe, simplemente cargar la base de datos
     con <- dbConnect(RSQLite::SQLite(), ruta_sqlite)
   }
   else 
   {
     # Obtén la ruta al archivo MUNICIPIOS.csv en la carpeta inst
    ruta_provincias <- system.file("inst", "PROVINCIAS.csv", package = "inversion")
    # Cargar datos de provincias y municipios (supongamos que tienes los marcos de datos)
    provincias <- read.csv(ruta_provincias, sep = ",", encoding = "UTF-8")
    rm(ruta_provincias)
    # Obtén la ruta al archivo MUNICIPIOS.csv en la carpeta inst
    ruta_municipios <- system.file("inst", "MUNICIPIOS.csv", package = "inversion")
    # Luego puedes usar la ruta para cargar el archivo, por ejemplo, con read.csv
    municipios <- read.csv(ruta_municipios, sep = ",", encoding = "UTF-8")
    rm(ruta_municipios)
    
    # Escribir datos de provincias en la base de datos
    dbWriteTable(con, "provincias", provincias, overwrite = TRUE)
    # Escribir datos de municipios en la base de datos
    dbWriteTable(con, "municipios", municipios, overwrite = TRUE)
   }
 #Selecciona los datos de la base de datos
   query <- "SELECT * FROM provincias"
   provincias <- dbGetQuery(con, query)
   query <- "SELECT * FROM municipios"
   municipios <- dbGetQuery(con, query)
   query <- "SELECT * FROM idealista_data"
   idealista_data <- dbGetQuery(con, query)
   return(list(provincias, municipios, idealista_data))
   dbDisconnect(con)
}

