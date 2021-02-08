#Leer datos de archivo csv
#librería para leer datos csv
library(readr)

#cargar datos
datos <- read_csv("C:/Users/ASUS/Downloads/PracticaDataMiningR/data_plantas.csv")
#mostrar datos
View(datos)

#Eliminar la columnas categóricas de la tabla y  colocar Codigo como nombres de filas
#quitar la primera columna
#data <- datos [,-1]

#Seleccionar columnas de la 4 a la 18
data <-datos[,c(4:20)]
#Etiquetar filas con codigo
rownames(data) <- datos$Codigo
View(data)
