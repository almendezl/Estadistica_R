library(readr)
library(dplyr)
library(ggplot2)
install.packages("modes")
library(modes)

DB <- read_csv(file = "C:/Users/Lenovo YOGA500/Downloads/Delito_Hurto_Personas.csv", 
               col_types = list(
                 "FECHA"=col_character(), # La importamos como caracter, luego le quitaremos la hora a las que tienen hora. P.ej. "05/01/2015 12:00:00 AM"
                 "DEPARTAMENTO"=col_factor(),
                 "MUNICIPIO"=col_factor(),
                 "DIA"=col_factor(),
                 "HORA"=col_time(),
                 "BARRIO"=col_character(),
                 "ZONA"=col_factor(),
                 "CLASE SITIO"=col_factor(),
                 "ARMA EMPLEADA"=col_factor(),
                 "MOVIL AGRESOR"=col_factor(),
                 "MOVIL VICTIMA"=col_factor(),
                 "EDAD"=col_number(),
                 "SEXO"=col_factor(),
                 "ESTADO CIVIL"=col_factor(),
                 "PAIS NACE"=col_factor(),
                 "CLASE EMPLEADO"=col_factor(),
                 "PROFESION"=col_factor(),
                 "ESCOLARIDAD"=col_factor(levels = c("ANALFABETA","PRIMARIA","SECUNDARIA","TECNICO","TECNOLOGO","SUPERIOR","NO REPORTADO")),
                 "CODIGO DANE"=col_character(),
                 "2015"=col_skip()               ))
View(DB)
# Modifica la fecha. Le quita usando la expresión regular sustituyendo (\d{2}/\d{2}/\d{4}).* por $1. Probar con "05/01/2015 12:00:00 AM"
DB$FECnciasHA <- gsub("(\\d{2}/\\d{2}/\\d{4}).*","\\1",DB$FECHA)
# Convierte el col_character en col_date
DB$FECHA <- parse_date(DB$FECHA, format = "%d/%m/%Y")
View(DB)
attach(DB)

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 2

#Tabla de frecuencia Sexo

genero <- as.data.frame(table(SEXO))
fA <- cumsum(genero$Freq)
fR <- prop.table(genero$Freq) 
fRA <-cumsum(fR) 
frecuenciasGenero<- cbind(genero,fA,fR,fRA)
frecuenciasGenero

#Diagrama de barras

ggplot(DB,aes(x=SEXO, fill=SEXO))+geom_bar()

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 3

#Tabla de frecuencia nivel EScolaridad

escolaridad <- as.data.frame(table(ESCOLARIDAD))
fA_E <- cumsum(escolaridad$Freq)
fR_E <- prop.table(escolaridad$Freq) 
fRA_E <-cumsum(fR_E) 
frecuenciasEscolaridad<- cbind(escolaridad,fA_E,fR_E,fRA_E)
frecuenciasEscolaridad

#Diagrama de barras

ggplot(DB,aes(x=ESCOLARIDAD, fill=ESCOLARIDAD))+geom_bar()

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 4

#Tabla de contingencia

table(SEXO,ESCOLARIDAD)

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 5

datos <- summary(select(DB,EDAD))
moda <-modes(EDAD)
rango <- range(EDAD, na.rm = T)
varianzaMuestral <- var(EDAD,na.rm = T)
datos
moda
rango
varianzaMuestral
desviacionEstadar<- sd(EDAD, na.rm = T)
desviacionEstadar
coeficienteDeVariacion<- desviacionEstadar/mean(EDAD,na.rm = T)
coeficienteDeVariacion

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 6

#Tabla de frecuenias para EDAD

edad <- as.data.frame(table(EDAD))
fA_edad <- cumsum(edad$Freq)
fR_edad <- prop.table(edad$Freq) 
fRA_edad <-cumsum(fR_edad)
frecuenciasEdad<- cbind(edad,fA_edad,fR_edad,fRA_edad)
frecuenciasEdad

#Histograma

