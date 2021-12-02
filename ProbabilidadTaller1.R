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


m<-ceiling( 1+3.3*log(length(EDAD)))
m
edad <- as.data.frame(table(tablax=factor(cut(EDAD,breaks = m))))
fA_edad <- cumsum(edad$Freq)
fR_edad <- prop.table(edad$Freq) 
fRA_edad <-cumsum(fR_edad)
frecuenciasEdad<- cbind(edad,fA_edad,fR_edad,fRA_edad)
frecuenciasEdad

#Histograma

ggplot(DB,aes(x=EDAD, fill=EDAD))+geom_histogram()

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 7

#Diagrama de bigotes

boxplot(EDAD)

# Proceso para eliminar datos atipicos

x<- quantile(EDAD,probs = 0.75,na.rm = T)
x
espacioInterQuartil<- x + 1.5 * IQR(EDAD, na.rm = T)
espacioInterQuartil

tablaNueva<-DB %>% 
  filter(EDAD<=espacioInterQuartil & !is.na(EDAD))

#histograma sin datos atipicos

histogramaTabla<- ggplot(tablaNueva,aes(x=tablaNueva$EDAD, fill=tablaNueva$EDAD))+geom_histogram()
histogramaTabla

#Nuevo diagrama de cajas y bigotes

boxplot(tablaNueva$EDAD)

#porcentaje de datos atipicos

porcentaje_datAtip <- 100-(nrow(tablaNueva)*100/nrow(DB))
porcentaje_datAtip

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#punto 8

#Separacion De Grupos

hombres <- filter(DB, SEXO == "MASCULINO")
View(hombres)
mujeres <- filter(DB, SEXO=="FEMENINO")
View(mujeres)  

#VAlores hombres

datos1<-summary(select(hombres,EDAD))
moda1 <-modes(hombres$EDAD)
rango1 <- range(hombres$EDAD, na.rm = T)
varianzaMuestral1 <- var(hombres$EDAD,na.rm = T)
datos1
moda1
rango1
varianzaMuestral1
desviacionEstadar1<- sd(hombres$EDAD, na.rm = T)
desviacionEstadar1
coeficienteDeVariacion1<- desviacionEstadar1/mean(hombres$EDAD,na.rm = T)
coeficienteDeVariacion1

#Valores mujeres

datos2<-summary(select(mujeres,EDAD))
moda2 <-modes(mujeres$EDAD)
rango2 <- range(mujeres$EDAD, na.rm = T)
varianzaMuestral2 <- var(mujeres$EDAD,na.rm = T)
datos2
moda2
rango2
varianzaMuestral2
desviacionEstadar2<- sd(mujeres$EDAD, na.rm = T)
desviacionEstadar2
coeficienteDeVariacion2<- desviacionEstadar2/mean(mujeres$EDAD,na.rm = T)
coeficienteDeVariacion2
