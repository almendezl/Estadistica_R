library(readr)
library(dplyr)
library(ggplot2)
install.packages("modes")
library(modes)

# Carga de datos de .csv a data frame
hurtos <- read_csv(file = "C:/Users/Angie Mendez/Documents/Universidad Piloto/5 semestre/Probabilidad y Estadistica/Corte I/A1/Delito_Hurto_Personas.csv", 
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
                 "2015"=col_skip()))
# Modifica la fecha. Le quita usando la expresión regular sustituyendo (\d{2}/\d{2}/\d{4}).* por $1. Probar con "05/01/2015 12:00:00 AM"
hurtos$FECHA <- gsub("(\\d{2}/\\d{2}/\\d{4}).*","\\1",hurtos$FECHA)
# Convierte el col_character en col_date
hurtos$FECHA <- parse_date(hurtos$FECHA, format = "%d/%m/%Y")
attach(hurtos)
#-------------------------------------------------------------------------
#Punto 2: Tabla de frecuencias y diagrama de barras sobre el GENERO
genero <- as.data.frame(table(SEXO))
f_acumulada <- cumsum(genero$Freq)
f_relativa <- prop.table(genero$Freq)
f_relativa_acum <- cumsum(f_relativa)
tabla_genero <- cbind(genero, f_acumulada, f_relativa, f_relativa_acum)
tabla_genero
#Diagrama de barras segun el GENERO de las victimas
ggplot(hurtos,aes(x=SEXO, fill=SEXO))+geom_bar()
#-------------------------------------------------------------------------
#Punto 3: Tabla de frecuencias del nivel de ESCOLARIDAD
escolaridad <- as.data.frame(table(ESCOLARIDAD))
f_acumulada <- cumsum(escolaridad$Freq)
f_relativa <- prop.table(escolaridad$Freq)
f_relativa_acum <- cumsum(f_relativa)
tabla_escolaridad <- cbind(escolaridad, f_acumulada, f_relativa, f_relativa_acum)
tabla_escolaridad
#Diagrama de barras segun el nivel de ESCOLARIDAD de las victimas
ggplot(hurtos,aes(x=ESCOLARIDAD, fill=ESCOLARIDAD))+geom_bar()
#-------------------------------------------------------------------------
#Punto 4: Tabla de contingencia SEXO vs ESCOLARIDAD
tabla_cont_SE <- table(SEXO, ESCOLARIDAD)
tabla_cont_SE
#-------------------------------------------------------------------------
#Punto 5: Medidas sobre la variable EDAD
datos_edad <- summary(select(hurtos,EDAD)) #Media, mediana, primer cuartil, segundo cuartil = mediana, tercer cuartil
datos_edad        
moda_edad <- modes(EDAD) #la moda
moda_edad
rango_edad <- max(EDAD, na.rm = T)-min(EDAD, na.rm = T)#rango
rango_edad
var(EDAD, na.rm = T)#varianza
desv_estandar <- sd(EDAD, na.rm = T) #desviacion estandar
desv_estandar
coe_variacion <- desv_estandar/mean(EDAD, na.rm = T)#coeficiente de variacion 
coe_variacion
#-------------------------------------------------------------------------
#Punto 6: Tabla de frecuencias de la EDAD
edad <- as.data.frame(table(EDAD))
f_acumulada <- cumsum(edad$Freq)
f_relativa <- prop.table(edad$Freq)
f_relativa_acum <- cumsum(f_relativa)
tabla_edad <- cbind(edad, f_acumulada, f_relativa, f_relativa_acum)
tabla_edad
#Histograma variable EDAD
ggplot(hurtos,aes(x=EDAD))+geom_histogram()
#-------------------------------------------------------------------------
#Diagrama de cajas y bigotes
boxplot(EDAD)
#-------------------------------------------------------------------------
#Separar hombres y mujeres 
hombres <- filter (hurtos, SEXO =="MASCULINO")
mujeres <- filter (hurtos, SEXO =="FEMENINO")
#Datos hombres
datos_hombres <- summary(hombres$EDAD) #Media, mediana, primer cuartil, segundo cuartil = mediana, tercer cuartil
datos_hombres        
moda_hombres <- modes(hombres$EDAD) #la moda
moda_hombres
tabla_edad_h <- table(hombres$EDAD)
rango_edad_h <- max(tabla_edad_h)-min(tabla_edad_h)#rango
rango_edad_h
var(hombres$EDAD, na.rm = T)#varianza
desv_estandar_h <- sd(hombres$EDAD, na.rm = T) #desviacion estandar
desv_estandar_h
coe_variacion_h <- desv_estandar_h/mean(hombres$EDAD, na.rm = T)#coeficiente de variacion 
coe_variacion_h
#Datos mujeres
datos_edad_m <- summary(mujeres$EDAD) #Media, mediana, primer cuartil, segundo cuartil = mediana, tercer cuartil
datos_edad_m        
moda_edad_m <- modes(mujeres$EDAD) #la moda
moda_edad_m
tabla_edad_m <- table(mujeres$EDAD)
rango_edad_m <- max(tabla_edad_m)-min(tabla_edad_m)#rango
rango_edad_m
var(mujeres$EDAD, na.rm = T)#varianza
desv_estandar_m <- sd(mujeres$EDAD, na.rm = T) #desviacion estandar
desv_estandar_m
coe_variacion_m <- desv_estandar_m/mean(mujeres$EDAD, na.rm = T)#coeficiente de variacion 
coe_variacion_m
