library(readr)
library(dplyr)
library(ggplot2)
#install.packages("modes")
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
cuartiles_edad <- quantile(EDAD, probs = c(0.25,0.5, 0.75, 1), na.rm = T) #cuartiles
media_edad <- mean(EDAD, na.rm = T)#media
mediana_edad <- median(EDAD, na.rm = T)#mediana
moda_edad <- modes(EDAD) #la moda
rango_edad <- max(EDAD, na.rm = T)-min(EDAD, na.rm = T)#rango
varianza <- var(EDAD, na.rm = T)#varianza
desv_estandar <- sd(EDAD, na.rm = T) #desviacion estandar
coe_variacion <- desv_estandar/mean(EDAD, na.rm = T)#coeficiente de variacion 

media_edad
mediana_edad
moda_edad
cuartiles_edad
rango_edad
varianza
desv_estandar
coe_variacion
#-------------------------------------------------------------------------
#Punto 6: Tabla de frecuencias de la EDAD
clases_edad <- cut(EDAD, breaks = seq(0,96,length=nclass.Sturges(EDAD)),lowest=T)
edad <- as.data.frame(table(clases_edad)) 
f_acumulada <- cumsum(edad$Freq)
f_relativa <- prop.table(edad$Freq)
f_relativa_acum <- cumsum(f_relativa)
tabla_edad <- cbind(edad, f_acumulada, f_relativa, f_relativa_acum)
tabla_edad

#Histograma variable EDAD
ggplot(hurtos,aes(x=EDAD))+geom_histogram()
#-------------------------------------------------------------------------
#Punto 7: Diagrama de cajas y bigotes
tercer_cuartil <- quantile(EDAD,probs = 0.75,na.rm = T)
atipicos <- tercer_cuartil+1.5*IQR(EDAD, na.rm = T)

sin_atipicos<- hurtos %>% 
  filter(EDAD <= atipicos & !is.na(EDAD))
histograma_conAtipicos <- ggplot(hurtos,aes(x=EDAD, fill=EDAD))+geom_histogram()
histograma_sinAtipicos <- ggplot(sin_atipicos,aes(x=sin_atipicos$EDAD, fill=sin_atipicos$EDAD))+geom_histogram()
porcentaje_atipicos <- (length(EDAD)-length(sin_atipicos$EDAD))*100/length(sin_atipicos$EDAD)

boxplot(EDAD) #Diagrama original
boxplot(sin_atipicos$EDAD) #Diagrama de cajas y bigotes sin atipicos
histograma_conAtipicos #histograma original
histograma_sinAtipicos #histograma sin atipicos
porcentaje_atipicos #porcentaje de datos atipicos
#-------------------------------------------------------------------------
#Punto8: Separar hombres y mujeres 
hombres <- filter (hurtos, SEXO =="MASCULINO")
mujeres <- filter (hurtos, SEXO =="FEMENINO")
#Datos hombres
cuartiles_hombres <- quantile(hombres$EDAD, probs = c(0.25,0.5, 0.75, 1), na.rm = T) #cuartiles
media_hombres <- mean(hombres$EDAD, na.rm = T)#media
mediana_hombres <- median(hombres$EDAD, na.rm = T)#mediana
moda_hombres <- modes(hombres$EDAD) #la moda
rango_hombres <- max(hombres$EDAD, na.rm = T)-min(hombres$EDAD, na.rm = T)#rango
varianza_hombres <- var(hombres$EDAD, na.rm = T)#varianza
desv_estandar_hombres <- sd(hombres$EDAD, na.rm = T) #desviacion estandar
coe_variacion_hombres <- desv_estandar_hombres/media_hombres#coeficiente de variacion 

media_hombres
mediana_hombres
moda_hombres
cuartiles_hombres
rango_hombres
varianza_hombres
desv_estandar_hombres
coe_variacion_hombres

#Datos mujeres
cuartiles_mujeres <- quantile(mujeres$EDAD, probs = c(0.25,0.5, 0.75, 1), na.rm = T) #cuartiles
media_mujeres <- mean(mujeres$EDAD, na.rm = T)#media
mediana_mujeres <- median(mujeres$EDAD, na.rm = T)#mediana
moda_mujeres <- modes(mujeres$EDAD) #la moda
rango_mujeres <- max(mujeres$EDAD, na.rm = T)-min(mujeres$EDAD, na.rm = T)#rango
varianza_mujeres <- var(mujeres$EDAD, na.rm = T)#varianza
desv_estandar_mujeres <- sd(mujeres$EDAD, na.rm = T) #desviacion estandar
coe_variacion_mujeres <- desv_estandar_mujeres/media_mujeres#coeficiente de variacion 

media_mujeres
mediana_mujeres
moda_mujeres
cuartiles_mujeres
rango_mujeres
varianza_mujeres
desv_estandar_mujeres
coe_variacion_mujeres