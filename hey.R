library(ggplot2)
library(dplyr)
library(readr)
library(modes)
baseDatos <- read_csv("../Downloads/Delito_Hurto_Personas.csv",
                      col_types = list(
                        "FECHA"=col_character(),
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
                        "2015"=col_skip()
                        )
                      )
baseDatos$FECHA <- gsub("(\\d{2}/\\d{2}/\\d{4}).*","\\1",baseDatos$FECHA)
baseDatos$FECHA <- parse_date(baseDatos$FECHA, format = "%d/%m/%Y")
attach(baseDatos)
## Tabla de frecuencias por genero.
tablaGENERO <- table(SEXO)
tablaGENERO
## Diagrama de barras por frecuencias GENERO
ggplot(baseDatos, aes(x=SEXO, fill=SEXO))+geom_bar()+scale_fill_manual(values=c("orange","black","blue","pink"))
## Tabla de frecuencias por nivel de escolaridad
tablaESCOLARIDAD <- table(ESCOLARIDAD)
tablaESCOLARIDAD
## Diagrama de barras por frecuencias ESCOLARIDAD
ggplot(baseDatos, aes(x=ESCOLARIDAD, fill=ESCOLARIDAD))+geom_bar()+scale_fill_manual(values=c("orange","black","blue","pink","purple","green","red"))
## Tabla contingencia Genero vs Escolaridad
tablaCONT <- table(SEXO, ESCOLARIDAD)
tablaCONT
## Diagrama de barras tabla contingencia
ggplot(baseDatos, aes(x=ESCOLARIDAD, fill=SEXO,y=SEXO))+geom_bar(stat="Identity")+scale_fill_manual(values=c("red","blue","black","orange"))
## Medidas de tendencia central
media <- mean(EDAD, na.rm = TRUE)
media
mediana <- median(EDAD, na.rm = TRUE)
mediana
modaEdad <- modes(EDAD)
modaEdad
## Medidas de variabilidad
RangoEdad <- max(EDAD, na.rm = TRUE)-min(EDAD, na.rm = TRUE)
RangoEdad
varianzaMues <- var(EDAD, na.rm = TRUE)
varianzaMues
desviacionMues <- sqrt(varianzaMues)
desviacionMues
coefiVar <- varianzaMues/mean(EDAD, na.rm = TRUE)
coefiVar
## Medidas de posicion
quantile(EDAD, probs = c(0.25,0.5,0.75), na.rm = TRUE)
## Tabla por intervalos de edad
intervalosEdad <- table(cut(EDAD,breaks=seq(0,96,length=nclass.Sturges(EDAD)),include.lowest=TRUE))
intervalosEdad
## Histograma por edad
ggplot(baseDatos, aes(x=EDAD, fill=EDAD))+geom_histogram(bins =nclass.Sturges(EDAD))
## Diagrama cajas y vigotes
boxplot(EDAD)
## Medidas de tendencia central y variabilidad por genero
tablaHom <- baseDatos %>%
  select(SEXO,EDAD)%>%
  filter(SEXO == "MASCULINO")
tablaMuj  <- baseDatos %>%
  select(SEXO,EDAD)%>%
  filter(SEXO == "FEMENINO")
## Tendencia Central Masculino
mediaHom <- mean(tablaHom$EDAD, na.rm = TRUE)
mediaHom
medianaHom <- median(tablaHom$EDAD, na.rm = TRUE)
medianaHom
modaHom <- modes(tablaHom$EDAD)
modaHom
## Tendencia Central Femenino
mediaMuj <- mean(tablaMuj$EDAD, na.rm = TRUE)
mediaMuj
medianaMuj <- median(tablaMuj$EDAD, na.rm = TRUE)
medianaMuj
modaMuj <- modes(tablaMuj$EDAD)
modaMuj
## Medidas de variabilidad Masculino
RangoHom <- max(tablaHom$EDAD, na.rm = TRUE)-min(tablaHom$EDAD, na.rm = TRUE)
RangoHom
varianzaHom <- var(tablaHom$EDAD, na.rm = TRUE)
varianzaHom
desviacionHom <- sqrt(varianzaHom)
desviacionHom
coefiVarHom <- varianzaHom/mean(tablaHom$EDAD, na.rm = TRUE)
coefiVarHom
## Medidas de variabilidad Femenino
RangoMuj <- max(tablaMuj$EDAD, na.rm = TRUE)-min(tablaMuj$EDAD, na.rm = TRUE)
RangoMuj
varianzaMuj <- var(tablaMuj$EDAD, na.rm = TRUE)
varianzaMuj
desviacionMuj <- sqrt(varianzaMuj)
desviacionMuj
coefiVarMuj <- varianzaHom/mean(tablaMuj$EDAD, na.rm = TRUE)
coefiVarMuj

