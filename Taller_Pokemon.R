library (readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

#cargar archivo
df <- read_csv(file = "C:/Users/Angie Mendez/Desktop/Pokemon.csv")
colnames(df)[3] <- "Tipo1"
colnames(df)[4] <- "Tipo2"
#View(df)
attach(df)
#__________________________________________________________________________#
  #1. cual es el tipo (principal) de pokemon mas frecuente?#
#Tabla de frecuencias segun el tipo de pokemon
tipo <- as.data.frame(table(Tipo1))
f_acumulada <- cumsum(tipo$Freq)
f_relativa <- prop.table(tipo$Freq)
f_relativa_acum <- cumsum(f_relativa)
tabla_PokTipo <- cbind(tipo, f_acumulada, f_relativa, f_relativa_acum)
View(tabla_PokTipo)
#saber el maximo segun el tipo
maxtipos <- max(df$Tipo1)
maxtipos

#grafica de los tipos de pokemon
ggplot(df,aes(x=df$Tipo1,fill=df$Tipo1))+geom_bar()

#__________________________________________________________________________#

# 2.cual generacion de pokemon que mas se repite?
generacion <- as.data.frame(table(Generation))
f_acumuladaG <- cumsum(generacion$Freq)
f_relativaG <- prop.table(generacion$Freq)
f_relativa_acumG <- cumsum(f_relativaG)
tabla_PokGen <- cbind(generacion, f_acumuladaG, f_relativaG, f_relativa_acumG)
View( tabla_PokGen)

maxGen<- max(Generation)
maxGen
ggplot(df,aes(x=Generation,fill=Generation))+geom_bar(fill="#7FB3D5", color="cyan")

#__________________________________________________________________________#

# 3. ¿Cual es el pokemon legendario mas poderoso?

Legendarios <- subset(df, Legendary=="TRUE")
view(Legendarios)
#para ver los mas poderosos
poder<-subset(Legendarios,Legendarios$Total== max(Legendarios$Total))
View(poder)

#__________________________________________________________________________#
#4.	¿Cuáles son los Pokémon legendario más débiles y sus tipos?
podermin<-subset(Legendarios,Legendarios$Total== min(Legendarios$Total))
View(podermin)

LegDeb <- as.data.frame(table(podermin$Tipo1))
f_acumuladaL <- cumsum(LegDeb$Freq)
f_relativaL <- prop.table(LegDeb$Freq)
f_relativa_acumL <- cumsum(f_relativaL)
tabla_LegD <- cbind(LegDeb, f_acumuladaL, f_relativaL, f_relativa_acumL)
View( tabla_LegD)


ggplot(podermin,aes(x=podermin$Tipo1, fill=podermin$Tipo1))+geom_bar(fill="#FAD7A0",color="#873600")

#__________________________________________________________________________#
#5.	¿Cuál de los Pokémon normal y de los legendarios posee mayor defensa?
#filtrar los pokemon normales
normales <- subset(df, Legendary=="FALSE")
view(normales)
#Pokemon normal con mas defensa (valor)
max(normales$Defense)
subset(normales, normales$Defense== max(normales$Defense))
#pokemon Legendario con mas defensa (valor)
max(Legendarios$Defense )
subset(Legendarios, Legendarios$Defense== max(Legendarios$Defense))
#__________________________________________________________________________#
#6.	¿Cuál es el porcentaje de sacar un Pokémon por cada generación?
#porcentaje de sacar un pokemon de cada generacion
total <- sum(gen)
pGen1 <- gen[c(1)]*100/total
pGen1
pGen2 <- gen[c(2)]*100/total
pGen2
pGen3 <- gen[c(3)]*100/total
pGen3
pGen4 <- gen[c(4)]*100/total
pGen4
pGen5 <- gen[c(5)]*100/total
pGen5
pGen6 <- gen[c(6)]*100/total
pGen6

#__________________________________________________________________________#
# 7. ¿el pokemon favorito de la comunidad, realmente esta al nivel del mas fuerte entre los normales?
ataqNorMax <- max(normales$Attack)
ataqNorMax

pokMasFuerte <- subset(normales, Attack==ataqNorMax)
pokMasFuerte

mean(normales$Total) #media de los pokemones normales
pokFavorito <- subset(normales, Name=="Charizard")
comparar <- c(pokFavorito)
comparar <- rbind(comparar, pokMasFuerte)
View(comparar)  
