library (readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

#cargar archivo
df <- read_csv(file = "C:/Users/Angie Mendez/Desktop/Pokemon.csv")
colnames(df)[3] <- "Tipo1"
colnames(df)[4] <- "Tipo2"
View(df)
attach(df)
-----------------------------------------------------------------------
  
#1. cual es el tipo (principal) de pokemon mas frecuente?
#tabla de los tipos de pokemon 

  r <- max(Tipo1)  
  
m<-  max(`Type 1`)
maxtipos <- max(df$`Type 1`)
maxtipos
#grafica de los tipos de pokemon
ggplot(df,aes(x=df$`Type 1`,fill=df$`Type 1`))+geom_bar()

-----------------------------------------------------------------------

# 2.cual generacion de pokemon que mas se repite?
gen <- table(Generation)
view(gen)
maxGen<- max(Generation)
maxGen
ggplot(df,aes(x=Generation,fill=Generation))+geom_bar()

-----------------------------------------------------------------------
# ¿Cual es el pokemon legendario mas fuerte?

Legendarios <- subset(df, Legendary=="TRUE")
view(Legendarios)
#para ver los mas poderosos
poder<-subset(Legendarios,Legendarios$Total== max(Legendarios$Total))
View(poder)
-----------------------------------------------------------------------
#el mas debil
podermin<-subset(Legendarios,Legendarios$Total== min(Legendarios$Total))
View(podermin)
ggplot(podermin,aes(x=podermin$Name, y=podermin$`Type 1`))+geom_bar(stat = "identity")
-----------------------------------------------------------------------
#filtrar los normales
normales <- subset(df, Legendary=="FALSE")
view(normales)
#pokemon normal con mas defensa (valor)
max(normales$Defense)
subset(normales, normales$Defense== max(normales$Defense))
#pokemon Legendario con mas defensa (valor)
max(Legendarios$Defense )
subset(Legendarios, Legendarios$Defense== max(Legendarios$Defense))

-----------------------------------------------------------------------
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

-----------------------------------------------------------------------
p2<- subset(Legendarios,Legendarios$`Type 1`=="Fire")
View(p2)
tipo<- filter()
ggplot(Legendarios,aes(x=Legendarios$Name, y=Legendarios$Attack == (Legendarios$`Type 1`=="Fire")))+geom_bar(stat = "identity")




filter(Legendarios, Attack==p)
ataqLegMax <- filter(p$Name,p$Attack )
ataqLegMax
sa<-table(df$Attack, ) 
hist(table(df $Attack), col= "green") #

subset(Legendarios, Attack==ataqLegMax)


barplot(prop.table(df$Name, df$Attack))

# ¿Cual es el pokemon legendario mas debil?

Legendarios <- subset(df, Legendary=="TRUE")
view(Legendarios)
ataqLegMin <- min(Legendarios$Attack)
ataqLegMin
subset(Legendarios, Attack==ataqLegMin)

# ¿Cual es el pokemon normal mas fuerte?

normales <- subset(df, Legendary=="FALSE")
View(normales)
ataqNorMax <- max(normales$Attack)
ataqNorMax
subset(normales, Attack==ataqNorMax)

# ¿Cual es el pokemon normal mas debil?

normales <- subset(df, Legendary=="FALSE")
View(normales)
ataqNorMin <- min(normales$Attack)
ataqNorMin
subset(normales, Attack==ataqNorMin)


