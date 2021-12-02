#install.packages("tidyr")
library(tidyr)
#install.packages("dplyr")

install.packages("ggplot2")
library(dplyr)
library(ggplot2)

table1
table2
table3
table4a
table4b

#casos por cada 10 mil habitantes
table1 %>%
  mutate(rate = cases / population*10000) #mutate crea nueva columna llamada rate que son las proporciones

#casos totales por year. el wt cuenta los casos de la variable cases
table1 %>%
  count(year, wt=cases) #count=conteo ponderado por el numero de casos
#grafico de el anio vs los casos, por pais
ggplot(table1, aes(as.factor(year), cases))+geom_line(aes(group=country), colour="cyan")+
  geom_point(aes(colour =country))
#as.factor convierte los valores numericos a factores, ene este caso el anio

#gather para cambiar las variables en diferentes columnas
tidy4a <- table4a%>%
  gather('1999','2000', key="year", value="cases") 

tidy4b <- table4b%>%
  gather('1999','2000', key="year", value="population")

left_join(tidy4a,tidy4b) #join pega las dos tablas indicadas

#spread es inversa a gather 
table2 %>%
  spread(key=type, valu =count)

table5 %>%
  separate(rate, into = c("cases", "population"), sep="/")

table5 %>%
  separate(rate, into = c("cases", "population"), convert = T) #los convierte a numerico

table5 %>%
  unite(year, century, year, sep="")
