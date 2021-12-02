library(dplyr)
library(tidyverse)
#########################VALOR ESPERADO FECHA 13 DE MARZO ############################
dado <- 1:6
sample(dado, size=1) # muestra aleatoria y permutaciones
lanzamientos <- c()
turnos <- 1:100
for (numTurno in turnos) { #simula el lanzamiento del dado 100 veces y acumula los resultados
  lanzamientos <- c(lanzamientos, sample(dado, size=1))
}
mean(lanzamientos)#hallar el promedio de los lanzamientos 
