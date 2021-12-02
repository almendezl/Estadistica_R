#####################TIPOS DE EXPERIMENTOS CON VARIABLE ALEATORIA#####################
#####################FECHA 20 DE MARZO ######################################### 
#####BINOMIAL
#modelar lanzamiento de 4 monedas con probabilidad de 0.5 (cara y sello)
dbinom(2,4,0.5) #calcular P(Y=y) = p(y)
pbinom(3,4,05) #Calcular P(Y <= y)
qbinom(0.987,4,0.5) # calcular el valor mas pequeño de y entre una probabilidad dada
rbinom(1,4,0.5) #genera numeros aleatorios de distribucion

#####GEOMETRICO
#simular que apuesto al color rojo en la ruleta 
dgeom(0, 18/37) 
pgeom(1, 18/37)
qgeom(0.5, 18/37)
rgeom(2, 18/37)
