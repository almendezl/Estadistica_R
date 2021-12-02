
# Calculadora -------------------------------------------------------------

2 + 2
sqrt(9)
log(10)

# Ingresando expresiones --------------------------------------------------

x <- 1
print(x)
x
msg <- "hola"
# este es un comentario

# Evaluación de expresiones -----------------------------------------------

x <- 5 # nada se imprime
x # auto-printing
print(x) # print explícito

y <- 11:40
y

# Objetos -----------------------------------------------------------------

x <- 1
print(x)
x
msg <- "hola"

y <- 1L

class(x)
typeof(x)
str(x)

class(y)
typeof(y)
str(y)

1/0
log(0)
0/0

# Atributos ---------------------------------------------------------------

x <- 1
print(x)
x
attributes(x)

# Vectores ----------------------------------------------------------------

x <- c(0.5,0.6) # numérico
x <- c(TRUE, FALSE) # lógico
x <- c(T, F) # lógico
x <- c("a", "b", "c") # caracteres
x <- 9:29 # enteros
x <- c(1+0i,2+4i) # complejos

x <- vector(mode = "numeric", length = 10)

y <- c(1.7, "a") # caracteres
y <- c(TRUE, 2) # numericos
y <- c("a", TRUE) # caracter

# Conversión explícita ----------------------------------------------------

x <- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
as.complex(x)

# Matrices ----------------------------------------------------------------

m <- matrix(nrow = 2, ncol = 3)
m
dim(m)
attributes(m)

m <- matrix(1:6, nrow = 2, ncol = 3)
m

x <- 1:3
y <- 10:12
cbind(x,y)
rbind(x,y)

# Listas ------------------------------------------------------------------

x <- list(1, "a", TRUE, 1+4i)
x

x <- vector(mode = "list", length = 5)
x

# Factores ----------------------------------------------------------------

x <- factor(c("yes","yes","no","yes","no"))
x
table(x)

unclass(x)

x <- factor(c("yes","yes","no","yes","no"))
x

x <- factor(c("yes","yes","no","yes","no"), levels = c("yes","no"))
x

# Valores faltantes/Missings ----------------------------------------------

x <- c(1, 2, NA, 10, 3)
is.na(x)
is.nan(x)

x <- c(1, 2, NaN, NA, 4)
is.na(x)
is.nan(x)

# Data Frames -------------------------------------------------------------

x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
nrow(x)
ncol(x)

data <- mtcars
class(data)

# Names -------------------------------------------------------------------

x <- 1:3
names(x)
x
names(x) <- c("New York", "Seattle", "Los Angeles")
x

x <- list("LOs Angeles" = 1, Boston = 2, London = 3)
names(x)
x

m <- matrix(1:4, nrow = 2, ncol = 2)
m
dimnames(m) <- list(c("a","b"),c("c","d"))
m

colnames(m) <- c("h","f")
rownames(m) <- c("x","z")
m

