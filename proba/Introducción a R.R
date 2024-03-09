
#######################    Introducción a R    ##########################

# 1. Operaciones básicas

# Podemos realizar operaciones básicas como si fuera una calculadora 

2+2
6-9
9*8
3/2
7**6
7 ^ 6
(7 - 5) ** 3.1415926535

## Shortcut: comentar o descomentar varias líneas Ctrl + Shift + C

# 2. Operadores lógicos

# TRUE y FALSE son las unidades lógicas básicas, 
# equivalen a 1 y 0, respectivamente
TRUE
FALSE 

# La igualdad como expresión lógica se representa con ==
1 == TRUE
# La negación en R se expresa con un !
0 != FALSE

# & es el operador lógico conjunción "y"
# | es el operador lógico disyunción "o"
1 & 0
0 | 1

1 & 1 && 0 & 1 & 1 & 1
1 | 1 || 0 | 0 | 0 

# Como TRUE y FALSE representan 1 y 0, respectivamente, podemos sumarlos
# TRUE+TRUE = ?
TRUE+TRUE
TRUE+FALSE+1.5
TRUE + TRUE + FALSE + TRUE
# Esto nos puede ayudar a crear condiciones lógicas

# 3. Asignación de valores

## Shortcut: para poner <- es ALT + -

# <- (Alt + -) se usa para asignar valores, el lado izquierdo de <- denota la variable 
# que será asignada y el lado derecho el valor que se le asignará

x <- 7
# = funciona igual que <- 
y = 7
# = e == no son lo mismo!
x == y

# Convencionalmente se utiliza <- 

# 4. Formas correctas de nombrar una variable

# Se inicia con letras (mayúsculas o minúsculas)
# Puedes utilizar el _
# Preferentemente evitar nombres con .
# R distingue mayúsculas de minúsculas

hola.1 = 0
HOLA.1 = 2
M <- 5
m <- FALSE + 5.8
adios_2 = 23
el_nombre_mas_largo_que_se_me_puede_ocurrir = NULL
missing_dato <- c(1,2,3,NA, 5,6)
a <- TRUE
nombre <- "probabilidad"
semestre <- "2022"

# Evitar! (no causan un error... inmediato, pero puede generar muchos problemas)
# Evitar acentos y caracteres como ñ
sí = 1
ño = 0
aÃ±o = 30

# PROHIBIDO (no podemos asignar nombres que empiecen con un número
# o que contengan caracteres especiales o espacios)
1.hola = 0
1hola = 0
pesitos$ = 5
circunflejo^5 = 0
ay! = 2
arroba@gmail = 5
un mal nombre <- FALSE

# Mandar llamar mis objetos con Ctrl + Enter
a
nombre
m
z <- 2+2
z

# O bien poniendo el objeto en ()
(y <- 5.6^2)

# Podemos mandar llamar varios objetos con ;
a;z
x <- 4*5; y <- 3/8
x;y

# 5. El objeto báscio de R: El vector

# Características:
# - Un vector tiene el mismo tipo que los datos que contiene, es decir,
#   un vector sólo puede contener datos de un sólo tipo.
# - Podemos obtener la longitud: es el número de elementos que contiene un vector. 
#   La longitud es la única dimensión que tiene esta estructura de datos.

# Forma básica de crear un vector, mediante c()

ejemplo <- c(1,2,3,4,5,6,7,8)
vector_1 <- c(1,2,3)
# Otra forma es con el operador :
vector_2 <- 1:3
vector_1 == vector_2 # operador lógico
vector_3 <- 1:7
letras <- c("a","b","c")
b <- c(TRUE,FALSE,FALSE)
c <- c(1.1,1.8,3.56)

# ¿Podemos crear vectores que tengan distintos tipos de variables dentro?
vector_4 <- c(1, "2", 3, "4")

# ¿Qué hizo R?
vector_4

# Cómo modificamos el tipo de variable que contiene el vector?
as.character(vector_4)
as.numeric(vector_4)
as.integer(vector_4)
nuevo <- as.integer(vector_4)
vector_4 <- as.integer(vector_4)

# Tipo de datos del vector: class()
class(vector_4)

# Otra forma de crear un vector, mediante una secuencia
secuencia <- seq(from = 0, to = 10, by = 0.1)
secuencia2 <- seq(3,7,1)

# Otra más, mediante una repetición
repeticion_0 <- rep(3, times = 7) # Repite el número 3, el número "times" de veces

# No confundir con una replicación!
rep_1 <- replicate(2, 3) # Repite 2 veces el número 3
repeticion_1 <- rep(2, 3)

rep_1 == repeticion_1

repeticion_2 <- rep(5, 4)
repeticion_3 <- rep(1, 10)

# Podemos concatenar vectores y crear uno nuevo
repeticiones <- c(repeticion_0, repeticion_1, repeticion_2, repeticion_3)

# Otros ejemplos de vectores
dado <- 1:6
urna <- c('Tauro', "Géminis", 'Virgo', 'Libra', "Cáncer")
nombres <- c("A", "B")
nombres_edades <- c("A", 23, "B", 27)

# Tamaño o longitud de un vector: length()
length(dado)
length(repeticiones)

# Cómo funcionan los operadores lógicos y los vectores
nombres == nombres_edades

c(1,0,0,1) & c(1,1,0,1) # Opera entrada a entrada 
c(1,0,0,1) && c(1,1,0,1) # Opera en conjunto
c(1,0,0,1) | c(1,1,0,1) # Opera entrada a entrada 
c(1,0,0,1) || c(1,1,0,1)  # Opera en conjunto


# Podemos acceder a las entradas del vector con el operador "[]"
nombres_edades
length(nombres_edades)

nombres_edades[3]
nombres_edades[1] # En R, el índice empieza en 1
nombres_edades[0] # En R, el índice empieza en 1
nombres_edades[5] # NA = Not Available (Missing Values)

# 6. Operaciones con vectores

a <- c(2,3,4,5,6,7)
b <- 7:15
c <- c(1,2,1,2,1,2)

# Operaciones aritméticas

(a+2) 
(a*5)
(b%%3) #Módulo
(a^2)
(a**4) 
# Nota: no es necesario hacer un for en las operaciones con vectores

# Entre vectores deben tener la misma longitud
length(a)
length(b)
length(c)
a-b # Error!
a/c

# Operaciones relacionales

a > 7
b >=5
a >5 | a <=4

# Acceder al vector con operaciones: vector[]

a[a > 7]
b[b >=5]
a[a >5 | a <=4]

# 7. Matrices

# Las matrices son una estructura con forma rectangular, con renglones 
# y columnas.

# Se crean matrices con la función: matrix()

# Puede aceptar 2 argumentos:
# nrow = número de renglones
# ncol = número de columnas

matrix(1:12) # Matriz sin especificar el número de renglones ni de columnas

# R arma las matrices por columnas (hacia abajo)

matrix(1:20, nrow=5)

matrix(1:20, nrow=5, ncol=4) # Especificar los 2 argumentos está demás

matrix(1:20, ncol=4)

matrix(1:30, ncol=10)

# Si quiero que la matriz se llene por renglones debo especificar con
# el argumento : byrow = TRUE

matrix(1:30,3, byrow = TRUE)

# También puedo crear una matriz uniendo vectores
# cbind() para unir vectores, usando cada uno como una columna.
# rbind() para unir vectores, usando cada uno como un renglón.

# Deben tener la misma longitud

vector_1 <- 1:4
vector_2 <- 5:8
vector_3 <- 9:12
vector_4 <- 13:16

matriz <- rbind(vector_1, vector_2, vector_3, vector_4) # por renglones
matriz2 <- cbind(vector_1, vector_2, vector_3, vector_4) # por columnas

# Crear una matriz vacía

aux <- matrix(nrow = 5,ncol=6)

# Puedo obtener el tamaño de una matriz: dim()
# Devuelve el número de renglones, número de columnas

dim(matriz)
dim(aux)
dim(aux)[1] # Primer elemento: renglones
columnas <- dim(aux)[1] # Segundo elemento: columnas

# 8. Operaciones con matrices

matriz + 5 # Suma de escalares
matriz * 2 # Multiplicación por un escalar
matriz2/3 
t(matriz2) # Transpuesta

# Entre matrices deben tener la misma dimensión

A <- matrix(1:9,ncol=3)
B <- matrix(rep(2,9),nrow=3)
C <- matrix(cbind(c(2,3,4),c(1,1,1),c(12,15,3)),ncol=3)

A+B
B %*% C # Multiplicación de matrices
solve(C) # Inversa de una matriz
det(C) # Determinante de una matriz
diag(A+B+C) # Diagonal

3*A %*% B %*% t(C)
A %*% A # A^2

# Seleccionar valores: matriz[renglones, columnas]
A[1,1] # renglón 1, columna 1
B[2,] # renglón 2, todas las columnas
C[,3] # todos los renglones, columna 4

# Asignar un nuevo valor
B
B[2,3] <- 4
B

ejemplo <- matrix(c(1/2, 1/4,2/3,6/7),ncol=2)
inv <- solve(ejemplo)
ejemplo %*% inv

# 9. Loops y condicionales

### Plantilla básica de un if:

# if(condicion lógica){
#   cuerpo del TRUE
# }

if(1234**2<exp(35)){
  print("Hola")
}


### Plantilla básica de un if-else:

# if(condicion lógica){
#   cuerpo del TRUE
# }else{
#   cuerpo de la negación de la condición lógica
# }

n <- 7
if(n%%2==0){
  print("n es par")
}else{
  print("n es impar")
}


### Plantilla básica de un if-else if-...-else if-else
# if(condicion lógica){
#   cuerpo del TRUE
# }else if(condicion lógica que no contenga la primer condición lógica){
#   cuerpo de la segunda condición lógica
# }else if(condicion lógica que no contenga a las 2 anteriores){
#   cuerpo de la tercera condición lógica
# }....
# }else{
#   En caso de que no se cumpla ninguna condición lógica de las anteriores
# }

n <- 98 
if(n%%7==0){
  print("n es multiplo de 7")
} else if(n%%5==0){
  print("n es multiplo de 5")
} else if(n%%2==0){
  print("n es multiplo de 2")
} else{
  print("n no es multiplo de 7, 5 o 2")
}



### Plantilla básica de un for
# for(contador in vector de contadores){
#   Qué hacer para cada uno de los contadores (en orden)
# }

auxiliar <- 1:20
for(i in 1:length(auxiliar)){
  cat("La tercera potencia de ", i, "es ",i^3,"\n") # "\n" se usa para dar enter
}

# Alternativa de lo anterior en R con operaciones con vectores
paste("La tercera potencia de ", auxiliar, "es", auxiliar^3)


### Plantilla básica de un while
# contador_inicial
# while(condición lógica acerca del contador){
#   Qué hacer para el paso contador en turno
#   contador <- contador + 1
#   # contador = contador + 1
# } # 0j0, si la condición lógica no llega a su fin el proceso quedará colgado  
# # Y tendremos que detener el proceso manualmente

contador_inicial = 1
while (contador_inicial > 0) {
  print("hola")
  contador_inicial <-  contador_inicial + 1
}

contador_inicial = 1
while (contador_inicial > 0 & contador_inicial<15) {
  print("hola")
  contador_inicial <-  contador_inicial + 1
}


# 10. Funciones propias de R

# Recordemos el vector
a

sum(a) # Suma todos los datos 
cumsum(a) # Suma acumulada
cos(a) 
cosh(a)
exp(1)
pi
exp(a*pi)
mean(a)

# Hay funciones con más de un parámetro (no todos se tienen que ocupar)
dado <- 1:6
sample(dado, size = 3, replace = TRUE)
sample(dado, 3, TRUE)
moneda <- c("águila","sol")
sample(moneda, size = 2, replace = FALSE)
sample(moneda, size = 6, replace = TRUE)
sample()

# LAs funciones se pueden componer!
round(mean(dado))
round(mean(dado), digits = 3)

# Otras funciones básicas muy importantes
# Fijar semillas: set.seed()
set.seed(27)

# Qué pasa si no ponemos ()?
set.seed

# 11. Pedir ayuda

# Ayuda, ayuda!!!!!
?sample
help("mean")
# Usamos doble para buscar coincidencias
??samp

# 12. Obtener la ruta de la carpeta

# ¿Dónde estoy?
getwd()
ruta <- "/home/evelyn/PASE"
setwd(ruta)
getwd()

# 13. Creando funciones 

## Plantilla básica de una función
# nombre_de_mi_funcion <- function(parametro1, parametro2, parametro3 = "algo_fijo"){ # Leve descripción de los parámetros o la dinamica de mi función
#   cuerpo de mi función
#   return(valor a regresar) # Situacional
# }

# Nuestra primera función, la media de un vector
media_de_un_vector = function(vector){ # vector numérico al cual se le extraera la media
  media <- sum(vector)/length(vector)
  media
}

#usamos la función
v <- c(2,3,4,5,5,6,6,7,6,7)
media_de_un_vector(v)

#Comparamos con la de R
mean(v)

# Sirve return()?
media__y_varianza_de_un_vector = function(vector){ # vector numérico al cual se le extraera la media
  media <- sum(vector)/length(vector)
  media
  varianza <- sum((vector - media)**2)/(length(vector)-1)
  varianza
}

#usamos la función
media__y_varianza_de_un_vector(v)

#En R la varianza es var()
var(v)

# Para que regrese lo que yo quiero
media__y_varianza_de_un_vector = function(vector){ # vector numérico al cual se le extraera la media
  media <- sum(vector)/length(vector)
  varianza <- sum((vector - media)**2)/(length(vector)-1)
  return(c(Media = media, Varianza = varianza))
}

# Usamos la función
media__y_varianza_de_un_vector(v)


# La función puede devolver una lista 
media__y_varianza_de_un_vector = function(vector){ # vector numérico al cual se le extraera la media
  media <- sum(vector)/length(vector)
  varianza <- sum((vector - media)**2)/(length(vector)-1)
  return(list(Media = media, Varianza = varianza))
}

# Usamos la función
media__y_varianza_de_un_vector(v)



# Ejemplo de funciones 2
girar_dado <- function(){
  sample(1:6, 1)
}

# Usar la función
girar_dado()

# ¿Cuál es más adecuada a nuestros propósitos?
girar_dado_n_veces <- function(n_veces = 1){
  sample(1:6, n_veces)
}

girar_dado_n_veces <- function(n_veces = 1){
  sample(1:6, n_veces, replace = TRUE)
}

# Usamos la función
girar_dado_n_veces(10)

# Ejemplo 3

absoluto <- function(x){
  if(x>0){
    return(x)
  }else if(x==0){
    print("Es cero")
  } else{
    return(-x)
  }
}

absoluto(7)







##########
# línea de código para Rmarkdown
# tinytex::install_tinytex()
