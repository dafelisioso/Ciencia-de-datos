
#' Genenerador lineal congruencial
#'
#' @param n es la cantidad de números a generar.
#' @param semilla es nuestro x0 donde empezara nuestro algoritmo.
#' @param m es el modulo.
#' @param a es la constante multiplicativa.
#' @param c es la constante aditiva.
#'
#' @return un vector de números pseudoaleatorios.
generador_congruencial <- function(n, semilla, m, a, c){
  x = c(semilla)
  for(i in 2:(n+1)){
    x[i] = (a*x[i-1]+c) %% m
  }
  return(x[2:(n+1)])
}

# Para n=3, semilla=15, mod=100, a=50, c=16
generador_congruencial(3, 15, 100, 50, 16)
generador_congruencial(10, 15, 100, 50, 16)
# Generando más números
generador_congruencial(28, 23, 100, 32, 53)
generador_congruencial(28, 67, 100, 45, 23)

# Cuando c = 0
generador_congruencial(50, 23, 100, 77, 0)
generador_congruencial(100, 45, 100, 67, 0)

# Pruebas para un m muy grande
generador_congruencial(101, 45, 1000, 67, 0)
generador_congruencial(100, 45, 10000, 67, 0)


#' Genenerador lineal congruencial uniforme distribuido entre [0,1]
#'
#' @param n es la cantidad de números a generar.
#' @param m es el modulo.
#' @param a es la constante multiplicativa.
#' @param c es la constante aditiva.
#'
#' @return un vector de números pseudoaleatorios.
generador_congruencial_u <- function(n, m = 1000, a = 32, c = 0){
  semilla <- as.numeric(Sys.time())
  x <-  c(semilla)
  norm <- c(x)
  for(i in 2:(n+1)){
    x[i] <- (a*x[i-1]+c) %% m
    norm[i]<- x[i] / (m-1)
  }
  return(norm[2:(n+1)])
}
generador_congruencial_u(20, 1000, 67, 0)

# Para esta prueba tenemos
# n = 100, m = 1000, a = 342
nums_pseudo = generador_congruencial_u(100, 1000, 342)
hist(nums_pseudo, main="Frecuencia de numero aleatorios")

# histograma con 500000 numeros
nums_pseudo = generador(34, 0, 100, 15, 500000)
hist(nums_pseudo, main="Histo", probability = TRUE)


gotas_dentro <- function(n){
  set.seed(cat(Sys.time(),"\n"))
  r1 <- generador_congruencial_u(n, 1000, 67, 0)
  r2 <- generador_congruencial_u(n, 1000, 67, 0)
  r3 <- generador_congruencial_u(n, 1000, 67, 0)
  contador_dentro <- 0
  for(i in 1:n){
    if(r1[i]^2+r2[i]^2+r3[i]^2 <= 1){
      contador_dentro = contador_dentro + 1
    }
  }
  return(contador_dentro)
}
gotas_dentro(1000)
grafica <- function(n){
  x=1:n
  y=c()
  for(i in 1:n){
    y[i]= (6*gotas_dentro(i))/i
  }
  plot(x,y, pch=16, cex=.5, col="red")
  abline(0,0,pi,0, col="blue")
}
grafica(2500)
