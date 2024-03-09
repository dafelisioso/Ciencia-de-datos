### Práctica 7

#Ejercicio 1
f1 <- function(r,n,l){
  p = l/n
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

f2 <- function(r,n,l){
  p =l/n
  return( ( exp(-n*p)*(n*p)^r )/( factorial(r) ) )
}

#Cálculo y gráfica 
graf <- function(n,l,rang){
  xs0 <- seq(0,rang,1)
  ys1 <- sapply(xs0,f1,n=n,l=l)
  ys2 <- sapply(xs0,f2,n=n,l=l)
  plot(xs0,ys1,type="o",col="red",main=
         paste("Disbinom (azul) vs Dispois (rojo). n= ", n, "l= ", l),xlab="n",ylab="y")
  lines(xs0,ys2,type="l",col="blue")
  ys3 <- abs(ys2-ys1)
  error <- max(ys3)
  
  return(error)
}

#Llamada a la función del error máximo
graf(1000, 10, 100)


f3 <- function(r,n,p){
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

#Ejercicio 2

#Proponemos el error cuadrático medio como método alternativo
#Para medir la velocidad de convergencia
ejer2 <- function(n,rang,p){
  xs0 <- seq(0,rang,1)
  ys1 <- sapply(xs0,f3,n=n,p=p)
  ys2 <- dnorm(xs0, n*p, sqrt(n*p*(1-p)))
  plot(xs0,ys1,type="l",col="red",
       main=paste("Disbinom (azul) vs Disnorm (rojo). n= ", n, "p = ", p),xlab="n",ylab="y")
  lines(xs0,ys2,type="o",col="blue")
  ys3 <- sum((ys2-ys1)^2)/rang
  return(ys3)
}
#Ejemplo
ejer2(1000,1000000,0.85)

