## Práctica 8
library(Rlab)
library(Pareto)
library(Bernoulli)
#Ejercicio 1

Ejer1 <- function(n,p,err){
  y = c()
  sum = 0
  for (i in 1:n){
    sum = sum + rbern(1,p)
    y[i] = sum/i
    if (y[i]-p <= err){
      N = i
    }
  }
  plot(y,type="l",main="Sumas parciales Bernoulli",xlab="Ensayos",ylab="Sum par")
  abline(h=p,col="blue")
  return(N)
}
res = Ejer1(1000,0.5,1e-5)
res

Ejer2 <- function(n,a,b,err){
  y = c()
  sum = 0
  for (i in 1:n){
    sum = sum + rPareto(1,a,b)
    y[i] = sum/i
    if (y[i]-(a*b)/(b-1) <= err){
      N = i
    }
  }
  plot(y,type="l",main="Sumas parciales Pareto",xlab="Ensayos",ylab="Sum par")
  abline(h=(a*b)/(b-1),col="blue")
  return(N)
}
Ejer2(10000,1.98,6,1e-3)

Ejer3 <- function(n,k){
  xs <- c()
  for (i in 1:n){
    xs[i] <- sum(sample(1:7,7,replace=T))
  }
  hist(xs,prob=TRUE)
  res = table(xs)/n
  print(res[k])
}
Ejer3(100000,"24")
