#Funciones auxiliares
set.seed(100)

Fn_hat <- function(x,xn){
  mean(xn <= x)
}

Fn_hat_gen <- function(x, xn){
  m <- length(x)
  if(m>1){
    yhat <- Fn_hat(x[1],xn)
    for (i in 2:m){
      yhat <- c(yhat,Fn_hat(x[i],xn))
    }
  }else{
    yhat <- Fn_hat(x,xn)
  }
  yhat
}

esta_contenida_normal <- function(reps,n,x_norm,mu,sigma,alpha){
  y_norm <- pnorm(x_norm,mu,sigma)
  count <- 0
  epsn <- sqrt(log(2/alpha)/(2*n))
  m <- length(x_norm)
  
  for (j in 1:reps){
    xn_norm <- rnorm(n,mu,sigma)
    yhat_norm <- Fn_hat_gen(x_norm,xn_norm)
    Ux_norm <- pmin(yhat_norm+epsn,1)
    Lx_norm <- pmax(yhat_norm-epsn,0)
    x_1 <- sum(y_norm < Ux_norm)
    x_2 <- sum(Lx_norm < y_norm)
    if(x_1 == m && x_2 == m){
      count <- count + 1
    }
  }
  count
}

esta_contenida_cauchy <- function(reps,n,x_cauchy,location,scale,alpha){
  y_cauchy <- pcauchy(x_cauchy,location,scale)
  count <- 0
  epsn <- sqrt(log(2/alpha)/(2*n))
  m <- length(x_cauchy)
  
  for (j in 1:reps){
    xn_cauchy <- rcauchy(n,location,scale)
    yhat_cauchy <- Fn_hat_gen(x_cauchy,xn_cauchy)
    Ux_cauchy <- pmin(yhat_cauchy+epsn,1)
    Lx_cauchy <- pmax(yhat_cauchy-epsn,0)
    x_1 <- sum(y_cauchy < Ux_cauchy)
    x_2 <- sum(Lx_cauchy < y_cauchy)
    if(x_1 == m && x_2 == m){
      count <- count + 1
    }
  }
  count
}

z <- esta_contenida_cauchy(1000,n,x_cauchy,location,scale,alpha)
print(z)

#Ejercicio 2

#Numero de muestras
n <- 100

#Parametros de la normal
mu <- 0
sigma <- 1

#Parametros de la Cauchy
location <- 0
scale <- 1

#Simulamos la normal y calculamos la funcion de distribucion empirica
x_norm <- seq(-5,5,length.out=1000)
xn_norm <- rnorm(n,mu,sigma)
y_norm <- pnorm(x_norm,mu,sigma)
yhat_norm <- Fn_hat_gen(x_norm,xn_norm)

#Simulamos la cauchy y calculamos la funcion de distribucion empirica
x_cauchy <- seq(-10,10,length.out=1000)
xn_cauchy <- rcauchy(n,location,scale)
y_cauchy <- pcauchy(x_cauchy,location,scale)
yhat_cauchy <- Fn_hat_gen(x_cauchy,xn_cauchy)

#Parametros para el intervalo de confianza
alpha <- 0.05
epsn <- sqrt(log(2/alpha)/(2*n))

#Calculamos la banda de la norma
Ux_norm <- pmin(yhat_norm+epsn,1)
Lx_norm <- pmax(yhat_norm-epsn,0)

#Calculamos la banda de la cauchy
Ux_cauchy <- pmin(yhat_cauchy+epsn,1)
Lx_cauchy <- pmax(yhat_cauchy-epsn,0)

#Graficamos la normal y su banda
plot(x_norm,y_norm,type="l",lwd=5,col="blue",main="CDF de una Normal(0,1) con una banda de confianza del 95%",xlab="X",ylab="P(X <= x)")
#lines(x_norm,yhat_norm,type = "l",col="red")
lines(x_norm,Lx_norm,type = "l",col="green",lwd=5)
lines(x_norm,Ux_norm,type = "l",col="green",lwd=5)

#Graficamos la cauchy y su banda
plot(x_cauchy,y_cauchy,type="l",lwd=5,col="blue",main="CDF de una Cauchy(0,1) con una banda de confianza del 95%",xlab="X",ylab="P(X <= x)")
#lines(x_cauchy,yhat_cauchy,type = "l",col="red")
lines(x_cauchy,Lx_cauchy,type = "l",col="green",lwd=5)
lines(x_cauchy,Ux_cauchy,type = "l",col="green",lwd=5)

z1 <- esta_contenida_normal(1000,n,x_norm,mu,sigma,alpha)
z2 <- esta_contenida_cauchy(1000,n,x_cauchy,location,scale,alpha)
print(z1)
print(z2)

