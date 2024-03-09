library(plot3D)

f_exp<-function(n,l){
  U <- runif(n,0,1)
  x <- log(U/l)/-l 
  return (x)
}

sim <- function(n){
  Y <- f_exp(n,1)+1
  X <- numeric(n)
  for(i in 1:n){
    X[i] <- exp(-Y[i])/(exp(-Y[i])+1)*f_exp(1,1/Y[i])
  }
  return(list(X,Y))
}

k <- sim(1000)

X <- unlist(k[1], recursive = FALSE)
Y <- unlist(k[2], recursive = FALSE)


##  Create cuts:
x_c <- cut(X, 20)
y_c <- cut(Y, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

##  Plot as a 2D heatmap:
image2D(z=z, border="black")


# ejercicio 2
M=cbind(c(1,0.7),c(0.7,1))
L=eigen(M)
V=M*(L$values)
Q=L$vector

set.seed(2)
n=10000
varX=c()
varY=c()
for(i in 1:n){
  X=rnorm(2,0,1)
  N=Q %*% (V^(1/2))
  Y=N %*% X +c(1,0)
  varX=c(varX,X)
  varY=c(varY,Y)
}
x_c <- cut(varX, 50)
y_c <- cut(varY, 50)
z=table(x_c,y_c)
hist3D(z=z, border="black")
par(mar=c(2, 2,2, 2))
image2D(z=z, border="black")

