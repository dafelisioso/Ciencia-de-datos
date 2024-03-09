#Parte 1

vec1 <- function(n){
  set.seed(15)
  Y <- rexp(n,1)
  X <- rexp(n,1/Y)
  a <- data.frame(X,Y)
}

plot(vec1(1000))


#Parte 2

#X <- c( rnorm(1,0,1), rnorm(1,0,1) )

set.seed(21)
X <- rnorm(2)

M <- matrix(c(1,0,0,1), nrow=2, ncol=2)

library(ggplot2)

b <- c(0,0)

Y <- M%*%X + b
Y

Y2 <- function(n){
  set.seed(21)
  Y3 <- c()
  for(i in 1:n){
    X <- rnorm(2)
    Y3 <- cbind(Y3, M%*%X + b)
  }
  Y3 <- t(Y3)
  Y3 <- data.frame(Y3)
  return (Y3)
}

g1 <- ggplot(data = Y2(10000), 
             mapping = aes(x = X1,
                           y = X2)) + 
  geom_point()
show(g1)
