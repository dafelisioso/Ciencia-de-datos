#1
vec1 <- function(n){
  set.seed(15)
  X <- rnorm(n)
  Y <- rnorm(n)
  a <- data.frame(X,Y)
}

vec2 <- function(n){
  set.seed(15)
  X <- rnorm(n)
  Y <- rnorm(n)
  Z <- X-Y
  W <- X+Y
  a <- data.frame(Z,W)
}

plot(vec1(1000))
plot(vec2(1000))


vec2 <- function(n){
  set.seed(15)
  X <- rnorm(n)
  Y <- rnorm(n)
  Z 

  a <- data.frame(Z,W)
}
#2
library(rgl)

Ysim=function(n){ 
  Y=c() 
  for(i in 1:n){ 
    densY=0 
    varY=runif(1) 
    if(varY<=1/6){ 
      densY=1 
      }
    else if(varY>1/6 & varY<=1/3){
      densY=2 
      }
    else if(varY>1/3){ 
      densY=3 
    }
    Y[i]=densY }
  return(Y)
  }
Xsim=function(n,m){
  X1=c() 
  X2=c() 
  X3=c() 
  for(i in 1:n){ 
    varY=Ysim(m) 
    X1[i]=sum(varY==1) 
    X2[i]=sum(varY==2) 
    X3[i]=sum(varY==3) } 
  df=data.frame(X1,X2,X3)
  } 


df=Xsim(1000,100) 
plot3d(df)
#3
plot(df[1:2])
