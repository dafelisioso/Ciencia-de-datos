## Práctica 6

betafunden <- function(x,a,b){
  r = ( x^(a-1)*(1-x)^(b-1) )/( beta(a,b) )
  return(r)
}
betafunden(0.5,2,3)

supremo <- function(f,a,b){
  k <- seq(a,b,0.01)
  vc <- c()
  for (i in 1:length(k)){
    vc[i] = f(k[i],a,b)
  }
  supr <- max(vc)
  return(supr)
}

identidad <- function(x,y,z){
  return(x)
}

supremo(identidad, 1,4)

AcepRech <- function(a,b){
  k <- seq(a,b,0.01)
  vc <- c()
  for (i in 1:length(k)){
    vc[i] = betafunden(k[i],a,b)
  }
  S <- max(vc)
  x <- runif(1,0,1)
  y <- runif(1,0,S)
  cont = 0
  e <-  betafunden(x,a,b)
  print(e)
  print(y)
  if(y <= e){
    x1 <- x
    break;
  } else {
    cont = cont + 1
    AcepRech(a,b)
  }
  return(c(x1,cont))
}
AcepRech(2,5)


dist_beta <- function(x,a,b){
  if(0<=x & x<=1){
    return((x^(a-1))*((1-x)^(b-1))/(beta(a,b)))
  }
  else{return(0)}
}

sim_beta <- function(a,b){
  p_1 = runif(1)
  if(a>1 & b>1){
    c = (a-1)/(a+b-2)
    p_2 = runif(1,0,c)
    
  }
}

b=function(x,y){
  gamma(x)*gamma(y)/(gamma(x+y))
}

pdf=function(x,alpha,beta){
  (x^(alpha-1)*(1-x)^(beta-1))/b(alpha,beta)
}

sim_beta <- function(a,b,n){
  exitos=0
  puntos=c()
  totales=0
  while(exitos<n){
    totales=totales+1
    p_1 = runif(1)
    if(a>1 & b>1){
      c = pdf((a-1)/(a+b-2),a,b)
      p_2 = runif(1,0,c)
      
      if(pdf(p_1,a,b)>=p_2){
        exitos=exitos+1
        puntos=c(puntos,p_1)
      }
    }
  }
  return(list(puntos, totales, n))
}

sim_beta(2,2,5)

b22=sim_beta(2,2,10000)

print(b22[[1]])
hist(b22[[1]], probability = TRUE, breaks = 100, col="lightpink",
     xlab = '', ylab = '', main = '')

hist(b22[[1]])

betaLine=c()

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}

lines(seq(0,1,0.01),betaLine,col='red',lwd=2)


# Promedio de totales en los que se llega a nvalues de exitos
nvalues=c(10,100,200,500,1000,2000,3000)
promedio=c()
for(n in nvalues){
  promedio=c(promedio,sim_beta(2,2,n)[[2]])
}
plot(nvalues,promedio)


