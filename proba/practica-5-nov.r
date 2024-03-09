library(MASS)

H <- function(W,V,Th){
  if(W<(1-Th)*V**(1-Th)){
    return((W*V**Th)/(1-Th))
  }
  else if( (1-Th)*V**(1-Th) <= W  && W < V**(1-Th) ){
    return(V)
  }
  else if( W >= V**(1-Th) ){
    return(W**((1)/(1-Th)))
  }
}

vector_aleat <- function(n,Th){
  set.seed(77)
  W <- runif(n)
  V <- runif(n)
  U <- H(W,V,Th)
  vector <- cbind(U,V)
  return(vector)
}

dat <- data.frame(vector_aleat(1000,0.85))

h1 <- hist(dat$U, breaks=30, plot=F)
h2 <- hist(dat$V, breaks=30, plot=F)
top <- max(dat$counts, h2$counts)
k <- kde2d(dat$U, dat$V, n=30)

oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
image(k) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)

vector_2 <- function(vector){
  X <- -log(vector[,1])
  Y <- -log(vector[,2])
  vector_ret <- cbind(X,Y)
  return(vector_ret)
}

dat <- data.frame(vector_2(vector_aleat(1000,0.9)))

h1 <- hist(dat$X, breaks=30, plot=F)
h2 <- hist(dat$Y, breaks=30, plot=F)
top <- max(dat$counts, h2$counts)
k <- kde2d(dat$X, dat$Y, n=30)

oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
image(k) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)

