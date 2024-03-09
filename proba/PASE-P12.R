
#Generar uniformes 


H<-function(W,V,thetha){
  
  if(W<(1-thetha)*V^(1-thetha)){
    x<- W/(1-thetha)*V^thetha
    
  }
  
  else if((1-thetha)*V^(1-thetha)<= W & W<V^(1-thetha)){
    x<-V
  }
  
  else if (W >= V^(1-thetha)){
    x<-W^(1-thetha)
  }  

  return(x)
}


func<- function(thetha){
  
  V<-runif(1,0,1)
  W<- runif(1,0,1)
  
  U<-H(W,V,0)
  
  return(c(U,V))
  
}

muestra<- c()
S<- c()
P<- c()


for (i in 1:1000){
  muestra<- func(0)
  S[i]<- muestra[1]
  P[i]<- muestra[2]
  
  
}

S
P

hist(S, probability = TRUE)
hist(P, probability = TRUE)


windows()
plot.new ()

variables<- data.frame(U=S, V=P)
variables$log_U<- -log(variables$U)
variables$log_V<- -log(variables$V)

hist(variables$log_U, probability = TRUE)
hist(variables$log_V, probability = TRUE)


