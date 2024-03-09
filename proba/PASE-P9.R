#Función binomial 


limite <- function(k,n,p){
  
  v<-rbinom(k,n,p)
  media<-n*p
  varianza<- n*p*(1-p)
  normal <- (sum(v)-k*media)/(sqrt(k*varianza))
  
  return(normal)
    
  

  } 

grafica <- function(k,n,p){
  
  x <- c()
  for (i in 1:10000) {
   
     x[i]<- limite(k,n,p)
     
  }
  
  hist(x, probability = TRUE, main = 'Densidad teórica vs empírica')
  sop = seq( ???6 ,6 , length.out = 1000)
  par (new = TRUE )
  lines (sop,dnorm(sop))
  
  
}


limite(10,10,0.99)

grafica(10,10,0.5)  

