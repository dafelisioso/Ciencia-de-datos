####Ejercicio II 


ruina<- function(M){
  max=2*M
  v<- c(M)
  while (M >0 & M <max) {
    volado = rbinom(1,1,0.5)
    
    if (volado == 0){
      M = M-1
      
    }else {
      M = M+1
    } 
  v=c(v,M)  
  }
  
  return(v)  
}

q<-ruina(50)

windows()
plot.new ()


plot(q, type= 'l', col='darkblue', main='Caminata')



#######Ejercicio II

t_paro<- function(M, k){
  max=2*M
  M_inicial<-M
  suma= 0
  
  for (i in 1:k) {
  
    M <- M_inicial
    contador= 0
    while (M >0 & M <max) {
      volado = rbinom(1,1,0.5)
      
      if (volado == 0){
        M = M-1
        
      }else {
        M = M+1
      } 
      contador= contador +1 
    }
  suma= contador + suma
  }

  return(suma / k)  
}


t_paro(150,100)


####Ejercicio 4


t_paro2<- function(M, k){
  
  M_inicial<-M
  suma= 0
  
  for (i in 1:k) {
    
    M <- M_inicial
    contador= 0
    while (M >0 ) {
      volado = rbinom(1,1,0.5)
      
      if (volado == 0){
        M = M-1
        
      }else {
        M = M+1
      } 
      contador= contador +1 
    }
    suma= contador + suma
  }
  
  return(suma / k)  
}

t_paro2(20,100)

