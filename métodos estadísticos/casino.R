Casino <- function(dices,cost, bet){
  results <- sample(1:6,dices,replace=TRUE)
  success <- 0
  for(i in 1:length(results)){
    if (results[i] == bet){
      success <- success + 1
    }
  }
  profit <- (success-1)*cost
  return(profit)
}


#Aplicamos la función para el J1
r<-1000
d<-3
c<-10
b<-6

#Simulación Esperanza

m<-c()
for(i in 1:r){
  m[i]<-Casino(d,c,b)
}
Esim<-sum(m)/r
print(Esim)


#Simulación Varianza

s<-c()
for(i in 1:r){
  s[i]<-(m[i]-Esim)^2
}
VarSim<-(sum(s)/r)
print(VarSim)



#Aplicamos la función para el J2
r<-1000
d<-3
c<-115
b<-6

#Simulación Esperanza

m<-c()
for(i in 1:r){
  m[i]<-Casino(d,c,b)
}
Esim<-sum(m)/r
print(Esim)


#Simulación Varianza

s<-c()
for(i in 1:r){
  s[i]<-(m[i]-Esim)^2
}
VarSim<-(sum(s)/r)
print(VarSim)





###J1
#Esperanza teorica
EJ1<-10*(5/72)+20*(1/216)-10*(125/216)
EJ1
#Varianza teorica
VJ1<-((((-10)^2)*(125/216)+(10^2)*(5/72)+(20^2)*(1/216))-(EJ1*EJ1))
VJ1

###J2
#Esperanza teorica
EJ2<-(-115*(125/216)+115*(5/72)+230*(1/216))
EJ2
#Varianza teorica
VJ2<-((((-115)^2)*(125/216)+(115^2)*(5/72)+(230^2)*(1/216))-(EJ2^2))
VJ2


