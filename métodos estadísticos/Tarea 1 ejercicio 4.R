library(ggplot2)

#Fijamos una semilla para tener reproducibilidad
set.seed(10)

#Creamos la funcion de con el parametro n que van a ser 
#los juegos del calamar que queramos simular
juego_del_calamar <- function(n){
  
  #Vamos a guardar nuestros resultados en el vector x
  x <- c()
  
  #Hacemos un for para hacer cada uno de los jeugos
  for (j in 1:n){
    
    #Inicializamos las variables
    jugadores_vivos <- 16
    posicion_del_peldano <- 0
    
    #Creamos un while con las condiciones necesarias para
    #que se acabe el juego
    while(jugadores_vivos>0 & posicion_del_peldano<18){
      
      #Voy a usar un truco ya que la binomial es suma de 
      #bernoullis entonces voy a usar esta para simular los
      #exitos y fracasos
      y <- rbinom(1,size=1,prob=0.5)
      
      if(y == 1){
        posicion_del_peldano <- posicion_del_peldano + 1
      }
      else{
        jugadores_vivos <- jugadores_vivos -1
      }
    }
    
    #Al final de cada juego guardamos el total de jugadores
    #que quedaron vivos
    x <- c(x,jugadores_vivos)
  }
  
  #Devolvemos el vector con todos nuestros resultados 
  #guardados
  return (x)
}

#Simulamos 500'000 casos
z <- juego_del_calamar(500000)


#Contamos el n�mero de ocasiones que paso cada suceso
x <- table(z)
print(x)


#4.1
#Contamos la cantidad de veces que pasaron 8 jugadores o mas y lo dividimos
#Entre el total de simulaciones para estimar la probabilidad
a <- sum(x[9:13])
print(a/500000) #0.037064


#4.2
#Sacamos el promedio de las personas que pasaron
b <- mean(z) 
print(b)  #1.445364


#4.3
#Estimamos la probabilidad de que pasen 1 o 0 jugadores contano la cantidad de 
#veces que pasaron 1 o 0 jugadores y lo dividimos entre el n�mero de jugadores
c <- sum(x[1:2]) 
print(c/500000) #0.701486


#4.4
#Primero tomamos las ocasiones que pasaron mas de 5 jugadores y despues sacamos la media
d <- mean(z[z>5])
print(d) #7.219489


##4.5
#Aqui no se me ocurrio como hacerlo de forma "formal" asi que tanteo
print(sum(x[1:5])/500000) #0.87353
print(sum(x[1:6])/500000) #0.92411

df = data.frame(z)
p <- ggplot(df,aes(x=z)) + geom_histogram(binwidth=1,color="black")
show(p)