library("igraph")
#Esta parte del codigo elige uno de los n nodos al alzar, todos tienen proba 1/n
###### Nodo inicial

nodoini<-function(n){
  u0<- runif(1,0,1) #cada nodo tiene la misma proba de ser elegido
  
  for(i in 1:n){   #revisamos en que intervalo cayó la uniforme y dependiendo de donde cae elegimos a ese nodo
    if(((i-1)/n)<=u0 & u0<(i/n)){
      x0<-i
    }
    else{
      nada<-0
    }
  }
  #print("Este es el nodo inicial")
  #print(x0)
  return(x0)  # regresa el nodo "inicial"
}



#Esta parte tiene el nodo vecino del nodo anterior que será  contagiado o que recuperará al nodo inicial
##### Nodo vecino

generador<-function(nodo, ady,n){
 
  posibles<-c() #guardamos los nodos que son elegibles, es decir todos los vecinos
  for(i in 1:n){
    if(ady[nodo,i]==1){
      posibles<-c(posibles,i) 
    }
  }
  p<-length(posibles)
  u2<-runif(1,0,1)  #generamos otra uniforme para saber qué vecino elegir
  for(j in 1:p){
    if(((j-1)/p)<=u2 & u2<(j/p)){
      x<-j      #dependiendo de donde cayó la uniforme elegimos a uno de los vecinos
    }
    else{
      nada<-0
    }
  }
  #print("Este es el indice")
  #print(x)
  nuevo<-posibles[x]
  return(nuevo)
}


###Esta es la función fea :( peeeero la idea es agarra el nodo y el vecino del nodo 
#y determina cual de los nodos se infecta o recupera


####Funcion de los colores
general<- function(adya,color,n, iter, probav=0.5, probar=0.5){

  #m1<-matrizz(n)
  o=0
  trayectoria<-c()
  colores<-c(color)
  cambios<-color
  #print("Este es el vector de colores original:")
  #print(color)
  for(j in 1:iter){ #este for itera tantas veces como el usuario le diga
    indice<-nodoini(n) #elegimos un nodo al azar
    #print(indice)
    new<- generador(indice,adya,n) #elegimos a uno de sus vecinos
    uj<-runif(1,0,1) #creamos una uniforme que ayudara a ver quien cambia a quien
    
    
    if(cambios[indice]==cambios[new]){  #si ambos nodos tienen el mismo color no hace nada
      o<-o+1
      #print('No hubo cambios')
    }
    
    else{ #si cad nodo tiene un color distinto entonces entra al else
    colnodin<-cambios[indice] #guardamos el color del nodo inicial 
    colorndele<-cambios[new]  #guardamos el color del vecino
    
    
    ####El nodo inicial es verde
    if(colnodin=='green'){      #si el nodo elegido es verde
      if(probav==probar){    #si la proba de cambiar de verde a rojo es igual que la de rojo a verde
        if(uj<probav){       #ponemos un criterio para elegir el color, si uj es menor a la proba de ser verde
          cambios[new]='green'   #cambiamos el nodo vecino a verde
        }else{                 #si uj es mayor a probaverde entonces cambiamos el nodo elegido a rojo
          cambios[indice]<-'red'
        }
      }
      #si la proba de ser rojo es menor a la proba de ser verde
      else if(probar<probav){      #nos fijamos en el uj
        if(uj<probar){             #si uj es menor a la proba de ser rojo
          cambios[indice]<-'red'  #cambiamos el nodo inicial a rojo
        }
        else{
          if(uj<probav){
            cambios[new]<-'green'
          }else{
          cambios[indice]<-'red'     #sino cambiamos al vecino a verde
          }
        }
      }
      #si la proba de ser verde es menor a la proba de ser rojo
      else{ 
        if(uj<probav){        #si uj es menor a probav entonces 
          cambios[new]='green'    #cambiamos al vecino a verde
        }
        else{                  #si uj es mayor a probav cambiamos al nodo inicial a rojo
          cambios[indice]='red'
        }
      }
      
      
    ###El nodo inicial es rojo
    }else{
      if(probav==probar){ #si la proba de pasar de verde a rojo es igual
        if(uj<probar){ #si uj es menor a probar 
          cambios[new]='red'     #cambiamos al nodo inicial a rojo
        }else{
          nuevocolor<-'green'
          cambios[indice]<-'green'
        }
      }
      else if(probar<probav){
        if(uj<probar){
          cambios[new]<-'red'
        }else{
          cambios[indice]<-'green'
        }
      }else{
        if(uj<probav){
          cambios[indice]='green'
        }else{
          cambios[new]='red'
        }
      }
      
    }
    }
    colores<-rbind(colores,cambios)
    trayectoria<-rbind(trayectoria,c(indice,new))
    #print('Este es el nodo elegido y su vecino elegido')
    #print(trayectoria)
    #print("Este es el cambio en los colores")
    #print(colores)
    #creamos la grafica
    g <-graph_from_adjacency_matrix(adya, mode = c('undirected'))
    # asignamos el color a cada nodo
    V(g)$color<-cambios
    # Hacemos el plot
    g$layout <- layout_in_circle
    plot(g)
    
  } #Cierra el for
  #return(list(trayectoria,colores))
}








#Esta parte crea una matriz de adyacencia donde cada nodo solo esta conectado con su nodo siguiente o nodo anterior
#######Matriz de adyacencia
matrizz<-function(n){
  matrizad<-rep(0,n)
  ad<-c()
  for(j in 1:n){
    for(i in 1:n){
      if((i==j+1)|(i==j-1)){
        ad[i]=1
      }
      else{
        ad[i]=0
      }
      #print(ad)
    }
    matrizad<-rbind(matrizad,ad)
    #print(matrizad)
  }
  for(k in 1:n){
    if(k==n-1){
      ad[k]=1
    }else{
      ad[k]=0
    }
  }
  matrizad<-rbind(matrizad,ad)
  #print(matrizad)
  matrizr<-matrizad[1:n+1, 1:n]
  #print(matrizr)
  return(matrizr)
}


#Prueba 1, con la matriz que hizo Laurita, es decir el 1 solo contagia o recupera al 2, el 2 contagia o recupera al 1 o al 3...

m1<-matrizz(40)

#Estos son unos chek de que estaban funcionando las funciones xdd, saltarlos
#iniciaal<-nodoini(5)
#g<-generador(iniciaal,m1,5)

#Aqui definimos ce que es el vector de colores inicales, este cambia dependiendo de la cantidad de nodos
ce<-c('green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red')


#Mandamos llamar la funcion, proba recuperar .3, contagiar .7
gener1<- general(m1,ce,40,50,0.3,0.7)
#Imprimimos el resultado
#print(gener)

#Mandamos llamar la funcion, proba recuperar .7, contagiar .3
gener1<- general(m1,ce,40,50,0.7,0.3)
#Imprimimos el resultado
#print(gener)


#Mandamos llamar la funcion, proba recuperar .7, contagiar .3
gener1<- general(m1,ce,40,50,0.5,0.5)




#######Matriz de adyacencia todos contra todos
matriztodos<-function(n){
  matrizad1<-rep(0,n)
  ad1<-c()
  for(j in 1:n){
    for(i in 1:n){
      if(i==j){
        ad1[i]=0
      }
      else{
        ad1[i]=1
      }
      #print(ad)
    }
    matrizad1<-rbind(matrizad1,ad1)
    #print(matrizad)
  }
  
  #print(matrizad)
  matrizr<-matrizad1[1:n+1, 1:n]
  #print(matrizr)
  return(matrizr)
}


m3<-matriztodos(40)
#Aqui definimos ce que es el vector de colores inicales, este cambia dependiendo de la cantidad de nodos
ce<-c('green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red',
      'green','red','green','red','green','red','green','red','green','red')

#Mandamos llamar la funcion
#Parametros: adya,color,n, iter, probav=0.3, probar=0.7

gener<- general(m3,ce,40,50,0.3,0.7)
#Imprimimos el resultado
#print(gener)

#Mandamos llamar la funcion
#Parametros: adya,color,n, iter, probav=0.7, probar=0.3

gener<- general(m3,ce,40,50,0.7,0.3)


#Parametros: adya,color,n, iter, probav=0.7, probar=0.3

gener<- general(m3,ce,40,50,0.5,0.5)










#Prueba matriz aleatoria

#c1<-c(0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1)
#c2<-c(1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
#c3<-c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0)
#c4<-c(0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
#c5<-c(0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0)
#c6<-c(1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0)
#c7<-c(0 ,0 ,0 ,1 ,0 ,0 ,0 ,1 ,1 ,0 ,0 ,1 ,0 ,0 ,1 ,1 ,0 ,0 ,0 ,0)
#c8<-c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 ,0 ,1 ,1 ,0 ,0 ,0)
#c9<-c(1 ,1 ,0 ,0 ,1 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,0 ,0 ,1)
#c10<-c(0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0)
#c11<-c(0 ,0 ,1 ,0 ,0 ,1 ,1 ,1 ,1 ,0 ,0 ,1 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0)
#c12<-c(1 ,0 ,1 ,1 ,1 ,0 ,0 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,0 ,1)
#c13<-c(0 ,1 ,1 ,1 ,1 ,0 ,0 ,1 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,1 ,0 ,1 ,1 ,1)
#c14<-c(0 ,0 ,0 ,1 ,0 ,1 ,1 ,1 ,0 ,1 ,0 ,1 ,1 ,0 ,0 ,1 ,0 ,0 ,0 ,0)
#c15<-c(1 ,1 ,0 ,1 ,1 ,1 ,1 ,0 ,1 ,0 ,1 ,1 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,1)
#c16<-c(0 ,1 ,1 ,0 ,1 ,1 ,1 ,0 ,1 ,0 ,1 ,1 ,1 ,1 ,0 ,0 ,1 ,1 ,0 ,0)
#c17<-c(1 ,0 ,1 ,0 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,0 ,1 ,1 ,1 ,0 ,0 ,0 ,1 ,1)
#c18<-c(0 ,0 ,1 ,0 ,1 ,0 ,0 ,0 ,1 ,1 ,0 ,1 ,1 ,0 ,0 ,1 ,0 ,0 ,0 ,1)
#c19<-c(0 ,1 ,0 ,0 ,1 ,1 ,0 ,1 ,0 ,0 ,1 ,0 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0)
#c20<-c(0 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,0 ,1 ,1 ,0 ,0)

#m2<-rbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20)
#print(m2)
#colores2<-c('red','red','red','red','green','green','green','red','green','green','green','red','red','red','green','green','green','red','red','green')

#g <-graph_from_adjacency_matrix(m2, mode = c('undirected'))

# asignamos el color a cada nodo
#V(g)$color<-colores2
#g$layout <- layout_in_circle
# Hacemos el plot
#plot(g)

#Orden de entradas: adya,color,n, iter, probav=0.5, probar=0.5

#gener1<- general(m2,colores2,6,20,0.8,0.2)
#print(gener1)





