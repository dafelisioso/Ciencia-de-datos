
# Ejercicio 7  --------------------------------------------------------------

#Considera X_1, ...., X_n una m.a t.q X_i ~ Unif(0 , thetha). 
#Sea thetha_hat= X_{max}= max { X_1, ...., X_n}.

#Definimos una semilla.

set.seed(2022)

#i) ----------- Genrerar un dataset de tamaño 50 con thetha=1 -----------------

n=50
muestra<- runif(n, min=0, max=1)

tetha_hat <- max(muestra)

#ii) ------------ Hayar la distribución  de thetha_hat. -----------------

#Obs: Para thetha fija. Si x_i > thehta_0 p.a 1<= i <= n, la muestra no proviene
#de una uniforme.

# f(thetha_hat | x1,...,xn) = 1/ thehta      Si max{x_i} <= thetha  


# --------- iii) Comparar la distribución de thetha_hat con el histograma----------

# No. de simulaciones: 

B <- 1000

thetha_boot <- rep(NA, B)
thetha_teo <- rep(NA, B)

for (i in 1: B){
  x_boot <- sample(muestra, size = n, replace = TRUE)
  
  thetha_boot[i]<- max(x_boot)
  thetha_teo[i]<- max(runif(n, min=0, max=1))
}

par(mfrow= c(1,2))

hist( thetha_boot, breaks = 6, col= 'darkblue', border='white',
      main='Distribución bootstrap', ylab = 'Densidad', probability = TRUE)

hist( thetha_teo, breaks = 15, col= 'darkgreen', border='white',
      main='Distribución teórica', ylab = 'Densidad', probability = TRUE)


# Conclusiones ------------------------------------------------------------
#Como se puede observar, la distribución del estimador bootstrap es bastante cercano
#A la distribución de thetha. Si bien el refinamiento de la distribución boots
#es menor en comparación a la teórica, esto posiblemente se puede solucionar aumentando
#El tamaño de lamuestra.

#En la mayoria de los casos el valor del estimador de thetha se aproxima baste bien
#Al real.
