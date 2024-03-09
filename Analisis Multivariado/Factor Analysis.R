

# Librerias ---------------------------------------------------------------
library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library(car)
library(tidyverse)

# Analisis de Factores ----------------------------------------------------

#Carga de datos

setwd('C:\Users\danie\OneDrive\Documentos')


#Cargamos los datos
data =read.table("food.dat", header=FALSE)

colnames(data) <- c('fam', 'pan','veg','frutas','carne','pollo','leche','vino')

#Nos quedamos solo con las variables
df= data %>% select(2:8)

#Análisis descriptivo:
describe(df)

#Extraer la matriz de varianzas y covarianzas.
covarianza = cor(df)

corrplot(covarianza, method="number")

#Prueba de esfericidad de Bartlett para datos correlacionados
    #  H_0: Matriz de Covarianzas = I_n  vs    H_a: Matriz de Covarianzas != I_n 

cortest.bartlett(df)
#Se rechaza H_0 por lo que tiene sentido hacer un análisis de factores.

#Calculamos el determinante de la matriz de covarianzas: 
det(covarianza)

#Vemos que aunque es un númro muy cercano a cero, es positivo por lo que el 
#Análisis tiene solución.

# Determinar el número de factores ----------------------------------------

fafitfree <- fa(df,nfactors = ncol(df), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

#Ahora graficamos el porcentaje de variabilidad acumulada

scree$proportion_of_variance=fafitfree$e.values/ sum(fafitfree$e.values)
scree$Cumulated_proportion=cumsum(scree$proportion_of_variance)

ggplot(scree, aes(x = Factor_n, y = Cumulated_proportion, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Cumulated proportion") +
  labs( title = "Cummulated Variance")

#Grafico de Parallel

parallel <- fa.parallel(df)

# Parallel analysis suggests that the number of factors =  2  and the number of components =  1 

#Realizamos el análisis de factores: 
Nfacs= 3

##Sin rotación

fit <- factanal(df, Nfacs, rotation="none")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

#Graficamos los factores 
load1 <- fit$loadings[,1:2]
load2 <- fit$loadings[,2:3]
load3 <- fit$loadings[,c(1,3)]

plot(load1,type="n", main='Factor 1 vs Factor 2') # set up plot
text(load1,labels=names(df),cex=.7)

plot(load2,type="n", main='Factor 2 vs Factor 3') # set up plot
text(load2,labels=names(df),cex=.7)

plot(load3,type="n", main='Factor 1 vs Factor 3') # set up plot
text(load3,labels=names(df),cex=.7)


## Rotación sin rotación

factores1 <- fit <- factanal(df, Nfacs, rotation="none")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

#Graficamos los factores 
load <- fit$loadings[,1:2]
plot(load,type="n", main='Factores sin rotación') # set up plot
text(load,labels=names(df),cex=.7)
