###########################################################################
#                        Ejemplo fácil                                    # 
# Sea X1,...,Xn una m.a. de una X ~ Gamma(5, 3) vamos a estimar la media  #
#             mu_hat = E(X) = 5/3 \approx 1.6                             #     
###########################################################################



###########################################################################
#                                                                         #
#         1 Mundo irreal de la simulación en donde conozco TODO           #
#                                                                         #
#       Para estimar supongo que no conozco nada.. pero                   #
#  puedo generar tantas m.a. de una X ~ Gamma(5, 3) como quiera           #
#                                                                         #
###########################################################################
n <- 100
B <- 100000
a <- 5
b <- 3
mu <- a/b
mu_hat <- rep(NA, B) 
for (t in 1:B) {
  xn <- rgamma(n, a, b)
  mu_hat[t] <- mean(xn)
}

se_hat_mu_hat <- sqrt((1/B)*sum((mu_hat - mean(mu_hat))^2)) # Error estandar irreal estimado vía simulación 
                            # siendo capaces de generar B muestras 
                            # cada una de tamaño n de X ~ Gamma(5, 3)

se_mu_hat <- sqrt(a/(b*b*n)) # error estandar real conociendo los parámetros
                             # de X ~ Gamma(5, 3)

# Intervalo de confianza irreal...
aalpha <- 0.05
mean(mu_hat) + c(-1, 1)*qnorm(1-aalpha/2)*se_hat_mu_hat
# valor que se busca estimar
mu
mean(mu_hat)



###########################################################################
#                                                                         #
#                         2 Mundo Bootstrap                               #
#                                                                         #
#     Sólo conozco X1,...,Xn una m.a. de una X ~ F                        #
#     vamos a estimar la media mu = E(X)                                  #
#                                                                         #
###########################################################################
n <- 100
xn <- rgamma(n, a, b)

B <- 10000
mu_hat_star <- rep(NA, B) 
for (t in 1:B) {
  xn_star <- sample(xn, size = n, replace = TRUE)
  mu_hat_star[t] <- mean(xn_star)
}

se_hat_mu_hat_BOOT <- sqrt((1/B)*sum((mu_hat_star - mean(mu_hat_star))^2)) # Error estandar estimado vía Bootstrap 


se_hat_mu_hat <- sd(mu_hat) # Error estandar irreal estimado vía simulación 

se_mu_hat <- sqrt(a/(b*b*n)) # error estandar real conociendo los parámetros
                             # de X ~ Gamma(5, 3)

# Intervalo de confianza Bootsrap normal
mean(xn) + c(-1, 1)*qnorm(1-aalpha/2)*se_hat_mu_hat_BOOT


# Intervalo vía percentil
quantile(mu_hat_star, probs = c(aalpha/2, 1 - aalpha/2))


# Intervalo de confianza irreal...
mean(mu_hat) + c(-1, 1)*qnorm(1-aalpha/2)*se_hat_mu_hat


mu           # valor que se busca estimar
mean(xn)     # estimador plug-in sólo con una muestra
mean(mu_hat) # estimador irreal 




# Por qué funciona tan bien el Bootstrap en este caso
x <- seq(0.01, 4, length.out = 1000)
Fx <- pgamma(x, a, b)

# Con una m.a. de 100 observaciones la distribución #
# empírica aproxima bien a la distribución real     #
plot(x, Fx, type = "l", lwd = 2, col = "blue")
lines(ecdf(xn), col = "red")


