set.seed(123)
library(MASS)
g=geyser

#con los datos que se tienen
mean(g$waiting)  
hist(g$waiting,main='Frecuencia del tiempo de espera geyser',xlab="Waiting time",ylab='Frecuencia',col='darkmagenta')
abline(v = mean(g$waiting), col = "red", lwd = 2)  #histograma de los datos reales


#Bootstrap
#Creamos B vectores de  n
n <- 100
B <- 10000
#Agarramos con remplazo de geyser y de allí sacamos su nueva media via bootstrap
mu_hat_star <- rep(NA, B) 
for (t in 1:B) {
  xn_star <- sample(g$waiting, size = n, replace = TRUE)
  mu_hat_star[t] <- mean(xn_star)
}
se_hat_mu_hat_BOOT <- sqrt((1/B)*sum((mu_hat_star - mean(mu_hat_star))^2)) # Error estandar estimado vía Bootstrap 

mean(mu_hat_star)

#intervalos de confianza del 90%
aalpha = 0.1
# Intervalo de confianza Bootsrap normal
mean(g$waiting) + c(-1, 1)*qnorm(1-aalpha/2)*se_hat_mu_hat_BOOT


# Intervalo vía percentil
quantile(mu_hat_star, probs = c(aalpha/2, 1 - aalpha/2))




#Sacamos el histrgrama de mu_hat_star, con su media, y su intervalo de confianza

hist(mu_hat_star,main='Frecuencia del tiempo de espera geyser bootstrap',xlab="Waiting time",ylab='Frecuencia',col='gray')
abline(v = mean(mu_hat_star), col = "red", lwd = 2) #media

abline(v = quantile(mu_hat_star, probs = c(aalpha/2, 1 - aalpha/2)), col = "blue", lwd = 2,lty = 3:3) #intervalo de confianza


