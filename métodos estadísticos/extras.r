# Para cualquier X ~ F(x), la asimetría esta dada por 
# k = E((X-mu)^3)/E((X-mu)^2)^(3/2) = E((X-mu)^3)/sigma^3
my_skewness<- function(x){
  x_bar <- mean(x)
  d <- x - x_bar
  m3 <- mean(d^3)
  m4 <- mean(d^2)^(3/2)
  m3/m4
}
f_norm_max <- function(x, n, mu, sigma){
  n*dnorm(x, mu, sigma)*pnorm(x, mu, sigma)^(n-1)
}
f_weibull_max <- function(x, n, a, b){
  n*dweibull(x, a, b)*pweibull(x, a, b)^(n-1)
}
intervalos_boot <- function(theta_boot, theta_hat, aalpha){
  z <- qnorm(1-0.5*aalpha)
  # Intervalo normal
  q1 <- theta_hat + c(-1, 0, 1)*z*sd(theta_boot)
  # Quantiles
  q2 <- quantile(theta_boot, probs = c(0.5*aalpha, 
                                       0.5, 1-0.5*aalpha))
  # Intervalo pivotal
  q3 <- 2*theta_hat + c(-1, 0, -1)*q2[3:1]
  q3[2] <- theta_hat
  #########################3
  res <- rbind(q1, q3, q2)
  row.names(res) <- c("normal", "pivotal", "quantiles")
  colnames(res) <- c("low", "est", "upp")
  res
}