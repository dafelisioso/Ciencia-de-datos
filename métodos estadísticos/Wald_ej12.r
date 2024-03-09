#simulamos la poisson con los parámetros dados

fun <- function(n,lambda_0){
  
  alpha <- 0.05
  l_0=lambda_0
  cont=0
  vec <-c()
  for(i in 0:10000){
  poisson <-rpois(n,lambda_0)
  l_hat=mean(poisson)
  s_hat= sqrt(l_hat/n)
  wald=abs((l_hat-l_0)/s_hat)
  norm=qnorm((p=alpha/2), lower.tail = FALSE)
  vec <-  c(vec,wald)
  if(wald>norm){
    cont =cont+1
  }
  }
  return(cont/10000)
}

fun(20,1)

