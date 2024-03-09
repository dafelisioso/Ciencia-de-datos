bernoulli <- function(p){
  x = runif(1)
  if(x<p){
    return(1)
  }
  else{
    return(0)
  }
}

geometrica <- function(p){
  contador = 0
  while(bernoulli(p)==0){
    contador = contador + 1
  }
  return(contador)
}

geometrica2 <- function(p){
  x = runif(1)
  intervalo = 0
  extremo = p
  k=0
  while(x>extremo){
    intervalo = intervalo + 1
    k = k + 1
    extremo = extremo + ((1-p)^k)*p
  }
  return(intervalo)
}
bernoulli(.8)

geometrica(.8)

geometrica2(.8)
bernoulli(.5)
