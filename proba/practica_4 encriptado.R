Stepanho <- function(p){
  x = runif(1)
  if(x<p){
    return(1)
  }
  else{
    return(0)
  }
}

gertrudis <- function(p){
  contador = 0
  while(Stepanho(p)==0){
    contador = contador + 1
  }
  return(contador)
}

ernestino <- function(p){
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
gertrudis(.4)
