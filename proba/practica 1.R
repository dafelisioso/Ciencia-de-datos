
mh = function(n){
  matrix = matrix(nrow=n,ncol=n)
  for (i in n) {
    for (j in i:n) {
      matrix[i,j]=1/(i+j-1)
    }
  }  
}
print(mh(4))



simpson= function(f,a,b,n){
  h= (b-a)/2
  for (i in n){
    x(i)=a+i*h
  }
  s=h/3[ f()]
  
}
