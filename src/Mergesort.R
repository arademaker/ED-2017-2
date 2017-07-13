#Merge e Mergesort

merge_ = function(x,y){
  k = length(x)
  l = length(y)
  if(k==1){
    return(c(x,y))
  }
  if(l==1){
    return(c(y,x))
  }
  if(x[1]<=y[1]){
    return(c(x[1],merge_(x[2:k],y)))
  }else{
    return(c(y[1],merge_(x,y[2:l])))
  }
}


mergesort = function(a){
  n = length(a)
  if(n>1){
    return(merge_(mergesort(a[1:(n/2)]),mergesort(a[(n/2):n])))
  }else{
    return(a)
  }
}
