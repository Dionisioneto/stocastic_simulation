

## testar a função

gx = function(x){
  valor = (3/4)*(1-x^2)
  return(valor)
}

gsx = function(x){
  valor = (3/4)
  return(valor)
}

set.seed(42)
amostra = envsample(n=1000,objfun=gx,envfun=gsx,
                    limsup=c(-1,1))


squeezesample = function(n,objfun,envfun,squeezefun,limsup){
  ind = 0
  xs = NULL
  niter = 0
  
  while(ind < n){
    xsup = runif(1,limsup[1],limsup[2])
    u = runif(n=1,0,1)
    compsqueeze = squeezefun(xsup)/envfun(xsup)
    
    if(compsqueeze>=u){
      xs[ind] = xsup
      ind = ind + 1
    } else{
      p = objfun(xsup)/envfun(xsup)
      if(p>=u){
        xs[ind] = xsup
        ind = ind + 1 
      }
    }
    niter = niter + 1
  }
  res = list(x = xs, niter = niter)
  return(res)
}

# p = objfun(xenv)/envfun(xenv)

