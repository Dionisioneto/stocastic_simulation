## -------
## Metodo da rejeicao adaptativa
## -------

## ---
## Passos do algoritmo
## ---

# 1. Escolha dos valores iniciais de xi em T;
# 2. Calculo da funcao squeeze [gI(x)] e envelope [gs(x)];
# 3. Simular de uma distribuicao proporcional a gs(x) e uma uniforme(0,1);
# 4. Aceitar x se u < [gI(x)/gs(x)];
# 5. Caso contrario, aceitar x se u <= [g(x)/gs(x)];
# 6. Em caso de rejeicao, incluir x em T, e retorna a 2.;
# 7. Repetir os passos 3 a 6 ate completar o tamanho amostral.

## ---
## 1.1 Definicao da funcao g(x), envelope
## ---

gx = function(x,mu,sigma2){
  func = exp((-1/2*sigma2)*(x-mu)^2)
  return(func)
}

## ---
## 1.2 Definicao da funcao gs(x), proporcional a g(x)
## ---

gsx = function(x){
  z1 = -0.5
  z2 = 0.5
  
  val = ifelse(x <= z1, (1/3)*exp(x+0.5),
               ifelse(z1 < x & x <= z2, 1/3,
                      (1/3)*exp(-x+0.5)))
  
  return(val)
}

## ---
## Plotando a funcao objetivo e a envelope
## ---

sup = seq(-4,4,length=500)
fx = dnorm(sup)

# fx
plot(sup,fx,type='l',ylim = c(0,0.5))

## T = (-1,0,1)
vetT = c(-1,0,1)
points(vetT[1],dnorm(vetT[1]),col = 'red',pch=16)
points(vetT[2],dnorm(vetT[2]),col = 'red',pch=16)
points(vetT[3],dnorm(vetT[3]),col = 'red',pch=16)

# gx
lines(sup, gsx(x=sup))







