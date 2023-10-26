## ---
## Simulacao estocastica
## Metodos MCMC
## ---

## Simular uma amostra da distribuição
## Binomial(n = 10, p = 0.2) usando uma Cadeia de Markov (CM)
## com Matriz de Transicao (MT) Q cujos elementos sao dados
## por qii = 0.50 e qij = 0.5/(n) para i diferente j.

## Construcao da Q
## Q é Simétrica, Reversível, Irredutível e Aperiódica.

n = 10
p = 0.2
Q = diag(0.5,n+1) ## suporte de 0 até 10.

for (i in 1:nrow(Q)){
  for(j in 1:ncol(Q)){
    if(i != j) Q[i,j] = 0.5/n
  }
}

rowSums(Q) ## verificando se as linhas somam um.

## Simulacao de uma funcao discreta

rdisc = function(prob){
  u = runif(1)
  cumprob = cumsum(p)
  val = sum(cumprob<u)+1
  return(val)
}

## ----
## Metropolis-Hastings para a simulacao
## estocastica da posteriori
## ----

set.seed(10)

iter = 5000 ## numero de iteracoes
acep = 0
prob_acep = NULL
x = NULL

x[1]=5 ## chute


for (i in 2:iter){
  x_prop = rdisc(Q[x[i-1]+1,])
  prob_acep[i] = min(1,dbinom(x_prop,n,p)/dbinom(x[i-1],n,p))
  u = runif(1)
  
  if(u<=prob_acep[i]){
    x[i]=x_prop
    acep=acep+1
  } else{x[i] = x[i-1]}
}

mean(x)
sd(x)

## verificar os valores gerados pelo algoritmo
hist(x)

plot(x=1:length(x),y=x, type='l')

## conferindo se a amostra parece realmente ser de uma 
## distribuição Binomial n e p
## sem conferir convergencia.
#

round(table(x)/iter,3)
round(dbinom(c(0,1:10),n,p),3)

## taxa de aceitacao
length(x)/iter


## ----
## Metropolis na simulacao da Qui-Quadrado inversa
## ----

## [objetivo]
## funcao densidade de probabilidade
## da distribuicao Qui-Quadrado inversa

dinvqui = function(x,df){
  fx = ((2^(-df/2))/(gamma(df/2)))*x^(-df/(2-1))*exp(-1/(2*x))
  return(fx)
  }

gl = 5
supinvqui = seq(0,10,length=1000)
deninvqui = dinvqui(x=supinvqui,df=gl)

plot(supinvqui,deninvqui, type='l')

## Como densidade de transicao, iremos
## utilizar da Uniforme(0,100)

set.seed(10)

iter = 100
aceit = 0

vetx = NULL     ## vetor de valores gerados
probacep = NULL ## vetor das probabilidades de aceitacao

vetx[1] = 0.1 ## chute

for(i in 2:iter){
  xprop = runif(1,min=0,max=5)
  ratio = dinvqui(x=xprop,df=gl)/dinvqui(x=vetx[i-1],df=gl)
  probacep[i] = min(1,ratio)
  
  u = runif(1)
  
  if(probacep[i] > u){
    vetx[i] = xprop
    aceit = aceit + 1
  } else{
    vetx[i] = vetx[i-1]
  }
}

vetx

