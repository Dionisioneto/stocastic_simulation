## ---
## Simulacao estocastica
## Resolucao da lista 2
## ---


## pacotes estatisticos

if(!require(pacman)) install.packages("pacman")
p_load(condMVNorm)


## Exercicio 1: Simulacao da normal de p = 3.

vetmu = c(-2,0,2)
sigma3 = matrix(data=c(1,0.7,-0.2,
                       0.7,1,0.5,
                       -0.2,0.5,1), ncol=3,nrow=3)


## Algoritmo de Gibbs

##1. Defina valores inicias para os parâmetros β0,β1,ϕ

##2. Obter um novo valor θ(i)=(β(i)0,β(i)1,ϕ(i)), através da geração 
##sucessivas das distribuições:
## 2.1 Distribuição condicional completa de β0|.
## 2.2 Distribuição condicional completa de β1|.
## 2.3 utilizando os valores gerados em 3.
## 2.3 Distribuição condicional completa de ϕ|, utilizando os valores 
## gerados em 3 e 4.

# Repita o processo a partir do passo 2 até obter a convergência.
# Valores iniciais


condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 1,
        given.ind= c(2,3), X.given = c(-1,1))$condMean

condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 1,
        given.ind= c(2,3), X.given = c(-1,1))$condVar


sx1 = NULL
sx2 = NULL
sx3 = NULL

sx1[1] = 1
sx2[1] = 1
sx3[1] = 2

## Numero de interacoes
iter = 5000

for(i in 2:iter){
  
  ## Amostrando da condicional de x1
  mu1cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 1,
                    given.ind= c(2,3), X.given = c(sx2[i-1],sx3[i-1]))$condMean
  
  var1cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 1,
                     given.ind= c(2,3), X.given = c(sx2[i-1],sx3[i-1]))$condVar
    
  sx1[i] = rnorm(1,mean=mu1cond, sd=sqrt(var1cond))
  
  ## Amostrando da condicional de x2
  
  mu2cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 2,
                    given.ind= c(1,3), X.given = c(sx1[i],sx3[i-1]))$condMean
  
  var2cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 2,
                     given.ind= c(1,3), X.given = c(sx1[i],sx3[i-1]))$condVar
  
  sx2[i] = rnorm(1,mean=mu2cond, sd=sqrt(var2cond))
  
  ## Amostrando da condicional de x3
  
  mu3cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 3,
                    given.ind= c(1,2), X.given = c(sx1[i],sx2[i]))$condMean
  
  var3cond = condMVN(mean=vetmu, sigma=sigma3, dependent.ind = 3,
                     given.ind= c(1,2), X.given = c(sx1[i],sx2[i]))$condVar
  
  sx3[i] = rnorm(1,mean=mu3cond, sd=sqrt(var3cond))
}

sx1
sx2
sx3

hist(sx1)
hist(sx2)
hist(sx3)

plot(1:iter,sx1,type='l')
plot(1:iter,sx2,type='l')
plot(1:iter,sx3,type='l')




gchute = c(1,1,1)


## -----
## Exercicio 2
## Distribuição Multinomial
## -----


iter = 800

probs = c(0.15,0.30,0.25,0.30)
n = 100

x1 = NULL; x1[1] = 25
x2 = NULL; x2[1] = 25
x3 = NULL; x3[1] = 25
x4 = NULL; x4[1] = 25

for (i in 2:iter){
  x1[i] = (factorial(n-x1[i-1]))/(factorial(x1[i-1])*(1-probs[1])^(n-x1[i-1]))
  x2[i] = (factorial(n-x2[i-1]))/(factorial(x2[i-1])*(1-probs[2])^(n-x2[i-1]))
  x3[i] = (factorial(n-x3[i-1]))/(factorial(x3[i-1])*(1-probs[3])^(n-x3[i-1]))
  x4[i] = (factorial(n-x4[i-1]))/(factorial(x4[i-1])*(1-probs[4])^(n-x4[i-1]))
}

x1









