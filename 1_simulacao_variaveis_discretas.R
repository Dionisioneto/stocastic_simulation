
## pelo metodo da inversa, simule de uma
## v.a X que segue uma bernoulli

## X e [1,0], com P(X = 1) = p

## p = 0.10

## F(X) = (P(X = 1), P(X = 1) + P(X = 0))
## F(X) = (0.1, 0.1 + 0.9)

## Se u e [0;p] entao x = 1; ) caso contrario.

## funcao para a geracao de dados discretos/cat
## com base na distribuicao acumulada


## geracao de uma amostra de 1000 valores

gendisc.unv = function(vetx, probs){
  u = runif(1)
  FX = cumsum(probs)
  x=vetx[sum(FX<u)+1]
  return(x)
}

gendisc = function(n, vetx, probs){
  sample = NULL
  for (i in 1:n){
    sample[i] = gendisc.unv(vetx = vetx,
                            probs = probs)
  }
  return(sample)
}


n.sample = 1000
velores.ber = c(0,1)
probabilidades = c(0.9,1-0.9)

amostraber = gendisc(n = n.sample, vetx =  velores.ber,
                  probs = probabilidades)

mean(amostraber)
var(amostraber)

## Cores dos olhos

cores = c("castanho", "preto", "azul", "verde")
coresdisc = c(0,1,2,3)

probs = c(0.6, 0.3, 0.05, 0.05)

n.sample = 500

cores = gendisc(n = n.sample, vetx = coresdisc, probs = probs)
prop.table(table(cores))

## ---
## Binomial
## ---

N = 14
x_realizados = 0:N
p = 0.5

fpbinom = dbinom(x = x_realizados, size = N, prob = p)

amostra_binomial = gendisc(n = 100, 
                    vetx = x_realizados,
                    prob = fpbinom)

mean(amostra_binomial)
var(amostra_binomial)

hist(amostra_binomial)

## ---
## Geometrica
## ---
## acumulo de sucessos ate o fracasso

## suporte limitado, por isso da na

X_realizacoes = 0:270
prob = 0.4

amostra_geometrica = gendisc(n = 100, 
                             vetx = X_realizacoes,
                             probs = dgeom(x=X_realizacoes, prob = prob))



## media da geometrica
1/prob
mean(amostra_geometrica)

## teste qui-Quadrado
table(amostra_geometrica)

chisq.test(table(amostra_geometrica),
           p = 100*(dgeom(x=X_realizacoes, prob = prob)[1:7]))

## ---
## Hipergeometrica
## ---

set.seed(42)
xobs = 0:6 ## valores possiveis que podem 
            ## ser amostrados no g1.

g1 = 12 ## quantidade do grupo foco
g2 = 8  ## quantidade do grupo não foco

kzin = 6 ## numero de amostras de g1 +g2 podem 
          ## ser amostradas sem reposicao


dhyper(x = xobs, m = g1, n = g2, k = kzin)

sample.hyper = gendisc(n = 300,
                       vetx = xobs ,
                       probs = dhyper(x = xobs, m = g1, n = g2, k = kzin))

sample.hyper

## Teste Qui-Quadrado de aderencia

table(sample.hyper)

chisq.test(x=table(sample.hyper),
           y=dhyper(x = xobs, m = g1, n = g2, k = kzin)*300)

## media e variancia da distribuicao
mean(sample.hyper)
kzin*(g1/(g1+g2))

## A variancia da distribuicao
var(sample.hyper)
kzin*(g1/(g1+g2))*(1-(g1/(g1+g2)))*((g1+g2-kzin)/(g1+g2-1))

## pessoas e hoteis

## o quarto e sorteado aleatoriamente assim que 
## chega ao hotel. Voce chega com o seu grupo de 
## excursao para se hospedar no hotel. Estude a
## probabilidade de voces pegarem 0, 1, 2 ou 3 
## quartos com varanda, quando temos um sorteio de
## 7 quartos na chegada.

## numero de quartos disponiveis com varanda (A)
A = 8
## numero de quartos disponiveis sem varanda (B)
B = 14

dhyper(x = 0:4, m = A, n = B, k = 7)

## simule uma amostra desta situacao


sample.hyper = gendisc(n = 10,
              vetx = 0:4,
              probs = dhyper(x = 0:4, m = A, n = B, k = 7))


mean(sample.hyper)


rDiscreta<-function(vetx,vetp){
  F<-cumsum(vetp)
  u<-runif(1)
  x<-vetx[sum(F<u)+1] ## uma outra forma de identificar em qual intervalo de F o valor u está
  return(x)}

n<-100
x<-NULL
set.seed(10)
for (i in 1:n) x[i]<-rDiscreta(vetx=0:4,dhyper(x = 0:4, m = A, n = B, k = 7))
table(x)

mean(x)

## ----
## Simulacao de uma Poisson(lambda = 10)
## ----

set.seed(42)
gendisc(n = 100,
        vetx = 5:15,
        probs = dpois(0:15, lambda = 10))

set.seed(10)
for (i in 1:10){
  valor = gendisc.unv(vetx = 0:10,probs = dpois(0:15, lambda = 10))
  print(runif(1))
  print(valor)
}






























