## -----
## Simulacao estocastica
## -----

## ---
## Distribuicao Beta(a,1)
## ---


set.seed(42)
a = 0.5
n = 500

u = runif(n)

x = (exp(log(u*gamma(a))/a))/(exp(log(gamma(1+a)))/a)

## histograma

hist(x)

## ---
## qqplot para testar o comportamento da amostra gerada
## ---

# Geracao de quantis teoricos da distribuicao

xord = sort(x)
pxord = (1:n)/n 
qesp = qbeta(pxord, shape1 = a, shape2 = 1)

plot(xord,qesp,
     xlab = "valores observados",
     ylab = "valores esperados",
     main = paste("Quantil-Quantil da Beta(a=", a, ",b=1)"))

## Teste de Kolmogorov Smirnov de aderencia
ks.test(x, "pbeta", shape1 = a, shape2 = 1) 

## ---
## Distribuicao Beta(1,b)
## ---

set.seed(42)

b = 2.4
n = 500

u = runif(n)

xu = 1 - exp(log((1-((u*gamma(b)*b)/(gamma(b+1)))))/b)

## histograma

hist(xu)

## grafico qqplot

xuord = sort(xu)

pxord = (1:n)/n
qespbetab = qbeta(pxord, shape1 = 1, shape2 = b)

plot(xuord, qespbetab)

## Teste de Kolmogorov Smirnov de aderencia
ks.test(xw, "pbeta", shape1 = 1, shape2 = b) 

## ---
## Distribuicao Weibull [Correcao]
## ---

set.seed(10)

n = 500
u = runif(n)

alpha = 2.3
beta = 7

xw = exp((1/alpha)*log(-log(1-u)/beta^alpha))

## histograma

hist(xw)

## qqplot

xword = sort(xw)
probwb = (1:n)/n
qwbesp = qweibull(probwb, shape = lamb, scale = k) 

qqplot(xword,qwbesp)

## Teste de Kolmogorov Smirnov de aderencia
ks.test(xw, "pweibull", shape = alpha, scale = beta) 

## ---
## Funcao logistica [Revisao]qlogis
## ---

set.seed(42)

n = 500
ulog = runif(n)

mu = 9 ## mu e R
s = 3  ## s > 0

xlog = mu - s*log((1/u)-1)

## histograma

hist(xlog)

## Grafico Quantil-Quantil

xlogord = sort(xlog)
proplog = (1:n)/n

qesplog = qlogis(proplog, location = mu, scale = s)

plot(xlogord,qesplog) ## existencia de outliers

## Teste Kolmogorov-Smirnov
ks.test(xlog, "plogis", location = mu, scale = s) 

## ---
## Funcao Cauchy nao-central [Revisao]
## ---

set.seed(42)

n = 500
ucauchy = runif(n)

theta_cauchy = 5     ## x e R.
lambda_cauchy = 0.2  ## gamma > 0.

xcauchy = lambda_cauchy * tan((u - 0.5)*pi) + theta_cauchy

hist(x)

## Q-Qplot

xcauchyord = sort(xcauchy)
probcauchy = (1:n)/n

qespcauchy = qcauchy(p = probcauchy, location = theta_cauchy, scale = lambda_cauchy)

plot(xcauchyord,qespcauchy) ## qq-pot nao ficou muito bom


## Teste de Kolmogorov Smirnov de aderencia
ks.test(xcauchy, "pcauchy", location = theta_cauchy, scale = lambda_cauchy) 


