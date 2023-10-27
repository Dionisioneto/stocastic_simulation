## ----
## Estimação bayesian WIG
## função proposta normal
## ----

library(compiler)

## ----
## Função de log-verossimilhaça
## ----

logverwig = function(t,alpha,beta,gamma){
  part1 = length(t)*log(gamma) + length(t)*log(beta) + length(t)*beta*log(alpha)
  part2 = (-beta-1)*sum(log(t))
  part3 = -gamma*alpha^(beta)*sum(t^(-beta))
  
  loglik = part1+part2+part3
  
  return(loglik)
}

## Distribuição a priori gamma

logpriorigamma = function(x,a,b){
  logp = (a-1)*log(x) - b*x
  return(logp)
}

## Funções para obter os parametros da log normal
## de acordo com uma localização

getmu = function(media,var){
  numerador = exp(2*log(media))
  denominador = 1 + (var/exp(2*log(media)))
  mu = (1/2)*log(numerador/denominador)
  return(mu)
}

getsigma2 = function(media,var){
  numerador = exp(2*log(media))
  denominador = 1 + (var/exp(2*log(media)))
  mu = (1/2)*log(numerador/denominador)
  
  sigma2 = 2*(log(media)-mu)
  return(sigma2)
}


### ----
### Inicio do algoritmo de Gibbs com
### passos de metropolis
### ----

## dados obtidos para os 69 bovinos que sofreram
## o evento de interesse.

dados = c(138,140,141,143,145,146,146,148,
          148,149,149,150,151,151,151,151,152,152,
          153,153,153,155,156,156,157,158,158,159,
          159,159,159,159,159,160,161,161,161,162,
          163,163,163,163,164,164,164,164,165,166,
          166,166,166,167,167,170,170,172,172,173,
          174,176,179,179,180,183,184,185,187,189,
          197)

## iteracao

iter = 5000

alpha = rep(NA,iter)
beta = rep(NA,iter)
gamma = rep(NA,iter)

## Definicao dos chutes iniciais

alpha[1]= 120
beta[1] = 100
gamma[1] = 100

naceitealpha = 0
naceitebeta = 0

## inicio do laco
mu_propalpha = getmu(media=130,var=5^2)
sigma2_propalpha = getsigma2(media=10,var=2)

mu_propbeta = getmu(media=10,var=5^2)
sigma2_propbeta = getsigma2(media=10,var=2)

set.seed(10)
enableJIT(3) ## ajudar a execução do for()
for(i in 2:iter){
  
  gamma[i] = rgamma(n=1,shape=length(dados) + 0.1,
                    rate=((alpha[i-1]^beta[i-1])*sum(dados^(-1*beta[i-1])))+0.1)
  
  alphaprop = rlnorm(n=1,meanlog=mu_propalpha,sdlog=sigma2_propalpha)
  
  numalpha= logverwig(t=dados,alpha=alphaprop,beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=alphaprop,a=0.1,b=0.1)+log(dlnorm(x=alpha[i-1],meanlog=mu_propalpha,sdlog=sqrt(sigma2_propalpha)))
  demalpha= logverwig(t=dados,alpha=alpha[i-1],beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=alpha[i-1],a=0.1,b=0.1)+log(dlnorm(x=alphaprop,meanlog=mu_propalpha,sdlog=sqrt(sigma2_propalpha)))
  
  pactalpha = min(1,exp(numalpha-demalpha))
  u1 = runif(1,0,1)
  
  if(pactalpha > u1){
    alpha[i] = alphaprop
    naceitealpha = naceitealpha+1
  } else{
    alpha[i] = alpha[i-1]
  }
  
  betaprop = rlnorm(n=1,meanlog=mu_propbeta,sdlog=sigma2_propbeta)
  
  numbeta= logverwig(t=dados,alpha=alpha[i],beta=betaprop,gamma=gamma[i])+logpriorigamma(x=betaprop,a=0.1,b=0.1)+log(dlnorm(x=beta[i-1],meanlog=mu_propbeta,sdlog=sqrt(sigma2_propbeta)))
  dembeta= logverwig(t=dados,alpha=alpha[i],beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=beta[i-1],a=0.1,b=0.1)+log(dlnorm(x=betaprop,meanlog=mu_propbeta,sdlog=sqrt(sigma2_propbeta)))
  
  pactbeta = min(1,exp(numbeta-dembeta))
  u2 = runif(1,0,1)
  
  if(pactbeta > u2){
    beta[i] = betaprop
    naceitebeta = naceitebeta+1
  } else{
    beta[i] = beta[i-1]
  }
  
}

naceitealpha/iter
naceitebeta/iter

par(mfrow=c(1,2))

plot(1:iter,gamma,type='l',
     ylab=expression(gamma), 
     xlab = "Iterações",
     main="Completo")

plot(1:iter,gamma,type='l', xlim = c(0,8000),
     ylab=expression(gamma), xlab = "Iterações",
     main="Recorte")

plot(1:iter,alpha,type='l',
     ylab=expression(alpha), xlab = "Iterações",
     main="Completo")

plot(1:iter,alpha,type='l', xlim = c(0,8000),
     ylab=expression(alpha), xlab = "Iterações",
     main="Recorte")

plot(1:iter,beta,type='l',
     ylab=expression(beta), xlab = "Iterações",
     main="Completo")

plot(1:iter,beta,type='l', xlim = c(0,8000),
     ylab=expression(beta), xlab = "Iterações",
     main="Recorte")



## Por meio do traceplot definiremos um burn-in de 
## 1.000 valores

burnin = 5000

gamma = gamma[-c(1:burnin)]
alpha = alpha[-c(1:burnin)]
beta = beta[-c(1:burnin)]

## ----
## Grafico de autocorrelação
## ----

acf(gamma,xlab="Salto",main=expression(gamma));#acf(gamma,plot=FALSE)
acf(alpha,xlab="Salto",main=expression(alpha));#acf(alpha,plot=FALSE)
acf(beta,xlab="Salto",main=expression(beta));#acf(beta,plot=FALSE)

## por conta do parametro gamma, devemos dar pulos 
## de 40 em nossa amostra

nposburin = length(gamma)

gamma = gamma[seq(1,nposburin,by=40)]
alpha = alpha[seq(1,nposburin,by=40)]
beta = beta[seq(1,nposburin,by=40)]


length(gamma) ## tamanho após recorte da autorrelação

## ---
## Gráficos de trajetoria pos-burn in das iteracoes
## ---

plot(1:length(gamma),gamma,type='l',
     ylab=expression(gamma), xlab = "Iterações")

plot(1:length(beta),alpha,type='l',
     ylab=expression(beta), xlab = "Iterações")

plot(1:length(alpha),beta,type='l',
     ylab=expression(alpha), xlab = "Iterações")

# Apenas para visualizar um recorte
#plot(1:length(gamma),gamma,type='l', xlim = c(0,2000))
#plot(1:length(beta),alpha,type='l', xlim = c(0,2000))
#plot(1:length(alpha),beta,type='l', xlim = c(0,2000))

## ---
## Gráficos de sobreposição de densidades pós burn-in.
## Se os histogramas do inicio da cadeia e do fim da
## cadeia estiverem de acordo (50%/50%), entao a covergencia
## está atiginda desde o inicio
## ---

## alpha
hist(alpha[1:(length(alpha)/2)], col=rgb(0,0,1,1/4),
     freq = F, main = '',
     xlab = expression(alpha),
     ylab="Frequência Relativa")  

hist(alpha[(length(alpha)/2)+1:length(alpha)], freq = F,
     col=rgb(1,0,0,1/4), add=T)  

## beta
hist(beta[1:(length(beta)/2)], col=rgb(0,0,1,1/4),
     freq = F, main = '',
     xlab = expression(beta),
     ylab="Frequência Relativa")  

hist(beta[(length(beta)/2)+1:length(beta)], freq = F,
     col=rgb(1,0,0,1/4), add=T) 

## gamma
hist(gamma[1:(length(gamma)/2)], col=rgb(0,0,1,1/4),
     freq = F, main = '',
     xlab = expression(gamma),
     ylab="Frequência Relativa")  

hist(gamma[(length(gamma)/2)+1:length(gamma)], freq = F,
     col=rgb(1,0,0,1/4), add=T)  

## ---
## Diagnosticos da cadeia por meio de testes
## ---
#install.packages("coda")
library(coda)

## Diagnostico de Geweke
## A estatistica de teste deve estar entre -1,96 e 1,96

geweke.diag(gamma, frac1 = 0.1, frac2 = 0.5) ## ok
geweke.diag(beta, frac1 = 0.1, frac2 = 0.5)  ## ok
geweke.diag(alpha, frac1 = 0.1, frac2 = 0.5) ## ok

## Diagnostico de Heidelberg e Welch's
## Hipótese nula: A cadeida gerada é estacionária da posteiori de interesse

heidel.diag(gamma) ## ok
heidel.diag(beta)  ## ok
heidel.diag(alpha) ## ok

## ---
## Medidas descritivas das amostras
## ---

parametros = cbind(alpha,beta,gamma)

## Média
apply(parametros,MARGIN = 2, FUN = mean)

## Desvio-padrão
apply(parametros,MARGIN = 2, FUN = sd)

## Quartil 1 (25%), Mediana (50%), Quartil 2 (75%)
apply(parametros,MARGIN = 2, FUN = quantile, probs = c(0.25,0.5,0.75))

## Histogramas das amostras
hist(alpha, freq = F, main = '',
     xlab = expression(alpha),
     ylab="Frequência Relativa",
     col="steelblue")

hist(beta, freq = F, main = '',
     xlab = expression(beta),
     ylab="Frequência Relativa",
     col="deeppink3")

hist(gamma, freq = F, main = '',
     xlab = expression(gamma), 
     ylab="Frequência Relativa",
     col="seagreen")

## ----
## Ajuste da curva de sobrevivencia estimada
## ----

## ---
## Função de sobrevivência da Weibull Inversa Generalizada
## ---

stWIG = function(t,alpha,beta,gamma){
  val = 1 - exp(-gamma*((alpha/t)^beta))
  return(val)
}


library(survival)

# Compute Kaplan-Meier survival estimates
survobj = Surv(dados, rep(1,length(dados)))
km_curve <- survfit(survobj~1)

# Plot the Kaplan-Meier survival curve
plot(km_curve,conf.int=F,
     xlab = "Tempo de sobrevivência (em dias)",
     ylab = "Probabilidade de Sobrevivência",
     xlim=c(138,200))

st_bayes = stWIG(t=km_curve$time,alpha=median(alpha),
                 beta=median(beta),
                 gamma=median(gamma))

## comparação com o trabalho do autor
# st_bayes = stWIG(t=km_curve$time,alpha=134.6,
#                  beta=14.14,
#                  gamma=7.596)

lines(km_curve$time,st_bayes,col="deeppink3",lwd=3)
legend("topright",legend="Ajuste Bayesiano",bty="n",
       col = "deeppink3", lwd = 3)


