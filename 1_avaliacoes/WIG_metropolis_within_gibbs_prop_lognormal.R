## ----
## Estimação bayesian WIG
## função proposta normal
## ----


## ----
## Função de verossimilhaça
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

iter = 50000

alpha = rep(NA,iter)
beta = rep(NA,iter)
gamma = rep(NA,iter)

## Definicao dos chutes iniciais

alpha[1]= 135
beta[1] = 10
gamma[1] = 5

naceitealpha = 0
naceitebeta = 0

## inicio do laco
mu_propalpha = getmu(media=135,var=12^2)
sigma2_propalpha = getsigma2(media=10,var=2)

mu_propbeta = getmu(media=14,var=1.4^2)
sigma2_propbeta = getsigma2(media=10,var=2)


for(i in 2:iter){
  
  gamma[i] = rgamma(n=1,shape=length(dados) + 0.1,
                    rate=0.01+((alpha[i-1]^beta[i-1])*sum(dados^(-1*beta[i-1]))))
  
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

plot(1:iter,gamma,type='l')
plot(1:iter,alpha,type='l')
plot(1:iter,beta,type='l')

