### Teste das condicionais analiticas
### para a estimação bayesiana


## Função de verossimilhança

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

## Log da distribuição condicional de alpha
# 
# logcondalpha = function(x,dados,beta,gamma,a1,b1){
#   valor = (beta*length(dados)+a1-1)*log(x) - gamma*alpha^(beta)*sum(dados^(-beta)) - b1*alpha
#   return(valor[1])
# }

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

alpha[1]= 135
beta[1] = 10
gamma[1] = 5

naceitealpha = 0
naceitebeta = 0

## inicio do laco
a_propalpha = 126.5625
b_propalpha = 0.9375

a_propbeta = 100
b_propbeta = 7.142857

for(i in 2:iter){
  
  gamma[i] = rgamma(n=1,shape=length(dados) + 0.1,
                     rate=0.01+((alpha[i-1]^beta[i-1])*sum(dados^(-1*beta[i-1]))))
  
  alphaprop = rgamma(n=1,shape=a_propalpha,rate=b_propalpha)
  
  numalpha= logverwig(t=dados,alpha=alphaprop,beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=alphaprop,a=0.1,b=0.1)+logpriorigamma(x=alpha[i-1],a=a_propalpha,b=b_propalpha)
  demalpha= logverwig(t=dados,alpha=alpha[i-1],beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=alpha[i-1],a=0.1,b=0.1)+logpriorigamma(x=alphaprop,a=a_propalpha,b=b_propalpha)
  
  pactalpha = min(1,exp(numalpha-demalpha))
  u1 = runif(1,0,1)
  
  if(pactalpha > u1){
    alpha[i] = alphaprop
    naceitealpha = naceitealpha+1
  } else{
    alpha[i] = alpha[i-1]
  }
  
  betaprop = rgamma(n=1,shape=a_propbeta,rate=b_propbeta)
  
  numbeta= logverwig(t=dados,alpha=alpha[i],beta=betaprop,gamma=gamma[i])+logpriorigamma(x=betaprop,a=0.1,b=0.1)+logpriorigamma(x=beta[i-1],a=a_propbeta,b=b_propbeta)
  dembeta= logverwig(t=dados,alpha=alpha[i],beta=beta[i-1],gamma=gamma[i])+logpriorigamma(x=beta[i-1],a=0.1,b=0.1)+logpriorigamma(x=betaprop,a=a_propbeta,b=b_propbeta)
  
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








