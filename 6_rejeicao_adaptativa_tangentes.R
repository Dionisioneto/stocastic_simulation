## ----
## Metodo da rejeicao adaptativa por
## retas tangentes
## ----

## ---
## SEM A PRESENCA DA FUNCAO SQUEEZE
## ---

## Escolha dos chutes iniciais
Tini = c(-1,0,1)

## Definicao g(x), proporcional a f(x) em foco.
gx = function(x,mu,sigma2) exp((-1/2*sigma2)*(x-mu)^2)

## Funcao para a h(x) e h'(x)
hx = function(x,mu,sigma2) log(gx(x=x,mu=mu,sigma2=sigma2))

devhx = function(x,mu,sigma2) {(-1/sigma2)*(x-mu)}

## Calculo dos interceptos [Melhorar para receber o vetor e já dar os pontos]

getintercept = function(vett,mu,sigma2){
  cont = 1
  lenvettt = length(vett)
  inter = NULL
  while(cont < lenvettt){
    numerador = (hx(x=vett[cont+1],mu=mu,sigma2=sigma2) - hx(x=vett[cont],mu=mu,sigma2=sigma2)) - (vett[cont+1]*devhx(x=vett[cont+1],mu=mu,sigma2=sigma2)) + (vett[cont]*devhx(x=vett[cont],mu=mu,sigma2=sigma2))
    denominador =  devhx(x=vett[cont],mu=mu,sigma2=sigma2) - devhx(x=vett[cont+1],mu=mu,sigma2=sigma2)
    inter[cont] = numerador/denominador
    cont = cont + 1
  }
  return(inter)
}

## obtendo os interceptos
getintercept(vett=Tini,mu=0,sigma2=1)

## Calculo de un(x)
unx = function(x, vett, mu, sigma2){
  inters = getintercept(vett=vett,mu=mu,sigma2=sigma2)
  ux = NULL
  
  leninters = length(inters)
  
  while(i <= leninters){
    if (x < inters[i]) ux[i] = hx(x=vett[i],mu=mu,sigma=sigma) + ((x-vett[i])*devhx(x=vett[i],mu=mu,sigma2=sigma2))
    if (x >= inters[i] & x <= inters[i+1]) ux[i] = hx(x=vett[i+1],mu=mu,sigma=sigma) + ((x-vett[i+1])*devhx(x=vett[i+1],mu=mu,sigma2=sigma2))
    i = i + 1
    }
    
}

## obtencao de g(x)
