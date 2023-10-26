## Esquema para avliar os valores do suporte 
## em relacao aos interceptos obtidos

z = c(0.05,0.5,2,3,10)  ## vetores de z para os intervalos 
x = seq(0,20,lenght=20)*0.42 ## vetores do suporte da distribuicao
lista = NULL       ## lista de valores em retorno

z;x

## estudar a posicao dos valores de dentro de z

for (i in 1:length(z)){
  elemento = z[i]
  
  ## somas com 0.002 indicarao o intervalo antes do primeiro elemento de z.
  ## somas com 0.003 indicarao entre intervalos de z.
  ## somas com 0.004 indicarao o intervalo apos o ultimo elmento de z.
  
  ## na primeira iteracao, estudar se x < z[1]
  ## ou se z[1] <= x < z[2]
  
  if(i == 1){
    for(c in 1:length(x)){
    if(x[c] < z[i]) lista[c] = x[c]+0.002 ## entao aplicque tal funcao
    if(x[c] >= z[i] & x[c] < z[i+1]) lista[c] = x[c] + 0.003
    }
  }
  
  ## nas iteracoes de z[2] ate z[length(z)], estudar se x
  ## e maior que o anterior e menor que o elemento, ou 
  ## se o x e maior que o elmento e menor que o proximo
  
  if(i != 1 & i < length(z)){
    anterior = z[i-1]
    
    for(c in 1:length(x)){
    if(x[c] >= anterior & x[c] < elemento){
      lista[c] = x[c] + 0.003
    }
    
    proximo = z[i+1]
    if(x[c] >= elemento & x[c] < proximo){
      lista[c] = x[c] + 0.003
    }
    }
  } 
  
  ## na iteracao para o ultimo elemento de, estudar os valores
  ## que sao maiores que este elmento
  
  if(i == length(z)){
    for(c in 1:length(x)){
    if(x[c] > z[i]) lista[c] = x[c] + 0.004
  }
  }
}

z;x
lista













