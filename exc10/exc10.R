#Questão 10: Luke Skywalker realizará o seguinte passeio aleatório na reta: a reta do passeio é formada pelos números inteiros de zero até 𝑁; Luke está em um ponto 𝐿 que é maior do que zero e menor do que 𝑁; Luke lança uma moeda honesta; se sair coroa, ele dá um passo para a esquerda (e termina na posição 𝐿 − 1 da reta); se sair cara, ele dá um passo para a direita (e termina na posição 𝐿 + 1 da reta). Luke continuará a lançar a moeda e se deslocará até que ele chegue em sua casa (e lá ele vai dormir e o passeio acaba) ou até que ele chegue (caia) no precipício (e, óbvio, o passeio também acaba nesse caso).


#(a) Para 𝑁 = 20, crie uma função cuja entrada seja 𝐿 (um número maior do que zero e menor do que 20) e que retorne 1 se Luke terminou um passeio em sua casa ou retorne zero se Luke caiu no precipício.
 
passeio <- function(L, N = 20){
  while(L >0 && L < N){
    
    #Sorteio da moeda: -1(coroa, esquerda), 1(cara, direita)
    passo <- sample(c(-1,1), size = 1)
    L <- L + passo
  }
  
  if(L == N){
    return(1) #Luke chegou em casa
  }else{
    return(0) #Luke caiu no precipício 
  }
}

passeio(L = 10)
passeio(L = 20)
passeio(L = 0)
passeio(L = 15)
passeio(L = 2)

#(b) Crie uma função cuja entrada seja 𝐿; esta função deverá replicar o passeio da letra (a) 10 mil vezes e retornar a proporção de vezes que Luke chegou em sua casa. Sugestão: crie um vetor que, para cada replicação, guardará o resultado de um passeio; cada entrada deste vetor será zero ou 1; zero se Luke caiu no precipício e 1 se Luke chegou em casa.


proporcao_casa <- function(L, N = 20, replicacoes = 10000){
  resultados <- numeric(replicacoes)
  
  for(i in 1:replicacoes){
    resultados[i] <- passeio(L, N)
  }
  
  proporcao <- mean(resultados)
  return(proporcao)
}

proporcao_casa(L = 10)

#c) Use a função criada em (b) para 𝐿 = 1, 2, … , 19 e, em seguida, use esses valores para plotar um gráfico de 𝑥 = 1 ∶ 19 por 𝑦, em que 𝑦 são as proporções retornadas pela função criada em (b) para cada 𝑥.

library(ggplot2)

valoresL <- 1:19
proporcoes <- numeric(length(valoresL))

for(L in valoresL){
  proporcoes[L] <- proporcao_casa(L)
}

dados <- data.frame(L = valoresL, Proporcao = proporcoes)

ggplot(dados, aes(x = L, y = Proporcao))+
  geom_line(color = "blue")+
  geom_point(color = "red")+
  labs(x = "L (Posição Inicial)", y = "Proporção de Sucesso", 
       title = "Proporção de Sucesso de Luke Chegar em Casa") +
  theme_minimal()
