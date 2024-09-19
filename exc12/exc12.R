# Questão 16
steven <- c(0,1,0)
garnit <- c(0,0,1)

partida <- function(){
  lancamentos <- sample(c(0,1), size = 3, replace = TRUE)
  
  # Vendo se algum dos jogadores ganhou nos 3 primeiros lançamentos 
  if (identical(lancamentos, steven)) {
    return("steven")
  } else if (identical(lancamentos, garnit)) {
    return("garnit")
  }
  
  # Looping para até que um jogador vença
  while(TRUE){
    novo_lancamento <- sample(c(0, 1), size = 1, replace = TRUE)
    lancamentos <- c(lancamentos, novo_lancamento)
    
    ultimos_tres <- tail(lancamentos, 3)
    
    if (identical(ultimos_tres, steven)) {
      return("steven")
    } else if (identical(ultimos_tres, garnit)) {
      return("garnit")
    }
  }
}

resultado <- partida()
print(resultado)


# Fazendo o jogo ser repetido dez mil vezes 
resultados <- c()
for (j in 1:10000) {
  resultado <- partida()
  resultados <- c(resultados, resultado)
}

media_garnit <- mean(resultados == "garnit")
media_steven <- mean(resultados == "steven")

print(media_steven)
print(media_garnit)

#interpretação: o código simula um jogo entre Garnit e Steven, em um lançamento de três moedas, o objetivo é determinar a 
#frequencia que cada jogador vence em um experimento de dez mil simulações. Ao final, é mostrado a média das vitórias de cada 
#jogador, mas, como o jogo é baseado em aleatoriedade, a média de vitórias pode variar, porém pode se aproximar de valores que 
#refletem a probabilidade teórica de cada jogador vencer. 
