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

#Interpretação: O código simula um jogo entre Steven e Garnit, onde cada um escolhe uma sequência de três lançamentos de moedas. 
#O objetivo é descobrir quantas vezes cada um vence em dez mil rodadas do jogo. Ao final, calculamos a média de vitórias de cada 
#jogador. Como o resultado depende da sorte, a média pode variar a cada execução, mas deve se aproximar da probabilidade real de 
#cada jogador vencer. Isso ajuda a entender como a aleatoriedade influencia os resultados e como as escolhas de cada jogador 
#impactam suas chances de ganhar!
