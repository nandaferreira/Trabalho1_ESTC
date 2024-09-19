#Experimento

resultado <- c()

for (j in 1:100000){
  lancamentoInicial <- sum(sample(x = 1:6, size = 2, replace = TRUE))
  if(lancamentoInicial == 7 || lancamentoInicial == 11){
    resultado <- c(resultado,1) #concatena o vetor com o resultado daquela rodada
  }else if(lancamentoInicial == 2 || lancamentoInicial == 3 || lancamentoInicial == 12){
    resultado <- c(resultado,0)
  }else{
    lancamento <- 0
    while(lancamento != 7 && lancamento != lancamentoInicial){
      lancamento <- sum(sample(x = 1:6, size = 2, replace = TRUE))
      #print(lancamento)
      if(lancamento == 7){
        resultado <- c(resultado,0)
      }
      if(lancamento == lancamentoInicial){
        resultado <- c(resultado, 1)
      }
    }
  }
  
}
resultado

#Proporção de vitórias

vitorias <- sum(resultado == 1)
mean(resultado == 1)


#OBS:
#sum(resultado)/length(resultado) #mean de resultado porque ele guarda valores em 0s e 1s, então quando soma o 0 é "desconsiderado", ou seja, nulo na soma

