#Nome: Anna Karolyna Pereira Santos         Matrícula: 12221BCC046
#Nome: Fernanda Ferreira de Melo            Matrícula: 12211BCC043

#--------------- Exercício 1 ---------------

vetor1 <- c(10:30) #(a)
vetor1

vetor2 <- c(30:10) #(b)
vetor2

vetor3 <-c(vetor1,vetor2[-1]) #(c)
vetor3

#--------------- Exercício 2 ---------------

# a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências do número 2 
vetorA <- rep(c(2,4,6,8), times = 10)
vetorA

#(b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8
vetorB <- c(rep(c(2,4,6,8), times = 10), 2)
vetorB
#--------------- Exercício 3 ---------------

#(a)Somatório índice(n) = 20, m = 30

somatorio1 <- c()

for (n in 20:30){
  somatorio1 <- c(somatorio1,((n^2) + 4*n))
}
resultado1 <- sum(somatorio1)

#(b) Somatório índice(n) = 10, m = 20

somatorio2 <- c()
for(k in 10:20){
  somatorio2 <-c(somatorio2,(((3^k)/k) +((2^k)/(k^2))))
}
resultado2 <- sum(somatorio2)

#OBS
#somatorio1  é um vetor que guarda o resultado da equação dada até o fim do intervalo
#o resultado final é o somatório de todos esses valores

#--------------- Exercício 4 ---------------
#Numa urna há bolas idênticas numeradas de 1 até 100. Serão extraídas 40 bolas com reposição desta urna. 
#Simule este experimento e guarde o resultado dos sorteios em um vetor.

set.seed(142)
sorteio <- sample(1:100, size = 40, replace = TRUE)
sorteio 

#(a) Quantas bolas pares foram sorteadas?
pares <- sorteio[sorteio %% 2 == 0]
qtde_pares <- length(pares)
qtde_pares

#(b) Quantas bolas maiores do que 70 foram sorteadas?
maiores_70 <- sorteio[sorteio > 70]
qtde_maiores70 <- length(maiores_70)
qtde_maiores70

#(c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?
posicoes_impares <- which(sorteio %% 2 != 0)
posicoes_impares

#--------------- Exercício 5 ---------------

lancamento <- function(){
  x <- 1 #sinalizador para continuar o laço (ou não)
  j <- 1 #contador para guardar os resultados dos dados dentro do vetor
  vetorDado <- c()
  while(x){
    dado <- sample(x= 1:6, size = 1, replace = TRUE)
    vetorDado[j] <- dado
    j <- j + 1
    if(sum(vetorDado == 4) == 2){
      x <- 0 #para o laço
      
    }
  }
  print(vetorDado)
  return(length(vetorDado))
}

quantidadeLancamentos <- lancamento()
quantidadeLancamentos

#Quando eu pergunto se vetor == 4, o R retorna "true" nas posições em que o número aparece
#se eu fizer o somatório ele me retorna quantas vezes o número apareceu, então já que a condição de parada é até o 4 aparecer duas vezes,
#"length(vetorDado)" vai retornar o tamanho do vetor, ou seja, a quantidade de vezes necessária para a aparição do número 2x

#--------------- Exercício 6 ---------------

lancamento <- function(){
  x <- 1 #sinalizador para continuar o laço (ou não)
  j <- 1 #contador para guardar os resultados dos dados dentro do vetor
  vetorDado <- c()
  while(x){
    dado <- sample(x= 1:6, size = 1, replace = TRUE)
    vetorDado[j] <- dado
    j <- j + 1
    if(sum(vetorDado == 4) == 2){
      x <- 0 #para o laço
      
    }
  }
  print(vetorDado)
  return(length(vetorDado))
}

quantidadeLancamentos <- lancamento()
quantidadeLancamentos


#Replicando o experimento dez mil vezes
quantidades <- c()
for(i in 1:10000){
  lancamento <- function(){
    x <- 1 
    j <- 1 
    vetorDado <- c()
    while(x){
      dado <- sample(x= 1:6, size = 1, replace = TRUE)
      vetorDado[j] <- dado
      j <- j + 1
      if(sum(vetorDado == 4) == 2){
        x <- 0 
        
      }
    }
    return(length(vetorDado))
  }
  
  #Armazenando o resultado da função no vetor
  quantidades[i] <- lancamento()
}

quantidades
mean(quantidades)

#Interpretação: O código realiza um experimento de probabilidade que envolve o lançamento de um dado, com o objetivo de determinar 
#quantos laçamentos serão necessários até que o número 4 apareça duas vezes. Depois, esse experimento é repetido dez mil vezes, 
#e ao final calcula-se a média do número de lançamentos necessários para atingir a condição de parada.

#--------------- Exercício 7 ---------------

fibonacci <- function(n) {
  if(n < 3) {
    break
  }
  # n tem que ser maior ou igual a três para conseguir fazer a soma de n com os dois imediatemente anteriores
  
  #garanto que as duas primeiras posições serão sempre 1
  fib <- numeric(n)
  fib[1] <- 1
  fib[2] <- 1
  
  for(i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  return(fib) 
}
fibonacci(10) #exemplo dado

#--------------- Exercício 8 ---------------

participantes <- c("Michael", "Dwight", "Kevin", "Jim", "Creed")

amigo_oculto <- function(){
  sorteio <- sample(participantes)
  if(any(sorteio == participantes)){
    return (0)
  }else{
    return(1)
  }
}

set.seed(150)
resultado <- replicate(100000, amigo_oculto())

head(resultado, 20)
proporcao_errado <- mean(resultado)
proporcao_errado

#--------------- Exercício 9 ---------------

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



#--------------- Exercício 10 ---------------





#--------------- Exercício 11 ---------------
#(a)

#coordenadas
x <- 0
y <- 0
direcoes <- c("R", "L", "U", "D")

for (j in 1:8){
  dado <- sample(x = direcoes, size = 1)
  
  if(dado == "U"){
    y <- y + 1
  }
  if(dado == "R"){
    x <- x + 1
  }
  if(dado == "L"){
    x <- x - 1
  }
  
  if(dado == "D"){
    y <- y - 1
  }
  
}
coordenadas <- c(x,y)


#(b) 10 mil vezes

caminho <- function(){
  x <- 0
  y <- 0
  direcoes <- c("R", "L", "U", "D")
  
  for (j in 1:8){
    dado <- sample(x = direcoes, size = 1)
    
    if(dado == "U"){
      y <- y + 1
    }
    if(dado == "R"){
      x <- x + 1
    }
    if(dado == "L"){
      x <- x - 1
    }
    
    if(dado == "D"){
      y <- y - 1
    }
    
  }
  coordenadas <- c(x,y)
  return(coordenadas)
}

voltaOrigem <- 0

for(i in 1:10000){
  coord<-caminho()
  if(coord[1] == 0 && coord[2] == 0){
    voltaOrigem <- voltaOrigem + 1
  }
  
}
voltaOrigem

proporcao <- voltaOrigem/10000 
# A cada 10.000 jogos, a proporção de vezes que o Link volta para o ponto de origem é de, aproximadamente, 7,78% (0.0778).

#(c)

caminho_n <- function(n){
  if(n%%2 != 0){
    return("Impossível retornar à origem depois de um número ímpar de passos")
  }else{
    for(i in 1:10000){
      x <- 0
      y <- 0
      direcoes <- c("R", "L", "U", "D")
      
      for (j in 1:n){
        dado <- sample(x = direcoes, size = 1)
        
        if(dado == "U"){
          y <- y + 1
        }
        if(dado == "R"){
          x <- x + 1
        }
        if(dado == "L"){
          x <- x - 1
        }
        
        if(dado == "D"){
          y <- y - 1
        }
        
      }
      
      if(x == 0 && y == 0){
        voltaOrigem <- voltaOrigem + 1
      }
      
    }
  }
  proporcao <- voltaOrigem/10000
  return(paste("Proporção de retorno de Link à origem: ", proporcao))
}

caminho_n(12)
caminho_n(7)

#--------------- Exercício 12 ---------------

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

#--------------- Exercício 13 ---------------

dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")
library(ggplot2)

#(a)

ggplot(data = dados, aes(x = Genero, fill = Genero))+
  geom_bar(color = "black")+
  labs(title = "Frequência de vítimas por gênero")+
  scale_fill_manual(values = c("Women" = "pink", "Men" = "lightblue"),labels = c("Women" = "Mulheres", "Men" = "Homens"))+
  theme_minimal()

#O gráfico mostra que, dentre as vítimas do assassino, a maioria são mulheres

#(b) Histograma
ggplot(data = dados, mapping = aes(x = Idade, fill = Genero))+
  geom_histogram(bins = 8, color = "black")+
  scale_fill_manual(values = c("Women" = "#bcbddc", "Men" = "#99d8c9"), labels = c("Women" = "Mulheres", "Men" = "Homens"))
theme_minimal()

#(c) boxplot da variável idade
ggplot(data = dados, mapping = aes(y = Idade)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Idade", y = "Idade") +
  theme_minimal()

#(d) Gráfico para representar o local da morte

ggplot(data = dados, aes(x = LocalDaMorte, fill = LocalDaMorte))+
  geom_bar(color = "black")+
  labs(title = "Frequência de mortes por local")+
  scale_fill_manual(values = c("Own home" = "#a6bddb", "Hospital" = "#ece2f0", "Nursing home" = "#1c9099"),labels = c("Own home" = "casa do paciente", "Hospital" = "hospital", "Nursing home" = "casa de repouso"))+
  theme_minimal()
#Analisando o gráfico, pode-se perceber que pouco mais de 200 mortes ocorreram dentro da casa do paciente, e menos de 50 mortes ocorreram ou no hospital ou na casa de repouso

#(e) Analise graficamente o ano da morte das vítimas
ggplot(data = dados, mapping = aes(x = AnoDaMorte))+
  geom_bar(fill = "#de2d26", color = "black")+
  theme_minimal()
# O intervalo de tempo em que houveram mais mortes foi de 1993-1998

#(f)
#De acordo com os gráficos acima, foi possível perceber alguns padrões na escolha das vítimas.
#Entre eles, podemos observar que: a maioria das suas vítimas são mulheres; a faixa etária das vítimas
# é a partir dos 40 anos, com maior índice de casos de pessoas entre 71 e 85 anos; dentre os três locais constatados de morte (hospital, casa de repouso e casa do paciente)
#o local mais escolhido para os crimes foi na casa do próprio paciente; e o intervalo de tempo em que os casos de morte foram
#mais evidentes foram na década de 90, mais especificamente, entre os anos 1993 e 1998, como constatado no gráfico da alternativa (e).



#--------------- Exercício 14 ---------------
