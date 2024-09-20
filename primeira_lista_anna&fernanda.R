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

# a)
vetorA <- rep(c(2,4,6,8), times = 10)
vetorA

#(b) 
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
#Questão 6: Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada replicação, guarde o número de lançamentos num vetor chamado quantidades. Por fim, calcule a média de quantidades. Interprete o resultado obtido

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


#Replicando o experimento dez mul vezes
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

#Interpretação: O código realiza um experimento de probabilidade que simula o lançamento de um dado com o objetivo de determinar 
# quantos lançamentos são necessários até que o número 4 apareça duas vezes. Esse experimento é repetido dez mil vezes, e a média 
# do número de lançamentos necessários para atingir essa condição de parada é calculada. O resultado obtido para a média representa 
# a expectativa do número de lançamentos que devemos esperar, em média, até que o número 4 apareça duas vezes. 
# Esse valor nos dá uma ideia da quantidade típica de lançamentos necessária para alcançar o objetivo definido.


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
proporcao <- mean(resultado == 1)
proporcao

#OBS:
#sum(resultado)/length(resultado) #mean de resultado porque ele guarda valores em 0s e 1s, então quando soma o 0 é "desconsiderado", ou seja, nulo na soma


#--------------- Exercício 10 ---------------

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
proporcao

#Conclusão: a cada 10.000 jogos, a proporção de vezes que o Link volta para o ponto de origem é de, aproximadamente, 7,78% (0.0778).

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

#Interpretação: O código simula um jogo entre Steven e Garnit, onde cada um escolhe uma sequência de três lançamentos de moedas. 
#O objetivo é descobrir quantas vezes cada um vence em dez mil rodadas do jogo. Ao final, calculamos a média de vitórias de cada 
#jogador. Como o resultado depende da sorte, a média pode variar a cada execução, mas deve se aproximar da probabilidade real de 
#cada jogador vencer. Isso ajuda a entender como a aleatoriedade influencia os resultados e como as escolhas de cada jogador 
#impactam suas chances de ganhar!

#--------------- Exercício 13 ---------------

dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")
library(ggplot2)

#(a)
ggplot(data = dados, aes(x = Genero, fill = Genero))+
  geom_bar(color = "black")+
  labs(title = "Frequência de vítimas por gênero")+
  scale_fill_manual(values = c("Women" = "pink", "Men" = "lightblue"),labels = c("Women" = "Mulheres", "Men" = "Homens"))+
  theme_minimal()

#Conclusão: o gráfico mostra que, dentre as vítimas do assassino, a maioria são mulheres

#(b) Histograma
ggplot(data = dados, mapping = aes(x = Idade, fill = Genero))+
  geom_histogram(bins = 8, color = "black")+
  scale_fill_manual(values = c("Women" = "#bcbddc", "Men" = "#99d8c9"), labels = c("Women" = "Mulheres", "Men" = "Homens"))
theme_minimal()

#Conclusão: pelo histograma é possível observar que o maior número de mortes, em relação a idade, foi de pessoas que tem entre 70 a 85 anos

#(c) boxplot da variável idade
ggplot(data = dados, mapping = aes(y = Idade)) +
  geom_boxplot(fill = "#c994c7", color = "black") +
  labs(title = "Idade", y = "Idade") +
  theme_minimal()

#(d) Gráfico para representar o local da morte

ggplot(data = dados, aes(x = LocalDaMorte, fill = LocalDaMorte))+
  geom_bar(color = "black")+
  labs(title = "Frequência de mortes por local")+
  scale_fill_manual(values = c("Own home" = "#a6bddb", "Hospital" = "#ece2f0", "Nursing home" = "#1c9099"),labels = c("Own home" = "casa do paciente", "Hospital" = "hospital", "Nursing home" = "casa de repouso"))+
  theme_minimal()

#Conclusão: analisando o gráfico, pode-se perceber que pouco mais de 200 mortes ocorreram dentro da casa do paciente, e menos de 50 mortes ocorreram ou no hospital ou na casa de repouso

#(e) Analise graficamente o ano da morte das vítimas
ggplot(data = dados, mapping = aes(x = AnoDaMorte))+
  geom_bar(fill = "#de2d26", color = "black")+
  theme_minimal()

#Conclusão: o gráfico plotado mostra que o intervalo de tempo no qual houveram mais mortes foi de 1993-1998.
# A primeira morte ocorreu no ano de 1975. 1997 foi o ano com maior número de mortes.

#(f) Conclusão final
#De acordo com os gráficos acima, foi possível perceber alguns padrões na escolha das vítimas.
#Entre eles, podemos observar que: a maioria das suas vítimas são mulheres; a faixa etária das vítimas
# é a partir dos 40 anos, com maior índice de casos de pessoas entre 71 e 85 anos; dentre os três locais constatados de morte (hospital, casa de repouso e casa do paciente)
#o local mais escolhido para os crimes foi na casa do próprio paciente; e o intervalo de tempo em que os casos de morte foram
#mais evidentes foram na década de 90, mais especificamente, entre os anos 1993 e 1998, como constatado no gráfico da alternativa (e).



#--------------- Exercício 14 ---------------

#(a)
dados <- read.table(file = "primatas.txt", header = TRUE, sep = ":")
dados
dados$especie <- as.factor(dados$especie)
dados$genero <- as.factor(dados$genero)

#Verificando a estrutura dos dados
str(dados)

#Resumo dos dados
summary(dados)

#(b)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(1711)
dados <- dados[sample(nrow(dados)),]

n <- round(0.8 * nrow(dados))
n

treino <- dados[1:n,]
teste <- dados[-(1:n),]

treino$especie <- as.factor(treino$especie)
treino$genero <- as.factor(treino$genero)

teste$especie <- as.factor(teste$especie)
teste$genero <- as.factor(teste$genero)

#Gráfico que mostra a quantidade espécies de bonobos e chipanzes 
ggplot(data = treino, mapping = aes(x = especie))+
  geom_bar(fill = "lightpink")+
  labs(title = "Quantidade de Espécies de Bonobos e Chimpazés", x = "Espécies", y = "Quantidade") + 
  theme_minimal()

#Gráfico que mostra a frequência de machos e fêmeas de cada espécie
ggplot(data = treino, mapping = aes(x = especie, fill = genero))+
  geom_bar(position = "dodge")+
  labs(title = "Frequência de Machos e Fêmeas por Espécie", x = "Espécies", y = "Frequência") + 
  theme_minimal()+
  scale_fill_manual(values = c("lightblue", "lightcoral"))+
  theme(legend.title = element_blank())



#(c)

# Gráfico para comparar a altura de machos e fêmeas dos bonobos
ggplot(data = treino %>% filter(especie == "bonobo"), mapping = aes(x = genero, y = altura, fill = genero)) +
  geom_boxplot() +
  labs(title = "Distribuição de Altura por Gênero nos Bonobos", x = "Gênero", y = "Altura") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme(legend.title = element_blank())

# Gráfico para comparar o peso de machos e fêmeas dos bonobos
ggplot(data = treino %>% filter(especie == "bonobo"), mapping = aes(x = genero, y = peso, fill = genero)) +
  geom_boxplot() +
  labs(title = "Distribuição de Peso por Gênero nos Bonobos", x = "Gênero", y = "Peso") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme(legend.title = element_blank())

# Gráfico para comparar a altura de machos e fêmeas dos chimpanzés
ggplot(data = treino %>% filter(especie == "chimpanze"), mapping = aes(x = genero, y = altura, fill = genero)) +
  geom_boxplot() +
  labs(title = "Distribuição de Altura por Gênero nos Chimpanzés", x = "Gênero", y = "Altura") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightcoral")) +
  theme(legend.title = element_blank())

# Gráfico para comparar o peso de machos e fêmeas dos chimpanzés
ggplot(data = treino %>% filter(especie == "chimpanze"), mapping = aes(x = genero, y = peso, fill = genero)) +
  geom_boxplot() +
  labs(title = "Distribuição de Peso por Gênero nos Chimpanzés", x = "Gênero", y = "Peso") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightcoral")) +
  theme(legend.title = element_blank())


#(d)
femeas <- treino %>% filter(genero == "femea")

# Gráfico para comparar a altura das fêmeas dos bonobos e dos chimpanzés
ggplot(data = femeas, mapping = aes(x = especie, y = altura, fill = especie)) +
  geom_boxplot() +
  labs(title = "Distribuição de Altura entre Fêmeas dos Bonobos e Chimpanzés", 
       x = "Espécie", y = "Altura") +
  theme_minimal() +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme(legend.position = "none") 

# Gráfico de boxplot para comparar o peso das fêmeas dos bonobos e dos chimpanzés
ggplot(data = femeas, mapping = aes(x = especie, y = peso, fill = especie)) +
  geom_boxplot() +
  labs(title = "Distribuição de Peso entre Fêmeas dos Bonobos e Chimpanzés", 
       x = "Espécie", y = "Peso") +
  theme_minimal() +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme(legend.position = "none")

# Gráfico para comparar machos dos bonobos e dos chimpanzés
machos <- treino %>% filter(genero == "macho")

# Gráfico de boxplot para comparar a altura dos machos dos bonobos e dos chimpanzés
ggplot(data = machos, mapping = aes(x = especie, y = altura, fill = especie)) +
  geom_boxplot() +
  labs(title = "Distribuição de Altura entre Machos dos Bonobos e Chimpanzés", 
       x = "Espécie", y = "Altura") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightyellow")) +
  theme(legend.position = "none")

# Gráfico de boxplot para comparar o peso dos machos dos bonobos e dos chimpanzés
ggplot(data = machos, mapping = aes(x = especie, y = peso, fill = especie)) +
  geom_boxplot() +
  labs(title = "Distribuição de Peso entre Machos dos Bonobos e Chimpanzés", 
       x = "Espécie", y = "Peso") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightyellow")) +
  theme(legend.position = "none")


#(f)
arvore <- rpart(especie ~ altura + peso + genero, data = treino, method = "class")

rpart.plot(arvore)

previsao <- predict(arvore, newdata = teste, type = "class")
acuracia <- mean(previsao == teste$especie)
print(paste("Acuracia do modelo de arvore de decisão:", round(acuracia * 100, 2),"%"))

#(e)

# Analise:Após a execução do código, o gráfico "Quantidade de Espécies de Bonobos e Chimpanzés" mostra que a quantidade de indivíduos de cada espécie é bastante próxima. Já o gráfico "Frequência de Machos e Fêmeas por Espécie" revela uma leve diferença na quantidade de fêmeas em relação aos machos na espécie Bonobo. Em contrapartida, na espécie de chimpanzés, a proporção entre machos e fêmeas é praticamente igual.

# Analisando os Resultados dos Bonobos: No gráfico "Distribuição de Altura por Gênero nos Bonobos", observa-se que a altura dos machos é ligeiramente maior do que a das fêmeas, com os machos variando entre 130 cm e 132 cm, enquanto as fêmeas variam entre 125 cm e pouco mais de 128 cm. No gráfico "Distribuição de Peso por Gênero nos Bonobos", nota-se uma diferença mais significativa: o peso dos machos varia entre 40 kg e pouco mais de 46 kg, enquanto as fêmeas apresentam um peso entre 30 kg e 35 kg.

# Analisando os Resultados dos Chimpanzés:No gráfico "Distribuição de Altura por Gênero nos Chimpanzés", observa-se uma diferença considerável entre machos e fêmeas. A altura dos machos varia entre 132 cm e pouco mais de 135 cm, enquanto as fêmeas têm altura entre 120 cm e 125 cm. No gráfico "Distribuição de Peso por Gênero nos Chimpanzés", a diferença também é bastante significativa: os machos variam de 55 kg a pouco mais de 60 kg, enquanto as fêmeas variam entre 35 kg e pouco mais de 45 kg.45kg. 

# Analisando as Fêmeas entre as Espécies de Bonobos e Chimpanzés: O gráfico de "Distribuição de Altura entre Fêmeas dos Bonobos e Chimpanzés" mostra que a altura das fêmeas da espécie de bonobos é consideravelmente maior do que a das fêmeas da espécie de chimpanzés. A altura das fêmeas bonobos varia entre 126 cm e 129 cm, enquanto a altura das fêmeas chimpanzés varia entre 123 cm e pouco mais de 125 cm. Em relação ao peso, as fêmeas da espécie de chimpanzés apresentam uma variação maior em comparação com as fêmeas dos bonobos. O peso das fêmeas chimpanzés varia entre 40 kg e 45 kg, enquanto o peso das fêmeas bonobos varia entre pouco mais de 30 kg e 35 kg. 

#Analisando os Machos entre as Espécies de Bonobos e Chimpanzés:O gráfico de "Distribuição de Altura entre Machos dos Bonobos e Chimpanzés" mostra que a altura dos machos da espécie de chimpanzés é consideravelmente maior do que a dos machos da espécie de bonobos. A altura dos machos chimpanzés varia entre pouco menos de 135 cm e pouco mais de 135 cm, enquanto a altura dos machos bonobos varia entre pouco mais de 130 cm e 132 cm. Em relação ao peso, os machos da espécie de chimpanzés apresentam uma variação maior em comparação com os machos dos bonobos. O peso dos machos chimpanzés varia entre 55 kg e 65 kg, enquanto o peso dos machos bonobos varia entre pouco mais de 40 kg e pouco mais de 45 kg.
