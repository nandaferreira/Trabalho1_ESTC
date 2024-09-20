#Questão 14

#Letra (a)
dados <- read.table(file = "primatas.txt", header = TRUE, sep = ":")
dados

#Verificando a estrutura dos dados
str(dados)

#Resumo dos dados
summary(dados)

#Letra (b)
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

#Letra (c)
#Gráfico para comparar fêmeas e machos de bonobos 
bonobos <- treino %>% filter(especie == "bonobo")

ggplot(data = bonobos, mapping = aes(x = genero))+
  geom_bar(fill = "lightcoral")+
  labs(title = "Comparação de Fêmeas e Machos dos Bonobos", x = "Gênero", y = "Quantidade")+
  theme_minimal()

#Gráfico para comparar fêmeas e machos dos chimpanzés 
chimpazes <- treino %>% filter(especie == "chimpanze")

ggplot(data = chimpazes, mapping = aes(x = genero))+
  geom_bar(fill = "lightgreen")+
  labs(title = "Comparação de Fêmeas e Machos dos Bonobos", x = "Gênero", y = "Quantidade")+
  theme_minimal()

#Letra(d)
#Gráfico para comparar fêmeas dos bonobos e dos chimpanzés
femeas <- treino %>% filter(genero == "femea")

ggplot(data = femeas, mapping = aes(x = especie))+
  geom_bar(fill = "lightblue")+
  labs(title = "Comparação de Fêmeas dos Bonobos e Chimpanzés", x = "Espécie", y = "Quantidade")+
  theme_minimal()

# Gráfico para comparar machos dos bonobos e dos chimpanzés
machos <- treino %>% filter(genero == "macho")

ggplot(data = machos, mapping = aes(x = especie)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Comparação de Machos dos Bonobos e Chimpanzés", x = "Espécie", y = "Quantidade") +
  theme_minimal()

#Letra (f)
arvore <- rpart(especie ~ altura + peso + genero, data = treino, method = "class")

rpart.plot(arvore)

previsao <- predict(arvore, newdata = teste, type = "class")
acuracia <- mean(previsao == teste$especie)
print(paste("Acuracia do modelo de arvore de decisão:", round(acuracia * 100, 2),"%"))
