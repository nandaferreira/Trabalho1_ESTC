#Questão 14

#=================================== Letra (a)=======================================
dados <- read.table(file = "primatas.txt", header = TRUE, sep = ":")
dados
dados$especie <- as.factor(dados$especie)
dados$genero <- as.factor(dados$genero)

#Verificando a estrutura dos dados
str(dados)

#Resumo dos dados
summary(dados)

#=================================== Letra (b)=======================================
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



#=================================== Letra (C)=======================================

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


#=================================== Letra (d)=======================================
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


#=================================== Letra (f)=======================================
arvore <- rpart(especie ~ altura + peso + genero, data = treino, method = "class")

rpart.plot(arvore)

previsao <- predict(arvore, newdata = teste, type = "class")
acuracia <- mean(previsao == teste$especie)
print(paste("Acuracia do modelo de arvore de decisão:", round(acuracia * 100, 2),"%"))
