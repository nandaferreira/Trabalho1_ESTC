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

#================================ Letra (E)========================================================:

# Analise:Após a execução do código, o gráfico "Quantidade de Espécies de Bonobos e Chimpanzés" mostra que a quantidade de indivíduos de cada espécie é bastante próxima. Já o gráfico "Frequência de Machos e Fêmeas por Espécie" revela uma leve diferença na quantidade de fêmeas em relação aos machos na espécie Bonobo. Em contrapartida, na espécie de chimpanzés, a proporção entre machos e fêmeas é praticamente igual.

# Analisando os Resultados dos Bonobos: No gráfico "Distribuição de Altura por Gênero nos Bonobos", observa-se que a altura dos machos é ligeiramente maior do que a das fêmeas, com os machos variando entre 130 cm e 132 cm, enquanto as fêmeas variam entre 125 cm e pouco mais de 128 cm. No gráfico "Distribuição de Peso por Gênero nos Bonobos", nota-se uma diferença mais significativa: o peso dos machos varia entre 40 kg e pouco mais de 46 kg, enquanto as fêmeas apresentam um peso entre 30 kg e 35 kg.

# Analisando os Resultados dos Chimpanzés:No gráfico "Distribuição de Altura por Gênero nos Chimpanzés", observa-se uma diferença considerável entre machos e fêmeas. A altura dos machos varia entre 132 cm e pouco mais de 135 cm, enquanto as fêmeas têm altura entre 120 cm e 125 cm. No gráfico "Distribuição de Peso por Gênero nos Chimpanzés", a diferença também é bastante significativa: os machos variam de 55 kg a pouco mais de 60 kg, enquanto as fêmeas variam entre 35 kg e pouco mais de 45 kg.45kg. 

# Analisando as Fêmeas entre as Espécies de Bonobos e Chimpanzés: O gráfico de "Distribuição de Altura entre Fêmeas dos Bonobos e Chimpanzés" mostra que a altura das fêmeas da espécie de bonobos é consideravelmente maior do que a das fêmeas da espécie de chimpanzés. A altura das fêmeas bonobos varia entre 126 cm e 129 cm, enquanto a altura das fêmeas chimpanzés varia entre 123 cm e pouco mais de 125 cm. Em relação ao peso, as fêmeas da espécie de chimpanzés apresentam uma variação maior em comparação com as fêmeas dos bonobos. O peso das fêmeas chimpanzés varia entre 40 kg e 45 kg, enquanto o peso das fêmeas bonobos varia entre pouco mais de 30 kg e 35 kg. 


#Analisando os Machos entre as Espécies de Bonobos e Chimpanzés:O gráfico de "Distribuição de Altura entre Machos dos Bonobos e Chimpanzés" mostra que a altura dos machos da espécie de chimpanzés é consideravelmente maior do que a dos machos da espécie de bonobos. A altura dos machos chimpanzés varia entre pouco menos de 135 cm e pouco mais de 135 cm, enquanto a altura dos machos bonobos varia entre pouco mais de 130 cm e 132 cm. Em relação ao peso, os machos da espécie de chimpanzés apresentam uma variação maior em comparação com os machos dos bonobos. O peso dos machos chimpanzés varia entre 55 kg e 65 kg, enquanto o peso dos machos bonobos varia entre pouco mais de 40 kg e pouco mais de 45 kg.

