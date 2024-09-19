#------------Exercício 13------------

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
