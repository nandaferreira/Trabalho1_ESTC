#Use a função help do R para descobrir o funcionamento das funções rep e seq. Em seguida, utilize estas funções para resolver os seguintes itens: 

# a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências do número 2 
vetorA <- rep(c(2,4,6,8), times = 10)
vetorA

#(b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8
vetorB <- c(rep(c(2,4,6,8), times = 10), 2)
vetorB