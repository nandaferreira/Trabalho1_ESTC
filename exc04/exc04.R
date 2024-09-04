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
