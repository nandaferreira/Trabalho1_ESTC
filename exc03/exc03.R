#Exercício 3

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
