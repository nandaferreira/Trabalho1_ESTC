#Exercício 5 

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

