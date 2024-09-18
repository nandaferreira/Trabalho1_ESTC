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
  return(length(vetorDado))
}

vetor<-c(1,3,5,6,8,9,7,7,8,7)

sum(vetor==7)
#Quando eu pergunto se vetor == 4, o R retorna "true" nas posições em que o número aparece
#se eu fizer o somatório ele me retorna quantas vezes o número apareceu, então

