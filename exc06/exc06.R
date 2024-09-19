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
