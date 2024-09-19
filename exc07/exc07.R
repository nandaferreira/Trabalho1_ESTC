#Exercício 7


fibonacci <- function(n) {
  if(n < 3) {
    break
  }
  # n tem que ser maior ou igual a três para conseguir fazer a soma de n com os dois imediatemente anteriores
  
  #garanto que as duas primeiras posições serão sempre 1
  fib <- numeric(n)
  fib[1] <- 1
  fib[2] <- 1
  
  for(i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  
  return(fib) 
}

fibonacci(10) #exemplo dado





