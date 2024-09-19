#(a)

#coordenadas
x <- 0
y <- 0
direcoes <- c("R", "L", "U", "D")

for (j in 1:8){
  dado <- sample(x = direcoes, size = 1)
  
  if(dado == "U"){
    y <- y + 1
  }
  if(dado == "R"){
    x <- x + 1
  }
  if(dado == "L"){
    x <- x - 1
  }
  
  if(dado == "D"){
    y <- y - 1
  }
  
}
coordenadas <- c(x,y)


#(b) 10 mil vezes

caminho <- function(){
  x <- 0
  y <- 0
  direcoes <- c("R", "L", "U", "D")
  
  for (j in 1:8){
    dado <- sample(x = direcoes, size = 1)
    
    if(dado == "U"){
      y <- y + 1
    }
    if(dado == "R"){
      x <- x + 1
    }
    if(dado == "L"){
      x <- x - 1
    }
    
    if(dado == "D"){
      y <- y - 1
    }
    
  }
  coordenadas <- c(x,y)
  return(coordenadas)
}

voltaOrigem <- 0

for(i in 1:10000){
  coord<-caminho()
  if(coord[1] == 0 && coord[2] == 0){
    voltaOrigem <- voltaOrigem + 1
  }

}
voltaOrigem

proporcao <- voltaOrigem/10000 
# A cada 10.000 jogos, a proporção de vezes que o Link volta para o ponto de origem é de, 
# aproximadamente, 7,78% (0.0778).

#(c)

caminho_n <- function(n){
  if(n%%2 != 0){
    return("Impossível retornar à origem depois de um número ímpar de passos")
  }else{
    for(i in 1:10000){
      x <- 0
      y <- 0
      direcoes <- c("R", "L", "U", "D")
    
      for (j in 1:n){
        dado <- sample(x = direcoes, size = 1)
        
        if(dado == "U"){
          y <- y + 1
        }
        if(dado == "R"){
          x <- x + 1
        }
        if(dado == "L"){
          x <- x - 1
        }
        
        if(dado == "D"){
          y <- y - 1
        }
        
      }

      if(x == 0 && y == 0){
        voltaOrigem <- voltaOrigem + 1
      }
      
    }
  }
  proporcao <- voltaOrigem/10000
  return(paste("Proporção de retorno de Link à origem: ", proporcao))
}

caminho_n(12)
caminho_n(7)