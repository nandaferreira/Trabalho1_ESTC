#Questão 8: Michael Scott é gerente regional da empresa Dunder Mufflin. Para as festividades de fim de ano, Michael propôs aos 
#funcionários Dwight Schrute, Jim Halpert, Kevin Malone e Creed Bratton a realização de um amigo oculto entre eles. 
#Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa sortear ela mesma (Michael tira Michael, 
#por exemplo). Simule o sorteio do amigo oculto. Se ele deu certo, atribua o valor 1; caso contrário, atribua o valor 0 (zero). 
#Em seguida, replique este experimento cem mil vezes e calcule a proporção de vezes que o amigo oculto deu errado.

participantes <- c("Michael", "Dwight", "Kevin", "Jim", "Creed")

amigo_oculto <- function(){
  sorteio <- sample(participantes)
  if(any(sorteio == participantes)){
    return (0)
  }else{
    return(1)
  }
}

set.seed(150)
resultado <- replicate(100000, amigo_oculto())


head(resultado, 20)
proporcao_errado <- mean(resultado)
proporcao_errado
