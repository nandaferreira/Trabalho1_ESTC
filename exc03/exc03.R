#Exercício 3

#(a)Somatório índice(n) = 20, m = 30


somatorio1 <- 0
resultado1 <- 0
for (n in 20:30){
  somatorio1 <- ((n^2) + 4*n)
  resultado1 <- sum(somatorio1)
  
}
print(resultado1)

#(b) Somatório índice(n) = 10, m = 20 **REVER

somatorio2 <- 0
resultado2 <- 0

for(k in 10:20){
  somatorio2 <-(((3^k)/k) +((2^k)/(k^2)))
}
somatorio2