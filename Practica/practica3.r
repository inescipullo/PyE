# EJERCICIO 1 

# a) Experiencia de tirar una moneda equilibrada

# si le asigno 1 al valor que me interesa analizar y 0 al resto, 
# calcular la proporcion es  equivalente a calcular el promedio
# quiero analizar cuantas veces sale cara
cara <- c(0,1)
lanzamientos <- c(10,100,1000,10000,100000,1000000)
frec_relativa <- vector(mode="numeric", length(lanzamientos))
puntos <- cbind(lanzamientos, frec_relativa)

# Hago las tiradas y calculo sus frecuencias relativas
for (i in 1:length(lanzamientos)) {
  puntos [i,2] <- (mean (sample(cara, puntos[i,1], replace=T)))
}
puntos

# Grafico
plot(puntos,type = "p",ylab = "frecuencias relativas",)
abline(0.5,0)


# b) Experiencia de tirar un dado equilibrado
cara_dado <- c(1,2,3,4,5,6)
lanzamientos <- c(10,100,1000,10000,100000,1000000)
frec1 <- vector(mode = "numeric", length(lanzamientos))
frec2 <- vector(mode = "numeric", length(lanzamientos))
frec3 <- vector(mode = "numeric", length(lanzamientos))
frec4 <- vector(mode = "numeric", length(lanzamientos))
frec5 <- vector(mode = "numeric", length(lanzamientos))
frec6 <- vector(mode = "numeric", length(lanzamientos))
puntos <- cbind(lanzamientos,frec1,frec2,frec3,frec4,frec5,frec6)

# Hago todas las tiradas
for (i in 1:length(lanzamientos)) {
  tabla = table(sample(cara_dado, puntos[i,1], replace = T))
  puntos[i,2:7] <- tabla[1:6]
}
puntos

# Calculo las frecuencias relativas
for (i in 1:length(lanzamientos))
  puntos[i,2:7] <- puntos[i,2:7]/puntos[i,1]
puntos

par(mfrow=c(2,3)) # para que se puedan visualizar los 6 graficos
# Grafico
for (i in 2:7) {
  plot(puntos[,1],puntos[,i],xlab = "lanzamientos", ylab = "frecuencias relativas")
  abline(0.1666666, 0)
}
par(mfrow=c(1,1))



# EJERCICIO 2
# distribución de la suma de los números que salen al tirar 4 dados para una muestra de tamaño 10000
cara_dado <- c(1,2,3,4,5,6)
lanzamientos = 10000
resultados = vector(mode = "numeric",lanzamientos)

for (i in 1:lanzamientos) {
  resultados[i] = sum(sample(cara_dado,4,replace = T)) # tiro 4 dados y calculo su suma
}

plot(table(resultados),xlab = "Suma de los 4 dados",ylab = "Cantidad de veces que salió")


# EJERCICIO 3
# urna con 3 bolas blancas y 5 bolas negras
urna = c(0,0,0,1,1,1,1,1)

# a) Se observa la extracción de una bola
if (sample(urna,1) == 0) {
  print("Observamos una bola blanca")
} else {
  print("Observamos una bola negra")
}

# b) Se observan 8 extracciones con reposición
plot(factor(sample(urna,8,replace = T),labels = c("Blanco","Negro")),xlab = "Color",ylab = "Cantidad de bolas",ylim = c(0,8),main = "Se observan 8 extracciones con reposición.")

# c) Se observa la cantidad de bolas negras que salen al extraer 30 bolas (con reposición) 
# Este procedimiento se repite 10000 veces.
resultado = vector(mode = "numeric",10000)
for (i in 1:10000) {
  resultado[i] = sum(sample(urna,30,replace = T))
}

plot(table(resultado),xlab = "Cantidad de bolas negras observadas",ylab = "Cantidad de veces que ocurrió",main = "Se observa la cantidad de bolas negras que salen al extraer\n 30 bolas (con reposición). Esto se repite 10000 veces.")




