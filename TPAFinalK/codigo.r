# PROBLEMA 1
# Se tiene un suceso para el cual la probabilidad de éxito es p.
# Se realizan intentos independientes de este suceso hasta obtener k éxitos
# consecutivos. Considere el proceso N_k que denota el número de ensayos
# necesarios para obtener k éxitos consecutivos.

# a. Simule un cierto número de trayectorias del proceso N_k considerando un
# valor de p < 0.5 e indique en cada caso cuántos realizaciones de la
# experiencia fueron necesarias hasta obtener k éxitos consecutivos. 
# (Elija un valor de k que satisfaga la siguiente desigualdad 1 < k < 5).

# b. A partir de los realizado en a) estime, de ser posible, la E(N_k).

# Simulacion del proceso N_k
proceso <- function(k) { # k es los de exitos esperados
  p <- 0.49 # probabilidad de exito, p < 0.5
  EXITO <- 1 
  FRACASO <- 0
  EM <- c(EXITO,FRACASO)
  
  N_k <- c() # guarda el resultado de cada experimento
  count <- 0 # contador de sucesos realizados
  exitos <- 0 # contador de sucesos exitosos consecutivos
  while (exitos < k) {
    i <- sample(EM, 1, replace = T, prob = c(p,1-p))
    count <- count + 1
    N_k <- c(N_k,i)
    if (i == EXITO) {
      exitos <- exitos + 1
    } else {
      exitos <- 0
    }
  }
  count # retorna el total de experimentos necestarios
  # hasta obtener k exitos, incluyendo este ultimo
}

# Apartado a)
{
  k <- 4 # Exitos esperados, 1 < k < 5
  n <- 1000 # cantidad de trayectorias de N_k a realizar
  trayectorias <- NULL # guarda el resultado de cada trayectoria del proceso
  i <- 0
  while (i < n) {
    i <- i + 1
    trayectorias <- c(trayectorias,proceso(k))
  }
  tabla_trayectorias <- table(trayectorias) # Agrupa por cantidad de tiradas
  tabla = data.frame(tabla_trayectorias, tabla_trayectorias/1000)
  tabla = tabla[,-3]
  tabla
}
# Observacion: Esta tabla genera una variable aletoria donde X es la cantidad
# de tiradas en un proceso del Problema 1 eligiendo p < 0.5 y 1 < k < 5.
boxplot(trayectorias, col = "green", main = "Cantidad de sucesos hasta obtener 4 éxitos consecutivos", yaxt="n")
axis(2, at = seq(0, 300, 20), las = 2)
summary(trayectorias)

# Apartado b) 
# Se asigna una probabilidad a los elementos de la tabla del apartado a)
# para luego calular la esperanza como sum(x_i*f(x_i)).
tabla_probabilidades <- data.frame(tabla_trayectorias/n)
probabilidades <- tabla_probabilidades[,"Freq"]
recorrido <- as.numeric(levels(tabla_probabilidades[,"trayectorias"]))
esperanza <- sum(probabilidades * recorrido)

# PROBLEMA 2
# a) 
pasos = 50
p = 0.8 # p > 0.7
x = c(0,1)
probabilities = c(1-p,p)
I_n = sample(x, pasos, replace = T, prob = probabilities)
D_n = 2 * I_n - 1

# b)
S_n = cumsum(D_n)
plot(1:pasos, S_n, type = "p", xlab = "Pasos", ylab = "Posición de la partícula", main="Simulación del proceso Sn")

# PROBLEMA 3
# a)
{
  k = 10 # apuesta inicial (estado inicial del proceso)
  s = 30 # monto en el que el jugador decide dejar de apostar
  evolucion = c(k)
  # distintas probabilidades de ganar una apuesta
  p1 = 0.25 # p < 0.5
  p2 = 0.5 # p = 0.5
  p3 = 0.75 # p > 0.5
  
  x = c(0,1) # posibles outcomes de J_n
  p = p3 # probabilidad elegida
  probabilities = c(1-p,p) # probabilidad de cada outcome
  
  while (k != 0 & k != s) { # calculo de J_n y luego de X_n
    J_n = sample(x, 1, prob = probabilities)
    k = k + 2 * J_n - 1
    evolucion = c(evolucion, k)
  }
  plot(0:(length(evolucion)-1), evolucion, xaxt = 'n', type = "p", xlab = "Número de juego", ylab = "Fortuna del jugador", main="Simulación del proceso con p=0.75")
  axis(side = 1, at = c(seq(0,length(evolucion),2)))
  evolucion
}

# b)
{
  s = 30 # monto en el que el jugador decide dejar de apostar
  p = 0.75 # probabilidad de ganar una apuesta
  x = c(0,1)
  probabilities = c(1-p,p)
  n = 10000
  contador = 0
  i = 0
  while (i < n) { #simula n trayectorias del capital del jugador
    k = 10 # apuesta inicial (estado inicial del proceso)
    evolucion = c(k)
    while (k != 0 & k != s) { # calculo de J_n y luego de X_n
      J_n = sample(x, 1, prob = probabilities)
      k = k + 2 * J_n - 1
      evolucion = c(evolucion, k)
    }
    if (k == 0) {
      contador = contador + 1
    }
    i = i + 1
  }
  print(contador)
  probabilidad_ruina = contador / n
  print(probabilidad_ruina)
}

# PROBLEMA 4

# dada una lista, calcula cuantas comparaciones realizaria el algoritmo quicksort al ordenarla
simulacion <- function(l) {
  # Dada una lista de números simula ordenarla para devolver
  # el numero de comparaciones que realizaría quick_sort
  n <- length(l)
  if (n < 2) {
    # Si esta ordenada hay 0 comparaciones
    0
  } else if (n == 2) {
    # Para dos elementos se requiere una sola comparacion
    1
  } else {
    # En este caso se hacen n-1 comparaciones
    i <- sample(1:n,1)
    xi <- l[i]
    S_l <- NULL # elementos menores
    S_h <- NULL # elementos mayores
    for (x in l) {
      if (x < xi) {
        S_l <- c(S_l,x)
      } else if (x > xi) {
        S_h <- c(S_h,x)
      }
    }
    n - 1 + simulacion(S_l) + simulacion(S_h)
  }
}

# Apartado a
lista_prueba <- sample(1:100, 7)
m <- 10000
n <- m
trayectorias <- NULL
while (n > 0) {
  n <- n - 1
  trayectorias <- c(trayectorias,simulacion(lista_prueba))
}
tabla_trayectorias <- table(trayectorias)
barplot(tabla_trayectorias, ylim = c(0,2500), xlab = "Cantidad de comparaciones (M_7)", ylab = "Cantidad de simulaciones", col = "lightblue", main="Frecuencia absoluta de M_7 en 10000 simulaciones")

tabla_probabilidades <- data.frame(tabla_trayectorias/m)
probabilidades <- tabla_probabilidades[,"Freq"]
recorrido <- as.numeric(levels(tabla_probabilidades[,"trayectorias"]))
esperanza <- sum(probabilidades * recorrido)

# PROBLEMA 5

P = matrix(c(0,0,0,0,1/2,1/2,0,
             1/3,0,1/3,0,0,1/3,0,
             0,0,0,1/2,0,1/2,0,
             0,0,0,0,0,1,0,
             1/4,0,0,1/4,0,1/4,1/4,
             1/2,1/2,0,0,0,0,0,
             1/6,1/6,1/6,1/6,1/6,1/6,0), nrow = 7, byrow = T,
           dimnames = list(c('a','b','c','d','e','f','g'),c('a','b','c','d','e','f','g')))
pi0 = matrix(c(1/7,1/7,1/7,1/7,1/7,1/7,1/7), nrow = 1, byrow = T)

"%^%" <- function(A,n) {
  if(n==1) A 
  else {
    B<-A
    for (i in (2:n)) {
      A<-A%*%B
    }
  }
  A 
}
P %^% 100 # matriz de transicion en 100 pasos

# b)
# simula una trayectoria de x pasos de un usuario en la red
trayectoria_red <- function(x) {
  pagina_actual = sample(1:7, prob = c(1/7,1/7,1/7,1/7,1/7,1/7,1/7), 1)
  trayectoria = c(pagina_actual)
  for (i in 1:x) {
    pagina_actual = sample(1:7, prob = P[pagina_actual,], 1)
    trayectoria = c(trayectoria, pagina_actual)
  }
  trayectoria
}

cien_pasos = trayectoria_red(100) # trayectoria
# grafico de la trayectoria marcada
plot(0:100, cien_pasos, yaxt = 'n', type = "b", ylab = "Página web", xlab = "Paso de la trayectoria", main = "Simulación de una trayectoria de 100 pasos")
axis(side = 2, at = 1:7, labels = as.character(c('a','b','c','d','e','f','g')))
# grafico de la frecuencia absoluta de visita de cada pagina
barplot(table(cien_pasos), names.arg = c('a','b','c','d','e','f','g'), ylim = c(0,35),xlab = "Página web", ylab = "Cantidad de veces que el usuario visitó el sitio", col = "pink", main = "Frecuencia absoluta de visitas a cada página\n web en un recorrido de 100 pasos.")

# c)
install.packages("markovchain")
library(markovchain)
mc = new("markovchain", transitionMatrix = P, states = c('a','b','c','d','e','f','g'),name = "ejercicio5")
plot(mc)
is.irreducible(mc)
period(mc)
steadyStates(mc)
steadyStates(mc) %*% P

# PROBLEMA 6

# Parametros:
#   - lambda: parametro de la distribucion de Poisson.
#   - desde: tiempo de inicio de los eventos.
#   - hasta: tiempo de finalizacion de los eventos.
# Genera un numero aleatorio n de la distribucion de Poisson con parametro
# lambda * t (t es el tiempo total, osea hasta - desde) y luego n valores 
# de la distribución uniforme entre los parametros dados y los devuelve
# ordenados de menor a mayor. Estos son los instantes donde ocurre un evento.
simulacion <- function(lambda, desde, hasta) {
  n <- rpois(1,lambda*(hasta-desde))
  x <- runif(n, desde, hasta)
  sort(x)
}
if (FALSE) {
  "
  EXPLICACION TEÓRICA DE LA FUNCION:
  El número aleatorio de la distribución de Poisson con parámetro 
  λ*t nos devuelve la cantidad de eventos que ocurrieron en el tiempo
  t en un Proceso de Poisson a una tasa λ.
  Luego las probabilidades de que un evento ocurra a un momento determinado
  es la misma para cada evento, por lo tanto se generan números aleatorios
  de la distribución uniforme hasta t.
  Para graficar se ordenan los valores obtenidos antes.
  "
}

# Apartado a
# El aeropuerto recibe vuelos comenzando a las 2 am a una tasa de 3
# aterriizajes por hora, de acuerdo a un Proceso Poisson. Simule el
# comportamiento de dicho proceso durante 24 horas y grafique la trayectoria
# obtenida.
apartado_a <- function(l, desde, hasta) {
  x <- simulacion(l, desde, hasta)
  y <- 1:length(x)
  x <- c(0,x)
  y <- c(0,y)
  par(mar=c(2,2.5,1.5,1))
  plot(x, y
       , type="s"
       , cex=0.5
       , axes=FALSE, xlab="", ylab="", xlim=c(0,hasta+1)
  )
  axis(1, at=c(seq(0,hasta-2,by=2),hasta)
       , las=1, padj=-2.75, cex.axis=0.6, tck=-0.01)
  axis(2, at=seq(0,max(y),by=l)
       , las=1, hadj=0.5, cex.axis=0.6, tck=-0.01, pos=-0.5)
  mtext("Instantes donde ocurre un evento"
        , side=1, line=1, font=2, cex=0.9)
  mtext("Numero de eventos"
        , side=2, line=1.5, font=2, cex=0.9)
  mtext(paste("Proceso Poisson con parametro λ =",l,"hasta t =",hasta)
        , side=3, line=0, font=2, cex=0.9)
  segments(max(x),max(y),hasta+1,max(y))
  abline(v=hasta,lwd=1,lty=3)
}
apartado_a(3, 2, 24)

# Apartado b
# Simule una trayectoria muestral durante un intervalo de tiempo
# suficientemente largo [0,T], Calcule los tiempos entre llegadas.
# Represente gráficamente los tiempos observados entre llegadas a través
# de un histograma y comente cuál es el modelo apropiado para para describir
# esa variable.
apartado_b <- function () {
  t <- 4000 # Simulacion hasta un T suficientemente grande
  tiempos_de_llegadas <- simulacion(3,0,t)
  tiempos_entre_llegadas <- NULL
  for (i in 2:length(tiempos_de_llegadas)) {
    tiempos_entre_llegadas <- c(tiempos_entre_llegadas, tiempos_de_llegadas[i]-tiempos_de_llegadas[i-1])
  }
  topex <- max(tiempos_entre_llegadas)
  x <- seq(0, topex, by = 0.25)
  if (topex > max(x)) x <- c(x, max(x)+0.25)
  y <- data.frame(table(cut(tiempos_entre_llegadas,breaks = x)))[,"Freq"]
  topey <- max(y)
  par(mar=c(3,3.5,2,1))
  hist(tiempos_entre_llegadas
       , breaks = x
       , xlim = c(0,topex)
       , ylim = c(0,topey)
       , main = paste("Tiempos entre arribos en [0,",t,"]", sep = "")
       , xlab = "", ylab = ""
       , axes = FALSE
       , col = "yellow"
  )
  #lines(x[1:length(x)-1] + 0.25/2, y, col = "red", lwd = 2)
  axis(1, at=x, las=1, padj=-1.5, cex.axis=0.75, tck=-0.01)
  axis(2, at=seq(0,topey,by=200), las=1, hadj=0.5, cex.axis=0.75, tck=-0.01)
  mtext("Intervalos de tiempos entre-arribos en horas", side=1, line=1.5, font=2, cex=1)
  mtext("Cantidad de arribos", side=2, line=2, font=2, cex=1)
}
apartado_b()


# PROBLEMA 7

P = matrix(c(0,0,0,0,1,
             0,8/13,3/13,1/13,1/13,
             1/16,3/16,3/8,1/4,1/8,
             0,1/11,4/11,5/11,1/11,
             0,1/8,1/2,1/8,1/4), nrow = 5, byrow = T,
           dimnames = list(c("80","135","139","445","NA"),c("80","135","139","445","NA")))
pi0 = matrix(c(0,0,0,0,1), nrow = 1, byrow = T)

# Apartado a
pi0 %*% (P %*% P %*% P)


library(markovchain)
mc = new("markovchain", transitionMatrix = P, states = c("80","135","139","445","NA"),name = "ejercicio7")
plot(mc)
is.irreducible(mc)
period(mc)
steadyStates(mc)
steadyStates(mc) %*% P
