setwd("/Users/inescipullo/Documents2.0/Probabilidad\ y\ Estadistica/PyE/TP1")
setwd("/home/administrador/Documentos/PyE")

data = read.table("usuarios9.csv", header = TRUE, sep = ",")
data2 = read.table("recorridos9.csv", header = TRUE, sep = ",")
attach(data)
attach(data2)


# ANALISIS UNIVARIADO


# tabla de frecuencias para GENERO
# es variable cualitativa
# grafico de torta
frec_abs_genero = table(data$genero_usuario)
frec_rel_genero = round(frec_abs_genero / sum(frec_abs_genero), 4)
tabla_genero = cbind(frec_abs_genero,frec_rel_genero)
attributes(tabla_genero)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
total = c(sum(frec_abs_genero),sum(frec_rel_genero))
tabla_genero = rbind(tabla_genero,total)
attributes(tabla_genero)$dimnames[[1]] = c("Femenino","Masculino","Otro","Total")
tabla_genero

#grafico de sectores para GENERO
x = table(data$genero_usuario)
title = "Género de los Usuarios \n del Sistema EcoBicis de CABA"
porcentaje = round(x / sum(x) * 100, 2)
options = c("Femenino", "Masculino", "Otro")
labels = paste(options, porcentaje, c("%"), sep = " ")
grafico_genero = pie(x, labels = labels, clockwise = TRUE, main = title, col = c("#ffff66","#99ff66","#ff6666"))

# moda = FEMENINO



# tabla de frecuencias para EDAD
# grafico histograma
breaks_edad = seq(18,68,5)
edad_intervalos = cut(data$edad_usuario, breaks = breaks_edad, right = FALSE)
frec_abs_edad = table(edad_intervalos) 
frec_rel_edad = round(frec_abs_edad / sum(frec_abs_edad),4)
frec_abs_ac_edad = cumsum(frec_abs_edad)
frec_rel_ac_edad = cumsum(frec_rel_edad)
tabla_edad = cbind(frec_abs_edad, frec_rel_edad, frec_abs_ac_edad,frec_rel_ac_edad)
attributes(tabla_edad)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
#NA_ = c(1,1/100,sum(frec_abs_edad)+1,sum(frec_rel_edad)+1/100)
total = c(sum(frec_abs_edad),sum(frec_rel_edad),NA,NA)
tabla_edad = rbind(tabla_edad,total)
tabla_edad

# grafico de histograma para EDAD
title2 = "Edad de los Usuarios del Servicio EcoBici de CABA"
hist(data$edad_usuario,border = "black",col = "lightblue", xaxt='n',ylim = c(0,30),breaks = breaks_edad,xlab = "Edad (en años)",ylab = "Cantidad de Usuarios",main = title2,)
axis(side = 1, at = breaks_edad)

# poligono de frecuencia y poligono acumulativo para EDAD
breaks_edad2 = seq(13,73,5)
xy = table(cut(data$edad_usuario,breaks = breaks_edad2,right = FALSE))
xy = round(xy/sum(xy),4)
# poligono de frecuencia:
plot(xy,type = "l",main = title2,ylab = "Frecuencia Relativa",xlab = "Edad (en años)",ylim = c(0,0.3))
grid()
# poligono acumulativo:
xy2 = as.table(cumsum(xy))
plot(xy2,type = "l",main = title2,ylab = "Frecuencia Relativa Acumulada",xlab = "Edad (en años)",ylim = c(0,1))
grid()

# Analisis de Medidas Descriptivas EDAD
round(summary(data$edad_usuario),4)
rango_intercuartil = 37 - 25
desvio_estandar = round(sd(data$edad_usuario,na.rm = T),4)
moda = "[28,33)"



# tabla de frecuencias para DIA
# grafico de barras
# ver de ponerlo en orden
frec_abs_dia = table(data2$dia) 
frec_abs_dia <- frec_abs_dia[c(1,3,4,5,2,7,6)]
frec_rel_dia = round(frec_abs_dia / sum(frec_abs_dia), 2)
frec_abs_ac_dia = cumsum(frec_abs_dia)
frec_rel_ac_dia = round(frec_abs_ac_dia / sum(frec_abs_dia), 2)
tabla_dia = cbind(frec_abs_dia,frec_rel_dia)
attributes(tabla_dia)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
rbind()
tabla_dia

# grafico de barras para DIA
barplot(frec_abs_dia,xlab = "Días de la Semana",ylab = "Cantidad",ylim = c(0,70),col = "lightgreen",main = "Cantidad de bicicletas públicas usadas por día")

# moda = SABADO



install.packages("qcc")
library(qcc)
# tabla de frecuencias DIRECCION ESTACION DE ORIGEN
fa_direc_or = table(data2$direccion_estacion_origen)
fr_direc_or = round(fa_direc_or/sum(fa_direc_or),4)
tabla_direc_or = cbind(fa_direc_or,fr_direc_or)
attributes(tabla_direc_or)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
TOTAL = c(sum(fa_direc_or),sum(fr_direc_or))
tabla_direc_or = rbind(tabla_direc_or,TOTAL)
#tabla_direc_or_10 = as.table(order(tabla_direc_or,-fa_direc_or))
#  tabla_direc_or[order(-fa_direc_or,fr_direc_or)]
#  as.table(fa_direc_or[c(122,24,43,32,47,15)])

# Diagrama de Pareto, cantidad de Estaciones de Origen de acuerda a su frecuencia absoluta,
# el Principio de Pareto que se visualiza es que la mayoria de las estaciones son poco utilizadas 
# como estacion de origen.
title = "Estaciones de EcoBicis de CABA según la cantidad de veces\n de que fueron utilizadas como estación de origen."
pareto.chart(table(tabla_direc_or[order(-fa_direc_or)]),ylab = "Cantidad de Estaciones",xlab = "Número de concurrencias",main = title,ylab2 = "Porcentaje Acumulado")

# Lista de las 10 estaciones de origen mas concurridas 
#Ramos Mejia, Av Dr Jose Maria Vargas& Av. Del Libertador                         15              0.0365
#2292 Montañeses                                                                  13              0.0316
#3912 Humahuaca                                                                   10              0.0243
#300 Almafuerte Av. & Los Patos                                                    9              0.0219
#441 Bulnes & Peron, Juan Domingo, Tte. General                                    8              0.0195
#1785 Espinosa                                                                     7              0.0170
#3084 Agrelo                                                                       7              0.0170
#Cordoba 6599                                                                      7              0.0170
#Av. Del Libertador, 3260                                                          6              0.0146
#Lavalle & Acuña De Figueroa, Francisco                                            6              0.0146

# moda = Ramos Mejia, Av Dr Jose Maria Vargas& Av. Del Libertador



# tabla de frecuencias DIRECCION ESTACION DE DESTINO
fa_direc_dt = table(data2$direccion_estacion_destino)
fr_direc_dt = round(fa_direc_dt/sum(fa_direc_dt),4)
tabla_direc_dt = cbind(fa_direc_dt,fr_direc_dt)
attributes(tabla_direc_dt)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
TOTAL = c(sum(fa_direc_dt),sum(fr_direc_dt))
tabla_direc_dt = rbind(tabla_direc_dt,TOTAL)

# Diagrama de Pareto, cantidad de Estaciones de Destino de acuerda a su frecuencia absoluta,
# el Principio de Pareto que se visualiza es que la mayoria de las estaciones son poco utilizadas 
# como estacion de destino.
title = "Estaciones de EcoBicis de CABA según la cantidad de veces\n de que fueron utilizadas como estación de destino."
pareto.chart(table(tabla_direc_dt[order(-fa_direc_dt)]), ylab = "Cantidad de Estaciones",xlab = "Número de concurrencias",main = title,ylab2 = "Porcentaje Acumulado")

# Lista de las 10 estaciones de destino mas concurridas 
#Lavalle & Bouchard                                                               20              0.0487
#441 Bulnes & Peron, Juan Domingo, Tte. General                                   14              0.0341
#Amenabar y Mendoza                                                               14              0.0341
#Quintino Bocayuva y Don Bosco                                                    14              0.0341
#Cevallos, Virrey& Yrigoyen, Hipolito Av.                                         13              0.0316
#Culpina 121                                                                      11              0.0268
#1355 San Martin Av.                                                              11              0.0268
#Av. Patricias Argentinas & Estivao                                                8              0.0195
#3084 Agrelo                                                                       7              0.0170
#3817 Traful                                                                       7              0.0170

# moda = Lavalle & Bouchard





# tabla de frecuencias para DISTANCIA
# divido por 1000 y pasan a ser kilometros
breaks = seq(0,11000,1000)/1000
distancia_intervalos = cut(data2$distancia/1000, breaks = breaks,right = FALSE)
frec_abs_distancia = table(distancia_intervalos) 
frec_rel_distancia = round(frec_abs_distancia / sum(frec_abs_distancia),4)
frec_abs_ac_distancia = cumsum(frec_abs_distancia)
frec_rel_ac_distancia = round(frec_abs_ac_distancia / sum(frec_abs_distancia),4)
tabla_distancia = cbind(frec_abs_distancia, frec_rel_distancia, frec_abs_ac_distancia,frec_rel_ac_distancia)
attributes(tabla_distancia)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
TOTAL = c(sum(frec_abs_distancia),sum(frec_rel_distancia),NA,NA)
tabla_distancia = rbind(tabla_distancia,TOTAL)
tabla_distancia

# grafico para DISTANCIA
# probamos un histograma
hist(data2$distancia/1000,border = "black",col = "yellow",xlim = c(0,12),breaks = breaks,xlab = "Distancia (en kilómetros)",ylab = "Cantidad de recorridos",main = "Distancias de recorridos en EcoBici en CABA")

# probamos un grafico de caja
boxplot(data2$distancia/1000, col="yellow", main = "Distancias de recorridos en EcoBici en CABA" , ylab="Distancia del recorrido (en kilómetros)")

# Analisis de Medidas Descriptivas DISTANCIA
summary(data2$distancia/1000)
rango_intercuartil = 3.039 - 1.006
desvio_estandar = sd(data2$distancia/1000)
moda = "[1,2)"




# tabla de frecuencias para DURACION DEL RECORRIDO
#se podria dividir un 60 y queda en minutos (?)
#breaks = round(seq(130,29130,1000)/60,2)
breaks = c(2,12,22,32,42,52,62,72,82,92,102,112,122,132,485)
duracion_intervalos = cut(data2$duracion_recorrido/60, breaks = breaks, rigth = FALSE)
frec_abs_duracion = table(duracion_intervalos) 
frec_rel_duracion = round(frec_abs_duracion / sum(frec_abs_duracion),4)
frec_abs_ac_duracion = cumsum(frec_abs_duracion)
frec_rel_ac_duracion = round(frec_abs_ac_duracion / sum(frec_abs_duracion),4)
tabla_duracion = cbind(frec_abs_duracion, frec_rel_duracion, frec_abs_ac_duracion,frec_rel_ac_duracion)
attributes(tabla_duracion)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
TOTAL = c(sum(frec_abs_duracion),frec_rel_ac_duracion[[14]],NA,NA)
tabla_duracion = rbind(tabla_duracion,TOTAL)
tabla_duracion

# grafico de caja para DURACION
title3 = "Duraciones de recorridos en EcoBici en CABA"
boxplot(dur_recorridos, col="orange", main = title3 , ylab="Duración del recorrido (en minutos)")

breaks_hist = c(2,12,22,32,42,52,62,72,82,92,102,112,122,132)
hist(dur_recorridos,border = "black",col = "orange",ylim = c(0,120),xaxt='n',breaks = breaks_hist,xlab = "Duración (en minutos)",ylab = "Cantidad de recorridos",main = "Duración de recorridos en EcoBici en CABA")
axis(side = 1, at = breaks)

# poligono de frecuencia y poligono acumulativo para DURACION
breaks_hist2 = c(-8,breaks_hist,142)
datos = table(cut(data2$duracion_recorrido/60, breaks = breaks_hist2, rigth = FALSE))
datos = round(datos/sum(datos),4)
# poligono de frecuencia:
plot(datos,type = "l",main = title3,ylab = "Frecuencia Relativa",xlab = "Duración (en minutos)")
grid()
# poligono acumulativo:
datos2 = as.table(cumsum(datos))
plot(datos2,type = "l",main = title3,ylab = "Frecuencia Relativa Acumulada",xlab = "Duración (en minutos)",ylim = c(0,1))
grid()

# Analisis de Medidas Descriptivas DURACION
round(summary(data2$duracion_recorrido/60),4)
rango_intercuartil = 31.91 - 11.67
desvio_estandar = sd(data2$duracion_recorrido/60)


#lista de duraciones de reorridos sin los 3 outliers mas extremos
# estos uotliers son:
outliers = c(484.550000,267.650000,191.300000)
dur_recorridos =  c(33.633333,  30.483333,7.283333,44.916667,16.533333,9.050000,19.916667,36.750000,36.116667
,10.566667,25.500000,12.400000,21.600000,28.200000,28.966667,24.683333,28.816667,30.466667
,11.666667,10.866667,25.733333,25.583333,19.133333,49.400000,47.216667,40.900000,8.150000
,19.700000,18.716667,18.533333,26.866667,45.866667,13.716667,20.316667,19.566667,14.616667
,14.850000,30.000000,37.250000,23.650000,25.933333,10.650000,22.966667,30.033333,9.850000
,16.600000,18.183333,15.583333,22.583333,23.450000,20.783333,37.166667,26.016667,50.216667
,8.383333,12.216667,14.866667,10.916667,20.616667,25.733333,16.416667,28.066667,33.283333
,60.750000,27.766667,15.783333,86.966667,28.833333,22.616667,32.700000,34.216667,4.233333
,40.633333,16.516667,10.100000,7.933333,8.216667,7.883333,8.833333,29.416667,40.316667
,13.500000,8.133333,9.100000,9.983333,17.150000,23.566667,23.016667,8.400000,29.483333
,40.400000,6.400000,6.050000,5.483333,5.733333,6.300000,5.183333,5.650000,7.133333
,5.966667,5.950000,5.166667,7.266667,57.100000,18.966667,25.683333,53.383333,40.450000
,23.333333,15.650000,23.500000,24.466667,19.166667,24.316667,28.450000,19.250000,16.366667
,19.433333,19.483333,9.850000,10.050000,10.216667,15.500000,24.966667,9.766667,19.033333
,57.400000,9.950000,40.166667,22.416667,14.600000,38.416667,11.366667,48.416667,12.350000
,28.633333,6.566667,29.883333,19.766667,21.266667,41.666667,34.316667,38.933333,22.550000
,32.533333,49.750000,12.966667,19.383333,16.516667,25.683333,24.200000,8.233333,23.166667
,31.900000,29.666667,19.083333,40.400000,22.650000,35.066667,14.766667,34.883333,4.300000
,4.683333,4.833333,4.316667,3.566667,2.200000,5.133333,11.116667,130.966667,19.783333
,17.583333,24.183333,17.400000,10.750000,123.600000,39.166667,89.583333,19.666667,14.866667
,21.250000,20.500000,21.950000,81.083333,11.500000,22.766667,7.916667,25.066667,31.833333
,31.383333,8.300000,25.133333,36.100000,21.383333,13.183333,19.416667,11.533333,10.983333
,20.983333,14.983333,31.850000,23.916667,17.316667,19.750000,26.400000,20.500000,55.283333
,27.100000,33.433333,10.200000,55.333333,9.983333,18.500000,23.400000,31.916667,60.133333
,27.250000,31.766667,13.733333,29.316667,9.033333,73.100000,16.000000,41.766667,34.083333
,23.750000,29.266667,41.100000,31.800000,56.950000,13.383333,29.300000,37.183333,11.666667
,22.516667,9.400000,10.733333,18.933333,8.016667,30.550000,18.816667,19.966667,20.883333
,31.000000,30.766667,19.333333,24.966667,20.450000,27.350000,33.233333,43.866667
,102.616667,42.216667,32.283333,16.316667,9.066667,35.150000,7.816667,8.883333
,11.400000,21.983333,104.483333,29.533333,9.116667,26.816667,28.933333,9.400000,23.566667
,17.850000,9.450000,67.233333,15.966667,12.833333,61.933333,27.650000,11.900000,27.500000
,24.350000,24.116667,25.833333,25.733333,6.866667,10.866667,16.233333,18.100000,9.333333
,7.900000,8.100000,7.700000,9.366667,24.200000,63.650000,23.916667,27.400000,22.533333
,51.950000,24.716667,40.450000,34.283333,47.200000,28.250000,33.083333,35.950000,26.666667
,9.400000,45.366667,26.800000,18.416667,22.433333,24.433333,14.016667,19.100000
,30.366667,24.716667,34.466667,100.966667,34.416667,52.600000,65.116667,13.250000,34.633333
,30.466667,33.600000,22.950000,53.683333,50.733333,20.950000,17.400000,58.533333,12.183333
,87.733333,24.983333,17.783333,12.250000,6.083333,3.550000,16.183333,47.133333,18.866667
,35.283333,5.233333,34.166667,26.933333,6.566667,6.516667,6.650000,6.350000,7.283333
,7.766667,7.300000,8.650000,18.200000,11.000000,7.533333,7.716667,9.050000,9.566667
,9.666667,6.633333,10.550000,9.650000,25.683333,7.166667,42.883333,18.216667,48.500000
,19.483333,31.366667,34.833333,12.033333,17.016667,15.750000,36.316667,80.183333,6.733333
,8.550000,15.633333,30.150000,62.116667,42.116667,10.850000,112.883333,27.316667,19.133333
,24.933333,29.700000,26.183333,3.850000,10.766667,27.983333,31.800000,32.200000,32.783333
,39.183333,38.916667,31.516667,36.300000,32.700000,32.516667,16.666667,17.150000,9.000000
,9.000000,15.483333,19.616667,40.500000,35.150000,37.316667)


dias = c("Domingo","Domingo","Miércoles","Domingo","Martes","Miércoles","Miércoles","Martes","Martes","Domingo"  
,"Viernes","Domingo","Jueves","Sábado","Miércoles","Domingo","Lunes", "Sábado","Domingo","Sábado",
"Viernes","Sábado","Sábado","Jueves","Lunes", "Sábado","Lunes", "Martes","Jueves","Sábado",
"Jueves","Domingo","Jueves","Domingo","Jueves","Jueves","Martes","Domingo","Lunes", "Lunes",
"Viernes","Lunes", "Lunes", "Domingo","Domingo","Jueves","Sábado","Sábado","Domingo","Sábado",
"Domingo","Lunes", "Miércoles","Martes","Domingo","Viernes","Lunes", "Domingo","Miércoles","Sábado",
"Domingo","Lunes", "Miércoles","Sábado","Domingo","Domingo","Lunes", "Sábado","Sábado","Domingo"  
,"Lunes", "Lunes", "Jueves","Domingo","Miércoles","Jueves","Martes","Lunes", "Jueves","Miércoles"
,"Miércoles","Martes","Lunes", "Martes","Jueves","Domingo","Domingo","Martes","Sábado","Domingo"  
, "Miércoles","Miércoles", "Miércoles", "Jueves","Martes","Viernes","Miércoles", "Sábado","Miércoles", "Viernes"  
,"Martes","Martes","Sábado","Sábado","Martes","Lunes", "Sábado","Domingo","Jueves","Jueves",
 "Lunes", "Lunes", "Domingo","Jueves","Martes","Sábado","Viernes","Viernes","Jueves","Viernes"  
, "Martes","Viernes","Martes","Miércoles", "Martes","Sábado","Jueves","Miércoles", "Lunes", "Sábado",
"Viernes","Sábado","Viernes","Lunes", "Lunes", "Miércoles", "Viernes","Jueves","Miércoles", "Jueves",
 "Domingo","Sábado","Jueves","Viernes","Miércoles", "Domingo","Jueves","Martes","Viernes","Domingo"  
, "Sábado","Lunes", "Viernes","Miércoles", "Martes","Sábado","Sábado","Lunes", "Sábado","Domingo"  
,"Sábado","Lunes", "Martes","Viernes","Jueves","Martes","Lunes", "Viernes","Martes","Martes",
"Jueves","Lunes", "Lunes", "Viernes","Miércoles", "Lunes", "Martes","Sábado","Jueves","Martes",
"Lunes", "Martes","Sábado","Lunes", "Viernes","Lunes", "Viernes","Miércoles", "Domingo","Viernes"  
,"Domingo","Domingo","Domingo","Martes","Lunes", "Miércoles", "Miércoles", "Sábado","Miércoles", "Miércoles"
,"Sábado","Jueves","Miércoles", "Jueves","Sábado","Miércoles", "Miércoles", "Jueves","Martes","Jueves",
 "Domingo","Jueves","Sábado","Martes","Sábado","Jueves","Viernes","Viernes","Viernes","Sábado",
"Lunes", "Martes","Miércoles", "Viernes","Sábado","Sábado","Lunes", "Domingo","Sábado","Martes",
 "Viernes","Domingo","Jueves","Miércoles", "Lunes", "Viernes","Viernes","Domingo","Jueves","Sábado",
"Viernes","Lunes", "Viernes","Lunes", "Domingo","Viernes","Sábado","Jueves","Jueves",
 "Domingo","Martes","Domingo","Martes","Lunes", "Miércoles", "Sábado","Viernes","Miércoles"
, "Martes","Martes","Jueves","Lunes", "Sábado","Viernes","Miércoles", "Sábado","Lunes", "Martes",
 "Viernes","Viernes","Lunes", "Sábado","Viernes","Domingo","Miércoles", "Martes","Lunes", "Domingo"  
, "Miércoles", "Domingo","Martes","Martes","Jueves","Viernes","Sábado","Jueves","Jueves","Miércoles"
, "Viernes","Jueves","Domingo","Sábado","Viernes","Miércoles", "Domingo","Jueves","Viernes","Lunes",
"Viernes","Martes","Martes","Sábado","Domingo","Domingo","Miércoles", "Lunes", "Domingo"
, "Jueves","Lunes", "Lunes", "Jueves","Domingo","Viernes","Viernes","Domingo","Sábado","Sábado",
 "Lunes", "Miércoles", "Jueves","Domingo","Lunes", "Lunes", "Domingo","Miércoles", "Lunes", "Viernes"  
, "Lunes", "Lunes", "Viernes","Lunes", "Sábado","Domingo","Domingo","Martes","Sábado","Jueves",
 "Domingo","Domingo","Sábado","Sábado","Sábado","Viernes","Viernes","Martes","Lunes", "Jueves",
"Martes","Jueves","Miércoles", "Jueves","Lunes", "Miércoles", "Domingo","Jueves","Miércoles", "Martes",
"Lunes", "Jueves","Domingo","Viernes","Jueves","Jueves","Sábado","Sábado","Sábado","Jueves",
"Lunes", "Sábado","Domingo","Domingo","Viernes","Martes","Miércoles", "Martes","Lunes", "Jueves",
"Martes","Viernes","Lunes", "Martes","Viernes","Miércoles", "Sábado","Jueves","Lunes", "Sábado",
"Sábado","Jueves","Jueves","Martes","Sábado","Martes","Miércoles", "Miércoles", "Lunes", "Martes",
"Lunes", "Jueves","Domingo","Martes","Lunes", "Sábado","Jueves","Viernes","Sábado","Miércoles","Sábado")


# ANALISIS BIVARIADO
breaks = c(2,12,22,32,42,52,62,72,82,92,102,112,122,132,485)
duracion_intervalos = cut(data2$duracion_recorrido/60, breaks = breaks, rigth = FALSE)
tabla_bivariada = table(data2$dia,duracion_intervalos)
tabla_bivariada <- tabla_bivariada[c(1,3,4,5,2,7,6),]
tabla_bivariada

dias = factor(dias, levels = c("Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sábado"))

title = "Duración de recorridos de EcoBicis de CABA\n de acuerdo al día de la semana."
boxplot(dur_recorridos ~ dias,col = "#e3b1d2",main = title,xlab = "Días de la semana",ylab = "Duración de los recorridos")


# promedio de la duracion de los recorridos por dia
table(data2$dia, data2$duracion_recorrido/60)["Jueves",]
sum(table(data2$dia, data2$duracion_recorrido/60)["Jueves",])

# DOMINGO
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Domingo") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Domingo",])
promedio


# LUNES
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Lunes") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Lunes",])
promedio


# MARTES
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Martes") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Martes",])
promedio


# MIERCOLES 
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Miércoles") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Miércoles",])
promedio


# JUEVES
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Jueves") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Jueves",])
promedio


# VIERNES
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Viernes") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Viernes",])
promedio



# SABADO
suma = 0
for (i in 1:length(data2$duracion_recorrido)) {
  if (data2$dia[i] == "Sábado") {
    suma = suma + (data2$duracion_recorrido/60)[i]
  }
}
promedio = suma / sum(table(data2$dia, data2$duracion_recorrido/60)["Sábado",])
promedio












suma_jueves = 
4.31666666666667 +
5.48333333333333 +
6.35 +
6.63333333333333  +
7.16666666666667 +
7.71666666666667  +
7.76666666666667 +
7.9 +
7.93333333333333 +
8.01666666666667  +
8.65 +
8.83333333333333 +
9.33333333333333 +
9.36666666666667 +
9.98333333333333 +
9.98333333333333 +
10.2 +
10.7666666666667  +
10.8666666666667  +
12.9666666666667 +
13.25  +
13.7166666666667 +
14.0166666666667  +
14.6166666666667 +
15.4833333333333 +
15.6333333333333  +
15.65  +
16.1833333333333 +
16.6 +
18.4166666666667 +
18.7166666666667 +
19.4833333333333 +
19.4833333333333 +
19.5666666666667 +
19.6666666666667 +
19.75 +
19.7833333333333 +
20.45 +
21.2666666666667 +
21.6 +
21.9833333333333 +
23.3333333333333 +
23.9166666666667 +
24.3166666666667 +
24.9333333333333 +
25.6833333333333  +
26.8666666666667  +
27.1 +
27.35 +
27.9833333333333  +
29.8833333333333 +
32.5166666666667 +
37.1833333333333 +
38.9333333333333 +
40.6333333333333 +
49.4 +
51.95 +
57.4 +
60.1333333333333

suma_jueves / 59
