setwd("/Users/inescipullo/Documents2.0/Probabilidad\ y\ Estadistica/PyE")

data = read.table("usuarios9.csv", header = TRUE, sep = ",")
data2 = read.table("recorridos9.csv", header = TRUE, sep = ",")
attach(data)
attach(data2)

# tabla de frecuencias para GENERO
# es variable cualitativa
# grafico de torta
frec_abs_genero = table(data$genero_usuario) # frecuencia absoluta de la variable 
frec_rel_genero = round(frec_abs_genero / sum(frec_abs_genero), 2)
frec_abs_ac_genero = cumsum(frec_abs_genero)
frec_rel_ac_genero = round(frec_abs_ac_genero / sum(frec_abs_genero), 2)
tabla_genero = cbind(frec_abs_genero,frec_rel_genero,frec_abs_ac_genero,frec_rel_ac_genero)
attributes(tabla_genero)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
tabla_genero

#grafico de sectores para GENERO
x = table(data$genero_usuario)
title = "GENERO USUSARIO"
porcentaje = round(x / sum(x) * 100, 2)
options = c("Femenino", "Masculino", "Otro")
labels = paste(options, porcentaje, c("%"), sep = " ")
grafico_genero = pie(x, labels = labels, main = title, col = c("#ff896b","#fbdb48","#00ff00"))



#tabla de frecuencias para EDAD
# garfico histograma
breaks = seq(18,68,5)
edad_intervalos = cut(data$edad_usuario, breaks = breaks, right = FALSE)
frec_abs_edad = table(edad_intervalos) 
frec_rel_edad = round(frec_abs_edad / sum(frec_abs_edad),2)
frec_abs_ac_edad = cumsum(frec_abs_edad)
frec_rel_ac_edad = round(frec_abs_ac_edad / sum(frec_abs_edad),2)
tabla_edad = cbind(frec_abs_edad, frec_rel_edad, frec_abs_ac_edad,frec_rel_ac_edad)
attributes(tabla_edad)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
tabla_edad

# grafico de histograma para EDAD
hist(data$edad_usuario,freq = F)



# tabla de frecuencias para DIA
# grafico de barras
# ver de ponerlo en orden
frec_abs_dia = table(data2$dia) 
frec_abs_dia <- frec_abs_dia[c(1,3,4,5,2,7,6)]
frec_rel_dia = round(frec_abs_dia / sum(frec_abs_dia), 2)
frec_abs_ac_dia = cumsum(frec_abs_dia)
frec_rel_ac_dia = round(frec_abs_ac_dia / sum(frec_abs_dia), 2)
tabla_dia = cbind(frec_abs_dia,frec_rel_dia,frec_abs_ac_dia,frec_rel_ac_dia)
attributes(tabla_dia)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
rbind()
tabla_dia

# grafico de barras para DIA
barplot(frec_abs_dia,xlab = "Días de la Semana",ylab = "Cantidad",ylim = c(0,70),col = "lightgreen",main = "Cantidad de bicicletas públicas usadas por día")



# tabla de frecuencias para DISTANCIA
breaks = seq(min(data2$distancia),max(data2$distancia),100)
distancia_intervalos = cut(data2$distancia, breaks = breaks, rigth = FALSE)
frec_abs_distancia = table(distancia_intervalos) 
frec_rel_distancia = round(frec_abs_distancia / sum(frec_abs_distancia),4)
frec_abs_ac_distancia = cumsum(frec_abs_distancia)
frec_rel_ac_distancia = round(frec_abs_ac_distancia / sum(frec_abs_distancia),4)
tabla_distancia = cbind(frec_abs_distancia, frec_rel_distancia, frec_abs_ac_distancia,frec_rel_ac_distancia)
attributes(tabla_distancia)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
tabla_distancia


# tabla de frecuencias para DURACION
breaks = seq(min(data2$duracion),max(data2$duracion),100)
duracion_intervalos = cut(data2$duracion_recorrido, breaks = breaks, rigth = FALSE)
frec_abs_duracion = table(duracion_intervalos) 

#frec_abs_duracion = table(data2$duracion_recorrido)
frec_rel_duracion = round(frec_abs_duracion / sum(frec_abs_duracion),4)
frec_abs_ac_duracion = cumsum(frec_abs_duracion)
frec_rel_ac_duracion = round(frec_abs_ac_duracion / sum(frec_abs_duracion),4)
tabla_duracion = cbind(frec_abs_duracion, frec_rel_duracion, frec_abs_ac_duracion,frec_rel_ac_duracion)
attributes(tabla_duracion)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
tabla_duracion        




