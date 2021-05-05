setwd("/Users/inescipullo/Documents2.0/Probabilidad\ y\ Estadistica/PyE")
setwd("/home/administrador/Documentos/PyE")

data = read.table("usuarios9.csv", header = TRUE, sep = ",")
data2 = read.table("recorridos9.csv", header = TRUE, sep = ",")
attach(data)
attach(data2)

# tabla de frecuencias para GENERO
# es variable cualitativa
# grafico de torta
frec_abs_genero = table(data$genero_usuario)
frec_rel_genero = round(frec_abs_genero / sum(frec_abs_genero), 2)
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



#tabla de frecuencias para EDAD
# garfico histograma
breaks_edad = seq(18,68,5)
edad_intervalos = cut(data$edad_usuario, breaks = breaks_edad, right = FALSE)
frec_abs_edad = table(edad_intervalos) 
frec_rel_edad = round(frec_abs_edad / sum(frec_abs_edad),2)
frec_abs_ac_edad = cumsum(frec_abs_edad)
frec_rel_ac_edad = cumsum(frec_rel_edad)
tabla_edad = cbind(frec_abs_edad, frec_rel_edad, frec_abs_ac_edad,frec_rel_ac_edad)
attributes(tabla_edad)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
NA_ = c(1,1/100,sum(frec_abs_edad)+1,sum(frec_rel_edad)+1/100)
total = c(sum(frec_abs_edad)+NA_[1],sum(frec_rel_edad)+NA_[2],NA,NA)
tabla_edad = rbind(tabla_edad,NA_,total)
tabla_edad

# grafico de histograma para EDAD
title2 = "Edad de los Usuarios del Servicio EcoBici de CABA"
hist(data$edad_usuario,border = "black",col = "lightblue",xlim = c(10,70),ylim = c(0,30),breaks = breaks_edad,xlab = "Edad (en años)",ylab = "Cantidad de Usuarios",main = title2,)

# poligono de frecuencia y poligono acumulativo para EDAD
breaks_edad2 = seq(13,73,5)
xy = table(cut(data$edad_usuario,breaks = breaks_edad2,right = FALSE))
xy = round(xy/sum(xy),2)
# poligono de frecuencia:
plot(xy,type = "l",main = title2,ylab = "Frecuencia Relativa",xlab = "Edad (en años)",ylim = c(0,0.3))
grid()
# poligono acumulativo:
xy2 = cumsum(xy)
plot(cumsum(xy),type = "l",main = title2,ylab = "Frecuencia Relativa Acumulada",xlab = "Edad (en años)",ylim = c(0,1))
grid()
# no queda bien el eje x y faltaria agg NA


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




# DIRECCION ESTACION DE ORIGEN
fa_direc_or = table(data2$direccion_estacion_origen)
fr_direc_or = round(fa_direc_or/sum(fa_direc_or),4)
tabla_direc_or = cbind(fa_direc_or,fr_direc_or)
attributes(tabla_direc_or)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
TOTAL = c(sum(fa_direc_or),sum(fr_direc_or))
tabla_direc_or = rbind(tabla_direc_or,TOTAL)
tabla_direc_or


# DIRECCION ESTACION DE DESTINO
fa_direc_dt = table(data2$direccion_estacion_destino)
fr_direc_dt = round(fa_direc_dt/sum(fa_direc_dt),4)
tabla_direc_dt = cbind(fa_direc_dt,fr_direc_dt)
attributes(tabla_direc_dt)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa")
TOTAL = c(sum(fa_direc_dt),sum(fr_direc_dt))
tabla_direc_dt = rbind(tabla_direc_dt,TOTAL)
tabla_direc_dt





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
breaks = seq(130,29130,1000)/1000
duracion_intervalos = cut(data2$duracion_recorrido/1000, breaks = breaks, rigth = FALSE)
frec_abs_duracion = table(duracion_intervalos) 

#frec_abs_duracion = table(data2$duracion_recorrido)
frec_rel_duracion = round(frec_abs_duracion / sum(frec_abs_duracion),4)
frec_abs_ac_duracion = cumsum(frec_abs_duracion)
frec_rel_ac_duracion = round(frec_abs_ac_duracion / sum(frec_abs_duracion),4)
tabla_duracion = cbind(frec_abs_duracion, frec_rel_duracion, frec_abs_ac_duracion,frec_rel_ac_duracion)
attributes(tabla_duracion)$dimnames[[2]] = c("Frecuencia Absoluta", "Frecuencia Relativa", "Frecuencia Absoluta Acumulada", "Frecuencia Relativa Acumulada")
tabla_duracion        


boxplot(frec_abs_duracion, col="lightblue", main = "Duración de Recorrido" , xlab=NA, ylab="Edad de la madre (en años)")
b
