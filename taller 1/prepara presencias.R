
################################################################
################################################################
#PREPARACION DE LAS TABLAS DE DATOS PARA HACER LOS MODELOS
################################################################
################################################################

setwd("C:/taller 1")

#CARGAMOS FUNCIONES (iremos dando un vistazo a las funciones cuando las usemos)
source("funcionesSDM_taller1.R")

library(raster) #TRABAJO CON DATOS RASTER
library(HH) #VARIANCE INFLATION FACTOR
library(rgeos) #OPERACIONES GEOM√âTRICAS CON INFO GEOGR√ÅFICA
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(rgdal)
library(sp)
library(rasterVis)
library(maptools)
library(rJava)
ReduceSpatialClustering = function(data, minimum.distance){
  
  #count rows
  row<-1
  
  
  #repite la operaci√≥n hasta que se cumple la condici√≥n de salida
  repeat{
    
    #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
    f<-data[row, ]
    
    #genera los l√???mites de la cuadr√???cula de b√∫squeda
    ymax<-f$latitude + minimum.distance
    ymin<-f$latitude - minimum.distance
    xmax<-f$longitude + minimum.distance
    xmin<-f$longitude - minimum.distance
    
    #selecciona de la tabla los datos con coordenadas dentro del rect√°ngulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
    data<-data[!((data$latitude <= ymax) & (data$latitude >= ymin) & (data$longitude <= xmax) & (data$longitude >= xmin) & (data$latitude != f$latitude | data$longitude != f$longitude)), ]
    
    #estima de filas por procesar
    print(paste("Processed rows: ", row, " out of ", nrow(data), sep=""))
    
    #suma 1 al contador de la fila
    row<-row+1
    
    #condici√≥n de salida cuando llega a la √∫ltima fila
    if(row>=nrow(data))break
  }
  
  return(data)
  
}#cargamos ya la funcion y asi no nos olvidamos


######## cargamos datos de la especie desde gbif

especie <- gbif("Anthyllis", "cytisoides*", geo=T)
data(especie)

dim(especie)
colnames(especie)

especie <- especie[, c("datasetID", "lat", "lon", "year", "species")]
especie

# en cualquier caso trabajeremos con tablas preparadas

#LISTADO DE VARIABLES
lista.variables <- list.files(path="./variables/",pattern='*.asc', full.names=TRUE)
lista.variables

#stack Y brick PREPARAN LAS VARIABLES EN UN UNICO OBJETO ESPACIAL
variables <- brick(stack(lista.variables))
names(variables)

plot(variables[["bioclim1"]])

vars_def <- variables[[c("bioclim3", "bioclim4", "bioclim8","bioclim9",  "bioclim12","bioclim15")]]

names(vars_def) <- c("bio3_eu", "bio4_eu", "bio8_eu","bio9_eu",  "bio12_eu","bio15_eu")

#IMPORTA REGISTROS DE PRESENCIA

Acyt_complet<-read.csv("C:/Taller 1/datos GBIF/Anthyllis cytisoides p.csv",sep=";", dec=".")

head(Acyt_complet)# es muy importante saber bien los nombres de cada columna

#hacemos una copia sobre la que haremos los cambios

Acyt<-Acyt_complet

#vemos la estructura
str(Acyt)
head(Acyt)

#------------------------------------------------------------------------------------------------------
######################################## LIMPIEZA DE LA TABLA #########################################
#------------------------------------------------------------------------------------------------------


# antes de proceder unificamos las columnas con registros de presencia en una sola

Acyt$decimalLatitude[is.na(Acyt$decimalLatitude)] <- Acyt$decimalLatitude.1[is.na(Acyt$decimalLatitude)]
Acyt$decimalLongitude[is.na(Acyt$decimalLongitude)] <- Acyt$decimalLongitude.1[is.na(Acyt$decimalLatitude)]


### FILTRADO POR PRESENCIA DE COORDENADAS
##------------------------------------------------------------------------------------------------------------

### comprobamos si existen datos sin coordenadas

unique(is.na(Acyt$decimalLatitude))
unique(is.na(Acyt$decimalLatitude[!is.na(Acyt$decimalLongitude)]))#para ver si hay valores con una sola de las dos coordenadas

### QUITAMOS LAS FILAS QUE NO TENGAN COORDENADAS


Acyt<-Acyt[!is.na(Acyt$decimalLatitude), ]
Acyt<-Acyt[!is.na(Acyt$decimalLongitude), ]

unique(is.na(Acyt$decimalLatitude))

### QUITAMOS TODOS LOS REGISTROS QUE QUEDAN FUERA DEL AREA DE TRABAJO O TIENEN VALORES NULOS PARA LAS VARIABLES
###-----------------------------------------------------------------------------------------------------------


Acyt.onvariables<-extract(x=vars_def, y=Acyt[ , c("decimalLongitude","decimalLatitude")])#extraemos los valores ambientales para cada coordenada
str(Acyt.onvariables)
is.data.frame(Acyt.onvariables)

#no es un data.frame, lo convertimos
Acyt.onvariables<-data.frame(Acyt.onvariables)
is.data.frame(Acyt.onvariables)
#comprobamos como qued√≥
str(Acyt.onvariables)

#lo unimos a las presencias
Acyt<-data.frame(Acyt, Acyt.onvariables)
nrow(Acyt) ## para ver el n˙mero de filas=n˙mero de presencias
rm(Acyt.onvariables)

# comprobamos si los datos de presencia poseen valores na para alguna de las variables (en cuyo caso la coordenada cae dentro de una zona sin datos, probablemente el mar)

unique(is.na(Acyt$bio3_eu))

Acyt<-Acyt[!(is.na(Acyt$bio3_eu)), ]

nrow(Acyt)

#ploteamos

plot(vars_def[[1]], col="gray80")
points(Acyt$decimalLongitude,Acyt$decimalLatitude, col="red", pch=20)


### QUITAMOS OTRAS ESPECIES (SI LAS HAY)

unique(Acyt$scientificName.1)## esta muy sucia, me la voy a cargar
Acyt$scientificName.1 <- NULL

unique(Acyt$scientificName)

unique(Acyt$datasetID) # hay varios nombres pero solo un codigo de especie. Todo guay
# si hubiese m·s cÛdigo seleccionamos el adecuado
#presencia<-presencia[presencia$species_id==codigoxxxxx, ]

# ponemos un ˙nico nombre a la columna de scientificName
Acyt$scientificName<-"Anthyllis cytisoides"


### FIILTRADO POR RESOLUCIoN ESPACIAL DE LOS REGISTROS
##---------------------------------------------------------------------------------------------


#vemos el tipo de los datos
str(Acyt$coordinatePrecision)
str(Acyt$coordinateUncertaintyInMeters)

#ojo, son caracteres, no n˙meros, los pasamos a numericos

Acyt$coordinatePrecision<-as.numeric(Acyt$coordinatePrecision)
Acyt$coordinateUncertaintyInMeters<-as.numeric(Acyt$coordinateUncertaintyInMeters)


#Donde estan los datos vacÌos?

unique(is.na(Acyt$coordinatePrecision))# todos los datos tienen precision en la coordenada
unique(is.na(Acyt$coordinateUncertaintyInMeters))

plot(vars_def[[1]], col="gray80")

points(Acyt$decimalLongitude,Acyt$decimalLatitude, pch=20, col="red")

points(Acyt[is.na(Acyt$coordinateUncertaintyInMeters), ]$decimalLongitude, Acyt[is.na(Acyt$coordinateUncertaintyInMeters), ]$decimalLatitude, col="green", cex=0.75)

# decidimos si quitamos los datos vacios o no en funciÛn de la cantidad de informaciÛn que perdemos (en este caso los quitamos)

# eliminamos los datos que no tengan precisiÛn en la coordenada

length(Acyt$coordinatePrecision[is.na(Acyt$coordinatePrecision)])
length(Acyt$coordinateUncertaintyInMeters[is.na(Acyt$coordinateUncertaintyInMeters)])

Acyt<-Acyt[!(is.na(Acyt$coordinatePrecision)), ]
Acyt<-Acyt[!(is.na(Acyt$coordinateUncertaintyInMeters)), ]

# antes de eliminar los datos en funciÛn de la precisiÛn comprobamos si vamos a eliminar demasiados datos

histogram(Acyt$coordinatePrecision)
histogram(Acyt$coordinateUncertaintyInMeters)

length(Acyt$coordinatePrecision[Acyt$coordinatePrecision<="707"])# seg˙n esto existen 7217 puntos con una precisiÛn de pixel mejor de 1 km2
length(Acyt$coordinateUncertaintyInMeters[Acyt$coordinateUncertaintyInMeters<="1000"])

length(Acyt$coordinatePrecision[Acyt$coordinatePrecision<="7071"])
length(Acyt$coordinateUncertaintyInMeters[Acyt$coordinateUncertaintyInMeters<="10000"])# no se como la misma coordenada tiene precisones diferentes en grados y metros
#somos conservadores y hacemos caso de la incertidumbre m·s pequeÒa, en este caso no eliminamos datos

# de eleminarlos se aplicaria la siguiente fÛrmula:

Acyt<-Acyt[Acyt$coordinatePrecision <= 707, ] 

#vemos los valores de los datos

unique(Acyt$coordinateprecision)

# volvemos a plotear

plot(vars_def[[1]], col="gray80")
points(Acyt$decimalLongitude,Acyt$decimalLatitude, pch=20, col="red")

#AUTOCORRELACI”N: SEPARACI√ìN DE LAS PRESENCIAS POR UNA DISTANCIA MÌNIMA
#-----------------------------------------------------

#Este tratamiento se hace para reducir la autocorrelaciÛn espacial de la muestra. De esta forma se reducen sesgos de muestreo, y se evita inflar los valores de evaluaci√≥n.

#veamos la resoluci√≥n de las variables
xres(vars_def)
yres(vars_def)
res.grados<-xres(vars_def)
#n˙mero de celdas vac√???as que vamos a dejar entre un punto y el siguiente
celdas.vacias<-1
#distancia mÌnima entre puntos consecutivos
distancia.minima<-res.grados*celdas.vacias
#A øcuanto es eso en km?
distancia.minima*111.19

#EMPEZAMOS UN PLOT PARA VER EL EFECTO DE LA FUNCIÛnN 
plot(vars_def[[1]], col="gray80", ext= c(-10, 6, 35, 45))
points(Acyt$decimalLongitude,Acyt$decimalLatitude, pch=20, cex=0.75)

#APLICAMOS FUNCION PARA REDUCIR LA DISTANCIA ENTRE PUNTOS
#veamos el c√≥digo fuente de la funci√≥n

ReduceSpatialClustering #las columnas de coordenadas deben llamarse "latitude" y "longitude"

Acyt$latitude <- Acyt$decimalLatitude
Acyt$longitude <- Acyt$decimalLongitude

#apliquemos la funci√≥n a los datos
Acyt_cor <- ReduceSpatialClustering(data=Acyt, minimum.distance=distancia.minima)

#TERMINAMOS EL PLOT
points(points(Acyt_cor$longitude,Acyt_cor$latitude, col="red", cex=0.75)) #en este caso comprobamos que no se pierde deamsia densidad (de hecho, no se pierde nada pq deben estar ya m·s alejados de 1km).

#decidimos aplicar la funciÛn a la matriz pq no se pierden demasiados datos

Acyt <- Acyt_cor


#LIMPIEZA DE DUPLICADOS EN LAS COORDENADAS
#---------------------------------------------------------------------------------------------
#buscamos registros duplicados en las coordenadas
duplicados<-duplicated(Acyt[ , c("latitude", "longitude")])
#¬øcuantos duplicados hay?
length(duplicados[duplicados==TRUE])
length(duplicados[!duplicados==TRUE])## solo hay 2323 puntos no duplicados!!! lo podemos comprobar con
sort(Acyt$longitude)
sort(Acyt$latitude)

Acyt_dupl<-Acyt[!duplicados, ]

plot(vars_def[[1]], col="gray80", ext=c(-10, 6, 35, 45))
points(Acyt$longitude,Acyt$latitude, pch=20, cex=0.75)
points(Acyt_dupl$longitude,Acyt_dupl$latitude, cex=0.7, col="green")#confirmamos que todo es correcto

Acyt <- Acyt_dupl
rm(Acyt_dupl)

#comprobamos que no han quedado duplicados (solo para cultivar vuestra f√©)
duplicados<-duplicated(Acyt[ , c("latitude", "longitude")])
length(duplicados[duplicados==TRUE])

#cuantas presencias tenemos
nrow(Acyt)


#### TENEMOS LOS DATOS LIMPIOS!!!

#guardando a png
png("C:/Taller 1/Ant cyt only.png", width=1200, height=1200, pointsize=30)
plot(vars_def[[1]], main="Anthyllis cytisoides presence", col="gray80", cex.main=0.7)
points(Acyt$longitude,Acyt$latitude, pch=20, cex=0.75, col="green")
dev.off()


### GUARDAMOS PARA MAXENT!
###------------------------------------------------------------------------------------------------

#LLEGADO ESTE PUNTO, YA TENEMOS LAS PRESENCIAS PARA TRABAJAR CON MAXENT, LAS GUARDAMOS
#crea presencia maxent
Anthyllis_cytisoides.maxent<-data.frame(Acyt$scientificName, Acyt$longitude, Acyt$latitude)

#le pone nombres correctos de las columnas
names(Anthyllis_cytisoides.maxent)<-c("Species","Lon","Lat")
head(Anthyllis_cytisoides.maxent)

#guardamos la tabla en carpeta para maxent

write.table(Anthyllis_cytisoides.maxent, file="C:/Taller 1/maxent/Anthyllis cytisoides.csv", sep=",", row.names=FALSE, quote=FALSE)

##### TERMINAMOS LA TABLA
##-------------------------------------------------------------------------------------------------

#TERMINAMOS DE PREPARAR LA TABLA DE PRESENCIA, ELIMINANDO ALGUNAS COLUMNAS QUE NO SIRVEN, Y A√ëADIMOS LA COLUMNA DE PRESENCIA CON UNOS.
#ESTO LO HACEMOS PARA QUE LA ESTRUCTURA DE LA TABLA DE PRESENCIA ENCAJE CON LA ESTRUCTURA DE LAS TABLAS DE BACKGROND, PSEUDOAUSENCIAS Y AUSENCIAS
#cambiamos latitude y longitude por y y x (m√°s corto)

Acyt$presence<-1

Acyt_def<-data.frame(Acyt$scientificName, Acyt$longitude, Acyt$latitude, Acyt$presence, Acyt$bio3_eu, Acyt$bio4_eu, Acyt$bio8_eu, Acyt$bio9_eu, Acyt$bio12_eu,Acyt$bio15_eu)
names(Acyt_def)<-c("specie","x","y", "presence", "bio3","bio4","bio8", "bio9", "bio12", "bio15")
head(Acyt_def)

Acyt <- Acyt_def
rm(Acyt_def)

#guardamos tambi√©n la tabla original de presencia
write.table(Acyt, file="C:/Taller 1/Anthyllis cytisoides_only.csv", sep=";", row.names=FALSE, quote=FALSE)

#recuperamos algo de memoria RAM borrando objetos que no vamos a usar mas
rm(celdas.vacias, distancia.minima, duplicados,res.grados)
gc()



############################# PREPARACI”N BACKGROUN Y PSEUDOAUSENCIA ###################################
########################################################################################################


help(randomPoints)

########### GENERAMOS LOS PUNTOS DE BACKGROUND

# para determinar el n˙mero de puntos de background debemos saber primero el n˙mero de celdas de nuestro raster

ncell(vars_def[["bio8_eu"]]) #en este caso hay 216000

#se suele considerar que el background es suficiente con el 10 o 20% del total de puntos.
# unos 20000 en este caso. Aunque en bibliografÌa se considera suficiente con 10000
background <- randomPoints(mask=vars_def, n=20000)

str(background)

#no es un data.frame, lo transformamos en data.frame
background<-data.frame(background)

#extraemos los valores de las variables sobre los puntos, convirtiendo a data.frame
background.vars<-data.frame(extract(vars_def, background))

#unimos las coordenadas con los valores de las variables
background<-cbind(background, background.vars)

#le a√±adimos la columna de presencia
background$presence<-0

#le a√±adimos la columna de presencia
background$specie<- "Anthyllis cytisoides"

background<-data.frame(background$specie, background$x, background$y, background$presence, background$bio3_eu, background$bio4_eu, background$bio8_eu, background$bio9_eu, background$bio12_eu,  background$bio15_eu)
names(background)<-c("specie","x","y", "presence", "bio3","bio4","bio8", "bio9", "bio12", "bio15")
head(background)


#unimos las presencias y el background en una ˙nica tabla

Acyt_def <- read.csv("C:/Taller 1/Anthyllis cytisoides_only.csv", sep=";", dec=".")

Acyt_presencia.background<-rbind(Acyt_def, background)

#guardamos la tabla

write.table(Acyt_presencia.background, "C:/aller 1/Anthyllis cytisoides_pback.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=";", dec=".")

#borramos objetos que no necesitamos
rm(background)
gc()

#guardando a png

png("C:/Taller 1/Acyt_presence_background.png", width=1200, height=1200, pointsize=30)
plot(variables[[1]], main="Background", col="gray80")
points(Acyt_presencia.background[Acyt_presencia.background$presence==0, ]$x, Acyt_presencia.background[Acyt_presencia.background$presence==0, ]$y, pch=20, cex=0.01, col="gray40")
points(Acyt_presencia.background[Acyt_presencia.background$presence==1, ]$x, Acyt_presencia.background[Acyt_presencia.background$presence==1, ]$y, pch=20, cex=0.3, col="red")
dev.off()


#################################        GENERAMOS LAS PSEUDOAUSENCIAS
###################################################################################################################


#usamos presencia.completa en lugar de presencia porque presencia.completa tiene todas las presencias posibles.
#sin embargo, queremos tantas pseudo-ausencias como presencias hay en presencia, de ahÌ (n=nrow(presencia))

Acyt_complet<-read.csv("C:/Taller 1/datos GBIF/Anthyllis cytisoides p.csv",sep=";", dec=".")## rescatamos y  preparamos la tabla de Acyt_complet

Acyt_complet$decimalLatitude[is.na(Acyt_complet$decimalLatitude)] <- Acyt_complet$decimalLatitude.1[is.na(Acyt_complet$decimalLatitude)]
Acyt_complet$decimalLongitude[is.na(Acyt_complet$decimalLongitude)] <- Acyt_complet$decimalLongitude.1[is.na(Acyt_complet$decimalLatitude)]


### QUITAMOS LAS FILAS QUE NO TENGAN COORDENADAS


Acyt_complet<-Acyt_complet[!is.na(Acyt_complet$decimalLatitude), ]
Acyt_complet<-Acyt_complet[!is.na(Acyt_complet$decimalLongitude), ]

unique(is.na(Acyt_complet$decimalLongitude))

### PREPARAMOS DATOS DE PSEUDOAUSENCIA
##-------------------------------------------------------------------------------------------------------------------

pseudoausencia <- randomPoints(mask=vars_def, n=nrow(Acyt_def), p=Acyt_complet[ , c("decimalLatitude","decimalLongitude")], excludep=TRUE)
str(pseudoausencia)## fijese en que queremos tantas pseudo-ausencias como presencias hay en presencia, de ahÌ (n=nrow(presencia))

#no es un data.frame, lo transformamos en data.frame
pseudoausencia<-data.frame(pseudoausencia)

#extraemos los valores de las variables sobre los puntos, convirtiendo a data.frame
pseudoausencia.variables<-data.frame(extract(vars_def, pseudoausencia))

#unimos las coordenadas con los valores de las variables
pseudoausencia<-cbind(pseudoausencia, pseudoausencia.variables)

#le aÒadimos la columna de presencia
pseudoausencia$presence<-0

#le aÒadimos la columna de especie
pseudoausencia$specie<- "Anthyllis cytisoides"


pseudoausencia<-data.frame(pseudoausencia$specie, pseudoausencia$x, pseudoausencia$y, pseudoausencia$presence, pseudoausencia$bio3_eu, pseudoausencia$bio4_eu, pseudoausencia$bio8_eu, pseudoausencia$bio9_eu, pseudoausencia$bio12_eu,  pseudoausencia$bio15_eu)
names(pseudoausencia)<-c("specie","x","y", "presence", "bio3","bio4","bio8", "bio9", "bio12", "bio15")
head(pseudoausencia)

#unimos las presencias y el background en una √∫nica tabla
Acyt_presencia.pseudoausencia<-rbind(Acyt_def, pseudoausencia)

#guardamos la tabla
write.table(Acyt_presencia.pseudoausencia, "C:/Taller 1/Anthyllis cytisoides_pseudo.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=";", dec=".")

#borramos objetos que no necesitamos
rm(pseudoausencia, pseudoausencia.variables)
gc()

#RECUERDA EL NOMBRE DE LA TABLA: Xxxx_presencia.pseudoausencia!!!

#guardando a png
png("C:/Taller 1/Acyt_presence_pseudoab.png", width=1200, height=1200, pointsize=30)
plot(vars_def[[1]], main="Pseudoausencia", col="gray80")
points(Acyt_presencia.pseudoausencia[Acyt_presencia.pseudoausencia$presence==0, ]$x, Acyt_presencia.pseudoausencia[Acyt_presencia.pseudoausencia$presence==0, ]$y, pch=20, cex=0.3, col="gray40")
points(Acyt_presencia.pseudoausencia[Acyt_presencia.pseudoausencia$presence==1, ]$x, Acyt_presencia.pseudoausencia[Acyt_presencia.pseudoausencia$presence==1, ]$y, cex=0.3, col="red")
dev.off()


##### THIS IS ALL!!!!!!!!


