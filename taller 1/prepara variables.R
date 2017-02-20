#INTRODUCCION
###################################

#DIRECTORIO DE TRABAJO

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


##########################################################################
#IGUALAR LA RESOLUCI”N Y LA EXTENSI”N DE MAPAS RASTER DE DISTINTO ORIGEN
##########################################################################
#ESTO ES SOLO UN EJEMPLO, ESTAS NO SON LAS VARIABLES QUE VAMOS A UTILIZAR PARA HACER LOS MODELOS

#DESCOMPRIME LOS DATOS
unzip("./0_mascara_region.zip", exdir="./igualar_variables", junkpaths=TRUE)

#importamos mapas

elev <- raster("./igualar_variables/elevacion.asc")
hfp<-raster("./igualar_variables/hfp.asc")
ndvi<-raster("./igualar_variables/ndvi.asc")

#plot
par(mfrow=c(1,3))
plot(elev, main="elevacion")
plot(hfp, main="human footprint")
plot(ndvi, main="ndvi")

#comprobamos extensiÛn
extent(elev)
extent(hfp)
extent(ndvi)

#comprobamos resolucion
xres(elev)
xres(hfp)
xres(ndvi)


#1 - IGUALAMOS RESOLUCION Y EXTENSION
#primero con elev
help(resample)
elev2<-resample(x=elev, y=ndvi, method="bilinear")#se usa ndvi como plantilla porque tiene resoluciÛn m·s gruesa
#comparamos extensiÛn
extent(ndvi)
extent(elev2)
#comparamos resoluciÛn
xres(ndvi)
xres(elev2)
#plot
par(mfrow=c(1,2))
plot(elev, main="elev")
plot(elev2, main="elev2")

#lo hacemos con el siguiente mapa, ya no hacen falta las comprobaciones
hfp2<-resample(x=hfp, y=ndvi, method="bilinear")#la plantilla sigue siendo ndvi

#comprobamos que ha ido bien
par(mfrow=c(1, 3))
plot(ndvi, main="ndvi")
plot(elev2, main="elev2")
plot(hfp2, main="hfp2")


#2 - PREPARAMOS UN MAPA DE CELDAS NULAS (M¡SCARA) COMUNES A TODOS LOS MAPAS (USAMOS MULTIPLICACI”N PARA PROPAGAR)
valores.nulos <- ndvi*elev2*hfp2 #la multiplicaciÛn de todos los mapas propaga los valores nulos
plot(valores.nulos)


#3 - APLICAMOS EL MAPA DE CELDAS NULAS A TODOS LOS MAPAS COMO UNA M?SCARA (mask function)
variables.brick<-brick(ndvi, elev2, hfp2)# la funcion brick agrupa todos los mapas juntos
names(variables.brick)<-c("ndvi", "elev", "hfp")
plot(variables.brick)
#aplicamos la m·scara de valores nulos a variables.brick
help(mask)
variables.brick<-mask(variables.brick, valores.nulos)#aplicas de golpe la m?scara a todas las varibles de variables.brick
plot(variables.brick)


#4 - RECORTE FINAL
#finalmente recortamos el brick con la extensiÛn del mapa con menor extensiÛn, porque nuestro mapa de referencia era el de mayor extensiÛn, pero las ·reas sobrantes han quedado ocultas por las celdas nulas
extent(elev)
extent(hfp) #parece que tiene una extensiÛn menor
variables.brick<-crop(x=variables.brick, y=extent(hfp))#hfp act˙a como plantilla por ser la m·s pequeÒa 
plot(variables.brick)

#5 - EXPORTAMOS LOS MAPAS
#guardamos las variables preparadas al disco duro
writeRaster(variables.brick[["elev"]], filename="./igualar_variables/elev_final.asc", format="ascii", overwrite=TRUE)
writeRaster(variables.brick[["hfp"]], filename="./igualar_variables/hfp_final.asc", format="ascii", overwrite=TRUE)
writeRaster(variables.brick[["ndvi"]], filename="./igualar_variables/ndvi_final.asc", format="ascii", overwrite=TRUE)

#BORRAMOS TODOS LOS MAPAS
rm(elev, elev2, hfp, hfp2, ndvi, valores.nulos, variables.brick)
gc()#limpmiar la memoria RAM




###### SELECCION DE VARIABLES  ########
#######################################

#LISTADO DE VARIABLES
lista.variables <- list.files(path="./variables/",pattern='*.asc', full.names=TRUE)
lista.variables

#stack Y brick PREPARAN LAS VARIABLES EN UN UNICO OBJETO ESPACIAL
variables <- brick(stack(lista.variables))
names(variables)

plot(variables[["bioclim1"]])

# COMPROBAMOS RESOLUCION DE LAS VARIABLES

res.grados<-xres(variables)
res.grados
#en km
res.km<-res.grados*111.19
res.km#aprox 10km


###################### ANALISIS DE CORRELACI”N DE VARIABLES   ##############################


#TRANSFORMA LOS MAPAS EN UNA TABLA
variables.tabla<-as.data.frame(variables) ## con mapas europeos de escala 1x1 para toda europa no es capaz de procesar

#ELIMINA LOS VALORES NULOS
variables.tabla<-na.omit(variables.tabla)

#MATRIZ DE CORRELACIÛN (cor function)
help(cor)
variables.correlacion<-cor(variables.tabla)
variables.correlacion

M <- variables.correlacion

#MATRIZ DE DISTANCIAS ('ABS' = VALOR ABSOLUTO, PARA ELIMINAR CORRELACIONES NEGATIVAS)as.dist convierte una matriz de correlacion en una matriz de distancia
help(as.dist)
help(abs)
variables.dist<-as.dist(abs(M))## se usa el valor absoluto para tener en cuenta tambiÈn las correlaciones negativas. 

#CLUSTER DE VARIABLES SEG√öN LA DISTANCIA (MENOR DISTANCIA = MAYOR CORRELACI√ìN)
help(hclust)
variables.cluster<-hclust(1-variables.dist)

#GRAFICO DEL CLUSTER DE CORRELACIONES 
par(mfrow=c(1,1))
plot(variables.cluster)
abline(h=0.25, col="red")

#GR√ÅFICO DEL CLUSTER DE CORRELACIONES EXPORTADO A PDF
pdf("C:/Taller 1/correlacion_europa.pdf", width=12, height=12, pointsize=20)
plot(variables.cluster)
abline(h=0.25, col="red")
dev.off()

# despuÈs de observar el cluster se han decido seleccionar:

variables.seleccionadas <- c("bioclim2", "bioclim3", "bioclim4", "bioclim8", "bioclim9", "bioclim10","bioclim12", "bioclim15", "bioclim17","bioclim19" )


##############    AN¡LISIS VIF    #######################################

#En este momento debemos transformar las varibles seleccionadas en data.frame para poder hacer el VIF.

variables.tabla<-as.data.frame(variables)
variables.tabla<-na.omit(variables.tabla)

variables.seleccionadas <- c("bioclim2", "bioclim3","bioclim4","bioclim8", "bioclim9", "bioclim10", "bioclim12", "bioclim15","bioclim17", "bioclim19")

#HACEMOS UNA NUEVA TABLA SOLO CON ESAS VARIABLES
variables.tabla2<-variables.tabla[ , variables.seleccionadas]# solo con las variables seleccionadas (que lo habr· hecho seg˙n el cluster)

#PERO PUEDE HABER VARIABLES QUE SON COMBINACIoN LINEAL DE OTRAS VARIABLES...
#CALCULAMOS EL VARIANCE INFLATION FACTOR

resultado.vif<-vif(variables.tabla2)#aplicas la funciÛn vif con las variables seleccioinadas anteriormente. Si el valor es mayor de 5 debemos eliminar la variable por estar correlacionada con otras
resultado.vif

variables.tabla2$bioclim2<-NULL# quitamos bioclim2, bioclim3 tiene m·s significado para mis modelos
resultado.vif<-vif(variables.tabla2)
resultado.vif

variables.tabla2$bioclim10<-NULL# quitamos bioclim10, bio12 tambien es interesante
resultado.vif<-vif(variables.tabla2)
resultado.vif

variables.tabla2$bioclim17<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

variables.tabla2$bioclim19<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif


## una vez obtenidos los resultados del VIF, las agrupamos como variables seleccionadas VIF

vars.vif<-c("bioclim3", "bioclim4", "bioclim8", "bioclim9", "bioclim12", "bioclim15")

variables_VIF<-stack(variables[[vars.vif]])

#ploteamos las variables
pdf("C:/Taller 1/variables VIF europa.pdf", width=12, height=12, pointsize=20)
plot(variables_VIF, main=names(variables_VIF))
dev.off()

#GUARDAMOS LAS VARIABLES PARA PODER USARLAS EN MAXENT (est· de adorno, es mejor hacer manualmente la subcarpeta, cambia la extensiÛn de las capas)

writeRaster(x=variables_VIF, filename="C:/Taller 1/variables/seleccionadas VIF/", suffix=names(variables_VIF), bylayer=TRUE, format="ascii")

## borramos global environment y ganamos espacio en la ram

gc()



