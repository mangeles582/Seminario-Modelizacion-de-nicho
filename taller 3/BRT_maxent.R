

##########################          SPECIES DISTRIBUTION MODELS             #####################################


library(rgdal)
library(tree)
library(party)
library(raster)
library(sp)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(plotmo) #LIBRERIA PARA VISUALIZACION DE CURVAS DE RESPUESTA
library(mgcv) #OTRA LIBRERIA PARA GAM
library(randomForest) #RANDOM FOREST
library(gbm)#BOSTED REGRESSION TREE
#library(gam)#GENERALIZED ADDITIVE MODELS
library(rJava)
library(png)
library(DHARMa)


################# BRT #################################


## VARIABLES PERIODO DE REFERENCIA

dir <- "C:/Taller 2/raster/"
lista.variables <- list.files(path=dir, full.names=TRUE)# igual me sobra
vars_def <- c(paste0(dir,"bio3_eu"), paste0(dir,"bio4_eu"),paste0(dir,"bio8_eu"),paste0(dir,"bio9_eu"),paste0(dir,"bio12_eu"), paste0(dir, "bio15_eu"))
variables<-brick(stack(vars_def))
names(variables) <- c("bio3", "bio4", "bio8", "bio9", "bio12", "bio15")
plot(variables[["bio3"]])

## VARIABLES PERIODO DE ANOMALÍA (AÑO HIDROLOGICO 2013-2014)

dir.sequia <- "C:/Taller 2/murcia anomalia/"
lista.variables <- list.files(path=dir.sequia, full.names=TRUE)# igual me sobra
vars_sequia <- c(paste0(dir.sequia,"bio3.tif"), paste0(dir.sequia,"bio4.tif"),paste0(dir.sequia,"bio8.tif"),paste0(dir.sequia,"bio9.tif"),paste0(dir.sequia,"bio12.tif"), paste0(dir.sequia, "bio15.tif"))
variables_sequia<-brick(stack(vars_sequia))
plot(variables_sequia[["bio3"]])



  especie <- read.table(paste0("C:/Taller 3/data/Rosmarinus officinalis_pseudo_training.csv"), sep=";", dec=".", header = T)
  especie <- especie[!is.na(especie$bio8), ]# no se en que momento se produjo el fallo pero en todas las tablas hay 18 filas con valor na para bio8. Este es el camino más rápido para solucionarlo

  tc <- if (nrow(especie)<500) 3 else 5
  especie.tc5.lr005 <- gbm.step(data=especie, gbm.x=5:10, gbm.y=4, family="bernoulli",
                                tree.complexity=tc, learning.rate=0.01, bag.fraction=0.75)
  
  png(paste0("C:/Taller 3/Rosmarinus_BRT_variable influence.png"), width=3000, height=2000, pointsize=50)
  summary(especie.tc5.lr005, main="Rosmarinus", cex.main=0.9)
  dev.off() 
  
  png(paste0("C:/Taller 3/Rosmarinus_response curves.png"), width=3000, height=2000, pointsize=50)
  plotmo( especie.tc5.lr005)#visreg no se por qué no funciona
  dev.off()
  
  #PREDICCION GEOGRAFICA REFERENCE PERIOD
  n.arboles <- if(especie.tc5.lr005$gbm.call$best.trees>1000)1000 else "nothing"
  brt.ref<-predict(variables, especie.tc5.lr005, n.trees=n.arboles, type="response")
  par(mfrow=c(1,1))
  dev.off()
  plot(brt.ref, main=paste0("BRT.pseudoabsence "), cex.main=0.8)

  #PREDICCION GEOGRAFICA ANOMALY PERIOD
  brt.sequia<-predict(variables_sequia,especie.tc5.lr005, n.trees=n.arboles, type="response")
  plot(brt.sequia, main=paste0("BRT.pseudoabsence"), cex.main=0.8)

  # GUARDAMOS EL RASTER DE IDONEIDAD PARA LA ESPECIE antes y despues de la sequía
  
  writeRaster(brt.ref, filename= paste0("C:/Taller 3/Rosmarinus_brt_ref.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(brt.sequia, filename= paste0("C:/Taller 3/Rosmarinus_brt_sequia.tif"), format="GTiff", overwrite=TRUE)
  
  
  #EVALUAMOS EL MODELO (cronologicamente deberia hacerse después de calibrar el modelo con datos del periodo de referencia, pero así ahorro espacio)
  
  especie_test <- read.csv(paste0("C:/Taller 3/data/Rosmarinus officinalis_pseudo_test.csv"), header=TRUE, sep=";", de=".")
  
  especie_test$idoneidad <- extract(brt.ref, especie_test[, c("x","y")])
  valores.presencias<-especie_test[especie_test$presence==1, "idoneidad"]
  valores.ausencias<-especie_test[especie_test$presence==0, "idoneidad"]
  
  evaluacion<-evaluate(p=valores.presencias, a=valores.ausencias)#p=presence points, a=absence points
  evaluacion
  
  par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
  density(evaluacion)
  boxplot(evaluacion, col=c("blue", "red"))
  plot(evaluacion, "ROC")
  par(mfrow=c(1,1))
  
gc()

mess.anomalia<-mess(x=variables_sequia, v=especie[, names(variables)], full=TRUE)
#no vienen los nombres en el grÃ¡fico, pero son estos
names(mess.anomalia)<-c(names(variables), "mess")
plot(mess.anomalia)# es practicamente idéntico al de MaxEnt, y comparando el mapa de mess con el de bio 15, vemos que son iguales, ergo, la que más extrapola es bio15 (estacionalidad de la precip), que sin embargo, por ser una variable importante no podemos quitar



########### MAXENT ###########

dir <- "C:/Taller 2/raster/"
lista.variables <- list.files(path=dir, full.names=TRUE)# igual me sobra
vars_def <- c(paste0(dir,"bio3_eu"), paste0(dir,"bio4_eu"),paste0(dir,"bio8_eu"),paste0(dir,"bio9_eu"),paste0(dir,"bio12_eu"), paste0(dir, "bio15_eu"))
variables<-brick(stack(vars_def))
names(variables) <- c("bio3", "bio4", "bio8", "bio9", "bio12", "bio15")
plot(variables[["bio3"]])

## VARIABLES PERIODO DE ANOMALÍA (AÑO HIDROLOGICO 2013-2014)

dir.sequia <- "C:/Taller 2/murcia anomalia/"
lista.variables <- list.files(path=dir.sequia, full.names=TRUE)# igual me sobra
vars_sequia <- c(paste0(dir.sequia,"bio3.tif"), paste0(dir.sequia,"bio4.tif"),paste0(dir.sequia,"bio8.tif"),paste0(dir.sequia,"bio9.tif"),paste0(dir.sequia,"bio12.tif"), paste0(dir.sequia, "bio15.tif"))
variables_sequia<-brick(stack(vars_sequia))
plot(variables_sequia[["bio3"]])

dir.create(paste0("C:/TAller 3/MAXENT_R/")) #to create all directories for save the species results
  

#jar <- paste(system.file(package="dismo"), "F:/TESIS/CURSOS/curso GBIF (mar 2016)/material_curso_modelos/taller2/maxent/maxent.jar", sep='')# necesario antes de correr MaxEnt la primera vez  
# so importat too. Put (manualmente), maxent.jar in the java folder of this package. In thisc case: C:\Users\Mariangeles\Documents\R\win-library\3.3\dismo\java
# info settings: https://groups.google.com/forum/#!topic/maxent/yRBlvZ1_9rQ


  especie <- read.table(paste0("C:/Taller 3/data/Rosmarinus officinalis_pseudo_training.csv"), sep=";", dec=".", header = T)
  especie <- especie[!is.na(especie$bio8), ]# no se en que momento se produjo el fallo pero en todas las tablas hay 18 filas con valor na para bio8. Este es el camino más rápido para solucionarlo
  
  especie <- data.frame(cbind(especie$x, especie$y))
  colnames(especie) <- c("lon","lat")
  
  especie.bg <- read.table(paste0("F:/TESIS/CAP TFM/occurence data/europe/training data/presence-background/", sp[i],"_pback_training", ".csv"), sep=";", dec=".", header = T)
  
  bg <- especie.bg[!especie.bg$presence==1, ]
  bg <- data.frame(cbind(bg$x, bg$y))
  colnames(bg) <- c("lon", "lat")
  
  # CORREMOS EL MODELO
  
  maxentout <- paste0("outputdirectory=F:/TESIS/CAP TFM/modelos/MAXENT/", sp[i])
  
  me <- maxent(variables, p=especie, a=bg, args=c("betamultiplier=3", "threshold=FALSE", "writeplotdata=TRUE",
                                                  "replicates=10", "replicatetype=crossvalidate", "outputgrids=FALSE","appendtoresultsfile=TRUE", "responsecurves=TRUE","jackknife=TRUE", maxentout))
  me# resulta que con el comando anterior guarda los archivos pero no guarda el modelo, y lo necesito
  
  me <- maxent(variables, p=especie, a=bg, args=c("betamultiplier=3", "outputgrids=FALSE","threshold=FALSE", "replicates=10", "replicatetype=crossvalidate","jackknife=FALSE", "responsecurves=FALSE"))
  
  # PLOT CURVAS DE RESPUESTA EN LA MISMA IMAGEN
  # curvas de respuesta con interacción
  
  response_bio3 <- read.table(paste0("F:/TESIS/CAP TFM/modelos/MAXENT/", sp[i], "/plots/species_bio3.dat"), sep=",", dec=".", header=T)
  response_bio4 <- read.table(paste0("F:/TESIS/CAP TFM/modelos/MAXENT/", sp[i], "/plots/species_bio4.dat"), sep=",", dec=".", header=T)

  par(mfrow=c(1,1))
  
  ### curvas de respuesta variables sin interacción
  
  response_bio3 <- read.table(paste0("F:/TESIS/CAP TFM/modelos/MAXENT/", sp[i], "/plots/species_bio3_only.dat"), sep=",", dec=".", header=T)
  response_bio4 <- read.table(paste0("F:/TESIS/CAP TFM/modelos/MAXENT/", sp[i], "/plots/species_bio4_only.dat"), sep=",", dec=".", header=T)

  
  #PREDICCION GEOGRAFICA REFERENCE PERIOD
  maxent.ref <- predict(me, variables, args=c( "doclamp=TRUE", "writeclampgrid=TRUE"))# tragicamente hay que usar logistic output para tener los resultados en tanto por 1, las otras dos opciones estan relacionadas con dar menos peso a los puntos extrapolados. y son las que aparecen por defecto en MaXent.
  maxent.ref.avg <- mean(maxent.ref[["layer.1"]],maxent.ref[["layer.2"]],maxent.ref[["layer.3"]],
                         maxent.ref[["layer.4"]],maxent.ref[["layer.5"]],maxent.ref[["layer.6"]],
                         maxent.ref[["layer.7"]],maxent.ref[["layer.8"]],maxent.ref[["layer.9"]],
                         maxent.ref[["layer.10"]])
  
  plot(maxent.ref.avg, main= paste0("MAXENT ", sp[i]), cex.main=0.8)
  points(zona_estudio, pch=19, col="red", cex=0.8)
  #points(especie[especie$presence==1,"x"], especie[especie$presence==1,"y" ], pch=19, cex=0.2, col="grey3")
  
  #PREDICCION GEOGRAFICA ANOMALY PERIOD
  maxent.sequia <- predict(me, variables_sequia, args=c( "doclamp=TRUE", "writeclampgrid=TRUE"))
  maxent.sequia.avg <- mean(maxent.sequia[["layer.1"]],maxent.sequia[["layer.2"]],maxent.sequia[["layer.3"]],
                            maxent.sequia[["layer.4"]],maxent.sequia[["layer.5"]],maxent.sequia[["layer.6"]],
                            maxent.sequia[["layer.7"]],maxent.sequia[["layer.8"]],maxent.sequia[["layer.9"]],
                            maxent.sequia[["layer.10"]])
  
  
  plot(maxent.sequia.avg, main=paste0("MAXENT", sp[i]), cex.main=0.8)
  points(zona_estudio, pch=19, col="red", cex=0.8) #sino hubiesen muchos es interesante verlo. Fines exploratorios, el mapa se ve mejor sin ellos
  
  # OBTENEMOS IDONEIDAD ZONAS DE MUESTREO

  # GUARDAMOS EL RASTER DE IDONEIDAD PARA LA ESPECIE antes y despues de la sequía
  
  writeRaster(maxent.ref.avg, filename= paste0("F:/TESIS/CAP TFM/modelos/MAXENT/raster idoneidad/",sp[i], "_ref.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(maxent.sequia.avg, filename= paste0("F:/TESIS/CAP TFM/modelos/MAXENT/raster idoneidad/",sp[i], "_sequia.tif"), format="GTiff", overwrite=TRUE)
  
  #EVALUAMOS EL MODELO (cronologicamente deberia hacerse después de calibrar el modelo con datos del periodo de referencia, pero así ahorro espacio)
  
  especie_test <- read.csv(paste0("F:/TESIS/CAP TFM/occurence data/europe/test data/presence-background/", sp[i], "_pback_test.csv"), header=TRUE, sep=";", de=".")
  especie_test <- especie_test[!is.na(especie_test$bio8), ]
  
  especie_test$idoneidad <- extract(maxent.ref.avg, especie_test[, c("x","y")])
  valores.presencias<-especie_test[especie_test$presence==1, "idoneidad"]
  valores.ausencias<-especie_test[especie_test$presence==0, "idoneidad"]
  
  evaluacion<-evaluate(p=valores.presencias, a=valores.ausencias)#p=presence points, a=absence points
  evaluacion
  
  par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
  density(evaluacion)
  boxplot(evaluacion, col=c("blue", "red"))
  plot(evaluacion, "ROC")


gc()
