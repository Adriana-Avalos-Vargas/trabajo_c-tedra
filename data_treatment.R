#####################################################################################################
#####################################################################################################
#Código que sólo incluye la estandarización de datos, no es parte del archivo rmd

#########################################################################################
#Fijamos directorio
base_o <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/original_sources"
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"

#########################################################################################
#Librerias
library(dplyr)
library(reshape2)
library(ggplot2)

#########################################################################################
#Llamamos a las bases de datos
setwd(base_o)
pib <- read.csv("pib_est.csv",header = TRUE,stringsAsFactors = FALSE)
saic_e <- read.csv("saic_est.csv",header = TRUE, stringsAsFactors = FALSE)
saic_m <- read.csv("saic_mun.csv",header = TRUE, stringsAsFactors = FALSE)
deflactores <- read.csv("deflactores.csv",header = T, stringsAsFactors = FALSE)

#########################################################################################
#Primera estandarización sobre el censo estatal a millones de pesos

#Estandarizamos a millones de pesos (las variables de 1994-2009) estatal
aux <-saic_e %>% filter(anio %in% c(1994,1999,2004,2009))
#El 2014 y 2019 no es necesario pues ya estan en millones
aux2 <- saic_e %>% filter(anio %in% c(2014,2019))

#En el df para 1994, 1999,2004 y 2009 dividimos entre 1000
aux$PBT <- aux$PBT/1000
aux$VACB <- aux$VACB/1000
aux$FBC <- aux$FBC/1000
aux$INV <- aux$INV/1000

#Unimos y reescribimos el archivo
saic_e <- rbind.data.frame(aux, aux2)

#########################################################################################
#Segunda estandarización sobre el censo estatal pesos del 2013

#Renombramos columna
colnames(deflactores)[1]<- "anio"

#Limpiamos df de deflactores
deflactores <- deflactores[,c("anio", "Deflactor")]

#Unimos bases estatal
saic_e <- merge(saic_e, deflactores, by="anio")

#Deflactamos estatal
saic_e$PBT <- (saic_e$PBT/saic_e$Deflactor)*100
saic_e$VACB <- (saic_e$VACB/saic_e$Deflactor)*100
saic_e$FBC <- (saic_e$FBC/saic_e$Deflactor)*100
saic_e$INV <- (saic_e$INV/saic_e$Deflactor)*100

#########################################################################################
#No es una estandarización, es una simple union del PIB con el saic

#Sacamos los años del censo
target <- unique(saic_e$anio)

#Filtramos el PIB
pib <- pib %>% filter(anio %in% target)

#Corregimos longitud del saic_e tiene dos líneas en 2019 extras con NA'S
saic_e <- subset(saic_e, is.na(saic_e$id_edo) == FALSE)

#Unimos 
saic_e <- merge(saic_e, pib, by=c("anio", "id_edo"), all =TRUE)

#Creamos un vector con las columnas que queremos guardar
keep <- c("anio", "id_edo", "Entidad.x", "PIB", "POT", "PBT", "VACB",  "FBC" , "INV")

#Filtramos columnas
saic_e <- saic_e[,keep]

#Dejamos los estados
estados_millones_2013 <- subset(saic_e, saic_e$id_edo != 0)

#Cambiamos de directorio para guardar las modificadas
setwd(base_m)

write.csv(estados_millones_2013, "estatal_millones_2013.csv")

#########################################################################################
#No es una estandarización, es una simple filtro para el nacional
nacional_millones_2013 <- saic_e %>% filter(id_edo %in% 0)

write.csv(nacional_millones_2013, "nacional_millones_2013.csv")


#Borramos cosas
rm(list = c("aux", "aux2", "estados_millones_2013","nacional_millones_2013","pib","target","keep"))

#########################################################################################
#Tercera estandarización sobre el censo estatal pesos del 2013 a variables sin unidades
#dividiendo entre el max

#Quitamos definitivamente los nacionales
saic_e <- subset(saic_e, saic_e$id_edo != 0)

#Extraemos el máximo
aux <- saic_e  %>% group_by(anio) %>% 
  summarise(max_pib = max(PIB, na.rm=FALSE), max_pot = max(POT, na.rm = FALSE), 
            max_pbt = max(PBT, na.rm = FALSE), max_vacb=max(VACB, na.rm = FALSE), 
            max_fbc = max(FBC,na.rm = FALSE), max_inv=max(INV))


#Unimos bases estatal
saic_e <- merge(saic_e, aux, by="anio")

#Dividimos
saic_e$PIBw <- saic_e$PIB/saic_e$max_pib
saic_e$POTw <- saic_e$POT/saic_e$max_pot
saic_e$PBTw <- saic_e$PBT/saic_e$max_pbt
saic_e$VACBw <- saic_e$VACB/saic_e$max_vacb
saic_e$FBCw <- saic_e$FBC/saic_e$max_fbc
saic_e$INVw <- saic_e$INV/saic_e$max_inv

#Generamos vector de nombres columnas
keep <- c("anio", "id_edo","Entidad.x", "PIBw","POTw", "PBTw", "VACBw", "FBCw", "INVw")   

estados_w =saic_e[,keep]

write.csv(estados_w, "estados_trans_max.csv")

rm(list=c("aux","estados_w","keep"))

#########################################################################################
#Cuarta estandarización sobre el censo estatal pesos del 2013 a logarítmo natural
saic_e$pib = log(saic_e$PIBw)
saic_e$pot = log(saic_e$POTw)
saic_e$pbt = log(saic_e$PBTw)
saic_e$vacb = log(saic_e$VACBw)
saic_e$fbc = log(saic_e$FBCw)
saic_e$inv = log(saic_e$INVw)

#Creamos vector de columnas a conservar
keep <- c("anio", "id_edo", "Entidad.x", "pib", "pot", "pbt", "vacb", "fbc", "inv")

estados_trans_ln <- saic_e[,keep]
write.csv(estados_trans_ln, "estados_trans_ln.csv")

#Borramos
rm(list = c("estados_trans_ln", "keep", "saic_e"))


#########################################################################################
#Primera estandarización sobre el censo municipal a millones de pesos

#Estandarizamos a millones de pesos (las variables de 1994-2009) municipal
aux <-saic_m %>% filter(anio %in% c(1994,1999,2004,2009))
#El 2014 y 2019 no es necesario pues ya estan en millones
aux2 <- saic_m %>% filter(anio %in% c(2014,2019))

#En el df para 1994, 1999,2004 y 2009 dividimos entre 1000
aux$PBT <- aux$PBT/1000
aux$VACB <- aux$VACB/1000
aux$FBC <- aux$FBC/1000
aux$INV <- aux$INV/1000

#Unimos y reescribimos el archivo
saic_m <- rbind.data.frame(aux, aux2)


#########################################################################################
#Segunda estandarización sobre el censo MUNICIPAL pesos del 2013


#Unimos bases estatal
saic_m <- merge(saic_m, deflactores, by="anio")

#Deflactamos estatal
saic_m$PBT <- (saic_m$PBT/saic_m$Deflactor)*100
saic_m$VACB <- (saic_m$VACB/saic_m$Deflactor)*100
saic_m$FBC <- (saic_m$FBC/saic_m$Deflactor)*100
saic_m$INV <- (saic_m$INV/saic_m$Deflactor)*100

#Creamos un vector con las columnas que queremos guardar
keep <- c("anio", "id_edo", "id_mun", "Entidad", "Municipio",  "POT", "PBT", "VACB",  "FBC" , "INV")
municipios_millones_2013 <- saic_m[keep]

write.csv(municipios_millones_2013, "municipios_millones_2013.csv")

#Borramos 
rm(list = c("aux", "aux2", "municipios_millones_2013", "keep"))

#########################################################################################

#########################################################################################
#Tercera estandarización sobre el censo estatal pesos del 2013 a variables sin unidades
#dividiendo entre el max


#Extraemos el máximo
aux <- saic_m  %>% group_by(anio) %>% 
  summarise(max_pot = max(POT, na.rm = FALSE), max_pbt = max(PBT, na.rm = FALSE), 
            max_vacb=max(VACB, na.rm = FALSE), max_fbc = max(FBC,na.rm = FALSE), 
            max_inv=max(INV))


#Unimos bases estatal
saic_m <- merge(saic_m, aux, by="anio")

#Dividimos
saic_m$POTw <- saic_m$POT/saic_m$max_pot
saic_m$PBTw <- saic_m$PBT/saic_m$max_pbt
saic_m$VACBw <- saic_m$VACB/saic_m$max_vacb
saic_m$FBCw <- saic_m$FBC/saic_m$max_fbc
saic_m$INVw <- saic_m$INV/saic_m$max_inv

#Generamos vector de nombres columnas
keep <- c("anio", "id_edo","id_mun","Entidad", "Municipio","POTw", "PBTw", "VACBw", "FBCw", "INVw")   

municipios_w =saic_m[,keep]

write.csv(saic_m, "municipios_trans_max.csv")

rm(list=c("aux","municipios_w","keep"))

#########################################################################################
#Cuarta estandarización sobre el censo estatal pesos del 2013 a logarítmo natural
saic_m$pot = log(saic_m$POTw)
saic_m$pbt = log(saic_m$PBTw)
saic_m$vacb = log(saic_m$VACBw)
saic_m$fbc = log(saic_m$FBCw)
saic_m$inv = log(saic_m$INVw)

#Creamos vector de columnas a conservar
keep <- c("anio", "id_edo","id_mun","Entidad", "Municipio", "pot", "pbt", "vacb", "fbc", "inv")

municipios_trans_ln <- saic_m[,keep]
write.csv(municipios_trans_ln, "municipios_trans_ln.csv")

#Borramos
rm(list = c("municipios_trans_ln", "keep", "saic_m","deflactores"))



