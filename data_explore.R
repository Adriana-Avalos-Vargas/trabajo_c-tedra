#This is the part of data exploration

##################################################################################################
#Directorio
base_o <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/original_sources"
setwd(base_o)


#################################################################################################
#Librería
library(dplyr)
library(ggplot2)
library(reshape2)


####################################################################################################
#Llamamos a las bases de datos
pib <- read.csv("pib_est.csv",header = TRUE,stringsAsFactors = FALSE)
saic_e <- read.csv("saic_est.csv",header = TRUE, stringsAsFactors = FALSE)
saic_m <- read.csv("saic_mun.csv",header = TRUE, stringsAsFactors = FALSE)

#Unimos el PIB estatal con los censos económicos estatales
#Primero quitamos na´s
aux <- merge(saic_e, pib, by=c("id_edo", "anio"), all.x = TRUE)

#Nos quedamos con nacionales
saic_n <- subset(aux, aux$id_edo ==0)


############################################################################################
#Unimos los datos estatales

saic_e <- subset(saic_e, saic_e$id_edo !=0)
#Filtramos el PIB por los años censales


#Creamos un vector con los años de los censos
target <- unique(saic_e$anio)

#Filtramos el PIB de acuerdo a los años
entidades <- pib %>% filter(anio %in% target)

#Unimos con la base del saic estatal
entidades <- merge(entidades, saic_e, by=c("id_edo", "anio"), all.y = TRUE)

#Seleccionamos las columnas de interes
entidades <- entidades[,c("id_edo", "Entidad.y", "PIB","POT","PBT","VACB", "FBC", "INV", "anio")]

#Renombramos columnas
colnames(entidades)<- c("id_edo", "Entidad", "PIB","POT","PBT","VACB", "FBC", "INV", "anio")


#Sacamos el resumen a escala estatal por anio
library(reshape2)
aux <- melt(entidades, id.vars=c(1,2,9))

aux2 <- aux %>% group_by(anio,variable) %>% 
  summarise(mini=min(value, na.rm = TRUE), Q1 =quantile(value, na.rm = TRUE)[[2]], 
            mediana = median(value,na.rm = TRUE), media=mean(value, na.rm = TRUE),
            Q3= quantile(value, na.rm=TRUE)[[4]],
            maxi = max(value,na.rm=TRUE), NA_s=sum(is.na(value)))

###########################################################################################
#Datos municipales
#Borramos
rm(list = c("aux", "aux2", "entidades"))

#Quitamos la variable inversión
saic_m <- saic_m[,-c(10)]
#Sacamos el resumen a escala municipal por anio
aux <- melt(saic_m, id.vars = c(1:5,10))
aux2 <- aux %>% group_by(anio,variable) %>% 
  summarise(mini=min(value, na.rm = TRUE), Q1 =quantile(value, na.rm = TRUE)[[2]], 
            mediana = median(value,na.rm = TRUE), media=mean(value, na.rm = TRUE),
            Q3=quantile(value, na.rm=TRUE)[[4]],maxi=max(value,na.rm=TRUE), NA_s=sum(is.na(value)))


##############################################################################################
#Sobre unidades
rm(list=c("aux", "aux2"))
Anio <- unique(saic_e$anio)
PIB <- rep("Millones de pesos precios 2013", times=length(Anio))
PBT <- c("Miles de pesos de 1994","Miles de pesos de 1999","Miles de pesos de 2004","Miles de pesos de 2009", "Millones de pesos 2014", "Millones de pesos 2019")
POT <- rep("Personas", times=length(Anio))

aux <- cbind.data.frame(Anio, PIB, PBT,POT )

#borramos objetos
rm(list = c("pib", "saic_e","saic_m","saic_n", "Anio", "PBT", "PIB", "POT", "aux"))





