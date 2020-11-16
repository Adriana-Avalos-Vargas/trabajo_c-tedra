#Panel data

#Directorio
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"
setwd(base_m)

#Llamamos a las bases de datos
base_elw <- read.csv("estados_trans_ln.csv", header = T, stringsAsFactors = F)
base_mlw <- read.csv("municipios_trans_ln.csv", header = T, stringsAsFactors = F)
estados <- read.csv("estatal_millones_2013.csv", header = T, stringsAsFactors = F)

#Limpiamos las bases de datos
#Quitamos el 2019
base_elw <- subset(base_elw, base_elw$anio != 2019)
base_mlw <- subset(base_mlw, base_mlw$anio != 2019)
estados <- subset(estados, estados$anio != 2019)

#Quitamos columnas
base_elw <- base_elw[,c("anio", "id_edo", "Entidad.x", "pib", "pbt", "pot")]
base_mlw <- base_mlw[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "pbt", "pot")]     
estados <- estados[,c("anio", "id_edo", "Entidad.x", "PIB")]

#Renombramos la columna de estados
names(estados)[4] <- "PIB_entidad"

#Llamamos a la librería para las regresiones tipo panel
require(plm)

#Modelos de efectos mixtos r squared between
fe<-plm(pib~pbt+pot, data=base_elw, model="within",index=c("id_edo","anio"))

rfe <-summary(fe)

a <- as.data.frame(rfe$coefficients)
frase_a <- paste("El valor de R^2 es:", rfe$r.squared, "con un p-value de", rfe$fstatistic[[4]], sep = " ")
frase_a

setwd("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_pd")
write.csv(a, "res_efectos_mixtos.csv")

#random effects model; the r-square value is the overall r-square 
#In this model, grade and race were added as between-subjects/case predictors.

re<-plm(pib~pbt+pot, data=base_elw, model="random",index=c("id_edo","anio"))

rre <-summary(re)

b <- as.data.frame(rre$coefficients)

frase_b <- paste("El valor de R^2 es:", rre$r.squared, "con un p-value de:", rre$fstatistic[[4]], sep= " ")
frase_b

write.csv(b, "res_efectos_aleatorios.csv")
###########################################################################################
#Prueba de Hausman
phtest(fe, re)

############################################################################################
#Borramos
rm(list=c("fe","re","rfe","rre", "frase_a","frase_b"))

#Calculamos el pib_estimado a escala estatal con el modelo de efectos fijos
alpha_1 <- a[1,1]
alpha_2 <- a[2,1]

base_elw$pib_estimado <- (alpha_1*base_elw$pbt) + (alpha_2*base_elw$pot)

library(ggplot2)
ggplot(base_elw, aes(x=pib, y=pib_estimado))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",color="darkred")+
  ggtitle("Comparación del pib con el pib estimado estatal")+
  facet_wrap(~anio)

##################################################################################################
#Cálculo del error
aux <- base_elw
aux$error <- aux$pib- aux$pib_estimado
aux$error_abs <- abs(aux$pib- aux$pib_estimado)
aux$error_cuad <- (aux$pib- aux$pib_estimado)**2
aux$error_porc <- abs((aux$pib- aux$pib_estimado)/aux$pib)
aux <- aux[,c("anio","id_edo", "pib", "pib_estimado","error", "error_abs","error_cuad", "error_porc")]

library(dplyr)
errores <- aux %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                MSE = mean(error_cuad), 
                                                RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_w_efectos_fijos.csv")

###################################################################################################
#Estimación del pib municipal 

#1.Borramos objetos
rm(list = c("aux","errores","b"))

#2. Creamos alpha1 y alpha2
alpha_1 <- a[1,1]
alpha_2 <- a[2,1]


#3. Quitamos municipios con inf o NAN
aux <- subset(base_mlw, is.na(base_mlw$pbt)== F)
aux <- subset(aux, is.infinite(aux$pbt) == F)

#4. calculamos el pib = ln(wi)
aux$pib = (alpha_1*aux$pbt) + (alpha_2*aux$pot)

#Sacamos estadística de resumen

resumen <-aux %>% group_by(anio) %>% summarise(min=min(pib, na.rm = TRUE), Q1 =quantile(pib, na.rm = TRUE)[[2]], 
                                               mediana = median(pib,na.rm = TRUE), media=mean(pib, na.rm = TRUE),
                                               Q3= quantile(pib, na.rm=TRUE)[[4]],
                                               max = max(pib,na.rm=TRUE))

ggplot(aux, aes(x=as.factor(anio), y=pib, color=as.factor(anio)))+
  geom_boxplot()+
  xlab("Año")+
  ylab("pib municipal")+
  ggtitle("Gráficas de cajas de la estimación del pib municipal")+
  theme(legend.position = "none")

##############################################################################################
#5. Calculamos wi=Exp(pib_i)
aux$wi = exp(aux$pib)

#6. Se calcula la suma por año y estado de los wi
aux2 <- aux %>% group_by(anio, id_edo) %>% summarise(suma_wi = sum(wi, na.rm = T))

#7. cRUZAMOS CON EL DEL PIB REAL
aux2 <- merge(estados, aux2, by =c("anio", "id_edo"), all = T)

#8. Sacamos el pib municipal máximo
aux2$pib_mun_max <- aux2$PIB_entidad/aux2$suma_wi

#Arreglamos para presentación
lista <- list()

#vector de tiempo
target <- unique(aux2$anio)

for (i in 1:length(target)) {
  aux3 <- subset(aux2, aux2$anio == target[[i]])
  names(aux3)[4] <- paste(names(aux3)[4], target[[i]], sep="-")
  names(aux3)[5] <- paste(names(aux3)[5], target[[i]], sep="-")
  names(aux3)[6] <- paste(names(aux3)[6], target[[i]], sep="-")
  lista[[i]] <- aux3
  
}

#Unimos
resumen <- merge(lista[[1]], lista[[2]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_entidad-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_entidad-1999", "suma_wi-1999", "pib_mun_max-1999")]

#Unimos
resumen <- merge(resumen, lista[[3]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_entidad-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_entidad-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_entidad-2004",
                      "suma_wi-2004",  "pib_mun_max-2004")]

#Unimos
resumen <- merge(resumen, lista[[4]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_entidad-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_entidad-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_entidad-2004",
                      "suma_wi-2004",  "pib_mun_max-2004", "PIB_entidad-2009", "suma_wi-2009", 
                      "pib_mun_max-2009")]

#Unimos
resumen <- merge(resumen, lista[[5]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_entidad-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_entidad-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_entidad-2004",
                      "suma_wi-2004",  "pib_mun_max-2004", "PIB_entidad-2009", "suma_wi-2009", 
                      "pib_mun_max-2009","PIB_entidad-2014", "suma_wi-2014" , "pib_mun_max-2014")]

names(resumen)[2] <- "Entidad"

#########################################################################################################
#Calculo del PIBmun = wi*max(PIB_mun)

#Borramos objetos
rm(list=c("aux3", "i" , "lista", "resumen", "target"))

#Limpiamos aux2 que contiene los máximos del PIB mun
aux2 <- aux2[,c("anio", "id_edo", "pib_mun_max")]

#Unimos con aux que es donde tenemos todas las estimaciones
aux <- merge(aux, aux2, by=c("anio", "id_edo"), all = T)

#Borramos objetos
rm(list = c("aux2"))

#Calculamos el PIBmun
aux$PIB_mun <- aux$wi*aux$pib_mun_max

#Sacamos estadísitica de resumen
resumen <- aux %>% group_by(anio) %>% summarise(min=min(PIB_mun, na.rm = TRUE), Q1 =quantile(PIB_mun, na.rm = TRUE)[[2]], 
                                                mediana = median(PIB_mun,na.rm = TRUE), media=mean(PIB_mun, na.rm = TRUE),
                                                Q3= quantile(PIB_mun, na.rm=TRUE)[[4]],
                                                max = max(PIB_mun,na.rm=TRUE))


#Para grabar csv
#Unimos con el estatal
pib_mun_est <- merge(aux, estados, by = c("anio", "id_edo"), all = T)
#Limpiamos
pib_mun_est <- pib_mun_est[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "PIB_entidad", "PIB_mun" )]

write.csv(pib_mun_est, "pib_mun_est_pd_fijo.csv")

######################################################################################################
#Métricas del error
#borramos cosas
rm(list = c("base_elw", "base_mlw", "pib_mun_est","resumen"))

#Sacamos la suma por entidad
suma <- aux %>% group_by(anio, id_edo) %>% summarise(PIB_ent_est = sum(PIB_mun, na.rm = T))

#uNIMOS CON ESTADOS (EL PIB REAL)
suma <- merge(estados, suma, by =c("anio", "id_edo"), all=T)

#Sacamos el error
suma$error <- suma$PIB_entidad - suma$PIB_ent_est
suma$error_abs <- abs(suma$error)
suma $error_2 <- suma$error ** 2

#Sacamos la métrica
errores <- suma %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                 MSE = mean(error_2), 
                                                 RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_pib_mun_pd_fijos.csv")

rm(list = c("aux", "errores", "estados", "suma","alpha_1","alpha_2") )



#https://www.r-graph-gallery.com/329-hexbin-map-for-distribution.html