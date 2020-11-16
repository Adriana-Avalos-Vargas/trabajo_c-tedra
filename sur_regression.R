#Método Sur

#Fijamos directorio
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"
setwd(base_m)

#librerias
library(dplyr)

############################################################################################
#Llamamos a la base de datos estandarizada
base_elw <- read.csv("estados_trans_ln.csv", header = T, stringsAsFactors = FALSE)

#municpal
base_mlw <- read.csv("municipios_trans_ln.csv", header = T, stringsAsFactors = FALSE)

#PIB ESTATAL REAL
estados <- read.csv("estatal_millones_2013.csv", header = T, stringsAsFactors = FALSE)



############################################################################################
#Limpiamos bases
#Quitamos el 2019
base_elw <- subset(base_elw, base_elw$anio != 2019)
base_mlw <- subset(base_mlw, base_mlw$anio != 2019)
estados <- subset(estados, estados$anio != 2019)

#Quitamos columnas que no son de utilidad
base_elw <- base_elw[,c("anio", "id_edo", "Entidad.x", "pib", "pot", "pbt")]
base_mlw <- base_mlw[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "pot", "pbt")]
estados <- estados[,c("anio", "id_edo", "Entidad.x", "PIB")]
names(estados)[4] <-"PIB_estatal"

#############################################################################################
#Generamos regresión lineal
reg_lin <- list()
sreg_lin <- list()
target <- unique(base_elw$anio)

for(i in 1:length(target)){
  aux <- subset(base_elw, base_elw$anio == target[[i]])
  reg_lin[[i]] <- lm(pib~pbt+pot, data=aux)
  sreg_lin[[i]] <- summary(reg_lin[[i]])
  
}

r_1994 <- sreg_lin[[1]]$residuals
r_1999 <- sreg_lin[[2]]$residuals
r_2004 <- sreg_lin[[3]]$residuals
r_2009 <- sreg_lin[[4]]$residuals
r_2014 <- sreg_lin[[5]]$residuals

residuales <- cbind.data.frame(r_1994,r_1999,r_2004,r_2009,r_2014)

#Gráficamos
matriz_cor <- cor(residuales)

#par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))

library(corrplot)

corrplot(matriz_cor, type="lower", method = "square", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75)

######################################################################################################
#Método SUR
#Borramos objetos
rm(list=c("aux", "matriz_cor", "reg_lin", "sreg_lin", "i", "r_1994", "r_1999", "r_2004", "r_2009", "r_2014"))

#Creamos el vector de variables dependientes en independientes por año

#Creamos una función que genera una lista con 3 elementos cada una un vector con las vars dep e indep
sistema <- function(df, year){
  pba <- df %>% filter(anio == year)
  y_pib <- as.vector(as.matrix(pba$pib))
  x_pot <- as.vector(as.matrix(pba$pot))
  x_pbt <- as.vector(as.matrix(pba$pbt))
  lista <- list(y_pib,x_pbt, x_pot)
  return(lista)
  
}

#Creamos los vectores embebidos en la lista

list_1994 <- sistema(base_elw,1994)
list_1999 <- sistema(base_elw,1999)
list_2004 <- sistema(base_elw,2004)
list_2009 <- sistema(base_elw,2009)
list_2014 <- sistema(base_elw,2014)

#Generamos ecuaciones
eq1994 <- list_1994[[1]]~ list_1994[[2]]+list_1994[[3]]
eq1999 <- list_1999[[1]]~ list_1999[[2]]+list_1999[[3]]
eq2004 <- list_2004[[1]]~ list_2004[[2]]+list_2004[[3]]
eq2009 <- list_2009[[1]]~ list_2009[[2]]+list_2009[[3]]
eq2014 <- list_2014[[1]]~ list_2014[[2]]+list_2014[[3]]

#Generamos el sistema de ecuaciones
sis_ec <- list (eq1994 = eq1994, eq1999 = eq1999, eq2004 =eq2004, eq2009 = eq2009, eq2014=eq2014)

#Aplicamos el método SUR
library(systemfit)
sur <- systemfit(sis_ec, method = "SUR", data = base_elw)
rsur <-summary(sur)


#Generamos tabla de resultados

#Vector con años
anio <- unique(base_elw$anio)


#vector de intercepto
a <- c(rsur$coefficients[1,1], rsur$coefficients[4,1], rsur$coefficients[7,1], rsur$coefficients[10,1],rsur$coefficients[13,1])
pv_a <- c(rsur$coefficients[1,4], rsur$coefficients[4,4], rsur$coefficients[7,4], rsur$coefficients[10,4],rsur$coefficients[13,4])
#vector alpha1
alpha_1 <- c(rsur$coefficients[2,1], rsur$coefficients[5,1], rsur$coefficients[8,1], rsur$coefficients[11,1],rsur$coefficients[14,1])
pv_alpha_1 <- c(rsur$coefficients[2,4], rsur$coefficients[5,4], rsur$coefficients[8,4], rsur$coefficients[11,4],rsur$coefficients[14,4])
#vector alpha2
alpha_2 <- c(rsur$coefficients[3,1], rsur$coefficients[6,1], rsur$coefficients[9,1], rsur$coefficients[12,1],rsur$coefficients[15,1])
pv_alpha_2 <- c(rsur$coefficients[3,4], rsur$coefficients[6,4], rsur$coefficients[9,4], rsur$coefficients[12,4],rsur$coefficients[15,4])

#vector_R2 AJUSTADA
R_2_ajustada <- c(0.588197,0.821114  ,0.880025 ,0.911492,0.898662)

#Unimos en un df
resultado <- cbind.data.frame(anio, a, pv_a, alpha_1, pv_alpha_1, alpha_2, pv_alpha_2, R_2_ajustada)

res_sur <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_sur"
setwd(res_sur)
write.csv(resultado, "resultados_sur.csv")

##################################################################################################
#Calculo gráfica de dispersión

rm(list=c("a", "alpha_1", "alpha_2", "eq1994", "eq1999", "eq2004", "eq2009", "eq2014", "list_1994",
          "list_1999", "list_2004", "list_2009", "list_2014", "pv_a", "pv_alpha_1",
          "pv_alpha_2", "R_2_ajustada", "sis_ec", "target", "residuales","sistema"))

#Quitamos columnas de resultado
resultado <- resultado[,c("anio", "a", "alpha_1", "alpha_2")]

#Unimos con base_elw
aux <- merge(base_elw, resultado, by="anio", all=T)

#Calculamos el pib_estatal
aux$pib_estimado = aux$a + (aux$alpha_1*aux$pbt) + (aux$alpha_2 * aux$pot)

#Graficamos
library(ggplot2)

ggplot(aux, aes(x=pib, y= pib_estimado))+
  geom_point()+ geom_abline()+
  ggtitle(paste("Comparación pib real y estimado estatal"))+
  facet_wrap(~anio)

##################################################################################################
#Cálculo del error

aux$error <- aux$pib- aux$pib_estimado
aux$error_abs <- abs(aux$pib- aux$pib_estimado)
aux$error_cuad <- (aux$pib- aux$pib_estimado)**2
aux$error_porc <- abs((aux$pib- aux$pib_estimado)/aux$pib)
aux <- aux[,c("anio","id_edo", "pib", "pib_estimado","error", "error_abs","error_cuad", "error_porc")]

errores <- aux %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                   MSE = mean(error_cuad), 
                                                   RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_w_sur.csv")

###################################################################################################
#Estimación del pib municipal 

#1.Borramos objetos
rm(list = c("aux","errores","rsur","sur", "anio"))

#2. Cruzamos el cuadro de resultado con la base_mlw (municipal)
aux <- merge(base_mlw, resultado, by ="anio", all=T)

#3. Quitamos municipios con inf o NAN
aux <- subset(aux, is.na(aux$pbt)== F)
aux <- subset(aux, is.infinite(aux$pbt) == F)

#4. calculamos el pib = ln(wi)
aux$pib = aux$a +(aux$alpha_1*aux$pbt) + (aux$alpha_2*aux$pot)

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
aux2$pib_mun_max <- aux2$PIB_estatal/aux2$suma_wi

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
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_estatal-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_estatal-1999", "suma_wi-1999", "pib_mun_max-1999")]

#Unimos
resumen <- merge(resumen, lista[[3]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_estatal-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_estatal-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_estatal-2004",
                      "suma_wi-2004",  "pib_mun_max-2004")]

#Unimos
resumen <- merge(resumen, lista[[4]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_estatal-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_estatal-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_estatal-2004",
                      "suma_wi-2004",  "pib_mun_max-2004", "PIB_estatal-2009", "suma_wi-2009", 
                      "pib_mun_max-2009")]

#Unimos
resumen <- merge(resumen, lista[[5]], by ="id_edo", all = T)
#lIMPIAMOS
resumen <- resumen[,c("id_edo","Entidad.x.x", "PIB_estatal-1994", "suma_wi-1994", "pib_mun_max-1994",
                      "PIB_estatal-1999", "suma_wi-1999", "pib_mun_max-1999", "PIB_estatal-2004",
                      "suma_wi-2004",  "pib_mun_max-2004", "PIB_estatal-2009", "suma_wi-2009", 
                      "pib_mun_max-2009","PIB_estatal-2014", "suma_wi-2014" , "pib_mun_max-2014")]

names(resumen)[2] <- "Entidad"

#########################################################################################################
#Calculo del PIBmun = wi*max(PIB_mun)

#Borramos objetos
rm(list=c("aux3", "i" , "lista", "res_sur", "resultado", "resumen", "target"))

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
pib_mun_est <- pib_mun_est[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "PIB_estatal", "PIB_mun" )]

write.csv(pib_mun_est, "pib_mun_est_sur.csv")

######################################################################################################
#Métricas del error
#borramos cosas
rm(list = c("aux2", "base_elw", "base_mlw", "pib_mun_est","resumen"))

#Sacamos la suma por entidad
suma <- aux %>% group_by(anio, id_edo) %>% summarise(PIB_ent_est = sum(PIB_mun, na.rm = T))

#uNIMOS CON ESTADOS (EL PIB REAL)
suma <- merge(estados, suma, by =c("anio", "id_edo"), all=T)

#Sacamos el error
suma$error <- suma$PIB_estatal - suma$PIB_ent_est
suma$error_abs <- abs(suma$error)
suma $error_2 <- suma$error ** 2

#Sacamos la métrica
errores <- suma %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                MSE = mean(error_2), 
                                                RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_pib_mun_sur.csv")

rm(list = c("aux", "errores", "estados", "suma") )
