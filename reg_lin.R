#Regresiones lineales

#Directorios
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"
res_rl <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_rl"

#FIJAMOS
setwd(base_m)
#Llamamos base estatal logaritmo(w)
############################################################################################
#Llamamos a la base de datos estatal estadarizada a w y a ln
base_elw <- read.csv("estados_trans_ln.csv", header = T, stringsAsFactors = FALSE)
#Llamamos a la base de datos municipal a w y a ln
base_mlw <- read.csv("municipios_trans_ln.csv", header = T, stringsAsFactors = FALSE)
#Base estatal sin transfomrar
ESTADOS <- read.csv("estatal_millones_2013.csv", header = T, stringsAsFactors = FALSE)

############################################################################################
#Limpieza de df

#QUITAMOS COLUMNAS
base_elw <- base_elw[,c("anio", "id_edo", "Entidad.x", "pib", "pot", "pbt" )]
base_mlw <- base_mlw[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "pot", "pbt")]
ESTADOS <- ESTADOS[,c("anio", "id_edo", "Entidad.x", "PIB")]

#Quitamos el 2019
base_elw <- subset(base_elw, base_elw$anio != 2019)
base_mlw <- subset(base_mlw, base_mlw$anio != 2019)
ESTADOS <- subset(ESTADOS, ESTADOS$anio != 2019)

#Renombramoms
names(ESTADOS)[4] ="PIB_ENT"

#extraemos los años
target <- unique(base_elw$anio)

#creamos lista vacía
modelo <- list()
smodelo <- list()

#Creamos ciclo
for(i in 1:length(target)){
  #i=3
  aux <- subset(base_elw, base_elw$anio==target[[i]])
  #Realizamos la regresión
  modelo[[i]] <- lm(pib~pbt+pot, data=aux)
  smodelo[[i]] <- summary(modelo[[i]])
}

#Extraemos los coeficientes
a <- c(smodelo[[1]]$coefficients[1,1], smodelo[[2]]$coefficients[1,1], smodelo[[3]]$coefficients[1,1],
       smodelo[[4]]$coefficients[1,1], smodelo[[5]]$coefficients[1,1])

alpha_1 <- c(smodelo[[1]]$coefficients[2,1], smodelo[[2]]$coefficients[2,1], smodelo[[3]]$coefficients[2,1],
             smodelo[[4]]$coefficients[2,1], smodelo[[5]]$coefficients[2,1])

alpha_2 <- c(smodelo[[1]]$coefficients[3,1], smodelo[[2]]$coefficients[3,1], smodelo[[3]]$coefficients[3,1],
             smodelo[[4]]$coefficients[3,1], smodelo[[5]]$coefficients[3,1])

#Extraemos estadísticos
R_2_multiple <- c(smodelo[[1]]$r.squared, smodelo[[2]]$r.squared, smodelo[[3]]$r.squared, 
                  smodelo[[4]]$r.squared, smodelo[[5]]$r.squared)
p_value_modelo <- c(5.962e-07, 2.217e-14, 2.2e-16, 2.2e-16, 2.2e-16 )

#Unimos en un gran cuadro
anio <- target
aux <- cbind.data.frame(anio, a,alpha_1, alpha_2, R_2_multiple, p_value_modelo)

#Fijamos directorio
setwd(res_rl)

write.csv(aux, "resultados_rl.csv")

##############################################################################################
#Gráfica del modelo
#Gráfica diagnóstico std_error
for(i in 1:length(target)){
  plot(modelo[[i]], which = 1,  main = paste("Estimación pib estatal", target[[i]], sep=" "))
}

##############################################################################################
#Gráfica QQ normalidad residuos

plot.new()
for(i in 1:length(target)){
  plot(modelo[[i]], which = 2,  main = paste("Estimación pib estatal", target[[i]], sep=" "))
}

##############################################################################################
#scale-location 

plot.new()
for(i in 1:length(target)){
  plot(modelo[[i]], which = 3,  main = paste("Estimación pib estatal", target[[i]], sep=" "))
}

#############################################################################################
#Gráfica de dispersión pib estatal real vs estimado
#Veamos con una gráfica de dispersión si se alejan muhco los estimados de los reales a escala estatal
prueba <- NULL

for (i in 1:length(target)) {
  #i=1 
  aux2 <- subset(base_elw, base_elw$anio == target[[i]])
  aux2$pib_est <- modelo[[i]]$fitted.values
  aux2$error <- aux2$pib- aux2$pib_est
  aux2$error_abs <- abs(aux2$pib- aux2$pib_est)
  aux2$error_cuad <- (aux2$pib- aux2$pib_est)**2
  aux2$error_porc <- abs((aux2$pib- aux2$pib_est)/aux2$pib)
  aux2 <- aux2[,c("anio","id_edo", "pib", "pib_est","error", "error_abs","error_cuad", "error_porc")]
  prueba <- rbind.data.frame(prueba, aux2)
  
}

#Graficamos
ggplot(prueba, aes(x=pib, y=pib_est))+
  geom_point()+ geom_abline()+
  ggtitle(paste("Comparación pib real y estimado"))+
  facet_wrap(~anio)

##################################################################################################
#Calculamos los errores por año

errores <- prueba %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                   MSE = mean(error_cuad), 
                                                   RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_w_rl.csv")

##################################################################################################
#Estimación pib municipal
#Borramos cosas
rm(list = c("a", "alpha_1", "alpha_2", "anio", "aux2", "errores", "i", "smodelo",
            "modelo", "p_value_modelo", "prueba", "R_2_multiple", "target"))

#Tabla de coeficientes por año
aux <- aux[,c("anio", "a", "alpha_1", "alpha_2")]

#Se deb quitar municipios con NA'S
base_mlw <- subset(base_mlw, is.na(base_mlw$pbt) == F)
base_mlw <- subset(base_mlw,is.infinite(base_mlw$pbt) == F)

#Unimos con los coefs
base_mlw <- merge(base_mlw, aux, by="anio", all=TRUE)

#Calculamos el PIB
base_mlw$pib <- base_mlw$a + (base_mlw$alpha_1*base_mlw$pbt)+ (base_mlw$alpha_2*base_mlw$pot)

#Encontremos la estadística de resumen

resumen <- base_mlw %>% group_by(anio) %>% summarise(mini=min(pib, na.rm = TRUE), Q1 =quantile(pib, na.rm = TRUE)[[2]], 
                                                     mediana = median(pib,na.rm = TRUE), media=mean(pib, na.rm = TRUE),
                                                     Q3= quantile(pib, na.rm=TRUE)[[4]],
                                                     maxi = max(pib,na.rm=TRUE))

################################################################################################
#Estimación de w
rm(list=c("aux", "resumen"))

#lOS WIS SON LA EXPONENCIAL DEL pib estimado
base_mlw$wis <- exp(base_mlw$pib)

#Estimación del máximo
#1. Se suman los wi's por estado
resumen  <- base_mlw %>% group_by(anio) %>% summarise(mini=min(wis, na.rm = TRUE), Q1 =quantile(wis, na.rm = TRUE)[[2]], 
                                                             mediana = median(wis,na.rm = TRUE), media=mean(wis, na.rm = TRUE),
                                                             Q3= quantile(wis, na.rm=TRUE)[[4]],
                                                             maxi = max(wis,na.rm=TRUE))

###############################################################################################
#Estimación del máximo
#1. Se suman los wis por anio y id_edo
#1. Se suman los wi's por estado
resumen  <- base_mlw %>% group_by(anio, id_edo) %>% summarise(suma_wis=sum(wis, na.rm = TRUE))

#2. Cruzamos con la base de datos del PIB REAL
pib_real <- merge(ESTADOS, resumen, by=c("anio","id_edo"))

#3. Sacamos el pib municipal máximo por entidad y por año
pib_real$PIB_mun_max <- pib_real$PIB_ENT/pib_real$suma_wis

#4. Cruzamos con la estimación base_mlw
pib_mun_est <- merge(base_mlw, pib_real, by=c("anio", "id_edo"), all = T)

#Reordenamos
#Vector de tiempo
target <- unique(pib_real$anio)

PBA <- list()

for (i in 1:length(target)) {
  aux <- subset(pib_real, pib_real$anio == target[[i]])
  names(aux)[4] <- paste("PIB_ENT", target[[i]], sep="_")
  names(aux)[5] <- paste("suma_wis", target[[i]], sep="_")
  names(aux)[6] <- paste("PIB_mun_max", target[[i]], sep="_")
  PBA[[i]] <- aux
}

aux2 <- merge(PBA[[1]], PBA[[2]], by=c("id_edo"), all = T)
aux2 <- aux2[, c("id_edo", "Entidad.x.x", "PIB_ENT_1994", "suma_wis_1994", "PIB_mun_max_1994",
               "PIB_ENT_1999", "suma_wis_1999", "PIB_mun_max_1999")]

aux2 <- merge(aux2, PBA[[3]], by=c("id_edo"), all = T)
aux2 <- aux2[, c("id_edo", "Entidad.x.x", "PIB_ENT_1994", "suma_wis_1994", "PIB_mun_max_1994",
                 "PIB_ENT_1999", "suma_wis_1999", "PIB_mun_max_1999", "PIB_ENT_2004",
                 "suma_wis_2004", "PIB_mun_max_2004")]

aux2 <- merge(aux2, PBA[[4]], by=c("id_edo"), all = T)
aux2 <- aux2[, c("id_edo", "Entidad.x.x", "PIB_ENT_1994", "suma_wis_1994", "PIB_mun_max_1994",
                 "PIB_ENT_1999", "suma_wis_1999", "PIB_mun_max_1999", "PIB_ENT_2004",
                 "suma_wis_2004", "PIB_mun_max_2004",  "PIB_ENT_2009", "suma_wis_2009", 
                 "PIB_mun_max_2009")]

aux2 <- merge(aux2, PBA[[5]], by=c("id_edo"), all = T)
aux2 <- aux2[, c("id_edo", "Entidad.x.x", "PIB_ENT_1994", "suma_wis_1994", "PIB_mun_max_1994",
                 "PIB_ENT_1999", "suma_wis_1999", "PIB_mun_max_1999", "PIB_ENT_2004",
                 "suma_wis_2004", "PIB_mun_max_2004",  "PIB_ENT_2009", "suma_wis_2009", 
                 "PIB_mun_max_2009", "PIB_ENT_2014", "suma_wis_2014", "PIB_mun_max_2014")]

#################################################################################################
#Estimación municipal
rm(list = c("aux", "aux2", "PBA", "pib_real","resumen","i", "target"))

#1. Multiplicamos wi por el maximo
pib_mun_est$PIB_MUN <- pib_mun_est$wis*pib_mun_est$PIB_mun_max

#2. Limpiamos el df
pib_mun_est <- pib_mun_est[,c("anio", "id_edo", "id_mun", "Entidad", "Municipio", "PIB_ENT" , "PIB_MUN" )]


#Pensar como presentar
write.csv(pib_mun_est, "pib_mun_est_rl.csv")

resumen <-pib_mun_est %>% group_by(anio) %>% summarise(min=min(PIB_MUN, na.rm = TRUE), Q1 =quantile(PIB_MUN, na.rm = TRUE)[[2]], 
                                                             mediana = median(PIB_MUN,na.rm = TRUE), media=mean(PIB_MUN, na.rm = TRUE),
                                                             Q3= quantile(PIB_MUN, na.rm=TRUE)[[4]],
                                                             max = max(PIB_MUN,na.rm=TRUE))


#####################################################################################################
#Estimación del error
rm(list = c("base_elw", "base_mlw", "resumen"))

#1. Sumamos por entidad y ano los pib's municipales
suma_pib_mun <- pib_mun_est %>% group_by(anio, id_edo) %>% summarise(PIB_ENT_est = sum(PIB_MUN, na.rm = T))

#2. Cruzamos con la base real

aux <- merge(ESTADOS, suma_pib_mun, by=c("anio", "id_edo"), all = T)

#Sacamos el error
aux$error <- aux$PIB_ENT- aux$PIB_ENT_est
aux$error_abs <- abs(aux$PIB_ENT- aux$PIB_ENT_est)
aux$error_cuad <- (aux$PIB_ENT- aux$PIB_ENT_est)**2
aux$error_porc <- abs((aux$PIB_ENT- aux$PIB_ENT_est)/aux$PIB_ENT)


#Calculamos los errores por año

errores <- aux %>% group_by(anio) %>% summarise(MAD = mean(error_abs),
                                                   MSE = mean(error_cuad), 
                                                   RSE = sum (error), TS = sum(error)/mean(error_abs))

write.csv(errores, "errores_pib_mun_rl.csv")
