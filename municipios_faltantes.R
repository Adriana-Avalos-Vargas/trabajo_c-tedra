#FIJAMOS
#Directorios
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"
setwd(base_m)
#Llamamos base estatal logaritmo(w)
############################################################################################
#Llamamos a la base de datos municipal a w y a ln
base_mlw <- read.csv("municipios_trans_ln.csv", header = T, stringsAsFactors = FALSE)

#Quitamos el 2019
base_mlw <- subset(base_mlw, base_mlw$anio != 2019)

#Se dejan municipios con NA'S o infinitos
aux <- subset(base_mlw, is.na(base_mlw$pbt) == T)
aux2 <- subset(base_mlw, is.infinite(base_mlw$pbt) ==  T)

aux <- rbind.data.frame(aux, aux2)

#Limpiamos cols
municipios_sin <- aux[,c("anio", "id_edo", "id_mun", "Entidad","Municipio")]

#Guardamos
setwd(res_rl)
write.csv(municipios_sin, "municipios_sin_estimación.csv")

rm(list = c("aux", "aux2", "base_mlw", "municipios_sin"))
