#Unamos los resultados encontrados 

#Llamamos a la base de rl
base_rl <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_rl/pib_mun_est_rl.csv", header = T, stringsAsFactors = F)
base_sur <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_sur/pib_mun_est_sur.csv", header = T, stringsAsFactors = F)
base_pd <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_pd/pib_mun_est_pd_fijo.csv", header = T, stringsAsFactors = F)

#Renombramos columnas

colnames(base_rl)[8]<- "PIB_mun_rl"
colnames(base_sur)[8]<- "PIB_mun_sur"
colnames(base_pd)[8]<- "PIB_mun_dp"

#Unimos en una super base

pib_mun <- merge(base_rl, base_sur, by=c("anio", "id_edo", "id_mun"), all=T)

#Arreglamos
pib_mun <- pib_mun[,c("anio", "id_edo", "id_mun", "Entidad.x", "Municipio.x", "PIB_ENT",
                      "PIB_mun_rl",  "PIB_mun_sur")]

#Unimos con la base faltante
pib_mun <- merge(pib_mun, base_pd, by=c("anio", "id_edo", "id_mun"), all=T)

#Arreglamos
pib_mun <- pib_mun[,c("anio", "id_edo","id_mun","Entidad", "Municipio","PIB_ENT",
                      "PIB_mun_rl","PIB_mun_sur", "PIB_mun_dp" )]

#borramos cosas
rm(list = c("base_pd", "base_rl", "base_sur"))

#Llamamos a los errores de la estimación estatal
#Llamamos a la base de rl
base_rl <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_rl/errores_w_rl.csv", header = T, stringsAsFactors = F)
base_sur <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_sur/errores_w_sur.csv", header = T, stringsAsFactors = F)
base_pd <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_pd/errores_w_efectos_fijos.csv", header = T, stringsAsFactors = F)

#Creamos una columna nueva
base_rl$method <- rep("Linear regression", times=length(base_rl$X))
base_sur$method <- rep("SUR", times=length(base_sur$X))
base_pd$method <- rep("Panel Data", times=length(base_pd$X))

#Los unimos en una sola base de datos
errores_w <- rbind.data.frame(base_rl, base_sur, base_pd)
errores_w <- errores_w[,-c(1)]

#borramos cosas
rm(list = c("base_pd", "base_rl", "base_sur"))

#Llamamos a los errores de la estimación estatal
#Llamamos a la base de rl
base_rl <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_rl/resultados_rl.csv", header = T, stringsAsFactors = F)
base_sur <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_sur/resultados_sur.csv", header = T, stringsAsFactors = F)
base_pd <- read.csv("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados_pd/res_efectos_mixtos.csv", header = T, stringsAsFactors = F)

#Tratamos la base
base_rl <- base_rl[,c("anio","a" ,"alpha_1", "alpha_2")]
base_sur <- base_sur[,c("anio", "a", "alpha_1", "alpha_2")]

anio  <- unique(base_sur$anio)
a <- rep(0, times=length(anio))
alpha_1 <- rep(base_pd[1,2], times=length(anio))
alpha_2 <- rep(base_pd[2,2], times=length(anio))

base_pd <- cbind.data.frame(anio, a, alpha_1, alpha_2)

#Agregamos columnas
#Creamos una columna nueva
base_rl$method <- rep("Linear regression", times=length(base_rl$anio))
base_sur$method <- rep("SUR", times=length(base_sur$anio))
base_pd$method <- rep("Panel Data", times=length(base_pd$anio))

#Unimos
reg_est <- rbind.data.frame(base_rl, base_sur,base_pd)

#Guardamos en una carpeta las bases de resultados
setwd("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados")

write.csv(pib_mun, "pib_mun.csv")
write.csv(errores_w, "errores_mod.csv")
write.csv(reg_est, "regresiones.csv")

rm(list=ls())
