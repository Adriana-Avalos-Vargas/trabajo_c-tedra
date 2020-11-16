#Análisis del PIB a escala municipal estimado

#Fijamos el directorio
setwd("C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados")

#Llamamos las bases de datos
pib_mun <- read.csv("pib_mun.csv", header = T, stringsAsFactors = F)
errores <- read.csv("errores_mod.csv", header=T, stringsAsFactors = F)
modelos <- read.csv("regresiones.csv", header = T, stringsAsFactors = F)

#Llamamos a las librerías para gráficar y para remodelar las bases de datos
library(reshape2)
library(ggplot2)
library(dplyr)


#Sacamos las medias

mu <-pib_mun %>% group_by(anio) %>% 
  summarise(media_rl= mean(PIB_mun_rl), media_sur = mean(PIB_mun_sur), media_pd = mean(PIB_mun_dp), 
            var_rl = var(PIB_mun_rl), var_sur = var(PIB_mun_sur), var_pd = var(PIB_mun_dp), 
            sd_rl = sd(PIB_mun_rl), sd_sur = sd(PIB_mun_sur),sd_pd = sd(PIB_mun_dp),
            cv_rl = sd(PIB_mun_rl)/mean(PIB_mun_rl),cv_sur = sd(PIB_mun_sur)/mean(PIB_mun_sur),
            cv_pd = sd(PIB_mun_dp)/mean(PIB_mun_dp))

dato_tr <- melt(pib_mun, id.vars = 1:7)

mu_nvo <-dato_tr %>% group_by(anio, variable) %>% 
  summarise(media_pib= mean(value))
            

aux <- merge(dato_tr, mu_nvo, by=c("anio", "variable"))            
#Generamos histogramas
target <- unique(dato_tr$anio)

ggplot(aux, aes(x=value)) + 
  geom_histogram(colour="black", fill="white", bins=15)+
  geom_vline(aes(xintercept=media_pib), color="blue", linetype="dashed", size=1)+
  facet_grid(variable~anio, scales = "free_x")

##############################################
#BUSQUEMOS LOS OUTLIERS

my_outlier<- function(vector_iqr) {
  val_vector <- vector_iqr
  q3 <- quantile(val_vector)
  q3 <- q3[4]
  IQRs <- IQR(val_vector)
  val_out <- q3+1.5*IQRs
  return(val_out)
}

dato_tr$super_id <- paste(dato_tr$variable, dato_tr$anio, sep="_")
target <- unique(dato_tr$super_id)

for(i in i:length(target)){
  i=1
  aux <- subset(dato_tr, dato_tr$super_id == target[[i]])
  vector_iqr <- as.vector(as.matrix(aux$value))
  valor <- my_outlier(vector_iqr)
  aux2 <- subset(aux, aux$value >= valor)
  aux3 <- subset(aux, aux$value < valor)
}

#########Esto no
#Contrastemos con pruebas paramétricas y no paramétricas
ggplot(dato_tr, aes(x=as.factor(variable), y=value, color=as.factor(variable)))+
  geom_boxplot()+
  facet_grid(anio~., scales = "free_y")

#Prruebas
target <- unique(pib_mun$anio)

pruebas_w <- list()

for (i in 1:length(target)) {
  aux <- subset(dato_tr, dato_tr$variable != "PIB_mun_dp" & dato_tr$anio == target[i])
  rl_sur <-wilcox.test(value ~ variable, data = aux, paired = TRUE)
  aux <- subset(dato_tr, dato_tr$variable != "PIB_mun_sur")
  rl_dp <-wilcox.test(value ~ variable, data = aux, paired = TRUE)
  aux <- subset(dato_tr, dato_tr$variable != "PIB_mun_rl")
  sur_dp <-wilcox.test(value ~ variable, data = aux, paired = TRUE)
  prueba <- list(rl_sur, rl_dp, sur_dp)
  pruebas_w[[i]] <- prueba
}

pruebas_w
prueba[[1]]
