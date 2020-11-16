#Gráficas y tablas de la transformación de los datos
#Directorio
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"

#Fijamos directorio
setwd(base_m)

############################################################################################
#Gráfica estatal millones 2013

#Leemos datos estatales en millones de 2013
estados <- read.csv("estatal_millones_2013.csv", header = T, stringsAsFactors = F)

#Ordenamos con un melt cambiamos pib y pot a avariables
aux <- melt(estados, id.vars=c(1:4,8:10))

#Quitamos el 2019
aux <- subset(aux, aux$anio != 2019)

#Graficamos
aux2 <- ggplot(data=aux, aes(x="", y=value, color=as.factor(anio)))+
  geom_boxplot()+ xlab("")+ylab("Millones de pesos 2013 y Personal ocupado")+
  stat_summary(fun =mean, geom="point", shape=23, size=4)+
  theme(legend.position="none", plot.title = element_text(size = 9)) +
  ggtitle("G1: Gráfica de las variables de censo económico estatal")+
  facet_grid(variable~anio,scales = "free_y" )

aux2

rm(list = c("aux", "aux2", "estados"))

############################################################################################
#Gráfica municipal millones 2013

#Llamamos la bd
municipio <- read.csv("municipios_millones_2013.csv", header = TRUE, stringsAsFactors = FALSE)

#Ordenamos con un melt cambiamos pib y pot a avariables
aux <- melt(municipio, id.vars=c(1:6,9:11))

#Quitamos el 2019
aux <- subset(aux, aux$anio != 2019)

#Graficamos
aux2 <- ggplot(data=aux, aes(x="", y=value, color=as.factor(anio)))+
  geom_boxplot()+ xlab("")+ylab("Millones de pesos 2013 y Personal ocupado")+
  stat_summary(fun =mean, geom="point", shape=23, size=4)+
  theme(legend.position="none", plot.title = element_text(size = 9)) +
  ggtitle("G2: Gráfica de las variables de censo económico estatal")+
  facet_grid(variable~anio,scales = "free_y" )

aux2

rm(list = c("aux", "aux2", "municipio"))


############################################################################################
#Gráficas dispersión variable transformadas a w

setwd(base_m)

#Leemos datos estatales en millones de 2013
estados <- read.csv("estados_trans_max.csv", header = T, stringsAsFactors = F)

#Ordenamos con un melt cambiamos pib y pot a avariables
aux <- melt(estados, id.vars=c(1:5,8:10))

#Quitamos el 2019
aux <- subset(aux, aux$anio != 2019)
#Graficamos
despues <-ggplot(data=aux, aes(x=value, y=PIBw))+
  geom_point()+
  theme(plot.title = element_text(size = 9))+
  ggtitle("Gráfica de dispersión de las variables transformadas a escala estatal ")+
  facet_grid(anio~variable, scales = "free_x")

despues

rm(list = c("aux", "despues", "estados"))

############################################################################################
#Gráficas dispersión variable transformadas a w municipales

setwd(base_m)

municipios <- read.csv("municipios_trans_max.csv", header = T, stringsAsFactors = F)

#Graficamos
#Reordenamos el df
aux <- melt(municipios, id.vars = c(1:6, 9:11))

#QUITAMOS 2019
aux <- subset(aux, aux$anio != 2019)

#Graficamos
despues <-ggplot(data=aux, aes(x="", y=value, color=as.factor(anio)))+
  geom_boxplot()+ xlab("")+ylab("Millones de pesos 2013 y Personal ocupado")+
  theme(legend.position="none", plot.title = element_text(size = 9)) +
  ggtitle("Gráfica de las variables transformadas del censo económico municipal")+
  facet_grid(variable~anio,scales = "free_y" )

#Presentamos las gráficas unidas
despues

rm(list = c("aux", "despues","municipios"))
