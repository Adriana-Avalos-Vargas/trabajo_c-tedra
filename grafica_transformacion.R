#Gr�ficas y tablas de la transformaci�n de los datos
#Directorio
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"

#Fijamos directorio
setwd(base_m)

############################################################################################
#Gr�fica estatal millones 2013

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
  ggtitle("G1: Gr�fica de las variables de censo econ�mico estatal")+
  facet_grid(variable~anio,scales = "free_y" )

aux2

rm(list = c("aux", "aux2", "estados"))

############################################################################################
#Gr�fica municipal millones 2013

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
  ggtitle("G2: Gr�fica de las variables de censo econ�mico estatal")+
  facet_grid(variable~anio,scales = "free_y" )

aux2

rm(list = c("aux", "aux2", "municipio"))


############################################################################################
#Gr�ficas dispersi�n variable transformadas a w

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
  ggtitle("Gr�fica de dispersi�n de las variables transformadas a escala estatal ")+
  facet_grid(anio~variable, scales = "free_x")

despues

rm(list = c("aux", "despues", "estados"))

############################################################################################
#Gr�ficas dispersi�n variable transformadas a w municipales

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
  ggtitle("Gr�fica de las variables transformadas del censo econ�mico municipal")+
  facet_grid(variable~anio,scales = "free_y" )

#Presentamos las gr�ficas unidas
despues

rm(list = c("aux", "despues","municipios"))
