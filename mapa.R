#Mapa general y mapita


#directorios
dic_pib <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados"
dic_map <-
dic_res <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/resultados"

##Base de datos con el PIB
#Fijamos el directorio
setwd(dic_pib)



#Llamamos las bases de datos
pib_mun <- read.csv("pib_mun.csv", header = T, stringsAsFactors = F)
#Veamos el mapa
library(rgdal)
library(RColorBrewer)
library(classInt)

#Directorio mapas
dir_map <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/municipios_shp"
setwd(dir_map)
poligonos <- readOGR("mun2019gw.shp")
#plot(poligonos)


#Veamos que tiene poligons
names(poligonos)
#Longitud de poligonos
length(poligonos)
#Ingresar a los datos de poligonos como matriz
poligonos@data
#Similar a df
View(poligonos@data)
#Calculamos los centroides
centroides <- coordinates(poligonos)
centroides #¿En que orden estarán?

#https://www.youtube.com/watch?v=YYh6D8x8qng&t=1071s
poligonitos <- poligonos@data
poligonitos$order <- 1:length(poligonitos$COV_ID)

#Creamos variables id_edo y id_mun 
poligonitos$id_edo <- as.numeric(as.character(poligonitos$CVE_ENT))
poligonitos$id_mun <- as.numeric(as.character(poligonitos$CVE_MUN))


#Unamos por año y guardamos en una lista
target <- unique(pib_mun$anio)

for (i in 1:length(target)) {
  i=1
  pibs = subset(pib_mun, pib_mun$anio == target[[i]])
  suma = sum(pibs$PIB_mun_rl)
  pibs$pib_mun <- pibs$PIB_mun_rl/suma
  #Unimos 
  aux <- merge(poligonitos, pibs, by=c("id_edo", "id_mun"), all=TRUE)
  aux <- aux[order(aux$order),]
  aux <- aux[,c(14,21)]
}
#https://www.youtube.com/watch?v=Jw9dlR3pkLg
#https://www.youtube.com/watch?v=YYh6D8x8qng&t=
#https://www.youtube.com/watch?v=mphvrn-SZIM

var_graf <- as.data.frame(aux[,2])
colnames(var_graf) <- "Var_graf"
row.names(var_graf) <- row.names(poligonos)
poligonos.data <- SpatialPolygonsDataFrame(poligonos, var_graf)

#Gráficas
plotvar <- poligonos.data$Var_graf

nclr <- 5

plotclr <- brewer.pal(nclr, "Blues")

class <- classIntervals(plotvar*100, nclr, style = quantile())
