#Análisis exploratorio de datos

#librerías
library(reshape2)
library(ggplot2)
library(dplyr)
library(outliers)
require(car)
library(nortest)
library(GGally)
#Fijamos directorio
base_m <- "C:/Users/avalo/OneDrive/Documentos/PIB_MUNI/codigos_finales/modified_sources"


#Directorio
setwd(base_m)

#########################################################################################
#Gráfica de cleveland
#Llamamos a las bases de datos estatales
estados_lw <- read.csv("estados_trans_ln.csv", header = T, stringsAsFactors = F)

#Quitamos 2019
estados_lw <- subset(estados_lw, estados_lw$anio != 2019)

estados_lw <- estados_lw[,c("anio", "id_edo", "Entidad.x", "pib", "pot", "pbt")]

aux <- estados_lw[order(estados_lw$anio, estados_lw$pib),]

aux$orden_datos <- rep(1:32, times=5)

aux <- melt(aux, id.vars = c(1:3,7))

ggplot(aux, aes(x=orden_datos, y=value))+
  geom_point()+
  ggtitle("Gráfica de Cleveland de los datos")+
  facet_grid(variable~anio)

##############################################################################################
#Gráfica de cajas

ggplot(aux, aes(X="", y=value))+
  geom_boxplot()+
  ggtitle("Gráficas de cajas para las variables pib, pbt y pot")+
  facet_grid(anio~variable)

##############################################################################################
#Prueba estadística outliers
library(outliers)
#Líneas 285 a la 349
#pruebas 
rm(list = c("aux"))


ppib <- list()
ppbt <- list()
ppot <- list()
target <- unique(estados_lw$anio)


for (i in 1:5) {
  # i=1
  aux <- subset(estados_lw, estados_lw$anio == target[[i]])
  ppib[[i]] <- grubbs.test(aux$pib, type = 10, opposite = FALSE, two.sided = TRUE)
  ppbt[[i]] <- grubbs.test(aux$pbt, type = 10, opposite = FALSE, two.sided = TRUE)
  ppot[[i]] <- grubbs.test(aux$pot, type = 10, opposite = FALSE, two.sided = TRUE)
  
}

#Generamos un cuadro de resultados

#pvalue

pib_p_value <- c(ppib[[1]]$p.value, ppib[[2]]$p.value, ppib[[3]]$p.value, ppib[[4]]$p.value, 
                 ppib[[5]]$p.value)

pbt_p_value <- c(ppbt[[1]]$p.value, ppbt[[2]]$p.value, ppbt[[3]]$p.value, ppbt[[4]]$p.value, 
                 ppbt[[5]]$p.value)

pot_p_value <- c(ppot[[1]]$p.value, ppot[[2]]$p.value, ppot[[3]]$p.value, ppot[[4]]$p.value, 
                 ppot[[5]]$p.value)


#estadisticos
pib_est <- rbind.data.frame(ppib[[1]]$statistic, ppib[[2]]$statistic, ppib[[3]]$statistic, ppib[[4]]$statistic, 
                            ppib[[5]]$statistic)

pbt_est <- rbind.data.frame(ppbt[[1]]$statistic, ppbt[[2]]$statistic, ppbt[[3]]$statistic, ppbt[[4]]$statistic, 
                            ppbt[[5]]$statistic)

pot_est <- rbind.data.frame(ppot[[1]]$statistic, ppot[[2]]$statistic, ppot[[3]]$statistic, ppot[[4]]$statistic, 
                            ppot[[5]]$statistic)

colnames(pib_est)<-c("G","U")
colnames(pbt_est)<-c("G","U")
colnames(pot_est)<-c("G","U")
#ha

pib_ha <- c(ppib[[1]]$alternative , ppib[[2]]$alternative, ppib[[3]]$alternative, ppib[[4]]$alternative, 
            ppib[[5]]$alternative)

pbt_ha <- c(ppbt[[1]]$alternative, ppbt[[2]]$alternative, ppbt[[3]]$alternative, ppbt[[4]]$alternative, 
            ppbt[[5]]$alternative)

pot_ha <- c(ppot[[1]]$alternative, ppot[[2]]$alternative, ppot[[3]]$alternative, ppot[[4]]$alternative, 
            ppot[[5]]$alternative)
#Unimos

Ppib <- cbind.data.frame(target, pib_est, pib_p_value, pib_ha)

Ppbt <- cbind.data.frame(target, pbt_est, pbt_p_value, pbt_ha)

Ppot <- cbind.data.frame(target, pot_est, pot_p_value, pot_ha)

rm(list = c("aux", "i" , "pbt_est", "pbt_ha", "pbt_p_value", "pib_est", "pib_ha", "pib_p_value",
            "pot_est", "pot_ha", "pot_p_value", "ppbt", "Ppbt", "ppib", "Ppib", "ppot", "Ppot",
            "target"  ))

##########################################################################################################
#Estadísitca varianza
aux <- melt(estados_lw, id.vars = 1:3)

a <- aux %>% group_by(anio, variable) %>% summarise(mediana = median(value), media=mean(value), varianza=var(value), desv_est=sd(value))

rm(list=c("a","aux"))

##########################################################################################################
#Pruebas de igualdad de varianzas 

#Generamos una tabla donde el nombre de las variables se vuelva una variable nueva
a <- melt(estados_lw, id.vars=1:3)

a$fvariable <- a$variable

#Creamos una lista vacía para cada prueba
plevene <- list()
pfligner <- list()

#Generamos el vector de años
target <- unique(a$anio)


#Creamos un ciclo para realizar las pruebas por año
for (i in 1:length(target)){
  #i=1
  prueba <- subset(a, a$anio== target[[i]])
  #prueba de levene
  plevene[[i]] <- leveneTest(value ~ fvariable, data = prueba,kruskal.test = TRUE) 
  # prueba de f-k
  pfligner[[i]] <-  fligner.test(value ~ fvariable, data = prueba)
}

#Extraemos el resultado de la prueba
DF_Levene <- c(plevene[[1]]$Df,plevene[[2]]$Df,plevene[[3]]$Df,plevene[[4]]$Df,plevene[[5]]$Df)
F_value_Levene <- c(plevene[[1]]$`F value`, plevene[[2]]$`F value`,plevene[[3]]$`F value`,plevene[[4]]$`F value`,plevene[[5]]$`F value`)
p_value_Levene <- c(plevene[[1]]$`Pr(>F)`,plevene[[2]]$`Pr(>F)`,plevene[[3]]$`Pr(>F)`,plevene[[4]]$`Pr(>F)`,plevene[[5]]$`Pr(>F)`)

df_Fligner_Killen <- c(pfligner[[1]]$parameter, pfligner[[2]]$parameter, pfligner[[3]]$parameter, pfligner[[4]]$parameter, pfligner[[5]]$parameter)
Xi_2_Fligner_Killen <- c(pfligner[[1]]$statistic,pfligner[[2]]$statistic,pfligner[[3]]$statistic,pfligner[[4]]$statistic,pfligner[[5]]$statistic)
p_value_Fligner_Killen <- c(pfligner[[1]]$p.value,pfligner[[2]]$p.value,pfligner[[3]]$p.value,pfligner[[4]]$p.value,pfligner[[5]]$p.value)

#Los unimos en un df
prueba2 <- cbind.data.frame(target,df_Fligner_Killen, Xi_2_Fligner_Killen,p_value_Fligner_Killen)

rm(list = c("a","pfligner","plevene","prueba","prueba2","df_Fligner_Killen","DF_Levene","F_value_Levene",
            "p_value_Fligner_Killen","p_value_Levene","target","Xi_2_Fligner_Killen","i"))

##################################################################################################
#Normalidad visual
#Creamos un nvo data frame con melt
pba <- melt(estados_lw, id.vars=1:3)
#Creamos un vector con los años
target <- unique(pba$anio)
varies <- unique(pba$variable)

#gruardamos param originales de margenes
margen <- par("mar")

#Primero partimos la ventana de gráficas y modificamos márgenes
par(mfrow=c(1,2))

#Creamos un ciclo para gráficar
for(i in 1:length(target)){
  for (j in 1:length(varies)) {
    #i=1
    #j=1
    par(mfrow=c(1,2))
    aux <- subset(pba, pba$anio == target[[i]] & pba$variable== varies[[j]])
    #Hacemos el histograma de la variable
    hist(aux$value, freq = F, border = "gray50", main=paste("Histograma", as.vector(as.matrix(aux[1,4])), target[[i]], sep=" ")) 
    lines(density(aux$value), lwd = 2) 
    curve(dnorm(x, mean(aux$value), sd(aux$value)), lwd = 2, col = "blue", add = T) 
    legend("topright", c("curva observada", "curva (normal) teórica"), lty = 1, lwd = 2, col = c("black", "blue"), bty = "n", cex = 0.6)
    
    #Encuentra los cuartiles asociados a la distribución normal del pib y los gráfica 
    qqnorm(aux$value)
    qqline(aux$value)
      }
  
}

#Restablecemos ventana
par(mar=margen, mfrow=c(1,1))
rm(list=c("aux", "pba","i","j", "margen","target","varies"))


##########################################################################################
#Pruebas de normalidad

#Creamos listas vacías para rellenar con las pruebas
ppib <- list()
ppot <- list()
ppbt <- list()

#Creamos vector de años
target <- unique(estados_lw$anio)

for(i in 1:length(target)){
  aux <- subset(estados_lw, estados_lw$anio==target[[i]])
  ppib[[i]]<- lillie.test(aux$pib)
  ppot[[i]]<- lillie.test(aux$pot)
  ppbt[[i]]<- lillie.test(aux$pbt)
}

#Construimos tabla de resultados

anio <- target
D_pib <- c(ppib[[1]]$statistic, ppib[[2]]$statistic, ppib[[3]]$statistic, ppib[[4]]$statistic, ppib[[5]]$statistic)
pvalue_pib <- c(ppib[[1]]$p.value, ppib[[2]]$p.value, ppib[[3]]$p.value, ppib[[4]]$p.value, ppib[[5]]$p.value)

D_pot <- c(ppot[[1]]$statistic, ppot[[2]]$statistic, ppot[[3]]$statistic, ppot[[4]]$statistic, ppot[[5]]$statistic)
pvalue_pot <- c(ppot[[1]]$p.value, ppot[[2]]$p.value, ppot[[3]]$p.value, ppot[[4]]$p.value, ppot[[5]]$p.value)

D_pbt <- c(ppbt[[1]]$statistic, ppbt[[2]]$statistic, ppbt[[3]]$statistic, ppbt[[4]]$statistic, ppbt[[5]]$statistic)
pvalue_pbt <- c(ppbt[[1]]$p.value, ppbt[[2]]$p.value, ppbt[[3]]$p.value, ppbt[[4]]$p.value, ppbt[[5]]$p.value)


aux <- cbind.data.frame(anio,D_pib, pvalue_pib, D_pot, pvalue_pot, D_pbt,pvalue_pbt)

#Borramos
rm(list=c("anio", "aux", "D_pbt", "D_pib", "D_pot", "i", "ppbt", "ppib", "ppot", "pvalue_pbt",
          "pvalue_pib", "pvalue_pot", "target"  ))

##################################################################################################
#Colinealidad

ggpairs(estados_lw, columns = 4:6, ggplot2::aes(colour=as.factor(anio))) 

#################################################################################################
#Autocorrelaciones

#Años 
target <- unique(estados_lw$anio)

#Generamos gráficas

for(i  in 1:length(target)) {
  #plot.new()
  aux <- subset(estados_lw, estados_lw$anio == target[[i]])
  acf(aux$pib, main=paste("pib", target[[i]], sep=" "))
  
}

rm(list = ls())
