# 5.) Mapa de Calor BSt UM por periodo y morfología 
library(vegan)
library(lattice)
library(ellipse)
library(plotrix)
require(SciViews)
require(stats)
library(cluster)
library("gplots")


datos= read.csv2("datos2.csv",row.names=1)
str(datos)
datos<-na.exclude(datos)
attach(datos)

#Se calculan las medias para cada Especie, para trabajar con datos promedio
datos1<-aggregate(datos[,c(3:6)],na.rm=TRUE, 
                  by=list(TP=TP),mean)
datos1

# Transformar el data frame a una matriz (iris 2)       
datos2<-as.matrix(datos1[,2:5])
datos2

# Se escoge género como la observación a graficar 
rownames(datos2)<-datos1[,1]
datos2


# MAPA DE CALOR OPCIÓN 2
hv <- heatmap(datos2, margins=c(8,8), distfun = vegdist,
              xlab ="Variables de estructura", 
              ylab= "Periodos y Parcelas", main = "")  


# Mapa
x11()
hclust.ave <- function(datos2) hclust(datos2, method="average")
hv <- heatmap(datos2, margins=c(6,6),distfun = vegdist, 
              xlab ="Variables de estructura", hclustfun=hclust.ave,
              ylab= "Periodos y Parcelas", main = "")


hclust.ave <- function(datos2) hclust(datos2, method="average")
x11()
heatmap.2(datos2, scale = "none", col = bluered(100),cexRow = 1, 
          cexCol=1.2,
          trace = "none", density.info = "none",distfun = vegdist,
          margins=c(6,5), hclustfun=hclust.ave, xlab ="Variables Morfométricas", 
          ylab= "Periodos y Parcelas")

# 5.) Mapa de Calor por periodo y morfología
library(vegan)
library(lattice)
library(ellipse)
library(plotrix)
require(SciViews)
require(stats)
library(cluster)
library("gplots")


datos= read.csv2("datos2.csv",row.names=1)
str(datos)
datos<-na.exclude(datos)
attach(datos)

#Se calculan las medias para cada Especie, para trabajar con datos promedio
datos1<-aggregate(datos[,c(3:6)],na.rm=TRUE, 
                  by=list(TP=TP),mean)
datos1

# Transformar el data frame a una matriz (iris 2)       
datos2<-as.matrix(datos1[,2:5])
datos2

# Se escoge género como la observación a graficar 
rownames(datos2)<-datos1[,1]
datos2


# MAPA DE CALOR OPCIÓN 2
hv <- heatmap(datos2, margins=c(8,8), distfun = vegdist,
              xlab ="Variables de estructura", 
              ylab= "Periodos y Parcelas", main = "")  


# Mapa
x11()
hclust.ave <- function(datos2) hclust(datos2, method="average")
hv <- heatmap(datos2, margins=c(6,6),distfun = vegdist, 
              xlab ="Variables de estructura", hclustfun=hclust.ave,
              ylab= "Periodos y Parcelas", main = "")


hclust.ave <- function(datos2) hclust(datos2, method="average")
x11()
heatmap.2(datos2, scale = "none", col = bluered(100),cexRow = 1, 
          cexCol=1.2,
          trace = "none", density.info = "none",distfun = vegdist,
          margins=c(6,5), hclustfun=hclust.ave, xlab ="Variables Morfométricas", 
          ylab= "Periodos y Parcelas")


#--------------------------
# MAPAS DE CALOR - Fito TAXAS (Farid)
library(vegan)
library(lattice)
library(ellipse)
library(plotrix)
require(SciViews)
require(stats)
library(cluster)



#----------------------------
# Mapa de calor 4 farid
datos= read.csv2("Datos3.csv")
str(datos)

# Para el ejercicio se escogen 13 de las 27 variables         
datos2<-as.matrix(datos[,c(2:82)])
datos2
# Se escoge género como la observación a graficar 
rownames(datos2)<-datos[,1]
datos2<-datos2[,1:81]
datos2

# Mapa
x11()
hclust.ave <- function(datos2) hclust(datos2, method="average")
hv <- heatmap(datos2, margins=c(5,5),distfun = vegdist, 
              xlab ="Taxones", hclustfun=hclust.ave,
              ylab= "Localidades", main = "Mapa de Calor")  


x11()
library("gplots")
hclust.ave <- function(datos2) hclust(datos2, method="average")
heatmap.2(datos2, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none",distfun = vegdist,
          margins=c(8,8), hclustfun=hclust.ave)


#-----------------
# Farid
datos= read.csv2("Datos6.csv")
str(datos)

# Para el ejercicio se escogen 13 de las 27 variables         
datos2<-as.matrix(datos[,c(2:6)])
datos2
# Se escoge género como la observación a graficar 
rownames(datos2)<-datos[,1]
datos2<-datos2[,1:5]
datos2

# Mapa
x11()
hclust.ave <- function(datos2) hclust(datos2, method="average")
hv <- heatmap(datos2, margins=c(5,5),distfun = vegdist, 
              xlab ="Grupos Funcionales", hclustfun=hclust.ave,
              ylab= "Periodos - Estaciones", main = "")

x11()
library("gplots")
hclust.ave <- function(datos2) hclust(datos2, method="average")
heatmap.2(datos2, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none",distfun = dist,
          margins=c(5,5), hclustfun=hclust.ave, xlab = "Grupos Funcionales",
          ylab = "Periodos - Estaciones",cex.lab=3)




#------------------
# iNEXT Farid
#cargar paquetes
library(devtools)
library(httr)
library(iNEXT)
library(ggplot2)

#cargar matriz de datos (datos.csv)
datos= read.csv2(file.choose())
datos
str(datos)

#omtir espacios vacios
datos1=na.omit(datos)
datos1

#calculo de hill
out=iNEXT(datos1[,1:2],q=c(0,1,2),datatype="abundance",endpoint=4000)
out
out$AsyEst
write.csv2(out$AsyEst,"Diver.fito.periodos.csv")


#grafico
x11()
datos_plot=ggiNEXT(out, type=1, facet.var="site", color.var="order")
datos_plot + labs(x = "Número de Individuos", y = "Diversidad")


#-----------------
#cargar matriz de datos (datos.csv) Farid
datos= read.csv2(file.choose())
datos
str(datos)

#omtir espacios vacios
datos1=na.omit(datos)
datos1

out=iNEXT(datos1[,1:2],q=c(0,1,2),datatype="abundance",endpoint=4000)
datos_plot = ggiNEXT(out, type=3, facet.var="site")
datos_plot + labs(x = "Cobertura", y = "Diversidad")




#------------
# Tesis Irma
datos= read.csv2("datos.prom.csv",row.names=1)
library(factoextra)

x11()
fviz_pca_ind(res.pca, label="none", habillage=datos$Zona,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")



#---------------------
# Mapa de calor Irma
datos= read.csv2("calor1.csv")
str(datos)
datos
attach(datos)

# Transformar el data frame a una matriz (iris 2)       
datos2<-as.matrix(datos[,2:75])
datos2

# Se escoge género como la observación a graficar 
rownames(datos2)<-datos[,1]
datos2

#
https://earlglynn.github.io/RNotes/package/gplots/heatmap2.html
https://sebastianraschka.com/Articles/heatmaps_in_r.html

cc <- rainbow(ncol(datos2), start=0, end=.3)
rc <- rainbow(nrow(datos2), start=0, end=.3)

hclust.ave <- function(datos2) hclust(datos2, method="average")
x11()
heatmap.2(datos2, col=cm.colors(255), scale="column",
          RowSideColors=rc, ColSideColors=cc, margin=c(5, 10),
          xlab="Especies por cada método", ylab= "Zonas",
          main="", distfun = vegdist, hclustfun=hclust.ave,
          tracecol="green", density="density")


#-------------------
# iNEXT Irma (datos2.)
veget= read.csv2(file.choose())
veget
str(veget)

#omtir espacios vacios
veget1=na.omit(veget)
veget1

#calculo de hill
out=iNEXT(veget1[,2:5],q=c(0,1,2),datatype="abundance",endpoint=8000)
out
out$AsyEst
write.csv2(out$AsyEst,"Diver.veget.csv")

#grafico
veget_plot=ggiNEXT(out, type=1, facet.var="site", color.var="order")
veget_plot + labs(x = "Número de Individuos", y = "Diversidad")





#---------------------------------
# 1) Relación Riqueza - Área - Irma
#==================
https://ciespinosa.github.io/AlphaDiversidad/rarefaccion.html#rarefaccion-basada-en-individuos

# Llamar archivo "datos5.zonas.csv"
p.zonas= read.csv2(file.choose())
str(p.zonas)
# Eliminar factor Región
p.zonas1=p.zonas[,3:123]
str(p.zonas1)

library(vegan)

# 1) Zona Alta
p.alta=p.zonas1[1:52,]
str(p.alta)

x11()
col<-specaccum(p.alta, method = "collector")
plot(col, xlab="Parcelas", ylab="Número de especies", col="blue")
points(col$richness, pch=19, col="darkblue")

# 1.1) Rarefacción por abundancias - z. alta
N <- colSums(p.alta)
N

# Construir vector con los tamaños de la muestra N
p.alta1 <- c(seq(500, 6000, by = 500), sum(N))
p.alta1

# Rarefacción para la zona alta (rar.a)
rar.a <- rarefy(N, sample = p.alta1, se = T, MARG = 2)
rar.a

# 1.2) Rarefacción por tamaño de muestras - z. alta
rand<-specaccum(p.alta, method = "random", permutations=100)
rand

# 1.4) Grafica de estimadores calculados z.alta (rar.a, rand, ace y chaoF)
par(mar=c(6,4,1,1))

# a) Gráfico de rarefacción basada en individuos (eje y: 40 a 75 sp estimadas en rar.a) 
plot(p.alta1, rar.a[1, ], ylab = "Riqueza de especies", 
     axes = FALSE, xlab = "", cex.lab=1.2, 
     type = "l", ylim = c(5, 75), xlim = c(500, 6000), lwd=1.8)
points(p.alta1,rar.a[1, ], pch=19)

# Desviación estándar.
segments(p.alta1, rar.a[1, ] + 2 * rar.a[2, ], 
         p.alta1, rar.a[1, ] - 2 * rar.a[2, ])

# Grafica los ejes y el cuadro
axis(1, at = 1:6 * 1000, cex.axis=0.7,mgp=c(3, 0.2, 0)) 
axis(2, cex.axis=0.7) 
box() 

# b) Gráfico de rarefacción basada en muestras (eje y: 8.7 a 73 sp estimadas en rand)
par(new=T)
#Hacemos un vector con el número de parcelas
x<- 1:52

# figura de los ejes y e x, para muestras (cuadrantes)
plot(x, rand$richness, type="l", col="red",ylab="", xlab="", 
     axes=FALSE, xlim=c(1, 52), ylim = c(5, 75), lwd=1.8)
points(rand$richness, pch=19, col="darkred")
segments(x, rand$richness + 2 * rand$sd, x, rand$richness - 2 * rand$sd, col="red")

# Curva de acumulación basada en cuadrantes o parcelas (curva colector - col)
par(new=T)
plot(col, xlab="", ylab="", col="blue", axes=FALSE, xlim=c(1,55), 
     ylim = c(5, 75), lwd=1.8)
points(col$richness, pch=19, col="darkblue")

axis(1, at=1:55, cex.axis=0.7, line =3, mgp=c(3, 0.2, 0)) 

# Rótulos de escalas en eje x
mtext("No. Individuos", side=1, line=1.3, cex=0.8, at=6)
mtext("No. Cuadrantes", side=1, line=4, cex=0.8, at=6)

# Leyenda de la figura
legend(21, 22, c("Curva de acumulación - Cuadrantes","Rarefacción - Cuadrantes", 
                 "Rarefacción - Individuos"), lty=c(1,1,1), pch=19,
       cex=1, col = c("blue","red", "black"))




#---------------------
# Lirios - Iris
# Ejemplo con lirios

data(iris)
attach(iris)
write.csv2(iris,"lirios.csv")

x11()
plot(iris[Species=="setosa", "Petal.Length"], iris[Species=="setosa",
                                                   "Sepal.Length"], xlim=c(0,8), ylim=c(3,9), pch=19, col="blue", xlab="",
     ylab="", bty="l")

points(iris[Species=="virginica", "Petal.Length"], iris[Species=="virginica",
                                                        "Sepal.Length"],pch=19,col="green")

points(iris[Species=="versicolor","Petal.Length"],iris[Species=="versicolor"
                                                       ,"Sepal.Length"],pch=21,bg="red")

title(main="", xlab="Longitud de
pétalo (cm)", ylab="Longitud de sépalo (cm)", font.main=2, font.lab=2,
      cex.main=1.5)

legend(locator(1), c("setosa","virginica","versicolor"), pch=c(19,19,19),
       col=c("blue","green","red"))




#-----------------------
# Elipses avanzadas Farid
datos=read.csv2("Datos6.csv",row.names=1)

library(corrplot)
## corrplot 0.84 loaded
M <- cor(datos)

#
x11()
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M, lower = "ellipse", upper = "circle")




#---------------
# ADC Nelson (datos4.csv)
library(candisc)

datos<-read.csv2("datos4.csv",row.names=1)
datos<-na.exclude(datos)
attach(datos)

#Modelo de regresión (Se excluye a A y Ar)
mod <- lm(cbind(PPI,pH,MO,Cas,Mgs,Ks,Ps,Fes,Cus,Bs,Nf,Pf,Kf,Caf,Mgf,Sf,Fef,
                Znf,Cuf,Bf,Microp,Macrop) ~ Finca1+Semana, data=datos)

# Peso de las variables x Finca
can <- candisc(mod, term="Finca1",data=datos,ndim=1)
datos$Finca1<-factor(datos$Finca1)
x11()
plot(can,titles.1d = c("Puntuación canónica", "Estructura"))


#------------------------
# PCA Nelson

#
library(ade4)  
pca.est<-scale((datos[,c(7,10:13,15,17,19,20,76:81,83:86,160:164,6)]))
cp<-dudi.pca(pca.est,scannf=F,nf=2)
summary(cp)

datos$Finca<-factor(datos$Finca)
str(datos)
coul <- c("purple","red","green","blue","lightblue","orange","magenta")
x11()
s.class(cp$li,datos$Finca, cell = 0, cstar = 0.5,col = coul)
s.arrow(cp$c1,lab=names(cp$tab))



