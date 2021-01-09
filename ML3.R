#------ Analyzing results in R: Bioclimatic zones in Iberian Peninsula --------#


library(raster)
library(readxl)
library(sp)
library(dplyr)
library(purrr)
library(rgdal)
library(maptools)
library(ggplot2)
library(sf)
library(ggpubr)
library(rgeos)


# https://rspatial.org/raster/intr/index.html

#read raster and renaming
mypath1 <- '/Users/Sergi/Desktop/My folder/research/CC_IP/data/Present/'
currentfiles<- list.files(mypath1, pattern = ".tif$", full.names = TRUE)

wc30seg <- stack(currentfiles)

names(wc30seg) <- c("a.m.t", 
                    "m.d.r", 
                    "iso", 
                    "t.s", 
                    "max.t.wm", 
                    "min.t.cm", 
                    "t.a.r",
                    "m.t.wetq",       
                    "m.t.dryq",
                    "m.t.warmq",
                    "m.t.coldq", 
                    "a.pre",
                    "pre.wetm", 
                    "pre.drym", 
                    "pre.s", 
                    "pre.wtq", 
                    "pre.dryq", 
                    "pre.warmq", 
                    "pre.coldq")  #Renaming variables

names(wc30seg)


#shapefile 
shape <- readOGR('/Users/Sergi/Desktop/My folder/research/CC_IP/data/countries_shp/countries.shp')
class(shape)
names(shape)

esp <- shape[shape$ISO2 == 'ES', ]
pt <- shape[shape$ISO2 == 'PT', ]

mask1 <- esp + pt
plot(mask1)

mask1$dis <- 1 

mask2 <- unionSpatialPolygons(mask1,mask1$dis)

plot(mask2)

mask3 <- disaggregate(mask2) 
plot(mask3[78])

#Getting points on raster
#extract IP bioclim data
IP_data<- crop(wc30seg,mask3[78])
IP_data <- mask(IP_data, mask3[78])
plot(IP_data)

points <-rasterToPoints(IP_data) %>% as.data.frame()

#---------- POST CLUSTERING ------------------------------- #


library(dplyr)

#r1eading results
hie <- read.csv("/Users/Sergi/Desktop/My folder/UPMthings/1ercuatri/ML/Bloque 3/Exercise/reults/predictions.csv")
knn <- read.csv("/Users/Sergi/Desktop/My folder/UPMthings/1ercuatri/ML/Bloque 3/Exercise/reults/predictions_knn.csv")
gmm <- read.csv("/Users/Sergi/Desktop/My folder/UPMthings/1ercuatri/ML/Bloque 3/Exercise/reults/predictions_gmm.csv")

colnames(knn)[4] <- "predictionKnn"
colnames(gmm)[5] <- "predictionGmm"

#reading original data
IP_data_r <- read.csv("/Users/Sergi/Desktop/My folder/research/CC_IP/data/IP_data")

#Joining dfs
IP_data_labeledhie <- cbind(IP_data_r,hie)
IP_data_labeledKNN <- cbind(IP_data_labeledhie, knn)
IP_data_labeled <- cbind(IP_data_labeledKNN, gmm)

#cleaning df
IP_data_labeled$X <- NULL
IP_data_labeled$X <- NULL
IP_data_labeled$ID <- NULL
IP_data_labeled$features <- NULL
IP_data_labeled$X <- NULL
IP_data_labeled$probability <- NULL
IP_data_labeled$X <- NULL 
IP_data_labeled$features <- NULL 
IP_data_labeled$ID <- NULL

colnames(IP_data_labeled)

IP_data_labeled$prediction = as.integer(IP_data_labeled$prediction)
IP_data_labeled$predictionKNN = as.integer(IP_data_labeled$predictionKNN)
IP_data_labeled$predictionGMM = as.integer(IP_data_labeled$predictionGMM)

#analysis on pyspark with this file
write.csv(IP_data_labeled, file = "labeledIPDATA.csv")


#---------------------------------------------------------------------------#
#visualizing points with condition

#cliping results, joining points df with  IP_data and labels knn gmm EM.

finaldf <- IP_data_r
finaldf$posx <- points$x
finaldf$posy <- points$y
finaldf$hie <- IP_data_labeled$prediction
finaldf$knn <- IP_data_labeled$predictionKnn
finaldf$gmm <- IP_data_labeled$predictionGmm

finaldf[finaldf == 0] <- 7

#plot

hie_distri <- ggplot(data = finaldf) +
  geom_point(mapping = aes(x = posx, y = posy), colour = finaldf$hie)

plot(hie_distri)

knn_distri <- ggplot(data = finaldf) +
  geom_point(mapping = aes(x = posx, y = posy), colour = finaldf$knn)

plot(knn_distri)

gmm_distri <- ggplot(data = finaldf) +
  geom_point(mapping = aes(x = posx, y = posy), colour = finaldf$gmm)

plot(gmm_distri)
