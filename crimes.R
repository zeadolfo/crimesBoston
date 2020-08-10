#Read the library I use in the analysis
library(data.table)
library(ggmap)
library(rjson)
library(purrr)
library(rgdal)
library(maptools)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(ggthemes)
library(jsonlite)
library(viridis)
library(scales)
library(geoR)
library(tmap)
library(lubridate)
library(geoRglm)
library(dplyr)
library(doBy)
library(cluster)
library(ggcorrplot)
library(gstat)

source("http://www.sthda.com/upload/rquery_cormat.r")
rm(list = ls())

#Read the data 
setwd("/home/ze/Área de Trabalho/Bases de dados/Crimes Boston/crimesBoston/")
dados <- fread("crime.csv")
mapa <- readOGR("Boston_Neighborhoods.geojson")
pobreza <- fread("pobreza_Boston.csv", header = T, dec = ",")

pobreza <- pobreza[-24]

dados[,.N, by = "STREET"]
dados[, .N, by = "MONTH"][order(MONTH)]
dados[, .N, by = "YEAR"][order(YEAR)]
dados[, .N, by = "DAY_OF_WEEK"][order(DAY_OF_WEEK)]
serie <- dados[, .N, by = c("MONTH", "YEAR")][order(YEAR, MONTH)]
plot(ts(serie[-c(1,40),]$N))
#Apparently in the winter the crimes get low than summer
dados[, .N, by = "OFFENSE_CODE_GROUP"]


#The database is quite clean but there some missing values and -1 values
dados[, .N, by = "Long"][order(Long, decreasing = T)]

dados <- dados[!is.na(Long)]
dados <- dados[Long != -1]

dados_crimenes <- dados[,.N, c("Long", "Lat", "OFFENSE_CODE_GROUP")]

coord <- SpatialPoints(data.frame(Long = dados_crimenes$Long, Lat = dados_crimenes$Lat))
# crimes <- SpatialPointsDataFrame(data.frame(Long = dados_crimenes$Long, Lat = dados_crimenes$Lat))

crimes_reshape <- dcast(data = dados_crimenes, Long + Lat ~ OFFENSE_CODE_GROUP, value.var = "N")

#Count by some interesting variables 

##Which kind of crime is more common in each distric?
crimes_coors <- crimes_reshape[,1:2]
crimes_data <- crimes_reshape[,3:69]

crimes_data <- apply(crimes_data, 2, f <- function(x) ifelse(is.na(x), 0 , x))
crimes_data <- SpatialPointsDataFrame(crimes_coors, data = data.frame(crimes_data), proj4string = CRS(proj4string(mapa)))
dados_tipo <- dados[,.N, by = c("Long", "Lat", "OFFENSE_CODE_GROUP")]
dados_mes <- dados[,.N, by = c("Long", "Lat", "MONTH", "OFFENSE_CODE_GROUP")]
dados_tipo[,N := as.numeric(N)]
dados_mes[,N := as.numeric(N)]

crimes <-  data.frame(Long = dados$Long, Lat = dados$Lat)

plot(mapa)
plot(crimes, add = T, col = "blue")

#We can only see the white areas, i.e., areas did not have crimes

res <- over(crimes_data, mapa)
res <- cbind(res, crimes_data@data)
crimes_dist <- summaryBy(data = res, res[,8:74] ~ Name, FUN = sum)


mat_cor <- cor(crimes_dist[,-1], method = "spearman")
print(mat_cor)
#There are more than 27 variables. So, it's important to know if it the variables are really important

ola <- apply(crimes_dist[,-1], 2, f <- function(x){(x - min(x))/(max(x)-min(x))})
comp_prin <- prcomp(ola)
summary(comp_prin)
#we need only five components instead of 27 variables


# conteo <- data.table(res)[!is.na(Name),.N, by = "Name"]
# conteo <- merge(conteo, mapa@data, by = "Name", all.x = T, all.y = F)






##########################33#First analysis about homicides #####################333333333



dados[OFFENSE_CODE_GROUP == "Homicide" & (YEAR %in% c(2016, 2017)), .N, by = "MONTH"] %>% group_by(MONTH) %>% summarize(n = sum(N)) %>% mutate(prop = n/sum(n))
#Apparently there is less homicides in the winter and more in the summer   


crimes <-  data.frame(Long = dados[OFFENSE_CODE_GROUP == "Homicide"]$Long, Lat = dados[OFFENSE_CODE_GROUP == "Homicide"]$Lat)
crimes <- SpatialPoints(crimes)
plot(mapa)
plot(crimes, add = T, col = "blue")
mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))

crimes <-  data.frame(Long = dados_tipo[OFFENSE_CODE_GROUP == "Homicide"]$Long, Lat = dados_tipo[OFFENSE_CODE_GROUP == "Homicide"]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa2)

conteo <- data.table(res)[,.N,by = "Name"]

mapa2@data <- merge(mapa2@data, conteo, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()

#Most of homicides is in Dorchester.



homicidios <- as.geodata(dados_tipo[OFFENSE_CODE_GROUP == "Homicide"], coords.col = 1:2, data.col = 4)


#We can see the homicides are related with the distance


res1.v <- variog(homicidios, trend = "1st")
plot(res1.v, type = "b")
res2.v <- variog(homicidios, trend = "2nd")
lines(res2.v, type = "b", lty = 2)
mc1 <- variog.mc.env(homicidios, obj = res1.v)
plot(res1.v, env = mc1, xlab = "u")
mc2 <- variog.mc.env(homicidios, obj = res2.v)
plot(res2.v, env = mc2, xlab = "u")

#By the variogram, the homicides happen until some distance. 

modelo1 <- likfit(homicidios, trend = "1st", ini.cov.pars = c(1,1))
KC <- krige.control(obj.model = modelo1)
OC <- output.control(n.pred = 1000, simul = TRUE, thres = 250)

#Simulate some coordinates
coordenadas <- pred_grid(t(mapa@bbox), by = 0.01)

#Kriging using the model above
pred <- krige.conv(homicidios, locations = coordenadas, krige = KC, output = OC)


#All the predictions are more than 1 and less than 2. It is expected because thare are no points with 0 and a few points with 2


#In both cases it is better the geo model 




############### Robbery ####################33

crimes <-  data.frame(Long = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Long, Lat = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Lat)
crimes <- SpatialPoints(crimes)
plot(mapa)
plot(crimes, add = T, col = "blue")

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Long, Lat = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa2)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()

mapa2@data <- merge(mapa2@data, pobreza, all.x = T, all.y = F, by.x = "Name", by.y = "District")
#Robberies happened more often in the Dorchester and Roxbury 

cor(mapa2@data$N, mapa2@data$pobreza, use = "complete.obs", method = "spearman")
cor(mapa2@data$N, mapa2@data$Impoverished, use = "complete.obs", method = "spearman")


robbery <- as.geodata(dados_tipo[OFFENSE_CODE_GROUP == "Robbery"], coords.col = 1:2, data.col = 4)
# robbery <- dados_tipo[OFFENSE_CODE_GROUP == "Robbery",]


res1.v <- variog(robbery, trend = "1st")
plot(res1.v, type = "b")
res2.v <- variog(robbery, trend = "2nd")
lines(res2.v, type = "b", lty = 2)
res3.v <- variog4(robbery)
plot(res3.v, type = "b")

#We can see the homicides are related with the distance, i.e., longer, more improbable to have 
#We can see the trend the second order, apparently, does not have difference with the first order


MC <- model.control()
PC <- prior.control(phi.discrete = seq(0, 6, l = 21), phi.prior = "reciprocal", tausq.rel.prior = "unif", tausq.rel.discrete = seq(0, 1, l = 11))
OC <- output.control(n.post = 1000, moments = T)
skb <- krige.bayes(robbery, loc = coordenadas, model = MC, prior = PC, output = OC)
#The model has a singular matrix because the most positions have only one robbery



