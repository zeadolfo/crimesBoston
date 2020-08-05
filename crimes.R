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
rm(list = ls())

#Read the data 
setwd("/home/ze/Área de Trabalho/Bases de dados/Crimes Boston/crimesBoston/")
dados <- fread("crime.csv")
mapa <- readOGR("Boston_Neighborhoods.geojson")


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

dados_crimenes <- dados[,.N, c("Location", "OFFENSE_CODE_GROUP")]
crimes <- SpatialPointsDataFrame(dados_crimenes$Location, data = dados_crimenes$OFFENSE_CODE_GROU)


#Count by some interesting variables 
dados_tipo <- dados[,.N, by = c("Long", "Lat", "OFFENSE_CODE_GROUP")]
dados_mes <- dados[,.N, by = c("Long", "Lat", "MONTH", "OFFENSE_CODE_GROUP")]
dados_tipo[,N := as.numeric(N)]
dados_mes[,N := as.numeric(N)]

#First analysis about homicides
dados[OFFENSE_CODE_GROUP == "Homicide" & (YEAR %in% c(2016, 2017)), .N, by = "MONTH"] %>% group_by(MONTH) %>% summarize(n = sum(N)) %>% mutate(prop = n/sum(n))
#Apparently there is small the homicide numbers in February and March 
crimes <-  data.frame(Long = dados_mes[OFFENSE_CODE_GROUP == "Homicide" & MONTH == 6]$Long, Lat = dados_mes[OFFENSE_CODE_GROUP == "Homicide" & MONTH == 6]$Lat)
crimes <- SpatialPoints(crimes)
plot(mapa)
plot(crimes, add = T, col = "blue")

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_mes[OFFENSE_CODE_GROUP == "Homicide"]$Long, Lat = dados_mes[OFFENSE_CODE_GROUP == "Homicide"]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()

#Most of homicides is in Dorchester.



homicidios <- as.geodata(dados_tipo[OFFENSE_CODE_GROUP == "Homicide"], coords.col = 1:2, data.col = 4)


#We can see the homicides are related with the distance, i.e., longer, more improbable to have 

res1.v <- variog(homicidios, trend = "1st")
plot(res1.v, type = "b")
res2.v <- variog(homicidios, trend = "2nd")
lines(res2.v, type = "b", lty = 2)
mc1 <- variog.mc.env(homicidios, obj = res1.v)
plot(res1.v, env = mc1, xlab = "u")
mc2 <- variog.mc.env(homicidios, obj = res2.v)
plot(res2.v, env = mc2, xlab = "u")



modelo1 <- likfit(homicidios, trend = "1st", ini.cov.pars = c(1,1))
modelo2 <- likfit(homicidios, trend = "1st", ini.cov.pars = c(1,1),cov.model = "matern", kappa = 1.5)  

#In both cases it is better the geo model 




############### Robbery ####################33

crimes <-  data.frame(Long = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Long, Lat = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Lat)
crimes <- SpatialPoints(crimes)
plot(mapa)
plot(crimes, add = T, col = "blue")

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Long, Lat = dados_tipo[OFFENSE_CODE_GROUP == "Robbery"]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()

#Robberies happened more often in the Dorchester and Roxbury 

robbery <- as.geodata(dados_tipo[OFFENSE_CODE_GROUP == "Robbery"], coords.col = 1:2, data.col = 4)

plot(variog(robbery))

#We can see the homicides are related with the distance, i.e., longer, more improbable to have 

res1.v <- variog(robbery, trend = "1st")
plot(res1.v, type = "b")
res2.v <- variog(robbery, trend = "2nd")
lines(res2.v, type = "b", lty = 2)
mc1 <- variog.mc.env(robbery, obj = res1.v)
plot(res1.v, env = mc1, xlab = "u")
mc2 <- variog.mc.env(robbery, obj = res2.v)
plot(res2.v, env = mc2, xlab = "u")

modelo1 <- likfit(robbery, trend = ~factor(MONTH), ini.cov.pars = c(1,1),cov.model = "matern", kappa = 1.5)
modelo2 <- likfit(robbery, trend = "1st", ini.cov.pars = c(1,1),cov.model = "matern", kappa = 1.5)


locales <- car_lacerny$coords
KC <- krige.control(type = "sk", obj.mod = modelo1)
sk <- krige.conv(car_lacerny, krige = KC, loc = locales)
KCt <- krige.control(type = "sk", obj.mod = modelo2)
skt <- krige.conv(car_lacerny, krige = KCt, loc = locales)













###### We know the importance of the weekends in the violence
###### Now, we change a little 

dados[OFFENSE_CODE_GROUP == "Larceny", .N, by = "HOUR"][order(HOUR)]
dados[OFFENSE_CODE_GROUP == "Towed", .N, by = "HOUR"][order(HOUR)]

dados_hora <- dados[,.N, by = c("Long", "Lat", "HOUR", "OFFENSE_CODE_GROUP")]

crimes <-  data.frame(Long = dados_hora[OFFENSE_CODE_GROUP == "Larceny"]$Long, Lat = dados_hora[OFFENSE_CODE_GROUP == "Larceny"]$Lat)
crimes <- SpatialPoints(crimes)
plot(mapa)
plot(crimes, add = T, col = "blue")


######## First hour

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 1]$Long, Lat = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 1]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()


###### Second hour

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 2]$Long, Lat = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 2]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()



###### 3AM

mapa2 <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
crimes <-  data.frame(Long = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 3]$Long, Lat = dados_hora[OFFENSE_CODE_GROUP == "Larceny" & HOUR == 3]$Lat)
crimes <- SpatialPointsDataFrame(coords = crimes, data = data.frame(id=1:dim(crimes)[1]), proj4string = CRS(proj4string(mapa2)))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "Name"]
mapa2@data <- merge(mapa2@data, res, by = "Name", all.x = T, all.y = F)
mapa2@data$N <- ifelse(is.na(mapa2@data$N) , 0, mapa2@data$N)
tm_shape(mapa2) + tm_fill(col = "N") + tm_borders()











lacerny <- as.geodata(dados_semana[OFFENSE_CODE_GROUP == "Larceny"], coords.col = 1:2, data.col = 4, covar.col = 2)

plot(variog(car_lacerny))

#We can see the homicides are related with the distance, i.e., longer, more improbable to have 

res1.v <- variog(car_lacerny, trend = "1st")
plot(res1.v, type = "b")
res2.v <- variog(car_lacerny, trend = "2nd")
lines(res2.v, type = "b", lty = 2)
# mc1 <- variog.mc.env(car_lacerny, obj = res1.v)
# plot(res1.v, env = mc1, xlab = "u")
# mc2 <- variog.mc.env(car_lacerny, obj = res2.v)
# plot(res2.v, env = mc2, xlab = "u")

modelo1 <- likfit(car_lacerny, trend = "1st", ini.cov.pars = c(1,1))
modelo2 <- likfit(car_lacerny, trend = "1st", ini.cov.pars = c(1,1),cov.model = "matern", kappa = 1.5)


locales <- car_lacerny$coords
KC <- krige.control(type = "sk", obj.mod = modelo1)
sk <- krige.conv(car_lacerny, krige = KC, loc = locales)
KCt <- krige.control(type = "sk", obj.mod = modelo2)
skt <- krige.conv(car_lacerny, krige = KCt, loc = locales)
