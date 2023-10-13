library(tidyverse)
library(spdep)    
library(sp)
library(sf)
library(sfdep)


data <- read.csv("diversity_seasonality.csv")

data2019=subset(data,year=="2019")%>%group_by(trap_ID,long,lat)%>%summarise(num=sum(Ae_aegypti))

data2019$lat = as.numeric(data2019$lat)
data2019$long = as.numeric(data2019$long)
data2019=data.frame(data2019)

trap_data_sp <- SpatialPoints(data2019[,c("long", "lat")])

radius <- 0.01 # Adjust the radius as needed
nb <- dnearneigh(trap_data_sp, 0, radius)

nb_listw <- nb2listw(nb, style = "W")
hotspot_result <- localG(data2019$num, nb_listw)

print(hotspot_result)
write.csv(data2019,"data2019.csv")






captiva=st_read("captiva_island.shp")
sanibel=st_read("sanibel_island.shp")


head(captiva)

ggplot() +
  geom_sf(data = captiva, fill = "white")+
  geom_sf(data = sanibel, fill = "white")+
  geom_point(data = data2019, aes(x = long, y = lat), size = 1)+
geom_text(data = data2019, aes(x = long, y = lat, label = trap_ID), vjust = 1)










data <- read.csv("diversity_seasonality.csv")

data2020=subset(data,year=="2020")%>%group_by(trap_ID,long,lat)%>%summarise(num=sum(Ae_aegypti))

data2020$lat = as.numeric(data2020$lat)
data2020$long = as.numeric(data2020$long)
data2020=data.frame(data2020)

trap_data_sp <- SpatialPoints(data2020[,c("long", "lat")])

radius <- 0.01 # Adjust the radius as needed
nb <- dnearneigh(trap_data_sp, 0, radius)

nb_listw <- nb2listw(nb, style = "W")
hotspot_result <- localG(data2020$num, nb_listw)

print(hotspot_result)
write.csv(data2020,"data2020.csv")













