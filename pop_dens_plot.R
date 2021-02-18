# https://git.digitaltransport4africa.org/data/africa
# https://www.overleaf.com/project/5fb920c2bf03cb0c6e3f8cf3

library(sf)
library(raster)
library(tidyverse)
library(gdistance)
library(GADMTools)
library(ggsn)

# minutes per meter assuming walking speed of 5 km / h
minperm <- 0.012

setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit")

city_boundaries <- read_sf("GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")

city_boundaries$geometry = city_boundaries$geom
city_boundaries$geom=NULL
city_boundaries <- st_as_sf(city_boundaries)

city_boundaries <- st_transform(city_boundaries, 4326) 

#

wgs84.tif.list <- lapply(list.files(pattern = "population_AF"), raster)

nairobi <- filter(city_boundaries, eFUA_name=="Nairobi")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], nairobi, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(nairobi))
pop_kenya <- rgis::fast_mask(pop_kenya, nairobi) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

nairobi_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Nairobi")+ 
  theme(axis.text.x = element_text(angle = 90))

#

Freetown <- filter(city_boundaries, eFUA_name=="Freetown")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], Freetown, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(Freetown))
pop_kenya <- rgis::fast_mask(pop_kenya, Freetown) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

freetown_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Freetown")+ 
  theme(axis.text.x = element_text(angle = 90))
#

Addis <- filter(city_boundaries, eFUA_name=="Addis Ababa")


# load gridded population of kenya
pop_kenya <- raster("population_eth_2018-10-01.tif")
pop_kenya <- crop(pop_kenya, extent(Addis))
pop_kenya <- rgis::fast_mask(pop_kenya, Addis)
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

addis_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Addis Ababa")+ 
  theme(axis.text.x = element_text(angle = 90))


#

Kampala <- filter(city_boundaries, eFUA_name=="Kampala")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], Kampala, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(Kampala))
pop_kenya <- rgis::fast_mask(pop_kenya, Kampala) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

kampala_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Kampala")+ 
  theme(axis.text.x = element_text(angle = 90))

#

Harare <- filter(city_boundaries, eFUA_name=="Harare")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], Harare, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(Harare))
pop_kenya <- rgis::fast_mask(pop_kenya, Harare) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

harare_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Harare")+ 
  theme(axis.text.x = element_text(angle = 90))

#

Abidjan <- filter(city_boundaries, eFUA_name=="Abidjan")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], Abidjan, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(Abidjan))
pop_kenya <- rgis::fast_mask(pop_kenya, Abidjan) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

abidjan_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Abidjan")+ 
  theme(axis.text.x = element_text(angle = 90))

#

Accra <- filter(city_boundaries, eFUA_name=="Accra")

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], Accra, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(Accra))
pop_kenya <- rgis::fast_mask(pop_kenya, Accra) 
pop_kenya <- aggregate(pop_kenya, fact=30, fun="sum")
pop_kenya <- st_as_sf(rasterToPolygons(pop_kenya))
pop_kenya$area <- st_transform(pop_kenya, 3395) %>% st_area() / 1e6
pop_kenya$area <- as.numeric(pop_kenya$area)
pop_kenya$layer <- pop_kenya$layer / pop_kenya$area

accra_plot <- ggplot(pop_kenya)+
  geom_sf(aes(fill=layer), colour = NA)+
  ggtitle("")+
  scale_fill_binned(trans="log", breaks=c(500, 1000, 2500, 5000, 10000, 25000, 50000), type = "viridis", name="")+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(1.5, "cm"), legend.key.height =  unit(0.1, "cm"))+
  ggtitle("Accra")+ 
  theme(axis.text.x = element_text(angle = 90))

maps <- cowplot::plot_grid(abidjan_plot, accra_plot, addis_plot, freetown_plot, harare_plot, kampala_plot, nairobi_plot, ncol = 2)
ggsave("maps_pop.png", maps, scale = 2, height = 5)
