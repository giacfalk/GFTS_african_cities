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

city_boundaries <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

city_boundaries$geometry = city_boundaries$geom
city_boundaries$geom=NULL
city_boundaries <- st_as_sf(city_boundaries)

#

wgs84.tif.list <- lapply(list.files(pattern = "population_AF"), raster)

# load walking distance friction

wdf <- raster("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/2020_walking_only_friction_surface.tif")

# 1) Nairobi
# load bus stops

bus_nairobi <- read.csv("GTFS/Nairobi/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_nairobi) <- 4326

# nairobi <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# nairobi <- nairobi[nairobi$city %in% "Nairobi", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
nairobi <- filter(city_boundaries, UC_NM_MN=="Nairobi")

a <- list()
for (i in 1:28){
   a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], nairobi, fun="sum")
}

# load gridded population of kenya
pop_kenya <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_kenya <- crop(pop_kenya, extent(nairobi))
pop_kenya <- rgis::fast_mask(pop_kenya, nairobi)
pop_dens_kenya <- pop_kenya*1111.111

# # load shapefile 
# gadm_kenya<-raster::getData('GADM', country='KEN', level=1) %>% st_as_sf() %>%  filter(grepl('Nairobi', NAME_1)) %>% summarise()
# st_crs(gadm_kenya) <- 4326
# 
# # crop to nairobi GADM
# 
# bus_nairobi <- st_filter(bus_nairobi, gadm_kenya, .predicate = st_within)

#pop_kenya <- rgis::fast_mask(pop_kenya, gadm_kenya)

wdf_kenya <- crop(wdf, extent(nairobi))
wdf_kenya <- disaggregate(wdf_kenya, fact=30)
#values(wdf_kenya) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_kenya, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_nairobi))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_nairobi <- accCost(T.GC, xy.matrix)
#tr_t_nairobi <- crop(tr_t_nairobi, extent(bus_nairobi))

# plot
tr_t_nairobi <- rgis::fast_mask(tr_t_nairobi, nairobi)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

tr_t_nairobi <- projectRaster(tr_t_nairobi, pop_kenya)


all_nairobi <- as.data.frame(stack(tr_t_nairobi, pop_kenya), xy=T)

colnames(all_nairobi)[1:2] <-c("long", "lat")

all_nairobi$ISO="Nairobi"

all_nairobi$layer.0 <- all_nairobi$layer.1 

all_nairobi$layer.1 <- (ifelse(all_nairobi$layer.1 >=0 & all_nairobi$layer.1 < 5, "<5", ifelse(all_nairobi$layer.1 >=5 & all_nairobi$layer.1 < 10, "<10", ifelse(all_nairobi$layer.1 >=10 & all_nairobi$layer.1 < 15, "<15", ifelse(all_nairobi$layer.1 >=15 & all_nairobi$layer.1 < 30, "<30", ifelse(all_nairobi$layer.1 >=30, ">30", NA))))))
all_nairobi$layer.1 <- factor(all_nairobi$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))


plot_nairobi<- ggplot() +
  theme_classic()+
  geom_tile(data = all_nairobi, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= nairobi, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Nairobi")+
#  scalebar(all_nairobi, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")


# 2) Freetown
# load walking distance friction

wdf <- raster("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/2020_walking_only_friction_surface.tif")

# freetown <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# freetown <- freetown[freetown$city %in% "Freetown", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326) %>% filter(iso3=="SLE")
freetown <- filter(city_boundaries, UC_NM_MN=="Freetown")

# load bus stops

bus_freetown <- read.csv("GTFS/Freetown/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_freetown) <- 4326

# load gridded population of sierraleone

a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], freetown, fun="sum")
}

# load gridded population of kenya
pop_sierraleone <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_sierraleone <- crop(pop_sierraleone, extent(freetown))
pop_sierraleone <- rgis::fast_mask(pop_sierraleone, freetown)
pop_dens_sierraleone <- pop_sierraleone*1111.111

# # load shapefile 
# gadm_sierraleone<-raster::getData('GADM', country='SLE', level=3) %>% st_as_sf() %>%  filter(grepl('Freetown', NAME_3)) %>% summarise()
# st_crs(gadm_sierraleone) <- 4326
# 
# # crop to freetown GADM
# 
# bus_freetown <- st_filter(bus_freetown, gadm_sierraleone, .predicate = st_within)

#pop_sierraleone <- rgis::fast_mask(pop_sierraleone, gadm_sierraleone)

wdf_sierraleone <- crop(wdf, extent(freetown))
wdf_sierraleone <- disaggregate(wdf_sierraleone, fact=30)
#values(wdf_sierraleone) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_sierraleone, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_freetown))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_freetown <- accCost(T.GC, xy.matrix)
#tr_t_freetown <- crop(tr_t_freetown, extent(bus_freetown))

# plot
tr_t_freetown <- rgis::fast_mask(tr_t_freetown, freetown)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_sierraleone <- crop(pop_sierraleone, extent(freetown))
pop_sierraleone <- rgis::fast_mask(pop_sierraleone, freetown)
tr_t_freetown <- projectRaster(tr_t_freetown, pop_sierraleone)

all_freetown <- as.data.frame(stack(tr_t_freetown, pop_sierraleone), xy=T)

colnames(all_freetown)[1:2] <-c("long", "lat")

all_freetown$ISO="Freetown"

all_freetown$layer.0 <- all_freetown$layer.1 

all_freetown$layer.1 <- (ifelse(all_freetown$layer.1 >=0 & all_freetown$layer.1 < 5, "<5", ifelse(all_freetown$layer.1 >=5 & all_freetown$layer.1 < 10, "<10", ifelse(all_freetown$layer.1 >=10 & all_freetown$layer.1 < 15, "<15", ifelse(all_freetown$layer.1 >=15 & all_freetown$layer.1 < 30, "<30", ifelse(all_freetown$layer.1 >=30, ">30", NA))))))
all_freetown$layer.1 <- factor(all_freetown$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))

plot_freetown <- ggplot() +
  theme_classic()+
  geom_tile(data = all_freetown, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= freetown, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Freetown")+
#  scalebar(all_freetown, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")

# 3) Accra
# load bus stops

bus_accra <- read.csv("GTFS/Accra/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_accra) <- 4326

# accra <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# accra <- accra[accra$city %in% "Accra", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
accra <- filter(city_boundaries, UC_NM_MN=="Accra")


# load gridded population of ghana
a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], accra, fun="sum")
}

pop_ghana <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_ghana <- crop(pop_ghana, extent(accra))
pop_ghana <- rgis::fast_mask(pop_ghana, accra)
pop_dens_ghana <- pop_ghana*1111.111

# # load shapefile 
# gadm_ghana<-raster::getData('GADM', country='GHA', level=2) %>% st_as_sf() %>%  filter(grepl('Accra', NAME_2)) %>% summarise()
# st_crs(gadm_ghana) <- 4326
# 
# # crop to accra GADM
# 
# bus_accra <- st_filter(bus_accra, gadm_ghana, .predicate = st_within)

#pop_ghana <- rgis::fast_mask(pop_ghana, gadm_ghana)

wdf_ghana <- crop(wdf, extent(accra))
wdf_ghana <-disaggregate(wdf_ghana, fact=30)
#values(wdf_ghana) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_ghana, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_accra))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_accra <- accCost(T.GC, xy.matrix)
#tr_t_accra <- crop(tr_t_accra, extent(bus_accra))

# plot
tr_t_accra <- rgis::fast_mask(tr_t_accra, accra)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_ghana <- crop(pop_ghana, extent(accra))
pop_ghana <- rgis::fast_mask(pop_ghana, accra)
tr_t_accra <- projectRaster(tr_t_accra, pop_ghana)

all_accra <- as.data.frame(stack(tr_t_accra, pop_ghana), xy=T)

library(ggsn)

colnames(all_accra)[1:2] <-c("long", "lat")

all_accra$ISO="Accra"

all_accra$layer.0 <- all_accra$layer.1 

all_accra$layer.1 <- (ifelse(all_accra$layer.1 >=0 & all_accra$layer.1 < 5, "<5", ifelse(all_accra$layer.1 >=5 & all_accra$layer.1 < 10, "<10", ifelse(all_accra$layer.1 >=10 & all_accra$layer.1 < 15, "<15", ifelse(all_accra$layer.1 >=15 & all_accra$layer.1 < 30, "<30", ifelse(all_accra$layer.1 >=30, ">30", NA))))))
all_accra$layer.1 <- factor(all_accra$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))


plot_accra <- ggplot() +
  theme_classic()+
  geom_tile(data = all_accra, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= accra, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Accra")+
#  scalebar(all_accra, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")

# 3) Abidjan
# load bus stops

bus_abidjan <- read.csv("GTFS/Abidjan/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_abidjan) <- 4326

# abidjan <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# abidjan <- abidjan[abidjan$city %in% "Abidjan", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
abidjan <- filter(city_boundaries, UC_NM_MN=="Abidjan")

# load gridded population of civ
a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], abidjan, fun="sum")
}

pop_civ <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_civ <- crop(pop_civ, extent(abidjan))
pop_civ <- rgis::fast_mask(pop_civ, abidjan)
pop_dens_civ <- pop_civ*1111.111


# # load shapefile 
# gadm_civ<-raster::getData('GADM', country='CIV', level=2) %>% st_as_sf() %>%  filter(grepl('Abidjan', NAME_2)) %>% summarise()
# st_crs(gadm_civ) <- 4326
# 
# # crop to abidjan GADM
# 
# bus_abidjan <- st_filter(bus_abidjan, gadm_civ, .predicate = st_within)

#pop_civ <- rgis::fast_mask(pop_civ, gadm_civ)

wdf_civ <- crop(wdf, extent(abidjan))
wdf_civ <-disaggregate(wdf_civ, fact=30)
#values(wdf_civ) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_civ, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_abidjan))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_abidjan <- accCost(T.GC, xy.matrix)
#tr_t_abidjan <- crop(tr_t_abidjan, extent(bus_abidjan))

# plot
tr_t_abidjan <- rgis::fast_mask(tr_t_abidjan, abidjan)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_civ <- crop(pop_civ, extent(abidjan))
pop_civ <- rgis::fast_mask(pop_civ, abidjan)
tr_t_abidjan <- projectRaster(tr_t_abidjan, pop_civ)

all_abidjan <- as.data.frame(stack(tr_t_abidjan, pop_civ), xy=T)

library(ggsn)

colnames(all_abidjan)[1:2] <-c("long", "lat")

all_abidjan$ISO="Abidjan"

all_abidjan$layer.0 <- all_abidjan$layer.1 

all_abidjan$layer.1 <- (ifelse(all_abidjan$layer.1 >=0 & all_abidjan$layer.1 < 5, "<5", ifelse(all_abidjan$layer.1 >=5 & all_abidjan$layer.1 < 10, "<10", ifelse(all_abidjan$layer.1 >=10 & all_abidjan$layer.1 < 15, "<15", ifelse(all_abidjan$layer.1 >=15 & all_abidjan$layer.1 < 30, "<30", ifelse(all_abidjan$layer.1 >=30, ">30", NA))))))
all_abidjan$layer.1 <- factor(all_abidjan$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))


plot_abidjan <- ggplot() +
  theme_classic()+
  geom_tile(data = all_abidjan, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= abidjan, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Abidjan")+
#  scalebar(all_abidjan, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")

# 3) Addis
# load bus stops

bus_addis_1 <- read.csv("GTFS/Addis/AnbessaCityBusServiceEnterprise_GTFS/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_addis_1) <- 4326

bus_addis_2 <- read.csv("GTFS/Addis/MinibusTaxi_GTFS/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_addis_2) <- 4326

bus_addis_3 <- read.csv("GTFS/Addis/ShegerCityBus_GTFS/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_addis_3) <- 4326

bus_addis <- rbind(bus_addis_1, bus_addis_2, bus_addis_3) %>% st_as_sf()

# addis <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# addis <- addis[addis$city %in% "Addis Ababa", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
addis <- filter(city_boundaries, UC_NM_MN=="Addis Ababa")

# load gridded population of eth
pop_eth <- raster("population_eth_2018-10-01.tif")
pop_eth <- crop(pop_eth, extent(addis))
pop_eth <- rgis::fast_mask(pop_eth, addis)
pop_dens_eth <- pop_eth*1111.111

# # load shapefile 
# gadm_eth<-raster::getData('GADM', country='ETH', level=2) %>% st_as_sf() %>%  filter(grepl('Addis', NAME_2)) %>% summarise()
# st_crs(gadm_eth) <- 4326
# 
# # crop to addis GADM
# 
# bus_addis <- st_filter(bus_addis, gadm_eth, .predicate = st_within)

#pop_eth <- rgis::fast_mask(pop_eth, gadm_eth)

wdf_eth <- crop(wdf, extent(addis))
wdf_eth <-disaggregate(wdf_eth, fact=30)
#values(wdf_eth) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_eth, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_addis))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_addis <- accCost(T.GC, xy.matrix)
#tr_t_addis <- crop(tr_t_addis, extent(bus_addis))

# plot
tr_t_addis <- rgis::fast_mask(tr_t_addis, addis)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_eth <- crop(pop_eth, extent(addis))
pop_eth <- rgis::fast_mask(pop_eth, addis)
tr_t_addis <- projectRaster(tr_t_addis, pop_eth)

all_addis <- as.data.frame(stack(tr_t_addis, pop_eth), xy=T)

library(ggsn)

colnames(all_addis)[1:2] <-c("long", "lat")

all_addis$ISO="Addis Ababa"

all_addis$layer.0 <- all_addis$layer.1 

all_addis$layer.1 <- (ifelse(all_addis$layer.1 >=0 & all_addis$layer.1 < 5, "<5", ifelse(all_addis$layer.1 >=5 & all_addis$layer.1 < 10, "<10", ifelse(all_addis$layer.1 >=10 & all_addis$layer.1 < 15, "<15", ifelse(all_addis$layer.1 >=15 & all_addis$layer.1 < 30, "<30", ">30")))))
all_addis$layer.1 <- factor(all_addis$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))


plot_addis <- ggplot() +
  theme_classic()+
  geom_tile(data = all_addis, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= addis, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Addis Ababa")+
#  scalebar(all_addis, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")

# 3) Harare
# load bus stops

bus_harare_1 <- read.csv("GTFS/Harare/Harare City Shuttle/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_harare_1) <- 4326

bus_harare_2 <- read.csv("GTFS/Harare/Harare Kombis/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_harare_2) <- 4326

bus_harare <- rbind(bus_harare_1, bus_harare_2) %>% st_as_sf()

# harare <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# harare <- harare[harare$city %in% "Harare", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
harare <- filter(city_boundaries, UC_NM_MN=="Harare")


# load gridded population of zimbabwe
a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], harare, fun="sum")
}

pop_zimbabwe <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_zimbabwe <- crop(pop_zimbabwe, extent(harare))
pop_zimbabwe <- rgis::fast_mask(pop_zimbabwe, harare)
pop_dens_zimbabwe <- pop_zimbabwe*1111.111

# # load shapefile 
# gadm_zimbabwe<-raster::getData('GADM', country='ZWE', level=2) %>% st_as_sf() %>%  filter(grepl('Harare', NAME_2)) %>% summarise()
# st_crs(gadm_zimbabwe) <- 4326

# crop to harare GADM

#bus_harare <- st_filter(bus_harare, gadm_zimbabwe, .predicate = st_within)

#pop_zimbabwe <- rgis::fast_mask(pop_zimbabwe, gadm_zimbabwe)

wdf_zimbabwe <- crop(wdf, extent(harare))
wdf_zimbabwe <-disaggregate(wdf_zimbabwe, fact=30)
#values(wdf_zimbabwe) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_zimbabwe, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_harare))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_harare <- accCost(T.GC, xy.matrix)
#tr_t_harare <- crop(tr_t_harare, extent(bus_harare))

# plot
tr_t_harare <- rgis::fast_mask(tr_t_harare, harare)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_zimbabwe <- crop(pop_zimbabwe, extent(harare))
pop_zimbabwe <- rgis::fast_mask(pop_zimbabwe, harare)
tr_t_harare <- projectRaster(tr_t_harare, pop_zimbabwe)

all_harare <- as.data.frame(stack(tr_t_harare, pop_zimbabwe), xy=T)

all_harare$ISO="Harare"

library(ggsn)

colnames(all_harare)[1:2] <-c("long", "lat")

all_harare$layer.0 <- all_harare$layer.1 

all_harare$layer.1 <- (ifelse(all_harare$layer.1 >=0 & all_harare$layer.1 < 5, "<5", ifelse(all_harare$layer.1 >=5 & all_harare$layer.1 < 10, "<10", ifelse(all_harare$layer.1 >=10 & all_harare$layer.1 < 15, "<15", ifelse(all_harare$layer.1 >=15 & all_harare$layer.1 < 30, "<30", ">30")))))
all_harare$layer.1 <- factor(all_harare$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))

plot_harare <- ggplot() +
  theme_classic()+
  geom_tile(data = all_harare, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= harare, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Harare")+
#  scalebar(all_harare, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")


#
# 3) Kampala
# load bus stops

bus_kampala <- read.csv("GTFS/Kampala/stops.txt") %>% st_as_sf(coords=c("stop_lon", "stop_lat")) 
st_crs(bus_kampala) <- 4326

# kampala <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# kampala <- kampala[kampala$city %in% "Kampala", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
kampala <- filter(city_boundaries, UC_NM_MN=="Kampala")

# load gridded population of uganda
a <- list()
for (i in 1:28){
  a[i] <- exactextractr::exact_extract(wgs84.tif.list[[i]], kampala, fun="sum")
}

pop_uganda <- wgs84.tif.list[[which.max(do.call(rbind, a)>0)]]
pop_uganda <- crop(pop_uganda, extent(kampala))
pop_uganda <- rgis::fast_mask(pop_uganda, kampala)
pop_dens_uganda <- pop_uganda*1111.111

# # load shapefile 
# gadm_uganda<-raster::getData('GADM', country='ZWE', level=2) %>% st_as_sf() %>%  filter(grepl('Kampala', NAME_2)) %>% summarise()
# st_crs(gadm_uganda) <- 4326

# crop to kampala GADM

# bus_kampala <- st_filter(bus_kampala, gadm_uganda, .predicate = st_within)

#pop_uganda <- rgis::fast_mask(pop_uganda, gadm_uganda)

wdf_uganda <- crop(wdf, extent(kampala))
wdf_uganda <-disaggregate(wdf_uganda, fact=30)
#values(wdf_uganda) <- minperm

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_uganda, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

# 

points = as.data.frame(st_coordinates(bus_kampala))

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

tr_t_kampala <- accCost(T.GC, xy.matrix)
#tr_t_kampala <- crop(tr_t_kampala, extent(bus_kampala))

# plot
tr_t_kampala <- rgis::fast_mask(tr_t_kampala, kampala)

# plot along with population density (bi-dimensional legend)
# https://stackoverflow.com/questions/60572833/is-there-a-way-in-r-to-plot-a-legend-with-two-axes

pop_uganda <- crop(pop_uganda, extent(kampala))
pop_uganda <- rgis::fast_mask(pop_uganda, kampala)
tr_t_kampala <- projectRaster(tr_t_kampala, pop_uganda)

all_kampala <- as.data.frame(stack(tr_t_kampala, pop_uganda), xy=T)

all_kampala$ISO="Kampala"

library(ggsn)

colnames(all_kampala)[1:2] <-c("long", "lat")

all_kampala$layer.0 <- all_kampala$layer.1 

all_kampala$layer.1 <- (ifelse(all_kampala$layer.1 >=0 & all_kampala$layer.1 < 5, "<5", ifelse(all_kampala$layer.1 >=5 & all_kampala$layer.1 < 10, "<10", ifelse(all_kampala$layer.1 >=10 & all_kampala$layer.1 < 15, "<15", ifelse(all_kampala$layer.1 >=15 & all_kampala$layer.1 < 30, "<30", ">30")))))
all_kampala$layer.1 <- factor(all_kampala$layer.1, levels = c("<5", "<10", "<15", "<30", ">30"))

plot_kampala <- ggplot() +
  theme_classic()+
  geom_tile(data = all_kampala, aes(x = long, y = lat, fill = layer.1))+
  geom_sf(data= kampala, fill=NA, colour="black")+
  scale_fill_viridis_d(name="Minutes", direction=-1, na.translate=FALSE)+
  ggtitle("Kampala")+
#  scalebar(all_kampala, dist = 2, dist_unit = "km",
           #transform = TRUE, model = "WGS84")+
  xlab("")+
  ylab("")

title <-  cowplot::ggdraw() + 
  cowplot::draw_label(
    "Walking time to the nearest transit stop",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )

maps <- cowplot::plot_grid(title, cowplot::plot_grid(plot_addis + theme(legend.position = "none"), plot_harare+ theme(legend.position = "none"), plot_accra+ theme(legend.position = "none"), plot_abidjan+ theme(legend.position = "none"), plot_nairobi+ theme(legend.position = "none"), plot_freetown+ theme(legend.position = "none"), plot_kampala+ theme(legend.position = "none")), cowplot::get_legend(plot_nairobi), rel_heights = c(0.05, 1, 0.05), ncol = 1)

ggsave("maps.png", maps, scale = 2.5)

### areas of each city

cities <- rbind(addis, harare, accra, abidjan, nairobi, freetown, kampala)

cities <- cities %>% st_transform(4326) %>% mutate(area=st_area(.)/1e6) %>% st_transform(4326) 

ggplot(cities, aes(x=UC_NM_MN,  y=AREA, fill=UC_NM_MN)) +
  theme_classic()+
  geom_col()+
  ggtitle("Urban center area")+
  ylab("Sq. km")+
  xlab("City")+
  theme(legend.position = "none")


ggsave(plot = last_plot(), device = "png", filename = "area.png", scale=1.2)

#
stat_ecdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      weight =  NULL, 
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      weight = weight,
      ...
    )
  )
}


# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export

StatEcdf <- ggplot2::ggproto("StatEcdf", ggplot2::Stat,
                             compute_group = function(data, scales, weight, n = NULL, pad = TRUE) {
                               # If n is NULL, use raw values; otherwise interpolate
                               if (is.null(n)) {
                                 x <- unique(data$x)
                               } else {
                                 x <- seq(min(data$x), max(data$x), length.out = n)
                               }
                               
                               if (pad) {
                                 x <- c(-Inf, x, Inf)
                               }
                               y <- spatstat::ewcdf(data$x, weights=data$weight / sum(data$weight))(x)
                               
                               data.frame(x = x, y = y)
                             },
                             
                             default_aes = ggplot2::aes(y = ggplot2::stat(y)),
                             
                             required_aes = c("x")
)


all <- rbind(all_addis, all_harare, all_accra, all_abidjan, all_nairobi, all_freetown, all_kampala)

all$layer.2 <- ifelse(all$layer.2<0, 0, all$layer.2)

# ecdf curve

ecdf <- ggplot(na.exclude(all), aes(x=layer.0,  weight = layer.2, group=ISO, colour=ISO)) +
  theme_classic()+
  stat_ecdf(size=0.75)+
  coord_cartesian(xlim=c(0, 45))+
  xlab('Minutes')+
  ylab("Cumulative fraction of the population")+
  scale_x_continuous(breaks = c(0, 5, 10, 20, 30, 45))+
  geom_vline(xintercept=c(5, 10, 20, 30, 45), linetype="dotted")+
  ggtitle("Walking time to the nearest transit stop")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_discrete(name="City")

ggsave("ecdf.png", ecdf, scale=1)

# pop. weighted average travel time

all_s <- all[!is.na(all$layer.2),]

all_s <- all_s %>% group_by(ISO) %>% summarise(pwatt=weighted.mean(layer.0, layer.2, na.rm=T)) %>% ungroup()

p_a <- ggplot(all_s, aes(x=ISO,  y=pwatt, fill=ISO)) +
  theme_classic()+
  geom_col()+
  ggtitle("Population weighted average travel time")+
  ylab("Minutes")+
  xlab("City")+
  theme(legend.position = "none")

all_s <- merge(all_s, cities, by.x="ISO", by.y="UC_NM_MN")

p_b <- ggplot(all_s, aes(x=ISO,  y=pwatt/(AREA/mean(AREA)), fill=ISO)) +
  theme_classic()+
  geom_col()+
  ggtitle("Population weighted average travel time, weighted by urban center area")+
  ylab("Minutes")+
  xlab("City")+
  theme(legend.position = "none")

ggsave(plot = cowplot::plot_grid(p_a, p_b, labels="AUTO", ncol = 1), device = "png", scale=1.1, filename = "average.png")

# Chart of people NOT within threshold by city

all_prova <- all %>% group_by(ISO) %>% dplyr::summarise(pop5min=sum(layer.2[layer.0>15], na.rm = T))

p_a <- ggplot(all_prova, aes(x=ISO,  y=pop5min/1e3, fill=ISO)) +
  theme_classic()+
  geom_col()+
  ggtitle("People not within a 15-minute walk from a transit stop")+
  ylab("Population (thousands)")+
  xlab("City")+
  theme(legend.position = "none")

all_prova <- all %>% group_by(ISO) %>% dplyr::summarise(pop5min=sum(layer.2[layer.0>20], na.rm = T))

p_b <- ggplot(all_prova, aes(x=ISO,  y=pop5min/1e3, fill=ISO)) +
  theme_classic()+
  geom_col()+
  ggtitle("People not within a 30-minute walk from a transit stop")+
  ylab("Population (thousands)")+
  xlab("City")+
  theme(legend.position = "none")

ggsave(plot = cowplot::plot_grid(p_a, p_b, labels="AUTO", ncol = 1), device = "png", scale=1.1, filename = "deprivation.png")

