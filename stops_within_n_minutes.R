library(sf)
library(raster)
library(tidyverse)
library(gdistance)
library(GADMTools)
library(ggsn)

setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit")

city_boundaries <- read_sf("GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")

city_boundaries$geometry = city_boundaries$geom
city_boundaries$geom=NULL
city_boundaries <- st_as_sf(city_boundaries)

city_boundaries <- st_transform(city_boundaries, 4326)

# load walking distance friction

wdf <- raster("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/2020_walking_only_friction_surface.tif")

# 1) Freetown
# load bus stops

bus_freetown <- read.csv("GTFS/Freetown/shapes.txt")

bus_freetown <-  st_as_sf(bus_freetown, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_freetown <- bus_freetown %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_freetown) <- 4326

# freetown <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# freetown <- freetown[freetown$city %in% "Freetown", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
freetown <- filter(city_boundaries, eFUA_name=="Freetown")

wdf_sierraleone <- crop(wdf, extent(freetown))
wdf_sierraleone <- rgis::fast_mask(wdf_sierraleone, freetown)
wdf_sierraleone <- disaggregate(wdf_sierraleone, fact=4)

# plot(wdf_sierraleone)
# plot(bus_freetown, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_sierraleone))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_sierraleone, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 1 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_freetown)$shape_id)) /2
}

freetown_grd <- grd

freetown <- ggplot(freetown_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Freetown")+ 
  theme(axis.text.x = element_text(angle = 90))

#

# 2) Nairobi
# load bus stops

bus_nairobi <- read.csv("GTFS/Nairobi/shapes.txt")

bus_nairobi <-  st_as_sf(bus_nairobi, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_nairobi <- bus_nairobi %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_nairobi) <- 4326

# nairobi <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# nairobi <- nairobi[nairobi$city %in% "Nairobi", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
nairobi <- filter(city_boundaries, eFUA_name=="Nairobi")

wdf_kenya <- crop(wdf, extent(nairobi))
wdf_kenya <- rgis::fast_mask(wdf_kenya, nairobi)
wdf_kenya <- disaggregate(wdf_kenya, fact=4)

# plot(wdf_kenya)
# plot(bus_nairobi, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_kenya))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_kenya, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 2 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_nairobi)$shape_id)) /2
}

nairobi_grd <- grd

nairobi <- ggplot(nairobi_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Nairobi")+ 
  theme(axis.text.x = element_text(angle = 90))


# 3) Accra
# load bus stops

bus_accra <- read.csv("GTFS/Accra/shapes.txt")

bus_accra <-  st_as_sf(bus_accra, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_accra <- bus_accra %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_accra) <- 4326

# accra <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# accra <- accra[accra$city %in% "Accra", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
accra <- filter(city_boundaries, eFUA_name=="Accra")

wdf_ghana <- crop(wdf, extent(accra))
wdf_ghana <- rgis::fast_mask(wdf_ghana, accra)
wdf_ghana <- disaggregate(wdf_ghana, fact=4)

# plot(wdf_ghana)
# plot(bus_accra, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_ghana))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_ghana, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 3 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_accra)$shape_id)) /2
}

accra_grd <- grd

accra <- ggplot(accra_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Accra")+ 
  theme(axis.text.x = element_text(angle = 90))

# 4) Abidjan
# load bus stops

bus_abidjan <- read.csv("GTFS/Abidjan/shapes.txt")

bus_abidjan <-  st_as_sf(bus_abidjan, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_abidjan <- bus_abidjan %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_abidjan) <- 4326

# abidjan <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# abidjan <- abidjan[abidjan$city %in% "Abidjan", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
abidjan <- filter(city_boundaries, eFUA_name=="Abidjan")

wdf_ivorycoast <- crop(wdf, extent(abidjan))
wdf_ivorycoast <- rgis::fast_mask(wdf_ivorycoast, abidjan)
wdf_ivorycoast <- disaggregate(wdf_ivorycoast, fact=4)

# plot(wdf_ivorycoast)
# plot(bus_abidjan, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_ivorycoast))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_ivorycoast, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 4 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_abidjan)$shape_id)) /2
}

abidjan_grd <- grd

abidjan <- ggplot(abidjan_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Abidjan")+ 
  theme(axis.text.x = element_text(angle = 90))


# 5) Kampala
# load bus stops

bus_kampala <- read.csv("GTFS/Kampala/shapes.txt")

bus_kampala <-  st_as_sf(bus_kampala, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_kampala <- bus_kampala %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_kampala) <- 4326

# kampala <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# kampala <- kampala[kampala$city %in% "Kampala", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
kampala <- filter(city_boundaries, eFUA_name=="Kampala")

wdf_uganda <- crop(wdf, extent(kampala))
wdf_uganda <- rgis::fast_mask(wdf_uganda, kampala)
wdf_uganda <- disaggregate(wdf_uganda, fact=4)

# plot(wdf_uganda)
# plot(bus_kampala, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_uganda))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_uganda, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 5 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_kampala)$shape_id)) /2
}

kampala_grd <- grd

kampala <- ggplot(kampala_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Kampala")+ 
  theme(axis.text.x = element_text(angle = 90))

# 6) Addis
# load bus stops

bus_addis <- read.csv("GTFS/Addis/shapes.txt")

bus_addis <-  st_as_sf(bus_addis, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_addis <- bus_addis %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_addis) <- 4326

# addis <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# addis <- addis[addis$city %in% "Addis", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
addis <- filter(city_boundaries, eFUA_name=="Addis Ababa")

wdf_ethiopia <- crop(wdf, extent(addis))
wdf_ethiopia <- rgis::fast_mask(wdf_ethiopia, addis)
wdf_ethiopia <- disaggregate(wdf_ethiopia, fact=4)

# plot(wdf_ethiopia)
# plot(bus_addis, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_ethiopia))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_ethiopia, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 6 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_addis)$shape_id)) /2
}

addis_grd <- grd

addis <- ggplot(addis_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Addis")+ 
  theme(axis.text.x = element_text(angle = 90))

# 7) Harare
# load bus stops

bus_harare <- read.csv("GTFS/Harare/shapes.txt")

bus_harare <-  st_as_sf(bus_harare, coords=c("shape_pt_lon", "shape_pt_lat"))

bus_harare <- bus_harare %>% 
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(bus_harare) <- 4326

# harare <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv') %>% st_as_sf(coords = c("lng", "lat"), crs=4326)
# harare <- harare[harare$city %in% "Harare", ] %>%  st_transform(3395) %>% st_buffer(dist = 10000) %>% st_transform(4326)
harare <- filter(city_boundaries, eFUA_name=="Harare")

wdf_zimbabwe <- crop(wdf, extent(harare))
wdf_zimbabwe <- rgis::fast_mask(wdf_zimbabwe, harare)
wdf_zimbabwe <- disaggregate(wdf_zimbabwe, fact=4)

# plot(wdf_zimbabwe)
# plot(bus_harare, add=T)

grd <- st_as_sf(rasterToPolygons(wdf_zimbabwe))

# friction 

function_sens <- function(x){
  a <-(1/mean(x))
}

Tr <- transition(wdf_zimbabwe, function_sens, 8) # RAM intensive, can be very slow for large areas
saveRDS(Tr, "study.area.T_sens.rds")
T.GC <- geoCorrection(Tr)
saveRDS(T.GC, "study.area.T.GC_sens.rds")

for (gridcell in 1:nrow(grd)){
  print(paste0("City 7 ", gridcell/nrow(grd)))
  r <- accCost(T.GC, st_coordinates(st_centroid(grd[gridcell,])))
  values(r) <- ifelse(values(r)>15, NA, values(r))
  r <- st_as_sf(rasterToPolygons(r))
  grd$stops_within_15_min[gridcell] <- length(unique(st_intersection(r, bus_harare)$shape_id)) /2
}

harare_grd <- grd

harare <- ggplot(harare_grd)+
  geom_sf(aes(fill=stops_within_15_min), colour = NA)+
  ggtitle("")+
  scale_fill_binned(name="#", type = "viridis", breaks=c(5, 25, 50, 100))+
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key.width =  unit(2, "cm"))+
  ggtitle("Harare")+ 
  theme(axis.text.x = element_text(angle = 90))

title <-  cowplot::ggdraw() + 
  cowplot::draw_label(
    "Number of lines within 15 mins. walking time",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )

maps <- cowplot::plot_grid(title, cowplot::plot_grid(addis + theme(legend.position = "none"), harare+ theme(legend.position = "none"), accra+ theme(legend.position = "none"), abidjan+ theme(legend.position = "none"), nairobi+ theme(legend.position = "none"), freetown+ theme(legend.position = "none"), kampala+ theme(legend.position = "none")), cowplot::get_legend(freetown), rel_heights = c(0.05, 1, 0.1), ncol = 1)

ggsave("maps_lines.png", maps, scale = 2)
