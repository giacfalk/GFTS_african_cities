library(sf)
library(raster)
library(tidyverse)
library(gdistance)
library(GADMTools)
library(ggsn)

setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit")

FUA <- read_sf("GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")

FUA$geometry = FUA$geom
FUA$geom=NULL
FUA <- st_as_sf(FUA)

FUA <- st_transform(FUA, 4326) 

#

UC <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

UC$geometry = UC$geom
UC$geom=NULL
UC <- st_as_sf(UC)

#####

UC <- filter(UC, UC_NM_MN=="Freetown" | UC_NM_MN=="Abidjan" | UC_NM_MN=="Accra" | UC_NM_MN=="Nairobi" | UC_NM_MN=="Harare" | UC_NM_MN=="Kampala" | UC_NM_MN=="Addis Ababa")
FUA <- filter(FUA, eFUA_name=="Freetown" | eFUA_name=="Abidjan" | eFUA_name=="Accra" | eFUA_name=="Nairobi" | eFUA_name=="Harare" | eFUA_name=="Kampala" | eFUA_name=="Addis Ababa")

#

pop <- raster("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

UC$pop <- exactextractr::exact_extract(pop, UC, fun="sum")
FUA$pop <- exactextractr::exact_extract(pop, FUA, fun="sum")

UC <- dplyr::select(UC, UC_NM_MN, pop)
FUA <- dplyr::select(FUA, eFUA_name, pop)
FUA$geometry=NULL

bind <- merge(UC, FUA, by.x="UC_NM_MN", by.y="eFUA_name")

library(reshape2)

bind <- reshape2::melt(bind, id.vars=1, measure.vars=c(2,3))

bind$Unit <- as.character(bind$variable)
bind$Unit[bind$Unit=="pop.x"] <- "UC"
bind$Unit[bind$Unit=="pop.y"] <- "FUA"

ggplot(bind, aes(x=UC_NM_MN, y=value/1e6, group=Unit, fill=Unit))+
  geom_col(position = "dodge")+
  xlab("City")+
  ylab("Population (million people)")

ggsave(last_plot(), device = "png", filename = "comparison_uc_fua.png")

##

FUA <- read_sf("GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")

FUA$geometry = FUA$geom
FUA$geom=NULL
FUA <- st_as_sf(FUA)

FUA <- st_transform(FUA, 4326) 

#

UC <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

UC$geometry = UC$geom
UC$geom=NULL
UC <- st_as_sf(UC)

#####

UC <- filter(UC, UC_NM_MN=="Freetown" | UC_NM_MN=="Abidjan" | UC_NM_MN=="Accra" | UC_NM_MN=="Nairobi" | UC_NM_MN=="Harare" | UC_NM_MN=="Kampala" | UC_NM_MN=="Addis Ababa")
FUA <- filter(FUA, eFUA_name=="Freetown" | eFUA_name=="Abidjan" | eFUA_name=="Accra" | eFUA_name=="Nairobi" | eFUA_name=="Harare" | eFUA_name=="Kampala" | eFUA_name=="Addis Ababa")

#

UC$area <- as.vector(st_area(UC)/1000000)
FUA$area <- as.vector(st_area(FUA)/1000000)

UC <- dplyr::select(UC, UC_NM_MN, area)
FUA <- dplyr::select(FUA, eFUA_name, area)
FUA$geometry=NULL

bind <- merge(UC, FUA, by.x="UC_NM_MN", by.y="eFUA_name")

library(reshape2)

bind <- reshape2::melt(bind, id.vars=1, measure.vars=c(2,3))

bind$Unit <- as.character(bind$variable)
bind$Unit[bind$Unit=="area.x"] <- "UC"
bind$Unit[bind$Unit=="area.y"] <- "FUA"

ggplot(bind, aes(x=UC_NM_MN, y=value, group=Unit, fill=Unit))+
  geom_col(position = "dodge")+
  xlab("City")+
  ylab("Land area (sq. km)")

ggsave(last_plot(), device = "png", filename = "comparison_uc_fua_area.png")
