library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)

#setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit")
setwd("C:/Users/miche/FONDAZIONE ENI ENRICO MATTEI/Giacomo Falchetta - Accessibility public transit")

rm(list=ls())


#### correct distances in shapes.txt ----

#city <- read_gtfs("GTFS/Accra.zip")
#city <- read_gtfs("GTFS/Nairobi.zip")
#city <- read_gtfs("GTFS/Kampala.zip")
#city <- read_gtfs("GTFS/Freetown.zip")
#city <- read_gtfs("GTFS/Abidjan.zip")

#city <- read_gtfs("GTFS/Addis-C.zip")
city <- read_gtfs("GTFS/Harare-B.zip")


# fix missing distances

city$shapes$shape_dist_traveled2 <- 0

for (i in 2:dim(city$shapes)[1]) {
  
  a <- st_sfc(st_point(c(city$shapes$shape_pt_lon[i],city$shapes$shape_pt_lat[i]))) %>% 
    st_set_crs(4326)
  
  b <- st_sfc(st_point(c(city$shapes$shape_pt_lon[i-1],city$shapes$shape_pt_lat[i-1]))) %>% 
    st_set_crs(4326)
  
  city$shapes$shape_dist_traveled2[i] <-  st_distance(a,b)
  print(i/dim(city$shapes)[1])
}

view(city$shapes)

city$shapes$shape_dist_traveled2[city$shapes$shape_pt_sequence==0] <- 0

city$shapes %>% group_by(shape_id) %>% mutate(shape_dist_traveled3 = round(cumsum(shape_dist_traveled2))) %>% ungroup() %>% 
  select(-shape_dist_traveled2,-shape_dist_traveled) %>% rename(shape_dist_traveled=shape_dist_traveled3) -> city$shapes

#write_gtfs(city,"GTFS/Accra2.zip",compression_level = 9, as_dir = FALSE)
#write_gtfs(city,"GTFS/Nairobi2.zip",compression_level = 9, as_dir = FALSE)
#write_gtfs(city,"GTFS/Kampala2.zip",compression_level = 9, as_dir = FALSE)
#write_gtfs(city,"GTFS/Freetown2.zip",compression_level = 9, as_dir = FALSE)
#write_gtfs(city,"GTFS/Abidjan2.zip",compression_level = 9, as_dir = FALSE)
#write_gtfs(city,"GTFS/Addis-C2.zip",compression_level = 9, as_dir = FALSE)

write_gtfs(city,"GTFS/Harare-B2.zip",compression_level = 9, as_dir = FALSE)


rm(city)




#### compute analyses----


files <- c("Kampala","Addis-A","Addis-B","Addis-C","Harare-A","Harare-B","Freetown","Abidjan","Nairobi","Accra")

dataset <- tibble()

for (file in files) {
  
  city <- read_gtfs(paste0("GTFS/",file,"2.zip"))
  
  attach(city)
  
  #shapes %>% group_by(shape_id) %>% summarise(length = max(shape_dist_traveled)/1000) %>% 
  #  ungroup() %>% summarise(tot=sum(length)/n())
  
  left_join(trips,shapes %>% group_by(shape_id) %>% summarise(length = max(shape_dist_traveled)/1000) %>% ungroup(),by="shape_id") %>% 
    left_join(.,stop_times %>% group_by(trip_id) %>%
                summarize(duration=(max(as.numeric(hms(departure_time)))-min(as.numeric(hms(arrival_time))))/3600) %>% 
                ungroup(),by = "trip_id") %>% mutate(speed = length/duration) -> temp
  
  #temp <- left_join(temp,frequencies %>% select(trip_id,headway_secs),by="trip_id")
  
  detach(city)
  
  temp$city <- file
  
  dataset <- bind_rows(dataset,temp)
  
}


dataset %>% mutate(city = if_else(city=="Addis-A","Addis Abeba",city)) %>% 
  mutate(city = if_else(city=="Addis-B","Addis Abeba",city)) %>% 
  mutate(city = if_else(city=="Addis-C","Addis Abeba",city)) %>% 
  mutate(city = if_else(city=="Harare-A","Harare",city)) %>% 
  mutate(city = if_else(city=="Harare-B","Harare",city)) -> dataset2
  

dataset2 %>% filter(is.finite(speed)) %>% group_by(city) %>% summarize(speed=median(speed))

ggplot(dataset2 %>% filter(is.finite(speed)))+
  geom_point(aes(x=length,y=speed,color=city),alpha=0.5,size=1)+
  geom_smooth(aes(x=length,y=speed,color=city))+
    labs(title="Average speed of bus routes in African Cities",x="Route length (km)",y="Average speed (km/h)")

p1 <- ggplot(dataset2 %>% filter(is.finite(speed)))+
  geom_boxplot(aes(x=city,y=speed,fill=city))+
  labs(x="",y="Calculated average speed (km/h)")+
  scale_fill_discrete(guide=FALSE)

ggsave(p1,device = "png",filename = "dist_speed.png",dpi = 300,width = 6,height = 4)

p2 <- ggplot(dataset2)+
  geom_boxplot(aes(x=city,y=headway_secs/60,fill=city))+
  labs(x="",y="Trip frequency (minutes)")+
  scale_fill_discrete(guide=FALSE)+
  scale_y_log10(breaks = c(5,10,15,20,25,30,60,90,120))

ggsave(p2,device = "png",filename = "dist_freq.png",dpi = 300,width = 6,height = 4)


ggplot(dataset2)+
  geom_density(aes(x=headway_secs/60,color=city))+
  labs(title="Distribution of frequencies of bus routes in African Cities",x="",y="Frequency (minutes)")+
  scale_fill_discrete(guide=FALSE)



ggplot(dataset %>% filter(is.finite(speed)))+
  geom_point(aes(x=length,y=speed,color=city),alpha=0.5,size=1)+
  geom_smooth(aes(x=length,y=speed,color=city))+
  labs(title="Average speed of bus routes in African Cities",x="Route length (km)",y="Average speed (km/h)")



city <- read_gtfs("GTFS/Addis-A2.zip")


attach(city)

#shapes %>% group_by(shape_id) %>% summarise(length = max(shape_dist_traveled)/1000) %>% 
#  ungroup() %>% summarise(tot=sum(length)/n())

left_join(trips,shapes %>% group_by(shape_id) %>% summarise(length = max(shape_dist_traveled)/1000) %>% ungroup(),by="shape_id") %>% 
  left_join(.,stop_times %>% group_by(trip_id) %>%
              summarize(duration=(max(as.numeric(hms(departure_time)))-min(as.numeric(hms(arrival_time))))/3600) %>% 
              ungroup(),by = "trip_id") %>% mutate(speed = length/duration) -> prova

detach(city)



ggplot(prova)+
  geom_point(aes(x=length,y=speed))+
  labs(title="Average speed of bus routes in Addis",x="Route length (km)",y="Average speed (km/h)")



# calculation of vkm

attach(city)

trips %>% left_join(shapes %>% group_by(shape_id) %>% summarise(length = max(shape_dist_traveled)/1000) %>% 
  ungroup() %>% select(shape_id,length), by = "shape_id") %>%
  left_join(frequencies %>% mutate(buses = round((as.numeric(hms(end_time)) - as.numeric(hms(start_time)))/headway_secs)) %>% 
              group_by(trip_id) %>% summarise(buses=sum(buses)) %>% ungroup(),by="trip_id") %>% 
  mutate(vkm=round(length*buses)) %>% 
  group_by(service_id) %>% summarize(buses=sum(buses,na.rm=T),vkm=sum(vkm,na.rm=T))

length(unique(stops$stop_id))
length(unique(routes$route_id))
  
detach(city)



#list routes by the number of stops they have
routes %>% inner_join(trips, by="route_id") %>%
  inner_join(stop_times) %>%
  inner_join(stops, by="stop_id") %>%
  group_by(route_long_name) %>%
  summarise(stop_count=n_distinct(stop_id)) %>%
  arrange(desc(stop_count))

detach(freetown)


summary(freetown)



stops_2 <- stop_times %>% left_join(stops %>% select(stop_id,stop_lat,stop_lon),by="stop_id") %>% 
  group_by(stop_id,stop_lat,stop_lon) %>% summarize(n=n()) %>% ungroup()


ggplot(stops_2)+
  geom_point(aes(x=stop_lon,y=stop_lat,color=factor(n)))+
  scale_color_discrete(name="Daily passages")












freetown <- read_gtfs("GTFS/Freetown.zip")


gtfs_sf <- gtfs_as_sf(freetown)
routes_sf <- get_route_geometry(gtfs_sf)

am_route_freq <- get_route_frequency(freetown, start_hour = 6, end_hour = 10)

routes_sf <- routes_sf %>% 
  inner_join(am_route_freq, by = 'route_id')


routes_sf_crs <- sf::st_transform(routes_sf, 26919) 

routes_sf_buffer <- st_buffer(routes_sf,
                              dist=routes_sf$total_departures/5e4)


routes_sf_buffer %>% 
  ggplot() + 
  geom_sf(colour=alpha("white",0),aes(fill=total_departures)) +
  scale_fill_gradientn(colours = rainbow(5))+
  theme_bw() 

