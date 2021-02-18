setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit/network analysis")

source("gtfs_to_igraph.R")

list <- list.files(path="D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit/GTFS", pattern=".zip", full.names = T, recursive = T)

g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)
save(g, file="Abidjan.Rdata")

i = list[2]
g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)
save(g, file="Accra.Rdata")

i = list[3]
g_1 <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)

i = list[4]
g_2 <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)

i = list[5]
g_3 <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)

g <- igraph::union(g_1, g_2, g_3, byname = "auto")
save(g, file="Addis.Rdata")

i = list[6]
g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)
save(g, file="Freetown.Rdata")

i = list[7]
g_1 <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)

i = list[8]
g_2 <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)

g <- igraph::union(g_1, g_2, byname = "auto")
save(g, file="Harare.Rdata")

i = list[9]
g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)
save(g, file="Kampala.Rdata")

i = list[10]
g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =F)
save(g, file="Nairobi.Rdata")
