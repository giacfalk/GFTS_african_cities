# export GTFS to igraph for network analysis

setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit/network analysis")

source("gtfs_to_igraph.R")

setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit/GTFS")

k_list <- list.files(recursive = T, pattern = "zip", full.names = T)
i = k_list[1]

for (i in k_list){
g <- gtfs_to_igraph(list_gtfs = i,  dist_threshold =30 , save_muxviz =T)
save(g, file=paste0(gsub(".zip", "", i), "_igraph.Rdata"))
}



