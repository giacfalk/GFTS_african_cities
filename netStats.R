library(igraph)
library(brainGraph)

rm(list=ls())
all.nets<-list.files(pattern = 'Rdata')

mynets<- lapply(all.nets, function(x) {
  load(file = x)
  get(ls()[ls()!= g])
})

all.nets<-gsub('.Rdata','',all.nets)
names(mynets) <- all.nets 

#===== BASE CHECK ========#
#summary(mynets$Abidjan)
#plot(mynets$Abidjan, edge.arrow.size = 0.2)

#vertex_attr_names(mynets$Abidjan)
#vertex_attr(mynets$Abidjan, "name")
#edge_attr(mynets$Abidjan)
#unique(edge_attr(mynets$Abidjan, "route_type"))

##### componets 
# CS = components(mynets$Abidjan, mode="strong")
# BigComp = which.max(CS$csize)
# Main_comp = induced_subgraph(mynets$Abidjan, 
#                              which(CS$membership == BigComp))



#===================================================================

netStats<-lapply(mynets, function(g){
#-----------------------------------------------
## Topological properties of the networks ##
## size, density, diameter, and clustering coefficient.
## These provide a very elementary and high-level information 
## about the network properties 
  
# Number of links
NL<-gsize(g)
# Number of nodes
ND<-gorder(g)
# diameter
DIAM<-diameter(g, directed=T)
# Density
DEN<-graph.density(g)

# Average clustering coefficient/transitivity
ACF<-transitivity(g, type = "global")

# Global efficiency
GF<-efficiency(g, type = "global")
#-----------------------------------------------
## centrality measures.These measures help finding out hubs, if present, 
## and better define the distribution of stations
## Average betweenness centrality
ABC<-mean(betweenness(g, directed=T,normalized = T))#weights = E(g)$weight)
#Average Closeness centrality
CS = components(g, mode="strong")
BigComp = which.max(CS$csize)
Main_comp = induced_subgraph(g, 
           which(CS$membership == BigComp))

ACC<-mean(closeness(Main_comp, mode ='all',normalized = T))
#vertex_connectivity/cohesion/Beta index/connectivity
#CON<-vertex_connectivity(Main_comp) #cohesion(g)

#SIZE OF THE BIGGEST COMPONENT
CBIG<-max(CS$csize)
#-----------------------------------------------
## Additiona Network measures
## helpful to understand the networks dynamic and topology
#mean distance /average path length
#APL<-mean_distance(g, directed=T)
APL<-average.path.length(g, directed=TRUE, unconnected=TRUE)

#-----------------------------------------------
#community structure
#simulated annealing (Guimerà and Amaral 2005). 
spin=cluster_spinglass(Main_comp)
length(unique(spin$membership))
# The modularity of a graph with respect to some division (or vertex types) measures
# how good the division is, or how separated are the different vertex types from each other.
MOD<-spin$modularity
NC<-length(unique(spin$membership))

list(NL,#'Number of links'
     ND,#'Number of nodes'
     DIAM,#'Diameter'
     DEN,#'Density'
     ACF,#'Transitivity'
     GF,#Global Efficiency
     ABC,#'Avg Betweenness centrality',
     ACC,#'Avg Closeness centrality'
     CBIG,#'Max Component Size'
     APL,#'Avg Path Length'
     MOD,#'Modularity',
     NC#'Number of Clusters'
       )

})
names(netStats)<-all.nets
library(data.table)

netTab<-rbindlist(netStats,idcol = 'City')

names(netTab)<-c('City',
  'NL',
  'ND',
  'DIAM',
  'DEN',
  'ACF',
  'GF',
  'ABC',
  'ACC',
  'CBIG',
  'APL',
  'MOD',
  'NC')



names(netTab)<-c('City',
  'Number of links',
  'Number of nodes',
  'Diameter',
  'Density',
  'Transitivity',
  'Global Efficiency',
  'Avg Betweenness Centrality',
  'Avg Closeness Centrality',
  'Max Component Size',
  'Avg Path Length',
  'Modularity',
  'Number of Clusters'
)

netTab<-netTab[, lapply(.SD, round, 4), City]

library(xtable)
print(xtable(netTab, type = "latex"), file = "netTab.tex")




