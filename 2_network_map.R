## Scripts related to manuscript titled 
## Shark and ray trade in and out of Indonesia: Addressing knowledge gaps on the path to sustainability
#
# https://doi.org/10.1016/j.marpol.2021.104714

# Library
library(readxl)
library(ggplot2)
require(maps)  #loading maps package
require(mapdata) #loading mapdata package
library(ggthemes) #package for ggplot2 theme
library(ggrepel) #extendig the plotting package ggplot2 for maps
library(geosphere)

## Working directory
setwd("Prasetyo_et_al_Indonesia_Sharks_Trade")


## Create basemap
# Source Data
source_dat <- read_excel("2_network_map/Indonesia_trade_data.xlsx", sheet = "InputMAP-destination")

Indonesia <- map('worldHires',c('Indonesia'), col="#bfbbbb", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0)

gg3 <- ggplot() + 
  geom_polygon(data = Indonesia, aes(x=long, y = lat, group = group), 
               fill = "gray85", color = "gray80") + 
  coord_fixed(1)

print(gg3)

Indo_source<-gg3 + xlim(94,142) + ylim(-11,7.5) + 
  geom_point(data = source_dat, aes(x = longitude, y = latitude, size = weight),
             color = "purple", alpha = 0.5, show.legend = T) + 
  theme_map()

print(Indo_source)



# Destination Data
dest_dat <- read_excel("2_network_map/Indonesia_trade_data.xlsx", sheet = "InputMAP-source")

Indonesia <- map('worldHires',c('Indonesia'), col="#bfbbbb", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0)

gg2 <- ggplot() + 
  geom_polygon(data = Indonesia, aes(x=long, y = lat, group = group), 
               fill = "gray85", color = "gray80") + 
  coord_fixed(1)

print(gg2)

Indo_dest<-gg2 + xlim(94,142) + ylim(-11,7.5) + 
  geom_point(data = dest_dat, aes(x = longitude, y = latitude, size = weight),
             color = "purple", alpha = 0.5, show.legend = T) + 
  theme_map()

print(Indo_dest)


# Create connection
# data
data <- read_excel("2_network_map/Indonesia_trade_data.xlsx", sheet = "data")

# Background map
map('worldHires',c('Indonesia'), col="transparent", plot=TRUE, fill=TRUE, bg="transparent", lwd=0.05,mar=rep(0,4), border =0)

# Circles for cities
points(x=data$long, y=data$lat, col="black", cex=0.3, pch=20)


# Data
all_pairs <- read_excel("2_network_map/Indonesia_trade_data.xlsx", sheet = "InputCONNECTION2")

# A function to plot connections
plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

# add every connections:
for(i in 1:nrow(all_pairs)){
  plot_my_connection(all_pairs$long1[i], all_pairs$lat1[i], all_pairs$long2[i], all_pairs$lat2[i], col="red", lwd=1)
}


## ADDITIONAL
## CREATE NETWORK
# Library
library(visNetwork)
library(networkD3)
library(dplyr)
library(tidyr)
library(readxl)

# data
edges <- read_excel("2_network_map/Indonesia_trade_data.xlsx",sheet = "edges5")
nodes <- read_excel("2_network_map/Indonesia_trade_data.xlsx",sheet = "nodes5")

# Network
visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/80 + 1)


visNetwork(nodes, edges, label = LETTERS[1:2], font.color =c ("red", "blue"), font.size=50, style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")



# networkD3
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)


forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", NodeID = "label", Value = "weight", font = "Verdana", fontSize = 24, unit = "Letter(s)")



