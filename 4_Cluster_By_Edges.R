library(tidyverse)
library(reshape2)
library(tidyr)
library(dbscan)

### Get binary data files
bin.files <- dir("cluster_data",full.names = T)
load(bin.files[2])

##########################################################
# fit model based on location of hits
mod1 <- kmeans(x = select(edges, top_x : left_y),
	   centers = 5)
edges$clust1 <- mod1$cluster

##########################################################
# plot clusters
#ggplot(edges) + 
#geom_point(aes(x = left_y, y = right_y, 
#	   color = factor(clust1)))

##########################################################
#### plot sketches for different clusters
train <- edges %>% unnest

ggplot(train) +
geom_path(aes(x = x, y = y), size = 0.003) + 
facet_wrap("clust1")

banana
bread
pizza
watermelon


