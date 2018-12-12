library(tidyverse)
library(reshape2)
source("0_Preprocessing_Functions.R")

if(!file.exists("cluster_data")) dir.create("cluster_data")

### Get binary data files
bin.files <- dir("binary_data")

for(i in bin.files){
	### Load a file
	load(file.path("binary_data",i))
	food.i <- sub(".rdata","",i) 
	
	# get info about edges and where they occur
	edges <- train %>% 
	select(-stroke) %>%
	nest(-key_id) %>%
	# select only lowest sidehits and rightmost top/bottom hits
	mutate(data1 = map(data, ~ rotate.vert(.) %>% integer.xy),
	       xmax = map(data1, ~max(.$x)) %>% unlist,
	       xmin = map(data1, ~min(.$x)) %>% unlist,
	       ymax = map(data1, ~max(.$y)) %>% unlist,
	       ymin = map(data1, ~min(.$y)) %>% unlist,
	       top_x = map(data1, ~max(.$x[.$y == max(.$y)]))%>% unlist,
	       top_y = ymax,
	       bottom_x = map(data1, ~max(.$x[.$y == min(.$y)]))%>% unlist,
	       bottom_y = ymin,
	       right_x = xmax, 
	       right_y = map(data1, ~min(.$y[.$x == max(.$x)]))%>% unlist,
	       left_x = xmin,
	       left_y = map(data1, ~min(.$y[.$x == min(.$x)]))%>% unlist,
	       height = ymax - ymin, width = xmax- xmin,
	       hw = height / width)
	
	### Save transformations
	save(edges, file = file.path("cluster_data",i))
	
	### Plot joint distributions of each side
	png(paste0("plots/",food.i,"_cluster.png"), 
	    width = 1200, height = 1200)
	pairs(select(edges, top_x, bottom_x, right_y, left_y, height, width))
	dev.off()
	cat(food.i, "/n")
}
