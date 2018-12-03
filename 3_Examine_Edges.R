library(tidyverse)
library(reshape2)

### Get binary data files
bin.files <- dir("binary_data")

for(i in bin.files){
	### Load a file
	load(file.path("binary_data",i))
	food.i <- sub(".rdata","",i) 
	
	# get info about edges and where they occur
	edges <- train %>% 
	mutate(y = y +255) %>% 
	select(-stroke) %>%
	nest(-key_id) %>%
	# select only lowest sidehits and rightmost top/bottom hits
	mutate(xmax = map(data, ~max(.$x)) %>% unlist,
	       xmin = map(data, ~min(.$x)) %>% unlist,
	       ymax = map(data, ~max(.$y)) %>% unlist,
	       ymin = map(data, ~min(.$y)) %>% unlist,
	       top_x = map(data, ~max(.$x[.$y == max(.$y)]))%>% unlist,
	       top_y = ymax,
	       bottom_x = map(data, ~max(.$x[.$y == min(.$y)]))%>% unlist,
	       bottom_y = ymin,
	       right_x = xmax, 
	       right_y = map(data, ~min(.$y[.$x == max(.$x)]))%>% unlist,
	       left_x = xmin,
	       left_y = map(data, ~min(.$y[.$x == min(.$x)]))%>% unlist) %>%
	mutate(height = ymax - ymin, width = xmax- xmin,
	       hw = height / width)
	
	### Plot joint distributions of each side
	png(paste0("plots/",food.i,"_cluster.png"), 
	    width = 1200, height = 1200)
	pairs(select(edges, top_x, bottom_x, right_y, left_y, height, width))
	dev.off()
}
