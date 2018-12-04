library(tidyverse)
source("0_Preprocessing_Functions.R")

### Get binary data files
bin.files <- dir("binary_data")

### Create directory for summary files
if(!file.exists("summary_rotated_data")) dir.create("summary_rotated_data")

### Loop through all full binary files
### generate pixel by pixel summaries
for(i in bin.files){
	load(file.path("binary_data",paste0(i)))
	df.sum <- train %>% 
		mutate(y=y+255) %>% 
		select(-stroke) %>%
		nest(-key_id) %>%
		mutate(data = map(data, ~ rotate.vert(.) %>% integer.xy %>% flip.y)) %>%
		unnest() %>% 
		group_by(x,y) %>%
		summarise(n = n())
	save(df.sum, file = file.path("summary_rotated_data",paste0(i)))
	cat(i, "summarized \n")
}

### TEST CODE
#df.sum <- train %>% 
#	mutate(y=y+255) %>% 
#	select(-stroke) %>%
#	nest(-key_id) %>%
#	mutate(data = map(data, rotate.vert),
#	       height = map(data, ~max(.$y) - min(.$y)),
#	       width = map(data, ~max(.$x) - min(.$x)))
#df.sum$height <- unlist(df.sum$height)
#df.sum$width <- unlist(df.sum$width)
#head(df.sum)	

#table(df.sum$height)
#df.sum$data[[400]] %>% plot
