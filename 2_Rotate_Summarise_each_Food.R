library(tidyverse)
source("0_Preprocessing_Functions.R")

### Get binary data files
bin.files <- dir("binary_data")

### Create directory for summary files (rotated and unrotated)
if(!file.exists("summary_data")) dir.create("summary_data")

### Loop through all full binary files
### generate pixel by pixel summaries
for(i in bin.files){
	load(file.path("binary_data", paste0(i)))
	
	# without rotation
	df.sum <- train %>%
		select(-stroke) %>%
		group_by(x,y) %>%
		summarise(n = n())
	save(df.sum, file = file.path("summary_data",paste0(i)))
	
	# add rotation
	df.sum <- train %>% 
		select(-stroke) %>%
		nest(-key_id) %>%
		mutate(data = map(data, ~ rotate.vert(.) %>% integer.xy)) %>%
		unnest() %>% 
		group_by(x,y) %>%
		summarise(n = n())
	save(df.sum, file = file.path("summary_data",paste0("rotated_",i)))
	cat(i, "summarized \n")
}


train %>% 
	select(-stroke) %>%
	nest(-key_id) %>%
	mutate(data = map(data, ~ rotate.vert(.) %>% integer.xy)) %>%
	unnest() %>%
	ggplot() + geom_path(aes(x,y,group = key_id), size = 0.005)

ggplot(likelihoods) +
geom_raster(aes(x,y, fill = rotated_banana))
