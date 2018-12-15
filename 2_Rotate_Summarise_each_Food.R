### This file reads in
# (1) preprocessing functions
# (2) the contents of "binary_data" from file (1)

### This file creates
# (1) a folder called "summary_data" 
# (2) In "summary_data" each category of drawing gets a binary .rdata file containing a data frame. The data frame consists of all x, y combinations in a 256 grid and the number of observed sample points at each grid point from the 5,000 images.
# (3) In "summary_data" each category of drawing gets a second binary .rdata file with the prefix "_rotated". This contains the same data structure, but each image was rotated using a PCA rotation before the data was summarized.

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
