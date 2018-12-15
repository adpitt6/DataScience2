
### This file reads in
# (1) the contents of "binary_data"
# (2) the contents of "summary_data"

### This file creates
# (1) a folder called "plots" in which each category of drawing gets 2 .png files--
# a sketch plot and a summary plot

### Load Packages
library(Hmisc)
library(tidyverse)

### Create Plot Directory
if(!file.exists("plots")) dir.create("plots")

### Grab Binary Files
bin.files <- dir("binary_data")

### Loop Through Files and Plot All Data
for(i in bin.files){
	### Load Files
	load(file.path("binary_data",i))
	load(file.path("summary_data",i))
	
	food.i <- sub(".rdata","",i) %>% capitalize()
	samp500 <- sample(unique(train$key_id), 500)
	
	### Trim Edges of Summary
	df.sum <- filter(df.sum, x != 255, x != 0, y != 0, y!= -255)

	{ 
	### Plot the foods
	png(file = file.path("plots",paste0(food.i,".png")), width = 500, height = 500)
	print(ggplot(df.sum) +
		geom_raster(aes(x = x, y = y, fill = n), size = 0.1) +
		theme_void() + 
		ggtitle(paste(food.i, ": Samples Per Pixel")) +
		theme(legend.position = "none")
	)
	dev.off()
	
	### Sketch Plot
	png(file = file.path("plots",paste0(food.i,"2.png")), width = 500, height = 500)
	print(ggplot(filter(train, key_id %in% samp500)) +
		geom_path(aes(x=x, y=y, group = stroke), size = 0.05) +
		ggtitle(paste0(food.i, ": 500 Drawings")) +
		theme_void()
	)
	dev.off()
	}
}
