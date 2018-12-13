### Packages
library(tidyverse)
library(spatstat)

### Grab Prediction Files
bin.files <- dir("Predictions",full.names = T)

combine.data <- function(file.list){
	df <- data.frame()
	for(i in file.list){
		load(i)
		df <- rbind(df, predictions)
		
	}
	df <- df %>% mutate(truth = gsub(" ", "", truth))
	return(df)
}

### grab data from both algorithms
pred.data <- combine.data(bin.files)

set.seed(1000)

df <-
pred.data %>% 
	select(key_id, truth, data) %>%
	group_by(truth) %>%
	sample_n(1) %>%
	ungroup %>%
	mutate(truth = capitalize(truth)) %>%
	unnest
head(df)

png(filename = "Sketch_30_Foods.png", 
    height = 1000, width = 1000)
ggplot(df) +
geom_path(aes(x,255-y)) +
facet_wrap("truth")+
theme_void() +
theme(strip.text = element_text(colour = "#FF5500",
		        face = "bold",
		        size = 24))
dev.off()
