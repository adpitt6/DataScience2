### Packages
library(tidyverse)
library(spatstat)

############################################################
# plot 30 sketches

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

############################################################
# plot apple on top of kernel

load(file = "Empirical_Kernel/Kernels_MLE1.rdata")
### 
likelihoods <- likelihoods %>% select(x,y,apple, broccoli)

df <- filter(df, truth == "Apple") %>% left_join(likelihoods, by = c("x" ,"y"))

png("Apple_AppleKernel_Plot.png", height =400, width = 400)
ggplot() + 
geom_raster(data = likelihoods, aes(x,255-y, fill = exp(apple))) +
scale_fill_continuous(low = "white", high = "black",
	          limits = exp(c(-17.5, -9.75)))+
geom_point(data = filter(df, truth == "Apple"),
           aes(x=x, y=255-y), color = "red") + 
geom_path(data = filter(df, truth == "Apple"),
          aes(x=x, y=255-y), color = "blue")+
annotate("text", x = 100, y = 100, 
         label = paste("ln(L) =", round(sum(df$apple), 1)), 
         color = "red", size = 10) +
theme_void() + 
dev.off()


png("Apple_BroccoliKernel_Plot.png", height =400, width = 400)
ggplot() + 
geom_raster(data = likelihoods, aes(x,255-y, fill = exp(broccoli))) +
scale_fill_continuous(low = "white", high = "black",
	          limits = exp(c(-17.5, -9.75)))+
geom_point(data = filter(df, truth == "Apple"),
           aes(x=x, y=255-y), color = "red") + 
geom_path(data = filter(df, truth == "Apple"),
          aes(x=x, y=255-y), color = "blue")+
annotate("text", x = 100, y = 100, 
	         label = paste("ln(L) =", round(sum(df$broccoli), 1)), 
	         color = "red", size = 10) +
theme_void()+ 
theme(legend.position = "none")
dev.off()

