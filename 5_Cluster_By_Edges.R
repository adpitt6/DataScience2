library(tidyverse)
library(reshape2)
library(tidyverse)
library(ks) # use for kernel estimation

### Get binary data files
bin.files <- dir("cluster_data")
f <- 
c("watermelon", "pizza", "bread", "banana") %>% 
sapply(grepl, x = bin.files) %>%
apply(1, any)

### Create file for sketch plots
if(!file.exists("Cluster_Plots")) dir.create("Cluster_Plots")

### generate data frame for likelihoods
likelihoods <- expand.grid(x = 0:255, y = 0:255) %>% as.tibble

### Loop Through Files
for(i in bin.files[f]){
	food.i <- sub(".rdata","",i)
	load(file.path("cluster_data",i))
	edges$cluster <- kmeans(x = select(edges, top_x : left_y),
		    centers = 5)$cluster
	train <- edges %>% select(data, data1, cluster) %>% unnest %>%
		mutate(y = y-255)
	
	### sketch plots for clusters
	png(file = file.path("Cluster_Plots",paste0(food.i,"_rotated.png")))
	print(
		ggplot(train) +
		geom_path(aes(x = x1, y = y1), size = 0.003) + 
		facet_wrap("cluster") +
		ggtitle(paste("K-Means Clusters for",food.i, "Rotated")) +
		theme_void()
	)
	dev.off()
	
	png(file = file.path("Cluster_Plots",paste0(food.i,".png")))
	print(
		ggplot(train) +
			geom_path(aes(x = x, y = y), size = 0.003) + 
			facet_wrap("cluster") +
			ggtitle(paste("K-Means Clusters for",food.i)) +
			theme_void()
	)
	dev.off()
	
	### summarise
	df.sum <- train %>%
	group_by(x,y,cluster) %>%
	summarise(n = n()) %>%
	mutate(cluster = paste0(food.i, cluster)) %>% 
	dcast(x+y~cluster, fill = 0, value.var = "n")
	
	df.sum1 <- train %>%
		group_by(x1,y1,cluster) %>%
		summarise(n = n()) %>%
		mutate(cluster = paste0("rotated_",food.i, cluster)) %>% 
		dcast(x1+y1~cluster, fill = 0, value.var = "n")
	
	# join with likelihood data frame
	likelihoods <- left_join(likelihoods, df.sum) %>% 
		left_join(df.sum1, by=c("x" = "x1", "y" = "y1"))
}

# add 0s to na values
likelihoods[is.na(likelihoods)] <- 0

# normalize to sum to 1 and take log
normal.log <- function(x) log(x / sum(x))

# kernel smooth each location
likelihoods <- likelihoods %>%
lapply(FUN = function(y){
	kde(x = likelihoods[,1:2],
	    w = y,
	    eval.points = likelihoods[,1:2])$estimate %>%
	normal.log
}) %>%
as.tibble() %>%
mutate(x = likelihoods[[1]], y = likelihoods[[2]])

likelihoods[is.na(likelihoods)] <- -50


save(likelihoods, file = file.path("Empirical_Kernel","Clustered_Kernels.rdata"))

