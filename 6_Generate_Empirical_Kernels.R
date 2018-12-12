# This file contains the code to process the data for the "dumb likelihood"
# approach. 

# packages
library(tidyverse)
library(stats)
library(ks)

### Grab food names
sum.files <- dir("summary_data")

### Create data frame for likelihoods
likelihoods <- expand.grid(x = 0:255, y = 0:255) %>% as.tibble

for(i in sum.files){
	food.i <- sub(".rdata","",i) 
	load(file.path("summary_data",i))
	
	names(df.sum)[3] <- food.i
	
	# quick fix
	if(min(df.sum$y) > 254) df.sum$y <- df.sum$y - 255

	# join with likelihood data frame
	likelihoods <- left_join(likelihoods, df.sum, by = c("x", "y"))
	
	# add 0s to na values
	likelihoods[is.na(likelihoods)] <- 0
	
	#
	likelihoods[[food.i]] <- 
		kde(x = likelihoods[,1:2],
		    w = likelihoods[[food.i]],
		    eval.points = likelihoods[,1:2])$estimate
	
	# normalize to sum to 1 and take log
	likelihoods[[food.i]] <- log(likelihoods[[food.i]] / sum(likelihoods[[food.i]]))
}

names(likelihoods) <- gsub(" ","",names(likelihoods))
save(likelihoods, file = file.path("Empirical_Kernel","Kernels_MLE1.rdata"))

### Merge with Clustered Kernels
load(file = file.path("Empirical_Kernel","Kernels_MLE1.rdata"))
df <- likelihoods
load(file ="Empirical_Kernel/Clustered_Likelihoods_MLE1.rdata")
likelihoods[is.na(likelihoods)]<- log(0.00000001)

likelihoods <- cbind(df, select(likelihoods,-x,-y))

save(likelihoods, file = file.path("Empirical_Kernel","Combined_Kernels_MLE1.rdata"))
