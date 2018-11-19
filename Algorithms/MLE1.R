# This file contains the code to process the data for the "dumb likelihood"
# approach. 

# packages
library(tidyverse)
library(stats)
library(ks)

### Grab food names
foods <- read_delim("food.txt", delim = "\n",col_names = F)[[1]]

### Create data frame for likelihoods
likelihoods <- expand.grid(x = 0:255, y = 0:255) %>% as.tibble

for(i in foods){
	load(file.path("summary_data",paste0(i, ".rdata")))
	
	# convert y values to positives
	df.sum <- df.sum %>% mutate(y = y+255)
	
	# name data column after image type
	names(df.sum)[3] <- i
	
	# join with likelihood data frame
	likelihoods <- left_join(likelihoods, df.sum)
	
	# add 0s to na values
	likelihoods[i][is.na(likelihoods[i]),] <- 0
	
	#
	likelihoods[i] <- 
		kde(x = likelihoods[,1:2],
	    	    w = likelihoods[[i]],
		    eval.points = likelihoods[,1:2])$estimate
	
	# normalize to sum to 1 and take log
	likelihoods[i] <- log(likelihoods[i] / sum(likelihoods[i]))
}

names(likelihoods) <- gsub(" ","",names(likelihoods))
save(likelihoods, file = file.path("Algorithms","Likelihoods_MLE1.rdata"))