
### This file contains basically the same code as the MLE1 predictions
### but it rotates each doodle before predicting. Hence, we are using 
### the pre-rotated kernels.

my.dir <- "Predictions"

### create directory of predictions data
if(!file.exists(my.dir)) dir.create(my.dir)

### packages
library(tidyverse)
source("0_Preprocessing_Functions.R")

### load kernels
load(file = file.path("Empirical_Kernel","Clustered_Kernels.rdata"))
clustered <- likelihoods
load(file = file.path("Empirical_Kernel","Kernels_MLE1.rdata"))
likelihoods <- likelihoods[,!grepl("[1-9]",colnames(likelihoods))] 
likelihoods <- cbind(likelihoods, clustered[,-1:-2])

likelihoods.rotated <- 
	cbind(likelihoods[,1:2],likelihoods[,grepl("rotated",colnames(likelihoods))])
likelihoods <- 
	cbind(likelihoods[,1:2], likelihoods[,!grepl("rotated",colnames(likelihoods))][,-1:-2])

### Grab Binary File Test Sets
bin.files <- dir("binary_data_test")

### Loop Through Files and Plot All Data
for(i in bin.files){
	### Load Files
	load(file.path("binary_data_test",i))
	### Save food name
	food.i <- sub(".rdata","",i) 
	
	### Make Predictions
	predictions <- 
	test %>%
	mutate(y = y-255) %>%
	select(-stroke) %>%
	nest(-key_id) %>%
	mutate(data.rotated = map(data, ~ rotate.vert(.) %>% integer.xy), 
	       lik = map(data, ~left_join(.,likelihoods, by = c("x","y")) %>%
	                 	select(., -x, -y) %>% 
	                 	colSums %>% 
	                 	sort(decreasing = T)
	                 ),
	       lik.rotated = map(data.rotated, ~left_join(.,likelihoods.rotated, by = c("x","y")) %>%
	                 	select(., -x, -y) %>% 
	                 	colSums %>% 
	                 	sort(decreasing = T)
	       ),
	       pred1 = map(lik, ~names(.[1])) %>% unlist,
	       pred2 = map(lik.rotated, ~names(.[1])) %>% unlist)

	# add truth label
	predictions$truth <- food.i
	
	# savel food-specific predictions data
	save(predictions, 
	     file = file.path(my.dir,
	     	          paste0(food.i,"_predictions.rdata")))
	cat(food.i,"predicted \n")
}
