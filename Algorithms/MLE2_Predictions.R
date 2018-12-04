
### This file contains basically the same code as the MLE1 predictions
### but it rotates each doodle before predicting. Hence, we are using 
### the pre-rotated kernels.

my.dir <- "MLE2_Predictions"

### create directory of predictions data
if(!file.exists(my.dir)) dir.create(my.dir)

### packages
library(tidyverse)
source("0_Preprocessing_Functions.R")

### load pre-rotated kernels
load(file = file.path("Algorithms","Rotated_Likelihoods_MLE1.rdata"))

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
	mutate(y = y+255) %>%
	select(-stroke) %>%
	nest(-key_id) %>%
	mutate(data = map(data, ~ rotate.vert(.) %>% integer.xy), 
	       lik = map(data, ~left_join(.,likelihoods, by = c("x","y"))),
	       lik = map(lik, ~select(., -x, -y) %>% 
	                 	colSums %>% 
	                 	sort(decreasing = T)
	                 ),
	       pred1 = map(lik, ~names(.[1])),
	       pred2 = map(lik, ~names(.[2])),
	       pred3 = map(lik, ~names(.[3])))
	# unlist predictions
	predictions$pred1 <- unlist(predictions$pred1)
	predictions$pred2 <- unlist(predictions$pred2)
	predictions$pred3 <- unlist(predictions$pred3)
	
	# add truth label
	predictions$truth <- food.i
	
	# savel food-specific predictions data
	save(predictions, 
	     file = file.path(my.dir,
	     	          paste0(food.i,"_predictions.rdata")))
	cat(food.i,"predicted \n")
}