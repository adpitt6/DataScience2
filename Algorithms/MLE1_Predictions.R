
### create directory of predictions data
if(!file.exists("MLE1_Predictions")) dir.create("MLE1_Predictions")

### packages
library(tidyverse)

### load kernels
load(file = file.path("Algorithms","MLE1_Likelihoods.rdata"))

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
	mutate(lik = map(data, ~left_join(.,likelihoods, by = c("x","y"))),
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
	     file = file.path("MLE1_Predictions",
	     	          paste0(food.i,"_predictions.rdata")))
	cat(food.i,"predicted \n")
}