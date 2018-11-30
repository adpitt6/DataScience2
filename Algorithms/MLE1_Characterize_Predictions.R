### Packages
library(tidyverse)

### Grab Prediction Files
bin.files <- dir("MLE1_Predictions",full.names = T)

for(i in bin.files){
	load(i)
	if(exists("pred.data")){
		pred.data <- rbind(pred.data, predictions)
	}else{
		pred.data <- predictions
	}
}
rm(bin.files, i, predictions)

### fix truth labels
pred.data <- 
pred.data %>% mutate(truth = gsub(" ", "", truth))


### get accuracy
accuracy <- 
pred.data %>%
mutate(truth = gsub(" ", "", truth)) %>%
group_by(truth) %>%
summarise(correct = sum(pred1==truth),
          mode1 = names(sort(table(pred1), decreasing = T)[1]),
          n.mode1 = sum(pred1==mode1),
          mode2 = names(sort(table(pred1), decreasing = T)[2]),
          n.mode2 = sum(pred1==mode2),
          mode3 = names(sort(table(pred1), decreasing = T)[3]),
          n.mode3 = sum(pred1==mode3)) %>%
arrange(desc(correct)) 

accuracy %>% print.data.frame()

# which foods look like other foods?
filter(accuracy, n.mode1 > correct) 

# prediction matrix
pred.mat <- pred.data %>% with(table(truth, pred1))
image(pred.mat)

# save accuracy
save(accuracy, file = file.path("Algorithms","MLE1_Accuracy.rdata"))

### overall accuracy
accuracy %>% summarise(total.acc =  sum(correct)/n()/1000)

### identify and plot bad predictions
bad.apples <- 
pred.data %>%
filter(truth !=pred1) %>%
group_by(truth) %>%
sample_n(100) %>%
select(-lik) %>%
unnest

pdf(file = "Algorithms/Bad_Apples.pdf", width = 15, height = 15)
for(i in unique(bad.apples$truth)){
	print(
	ggplot(subset(bad.apples, truth ==i)) +
		geom_path(aes(x=x, y=y)) +
		theme_void() +
		geom_text(aes(x = 100, y = 100, label = pred1), col = "red") +
		facet_wrap("key_id", nrow = 10) + 
		ggtitle(paste("100 Bad Predictions for", i))
	)
}
dev.off()

### look at clusters of mis-classifications
### look at correlations between kernels