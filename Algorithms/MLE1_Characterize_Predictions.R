### Packages
library(tidyverse)

### Grab Prediction Files
bin.files1 <- dir("MLE1_Predictions",full.names = T)
bin.files2 <- dir("MLE2_Predictions",full.names = T)

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
pred.data1 <- combine.data(bin.files1)
pred.data2 <- combine.data(bin.files2)

pred.data <- inner_join(pred.data1, select(pred.data2, -data), 
		by = c("key_id","truth"), suffix = c(".1", ".2"))
rm(bin.files1, bin.files2, pred.data1, pred.data2, combine.data)

### get accuracy
accuracy <- 
pred.data %>%
group_by(truth) %>%
summarise(correct1 = sum(pred1.1==truth),
          correct2 = sum(pred1.2==truth)) %>%
mutate(improved = correct2 > correct1,
       improvement = correct2/correct1) %>%
arrange(desc(correct1)) 

accuracy %>% print.data.frame()

# which foods look like other foods?
# filter(accuracy, n.mode1 > correct) 

# prediction matrix
pred.mat <- pred.data %>% with(table(truth, pred1.1))
image(pred.mat)

# save accuracy
save(accuracy, file = file.path("Algorithms","MLE2_Accuracy.rdata"))

### overall accuracy
#accuracy %>% summarise(total.acc =  sum(correct)/n()/1000)
accuracy %>%
summarise(total.acc1 =  sum(correct1)/n()/1000,
          total.acc2 =  sum(correct2)/n()/1000)

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