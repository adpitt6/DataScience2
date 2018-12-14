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

rm(bin.files, combine.data)

remove.prefix <- function(x) {
	x <- gsub("[0-9]","", x)
	x <- gsub("rotated_","", x)
}

### get highest from either algorithm
pred.data <- pred.data %>%
	mutate(lik.all  = map2(lik, lik.rotated, ~ c(.x,.y) %>% sort(decreasing = T)),
	       pred0 = map(lik, ~.[!grepl("[0-9]",names(.))] %>% sort(decreasing = T)),
	       pred0 = map(pred0, ~names(.)[1]) %>% unlist,
	       pred1 = remove.prefix(pred1),
	       pred2 = remove.prefix(pred2),
	       pred3 = map(lik.all, ~names(.)[1]) %>% unlist %>% remove.prefix)

### get accuracy
accuracy <- 
pred.data %>%
group_by(truth) %>%
summarise(pred0 = sum(pred0==truth),
          pred1 = sum(pred1==truth),
          pred2 = sum(pred2==truth),
          pred3 = sum(pred3==truth)) %>%
arrange(desc(pred0)) 

accuracy %>% print.data.frame()

### get false positives
truepos <- 
	pred.data %>%
	select(truth, pred0, pred1, pred2, pred3) %>%
	melt(id.vars = "truth") %>%
	group_by(value, variable) %>%
	summarise(truepos = mean(value==truth)) %>%
	dcast(value ~ variable,value.var = "truepos") %>%
	arrange(pred1)

truepos

both <- left_join(
	melt(truepos, id.vars = "value", value.name = "pos"),
	melt(accuracy, id.vars = "truth", value.name = "acc"),
	by = c("value"="truth", "variable"))

png("Empirical_Kernel/Model1.png", height = 400, width = 400)
ggplot(both %>% filter(variable == "pred0")) + 
geom_point(aes(x = pos, y = acc/1000)) +
geom_text(aes(x = pos, y = acc/1000, label = value),
          vjust = 1) +
xlab("Class Positive Predictive Value")+
ylab("Class Sensitivity") +
xlim(0,.55) + ylim(0,.85) + 
theme_bw()
dev.off()

### Plot # correct predictions by class
png("Empirical_Kernel/Correct_Classifications.png", height = 1000, width = 500)
{
	par(mar = c(1, 10, 5, 2) + 0.1,
	    ps = 20, cex = 1, cex.main = 1)
	plot(im(select(accuracy, correct1:correct3) %>% as.matrix),
	     main = "")
	title("No. Correct Classifications (n = 1000)", line = 0)
	text(par("usr")[1] , 1:30, adj = 1,
	     labels = accuracy$truth, cex = 1, xpd = TRUE)
}
dev.off()


# which foods look like other foods?
# filter(accuracy, n.mode1 > correct) 

# prediction matrix
pred.mat <- pred.data %>% with(table(truth, pred0))
ord <- order(diag(pred.mat),decreasing = T)
pred.mat <- pred.mat[ord,][,ord]
im(pred.mat) %>% plot

png("Empirical_Kernel/Classifications0.png", height = 1100, width = 1100)
{
	par(mar = c(7, 8, 1, 1) + 0.1,
	    ps = 20, cex = 1, cex.main = 2)
	plot(im(pred.mat) , main = "",
	     xlab = "Prediction",
	     ylab = "Truth") 
	title("Classification Matrix", line = -3)
	text(1:nrow(pred.mat), 
	     par("usr")[1] + 1, srt = 90, adj = 1,
	     labels = colnames(pred.mat), cex = 1, xpd = TRUE)
	text(par("usr")[1] + 1, 1:nrow(pred.mat), adj = 1,
	     labels = colnames(pred.mat), cex = 1, xpd = TRUE)
	text(30, par("usr")[1]-3, labels = "Prediction")
	text( par("usr")[1]+2,31, labels = "Truth")
}
dev.off()

# save accuracy
save(accuracy, file = file.path("Empirical_Kernel","Accuracy.rdata"))
save(truepos, file = file.path("Empirical_Kernel","True_Pos.rdata"))

### overall accuracy
#accuracy %>% summarise(total.acc =  sum(correct)/n()/1000)
accuracy %>%
summarise(total.acc0 =  sum(pred1)/n()/1000,
          total.acc1 =  sum(pred1)/n()/1000,
          total.acc2 =  sum(pred2)/n()/1000,
          total.acc3 =  sum(pred3)/n()/1000)

### identify and plot bad predictions
bad.apples <- 
pred.data %>%
filter(truth !=pred0) %>%
group_by(truth) %>%
sample_n(1) %>%
select(key_id, data, pred0) %>%
unnest

png(file = "Empirical_Kernel/Bad_Apples1.png", width = 1000, height = 1000)
	print(
	ggplot(bad.apples) +
		geom_path(aes(x=x, y=255-y)) +
		theme_void() +
		geom_text(aes(x = 255, y = 0, label = pred0), 
		          col = "red", size = 6,
		          hjust = 1, vjust = 0) +
		facet_wrap("truth", nrow = 6) + 
		theme(strip.text = element_text(size = 16, color = "#8bc34a"))
	)
dev.off()

### look at clusters of mis-classifications
### look at correlations between kernels