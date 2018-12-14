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
### specify which kernels we want to use in model 5
c("apple", "rotated_asparagus", "rotated_banana", "birthdaycake", "bread1", "bread2"               "bread3"               "bread4"              
[17] "bread5"               "broccoli"             "cake"                 "carrot"              
[21] "cookie"               "donut"                "grapes"               "hamburger"           
[25] "hotdog"               "icecream"             "lollipop"             "mushroom"            
[29] "onion"                "peanut"               "pear"                 "peas"                
[33] "pineapple"            "pizza"                "pizza1"               "pizza2"              
[37] "pizza3"               "pizza4"               "pizza5"               "popsicle"            
[41] "potato"               "rotated_apple"        "rotated_asparagus"    "rotated_banana"      
[45] "rotated_banana1"      "rotated_banana2"      "rotated_banana3"      "rotated_banana4"     
[49] "rotated_banana5"      "rotated_birthdaycake" "rotated_blackberry"   "rotated_blueberry"   
[53] "rotated_bread"        "rotated_bread1"       "rotated_bread2"       "rotated_bread3"      
[57] "rotated_bread4"       "rotated_bread5"       "rotated_broccoli"     "rotated_cake"        
[61] "rotated_carrot"       "rotated_cookie"       "rotated_donut"        "rotated_grapes"      
[65] "rotated_hamburger"    "rotated_hotdog"       "rotated_icecream"     "rotated_lollipop"    
[69] "rotated_mushroom"     "rotated_onion"        "rotated_peanut"       "rotated_pear"        
[73] "rotated_peas"         "rotated_pineapple"    "rotated_pizza"        "rotated_pizza1"      
[77] "rotated_pizza2"       "rotated_pizza3"       "rotated_pizza4"       "rotated_pizza5"      
[81] "rotated_popsicle"     "rotated_potato"       "rotated_sandwich"     "rotated_strawberry"  
[85] "rotated_stringbean"   "rotated_watermelon"   "rotated_watermelon1"  "rotated_watermelon2" 
[89] "rotated_watermelon3"  "rotated_watermelon4"  "rotated_watermelon5"  "sandwich"            
[93] "strawberry"           "stringbean"           "watermelon"           "watermelon1"         
[97] "watermelon2"          "watermelon3"          "watermelon4"          "watermelon5"         

### get predictions from all algorithms
pred.data <- pred.data %>%
	mutate(lik.all  = map2(lik, lik.rotated, ~ c(.x,.y) %>% sort(decreasing = T)),
	       pred0 = map(lik, ~.[!grepl("[0-9]",names(.))] %>% sort(decreasing = T)),
	       lik0 = map(pred0, ~.[1]) %>% unlist,
	       pred0 = map(pred0, ~names(.)[1]) %>% unlist,
	       pred1 = map(lik.rotated, ~.[!grepl("[0-9]",names(.))] %>% sort(decreasing = T)),
	       lik1 = map(pred1, ~.[1]) %>% unlist,
	       pred1 = map(pred1, ~names(.)[1]) %>% unlist %>% remove.prefix,
	       pred2 = ifelse(lik0 > lik1, pred0, pred1),
	       pred3 = map(lik.all, ~names(.)[1]) %>% unlist %>% remove.prefix)

pred.data <- pred.data %>%
select(key_id, truth, pred0, pred1, pred2, pred3, lik0, lik1, lik, lik.rotated, lik.all, data, data.rotated)

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