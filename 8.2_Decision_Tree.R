### Packages
library(tidyverse)
library(spatstat)
library(caret)
library(reshape2)
library(Hmisc)
#install.packages("rpart")
library(rpart)

###############################################################################
### Data Loading

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

###############################################################################
### Data Formatting
remove.prefix <- function(x) {
	x <- gsub("[0-9]","", x)
	x <- gsub("rotated_","", x)
}

### get highest from either algorithm
tree.data <- pred.data %>%
	mutate(lik.all  = map2(lik, lik.rotated, ~ c(.x,.y) %>% sort(decreasing = T)),
	       lik.class = map(lik.all, names),
	       height = map(data.rotated, ~max(.$y) - min(.$y)),
	       width = map(data.rotated, ~max(.$x) - min(.$x))) %>%
	select(-data,-data.rotated, -lik, -lik.rotated)

head(tree.data)

### more formatting
tree.data$height <- tree.data$height %>% unlist
tree.data$width <- tree.data$width %>% unlist
tree.data <- tree.data %>% unnest

head(tree.data)
### create wide format
tree.data  <- tree.data %>% 
	dcast(key_id + pred1 + pred2 + truth + height + width ~ lik.class,
	      value.var = "lik.all") 

tree.data <- tree.data %>% as.tibble
head(tree.data)

###############################################################################
### Fitting the Tree

# possible trees:
	# rpart, rpart2, rpart1SE (rpart)
	# ctree (party)
	# ada
	# blackboost, bstTree

# use 10-fold cross-validation
tree.data2 <-tree.data %>%
	group_by(truth)%>%
	mutate(isapple = ifelse(truth=="apple", "apple", "no")) %>%
	ungroup %>%
	select(-pred1, -pred2)
dim(tree.data2)




#tree1 <- train(x = select(tree.data2, -key_id, -truth),
	   y = factor(tree.data2$truth),
	   method = "rpart", 
	   metric = "accuracy",
	   verbose = T,
	   trControl = trainControl(method = "cv",number = 1))

# try using rpart function
tree1 <- rpart(data= select(tree.data2, -key_id, -isapple),
	   formula = truth ~ .,
	   method = "class",
	   parms = list(prior = rep(1/30,30)),
	   control =rpart.control(minsplit=20, minbucket=10, cp=0))

par(mai=c(0,0,0,0) )
plot(tree1,compress=T)
text(tree1)
table(tree.data2$pred2)

x <- predict(tree1, type="class")
y <- table(x, truth = tree.data2$truth)

mean(diag(y) / colSums(y))

summary(tree1$error.fold)

png()

