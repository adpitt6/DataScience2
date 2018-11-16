# This file contains the code to process the data for the "dumb likelihood"
# approach. 

# packages
library(tidyverse)

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
	
	# add small mass to na values
	likelihoods[i][is.na(likelihoods[i]),] <- 0.01
	
	# normalize to sum to 1 and take log
	likelihoods[i] <- log(likelihoods[i] / sum(likelihoods[i]))
}

names(likelihoods) <- gsub(" ","",names(likelihoods))
save(likelihoods, file = file.path("Algorithms","Likelihoods_MLE1.rdata"))




# convert to matrix
mat <-
df.sum  %>%
spread( y, n) %>%
ungroup %>%
select(-x) %>%
as.matrix
dim(mat)

# add a small amount of mass to coordinates with no
# observed points
mat[is.na(mat)] <- 0.01

# normalize to sum to 1
mat <- mat / sum(mat)
lmat <- log(mat) + min(log(mat)) + 0.01
lmat <- lmat / sum(lmat)

image(lmat)
image(mat)

# function for smoothing out matrix
smoo.mat <- function(mat, p, times=1){
	n = nrow(mat)
	a <- rbind(0,cbind(diag(n-1),0)) 
	smoo <- diag(n)*p + a*(1-p)/2 + t(a)*(1-p)/2
	mat <- (smoo^times) %*% mat %*% (smoo^times)
	mat <- mat / sum(mat)
	return(mat)
}

# smooth out matrix
par(mfrow = c(2,2))
image(smoo.mat(lmat, 0.8))
image(smoo.mat(lmat, 0.8, times = 2))
image(smoo.mat(lmat, 0.8, times = 8))
image(smoo.mat(lmat, 0.8, times = 32))


