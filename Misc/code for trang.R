library(dplyr)
library(spatstat)

cmat.rotated <- #start with some n x n matrix using whatever function you like. I used cor()

### Heat Map and Dendogram for Rotated Kernels

# this performs heirarcical clustering
# we will use the ordering from this in our heat map
mod1 <- 1 - cmat.rotated %>% as.dist %>% hclust(distance) 

# plot the dendogram (representation of heirarchical clustering)
plot(mod1, 
     main="Correlation Clustering of Rotated Kernels", xlab="")

# grab order for correlation matrix
a <- mod1$order

# rearrange by similarity
cmat.rotated <- cmat.rotated[a,][,a]
x <- colnames(cmat.rotated)

### Plot correlation matrix

png("Rotated_Kernel_Correlation_Mat.png", height = 1100, width = 1100)
{
	# Set margins to make room for x axis labels
	par(mar = c(7, 8, 1, 1) + 0.1,
	    ps = 20, cex = 1, cex.main = 2)
	
	# plots image of matrix
	plot(im(cmat.rotated), main = "") 
	
	# add title
	title("Similarity of Class Kernels: Correlation", line = -3)
	
	# adds appropriate labels to plot
	text(1:length(x), par("usr")[1] + 1, srt = 90, adj = 1,
	     labels = x, cex = 1, xpd = TRUE)
	text(par("usr")[1] + 1, 1:length(x), adj = 1,
	     labels = x, cex = 1, xpd = TRUE)
}
dev.off()

