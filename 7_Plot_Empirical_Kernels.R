library(reshape2)
library(tidyverse)
library(Hmisc)
library(spatstat)

### load kernels
load(file = file.path("Empirical_Kernel","Clustered_Kernels.rdata"))
clustered <- likelihoods %>% 
	melt(id.vars = c("x","y"))

load(file = file.path("Empirical_Kernel","Kernels_MLE1.rdata"))
likelihoods <- likelihoods[,!grepl("[1-9]",colnames(likelihoods))] %>% 
	melt(id.vars = c("x","y"))

### melt kernel data
likelihoods <- likelihoods %>%
	mutate(class = sub("rotated_","",variable),
	       rotated = grepl("rotated_",variable) ,
	       rotated = rotated %>% factor(labels = c("Raw", "Rotated")),
	       variable = paste(capitalize(class),rotated),
	       y = ifelse(rotated =="Rotated", y, 255-y))
	
### Plot without rotations
png(filename = file.path("Empirical_Kernel","Kernels_NoRotations.png"),
    width = 1800, height = 1800)
print(
	ggplot(likelihoods %>% filter(rotated =="Raw"), aes(x, y)) +
		geom_raster(aes(fill = exp(value)))+
		facet_wrap("variable", ncol  = 5) + 
		theme_void() +
		theme(legend.position = "none",
		      strip.text = element_text(size = 25))+
		scale_fill_continuous(low="white", high = "black")
)
dev.off()

### Plot with rotations
png(filename = file.path("Empirical_Kernel","Kernels_withRotations.png"),
    width = 3400, height = 1800)
print(
ggplot(likelihoods, aes(x, y)) +
	geom_raster(aes(fill = exp(value)))+
	facet_wrap("variable", ncol  = 12) + 
	theme_void() +
	theme(legend.position = "none",
	      strip.text = element_text(size = 25))+
	scale_fill_continuous(low="white", high = "black")
)
dev.off()

### Plot clustered kernels only
png(filename = file.path("Empirical_Kernel","Clustered_Kernels.png"),
    width = 1800, height = 1800)
print(
	ggplot(clustered, aes(x, y)) +
		geom_raster(aes(fill = exp(value)))+
		facet_wrap("variable", ncol  = 5) + 
		theme_void() +
		theme(legend.position = "none",
		      strip.text = element_text(size = 25))+
		scale_fill_continuous(low="black", high = "white")
)
dev.off()

###########################################################
### Compare correlation of kernels
library(spatstat) # "im" function 

load(file = file.path("Empirical_Kernel","Clustered_Kernels.rdata"))
clustered <- likelihoods
load(file = file.path("Empirical_Kernel","Kernels_MLE1.rdata"))
likelihoods <- likelihoods[,!grepl("[1-9]",colnames(likelihoods))]

# correlation matrix
cmat <- 
cbind(likelihoods[,!grepl("rotated",colnames(likelihoods))][,-1:-2],
      clustered[,!grepl("rotated",colnames(clustered))][,-1:-2])%>%
	cor() 

cmat.rotated <- 
	cbind(likelihoods[,grepl("rotated",colnames(likelihoods))],
	      clustered[,grepl("rotated",colnames(clustered))])%>%
	cor() 

rownames(cmat.rotated) <- sub("rotated_", "",rownames(cmat.rotated))
colnames(cmat.rotated) <- sub("rotated_", "",colnames(cmat.rotated))

### Plot Dendogram
dissimilarity <- 1 - cmat
distance <- as.dist(dissimilarity)
mod1 <- hclust(distance)
png("Empirical_Kernel/Kernel_Dendogram.png",height = 1000, width = 1000)
plot(mod1, 
     main="Correlation Clustering of Kernels", xlab="")
dev.off()

#grab order for matrix
a <- mod1$order

# rearrange by similarity
cmat <- cmat[a,][,a]
x <-  colnames(cmat)

### Plot correlation matrix
# Set margins to make room for x axis labels

# Create plot with no x axis and no x axis label
png("Empirical_Kernel/Kernel_Correlation_Mat.png", height = 1100, width = 1100)
{
	par(mar = c(7, 8, 1, 1) + 0.1,
	    ps = 20, cex = 1, cex.main = 2)
	plot(im(cmat), main = "") 
	title("Similarity of Class Kernels: Correlation", line = -3)
	text(1:length(x), par("usr")[1] + 1, srt = 90, adj = 1,
	     labels = x, cex = 1, xpd = TRUE)
	text(par("usr")[1] + 1, 1:length(x), adj = 1,
	     labels = x, cex = 1, xpd = TRUE)
}
dev.off()


### Heat Map and Dendogram for Rotated Kernels
dissimilarity <- 1 - cmat.rotated
distance <- as.dist(dissimilarity)
mod1 <- hclust(distance)
png("Empirical_Kernel/Rotated_Kernel_Dendogram.png",height = 1000, width = 1000)
plot(mod1, 
     main="Correlation Clustering of Rotated Kernels", xlab="")
dev.off()

#grab order for matrix
a <- mod1$order

# rearrange by similarity
cmat.rotated <- cmat.rotated[a,][,a]
x <-  colnames(cmat.rotated)

### Plot correlation matrix
# Set margins to make room for x axis labels

# Create plot with no x axis and no x axis label
png("Empirical_Kernel/Rotated_Kernel_Correlation_Mat.png", height = 1100, width = 1100)
{
par(mar = c(7, 8, 1, 1) + 0.1,
    ps = 20, cex = 1, cex.main = 2)
plot(im(cmat.rotated), main = "") 
title("Similarity of Class Kernels: Correlation", line = -3)
text(1:length(x), par("usr")[1] + 1, srt = 90, adj = 1,
     labels = x, cex = 1, xpd = TRUE)
text(par("usr")[1] + 1, 1:length(x), adj = 1,
     labels = x, cex = 1, xpd = TRUE)
}
dev.off()

