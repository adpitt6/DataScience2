### load kernels
#load(file = file.path("Algorithms","MLE1_Likelihoods.rdata"))
load(file = file.path("Algorithms","Rotated_Likelihoods_MLE1.rdata"))
library(reshape2)
library(tidyverse)

### melt likelihood data
likelihoods <- 
	melt(likelihoods, id.vars = c("x","y"))

png(filename = "Algorithms/MLE1_Kernels_Rotated.png",
    width = 1500, height = 1800)
#png(filename = "Algorithms/MLE1_Kernels.png",
#    width = 1500, height = 1800)
ggplot(likelihoods, aes(x, y)) +
	geom_raster(aes(fill = exp(value)))+
	facet_wrap("variable", nrow = 6) + 
	theme_void() +
	theme(legend.position = "none",
	      strip.text = element_text(size = 50)) +
	scale_fill_continuous(high ="#66FF66")
dev.off()

###########################################################
### Compare correlation of kernels
library(spatstat) # "im" function 

load(file = file.path("Algorithms","Rotated_Likelihoods_MLE1.rdata"))

# correlation matrix
cmat <- 
select(likelihoods, -x,-y) %>% 
cor() 

### Plot Dendogram
dissimilarity <- 1 - cmat
distance <- as.dist(dissimilarity)
mod1 <- hclust(distance)
png("Algorithms/Kernel_Dendogram.png",height = 1000, width = 1000)
plot(mod1, 
     main="Dissimilarity = 1 - Correlation", xlab="")
dev.off()

#grab order for matrix
a <- mod1$order

# rearrange by similarity
cmat <- cmat[a,][,a]
x <-  colnames(cmat)

### Plot correlation matrix
# Set margins to make room for x axis labels

# Create plot with no x axis and no x axis label
png("Algorithms/Kernel_Correlation_Mat.png", height = 1000, width = 1000)
{
par(mar = c(7, 8, 1, 1) + 0.1,
    ps = 20, cex = 1, cex.main = 2)
plot(im(cmat), main = "") 
title("Similarity of Class Kernels: Correlation", line = -3)
text(1:30, par("usr")[1] + 1, srt = 90, adj = 1,
     labels = x, cex = 1, xpd = TRUE)
text(par("usr")[1] + 1, 1:30, adj = 1,
     labels = x, cex = 1, xpd = TRUE)
}
dev.off()

