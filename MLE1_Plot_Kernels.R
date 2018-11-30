### load kernels
load(file = file.path("Algorithms","MLE1_Likelihoods.rdata"))
library(reshape2)

### melt likelihood data
likelihoods <- 
	melt(likelihoods, id.vars = c("x","y"))

png(filename = "Algorithms/MLE1_Kernels.png",
    width = 1500, height = 1800)
ggplot(likelihoods, aes(x, y)) +
	geom_raster(aes(fill = exp(value)))+
	facet_wrap("variable", nrow = 6) + 
	theme_void() +
	theme(legend.position = "none",
	      strip.text = element_text(size = 50)) 
dev.off()
