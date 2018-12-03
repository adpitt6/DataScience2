library(tidyverse)
library(reshape2)
library(tidyr)
library(dbscan)

### Get binary data files
bin.files <- dir("binary_data")
load(file.path("binary_data",bin.files[30]))

# get info about edges and where they occur
edges <- 
train %>% 
mutate(y = y +255) %>% 
select(-stroke) %>%
nest(-key_id) %>%
	# select only lowest sidehits and rightmost top/bottom hits
mutate(xmax = map(data, ~max(.$x)) %>% unlist,
       xmin = map(data, ~min(.$x)) %>% unlist,
       ymax = map(data, ~max(.$y)) %>% unlist,
       ymin = map(data, ~min(.$y)) %>% unlist,
       top_x = map(data, ~max(.$x[.$y == max(.$y)]))%>% unlist,
       top_y = ymax,
       bottom_x = map(data, ~max(.$x[.$y == min(.$y)]))%>% unlist,
       bottom_y = ymin,
       right_x = xmax, 
       right_y = map(data, ~min(.$y[.$x == max(.$x)]))%>% unlist,
       left_x = xmin,
       left_y = map(data, ~min(.$y[.$x == min(.$x)]))%>% unlist) %>%
mutate(height = ymax - ymin, width = xmax- xmin,
       hw = height / width)

### The set of points (xmax, xmin, ymax, ymin) give us information
### about how to draw a "frame" around the image

### The set of points (top_x, top_y, bottom_y, bottom_x, right_x, right_y,
### left_x, left_y) gives us the frame information as well as WHERE the 
### image hits that frame.

### We can perform clustering on both to try to sort out the image types.

# fit db clustering 
mod1 <- hdbscan(x = select(edges, xmax, xmin, ymax, ymin), minPts = 50)

ggplot(edges) + 
geom_histogram(aes(x = hw)) + 
xlim(0,4)

hullplot(select(edges, xmax, xmin, ymax, ymin), mod1)


# fit model based on location of hits
mod1 <- kmeans(x = select(edges, top_x : left_y),
	   centers = 3)
edges$clust1 <- mod1$cluster

mod2 <- hdbscan(x = select(edges, top_x : left_y), minPts = 200)
edges$clust2 <- mod2$cluster

hullplot(select(edges, top_x : left_y), mod2)

# plot clusters
ggplot(edges) + 
geom_point(aes(x = top_x, y = right_y, 
	   color = factor(clust1)))
ggplot(edges) + 
geom_point(aes(x = top_x, y = right_y, 
	   color = factor(clust2)))

# plot sketches for different clusters
train <- edges %>% unnest

ggplot(train) +
geom_path(aes(x = x, y = y), size = 0.003) + 
facet_wrap("clust1")

ggplot(train) +
geom_path(aes(x = x, y = y), size = 0.003) + 
facet_wrap("clust2")

train$tall <- with(train, factor(height > width))

ggplot(train) +
geom_path(aes(x = x, y = y), size = 0.003) + 
facet_wrap("tall")

