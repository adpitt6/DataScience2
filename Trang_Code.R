Data Science: Quick, Draw! Classification of Edibles
Trang Quynh Nguyen
library(tidyverse)
## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --
## v ggplot2 3.1.0     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.6
## v tidyr   0.8.2     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
library(rjson)
Data wrangling
Data download requires signing into the Kaggle competition, so we cannot download data automaticall. Here we check if the data file has been downloaded and placed in a subdirectory data in the project directory.
if(!dir.exists(here::here("data"))) { dir.create(here::here("data")) }
if(!file.exists(here::here("data", "train_simplified.zip"))) {
	paste0(c("You need to download the data file train_simplified.zip from\n",
	         "https://www.kaggle.com/c/quickdraw-doodle-recognition/data",
	         "first!"))
}
For this project, we will consider only a subset of the categories, including fruits, vegetables, cakes, candies and dishes, which we refer to as “some edibles”.
all.categories <- 
	unzip(here::here("data", "train_simplified.zip"), list = TRUE)$Name %>%
	as.character() %>%
	sort() %>%
	{sub(".csv", "", .)}
some.edibles <- c("apple", "asparagus",
	      "banana", "birthday cake", "blueberry",
	      "blackberry", "bread", "broccoli",
	      "cake", "carrot", "cookie",
	      "donut",
	      "grapes",
	      "hamburger", "hot dog",
	      "ice cream", 
	      "lollipop",
	      "mushroom",
	      "onion",
	      "peanut", "pear", "peas", "pineapple", "pizza", "popsicle",
	      "potato", 
	      "sandwich", "steak", "strawberry", "string bean",
	      "watermelon")
setdiff(some.edibles, all.categories)
## character(0)
The function get.500 below borrows heavily from Lacey’s function format.one.food. It extracts, for each category of drawing and each country of origin, two files, one including up to 500 first drawings that the AI of the game was able to recognize as belonging to the category and up to 500 first drawings that were not recognized. These are stored in two separate files. In each file, points (data rows) that define strokes are nested in strokes (variable stroke), which are nested in picture (variable key_id).
get.500 <- function(category, n_max = 500) {
	if(!dir.exists(here::here("data", "train_simplified"))) {
		dir.create(here::here("data", "train_simplified"))
	}
	if(!dir.exists(here::here("data", "train_simplified", category))) {
		dir.create(here::here("data", "train_simplified", category))
		
		dat <- read_csv(unz(here::here("data", "train_simplified.zip"),
			        paste0(category,".csv")), col_types = cols()) %>%
			select(-c(timestamp, word))
		
		sapply(unique(dat$countrycode), function(country) {
			
			country.dat <- dat %>% 
				filter(countrycode == country) %>%
				select(-countrycode)
			
			sapply(unique(country.dat$recognized), function(recognized.val) {
				sub <- country.dat %>%
					filter(recognized == recognized.val) %>%
					select(-recognized) %>%
					head(n_max) %>%
					mutate(drawing = map(drawing, fromJSON)) %>%
					unnest() %>%
					mutate(drawing = map(drawing, 
						         ~ do.call(., what="rbind") %>%
						         	t %>%
						         	as_tibble())) %>%
					unnest(.id = "stroke") %>%
					mutate(x = V1, y = 255 - V2,
					       x = as.integer(x), y = as.integer(y)) %>%
					select(-c(V1, V2))
				
				if(recognized.val == "True") {
					file.name <- paste0(country, "_recognized.csv")
				} else {
					file.name <- paste0(country, "_unrecognized.csv")
				}
				
				write_csv(sub, 
				          path = here::here("data", "train_simplified",
				          	      category, file.name))
			})
		})
	}
}
Let’s extract such data for all the edibles we are interested in, if the data are not yet extracted.
for(i in some.edibles) { get.500(i) }
Image pre-processing
The purpose of this part of the work is to hopefully come up with procedures to pre-process the images to enhance the signal of the categories they belong to.
These procedures can be applied to drawings in the train set before using them to train classification models, as well as to drawings in the test set before feeding them to the classifier.
Ideally we want to run one procedure on all the drawings (of any type), as that is the simplest.
I don’t yet rule out the option of multiple procedures tailored to different drawing categories. That makes things more complicated for sure, but there must be way to bundle pre-processing and classifcation such that for an unknown image, we could (1) pre-process as if apple and get an apple score, and (2) pre-process as if banana and get a banana score, etc. and (n) compare apple, banana, etc., scores and classify. This would require some scoring system that is pre-processing-invariant. The log-likelihood does not satisfy this criterion, as image distortion changes the data and thus changes the basis of the log-likelihood.
So the goal of this step is dual: find a pre-processing procedure that works best for each category, and find a pre-processing procedure that is category-blind.
We first look at each category separately, and then look at them together.
To simplify things, we will consider data from the US at this point.
Category apple
Below is a function to look at several recognized and unrecognized drawings of a category.
several.samples <- function(category, country = "US") {
	p <- bind_rows(
		read_csv(here::here("data", "train_simplified", category,
			        paste0(country, "_recognized.csv")),
		         col_types = cols()) %>%
			filter(key_id %in% sample(unique(key_id), 6)) %>%
			mutate(key_id = 
			       	factor(key_id, labels = paste0("sample ", seq(1, 6))),
			       status = "recognized"),
		read_csv(here::here("data", "train_simplified", category,
			        paste0(country, "_unrecognized.csv")),
		         col_types = cols()) %>%
			filter(key_id %in% sample(unique(key_id), 6)) %>%
			mutate(key_id = 
			       	factor(key_id, labels = paste0("sample ", seq(1, 6))),
			       status = "unrecognized")
	) %>%
		ggplot() +
		geom_path(aes(x = x, y = y, group = stroke)) +
		theme_bw() +
		theme(legend.position = "none") +
		facet_grid(status ~ key_id) +
		ggtitle(paste0("Several recognized and unrecognized ",
			   category, " samples"))
	p
}
Let’s look at some apples
several.samples("apple")
￼
We see that these drawings are scaled and centered so that they touch the top and left sides of the frame, and at least one of the other two sides. We will now overlay many apples (all recognized ones) on one another to see their commonality. We want to compare plots that use the original data and plots that use one or more image processing tricks: dropping the drawings to the bottom of the frame, centering, stretching, etc. To do this, we need the following functions.
to.bottom <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(y = y - min(y)) %>%
		ungroup()
}

to.top <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(y = y + (255 - max(y))) %>%
		ungroup()
}

to.right <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(x = x + (255 - max(x))) %>%
		ungroup()
}

to.left <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(x = x - min(x)) %>%
		ungroup()
}

center.x <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(x = x + (0+255)/2 - ((min(x)+max(x))/2)) %>%
		ungroup()
}

center.y <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate(y = y + (0+255)/2 - ((min(y)+max(y))/2)) %>%
		ungroup()
}

stretch.x <- function(data) {
	data %>%
		to.left() %>%
		group_by(key_id) %>%
		mutate(x = x * 255 / max(x)) %>%
		ungroup()
}

stretch.y <- function(data) {
	data %>%
		to.bottom() %>%
		group_by(key_id) %>%
		mutate(y = y * 255 / max(y)) %>%
		ungroup()
}

scale.to.width <- function(data) {
	data %>%
		to.bottom() %>%
		to.left() %>%
		group_by(key_id) %>%
		mutate(x2 = x * 255 / max(x),
		       y = y * 255 / max(x),
		       x = x2) %>%
		select(-x2) %>%
		ungroup()
}

scale.to.height <- function(data) {
	data %>%
		to.bottom() %>%
		to.left() %>%
		group_by(key_id) %>%
		mutate(y2 = y * 255 / max(y),
		       x = x * 255 / max(y),
		       y = y2) %>%
		select(-y2) %>%
		ungroup()
}

integer.xy <- function(data) {
	data %>%
		mutate(x = as.integer(round(x)),
		       y = as.integer(round(y)))
}
Let’s make sure these functions work the way they are supposed to
dat <- bind_rows(
	read_csv(here::here("data", "train_simplified", "apple",
		        "US_recognized.csv"),
	         col_types = cols()) %>% 
		filter(key_id == min(key_id)),
	read_csv(here::here("data", "train_simplified", "apple",
		        "US_recognized.csv"),
	         col_types = cols()) %>% 
		filter(key_id == max(key_id))
)
dat <- bind_rows(
	dat                       %>% mutate(image = "original"),
	dat %>% to.bottom()       %>% mutate(image = "to.bottom"),
	dat %>% to.right()        %>% mutate(image = "to.right"),
	dat %>% center.x()        %>% mutate(image = "center.x"),
	dat %>% center.y()        %>% mutate(image = "center.y"),
	dat %>% stretch.x()       %>% mutate(image = "stretch.x"),
	dat %>% stretch.y()       %>% mutate(image = "stretch.y"),
	dat %>% scale.to.width()  %>% mutate(image = "scale.to.width"),
	dat %>% scale.to.height() %>% mutate(image = "scale.to.height")
) %>%
	mutate(image = factor(image, levels = c("original",
				    "to.bottom",
				    "to.right",
				    "center.x",
				    "center.y",
				    "stretch.x",
				    "stretch.y",
				    "scale.to.width",
				    "scale.to.height")))

p1 <- dat %>% filter(key_id == min(key_id)) %>%
	ggplot() +
	geom_path(aes(x = x, y = y, group = stroke)) +
	theme_bw() +
	ylim(0, 255) + 
	xlim(0, 255) +
	facet_wrap(~ image, ncol = 3)

p2 <- dat %>% filter(key_id == max(key_id)) %>%
	ggplot() +
	geom_path(aes(x = x, y = y, group = stroke)) +
	theme_bw() +
	ylim(0, 255) + 
	xlim(0, 255) +
	facet_wrap(~ image, ncol = 3)

gridExtra::grid.arrange(p1, p2, ncol = 2)
￼
They do.
We will also use the function below a lot to overlay the drawings.
overlay.plot <- function(dat) {
	dat %>% 
		ggplot(aes(x = x, y = y)) + 
		geom_path(aes(group = stroke, alpha = .000001), size = .000001) +
		theme_void() +
		theme(legend.position = "none") +
		ylim(0, 255) + 
		xlim(0, 255)
}
Let’s look at a sample of 200 (recognized) apples from the US.
dat <- read_csv(here::here("data", "train_simplified", "apple",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 200))

p1 <- overlay.plot(dat) + ggtitle("original (top-left)")
p2 <- overlay.plot(dat %>% center.x) + ggtitle("top-center")
p3 <- overlay.plot(dat %>% to.right) + ggtitle("top-right")
p4 <- overlay.plot(dat %>% to.bottom) + ggtitle("bottom-left")
p5 <- overlay.plot(dat %>% to.bottom %>% center.x) + ggtitle("bottom-center")
p6 <- overlay.plot(dat %>% to.bottom %>% to.right) + ggtitle("bottom-right")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol= 3)
￼
I think the bottom-center version looks the best, in terms of heightening the signal. Actually, the top-center is similar to the bottom-center (probably because apple drawings tend to have larger height than width?). But let’s do something else.
p2b <- overlay.plot(dat %>% stretch.x) + ggtitle("top-stretch-x")
p5b <- overlay.plot(dat %>% to.bottom %>% stretch.x) + ggtitle("bottom-stretch-x")

gridExtra::grid.arrange(p1, p2, p2b, p3, p4, p5, p5b, p6, ncol= 4)
￼
The stretch-x versions look better than the non-stretched versions.
Asparagus
several.samples("asparagus")
￼
rotate.vert <- function(data) {
	data <- data %>%
		center.x() %>%
		center.y()
	
	new <- NULL
	for (i in unique(data$key_id)) {
		sub <- data %>% filter(key_id == i)
		sub <- sub %>% bind_cols(prcomp(sub %>% select(x, y),
				        center = TRUE)$x %>%
				 	as_tibble()) %>%
			rename(x.old = "x", y.old = "y",
			       x = "PC2", y = "PC1")
		new <- rbind(new, sub)
	}
	new %>%
		group_by(key_id) %>%
		to.left() %>%
		to.bottom() %>%
		scale.to.height() %>%
		center.x() %>%
		center.y() %>%
		mutate(d.x = abs(x - x.old), d.x.flip = abs((255 - x) - x.old),
		       d.y = abs(y - y.old), d.y.flip = abs((255 - y) - y.old)) %>%
		ungroup() %>%
		group_by(key_id) %>%
		mutate(mean.d.x = mean(d.x), mean.d.x.flip = mean(d.x.flip),
		       mean.d.y = mean(d.y), mean.d.y.flip = mean(d.y.flip)) %>%
		mutate(x = (mean.d.x <= mean.d.x.flip) * x + 
		       	(mean.d.x >  mean.d.x.flip) * (255 - x),
		       y = (mean.d.y <= mean.d.y.flip) * y +
		       	(mean.d.y >  mean.d.y.flip) * (255 - y)) %>%
		ungroup() %>%
		select(-c(x.old, y.old, 
		          d.x, d.x.flip, d.y, d.y.flip,
		          mean.d.x, mean.d.x.flip, mean.d.y, mean.d.y.flip))
}

rotate.hori <- function(data) {
	data <- data %>%
		center.x() %>%
		center.y()
	
	new <- NULL
	for (i in unique(data$key_id)) {
		sub <- data %>% filter(key_id == i)
		sub <- sub %>% bind_cols(prcomp(sub %>% select(x, y),
				        center = TRUE)$x %>%
				 	as_tibble()) %>%
			rename(x.old = "x", y.old = "y",
			       x = "PC1", y = "PC2")
		new <- rbind(new, sub)
	}
	new %>%
		group_by(key_id) %>%
		to.left() %>%
		to.bottom() %>%
		scale.to.width() %>%
		center.x() %>%
		center.y() %>%
		mutate(d.x = abs(x - x.old), d.x.flip = abs((255 - x) - x.old),
		       d.y = abs(y - y.old), d.y.flip = abs((255 - y) - y.old)) %>%
		ungroup() %>%
		group_by(key_id) %>%
		mutate(mean.d.x = mean(d.x), mean.d.x.flip = mean(d.x.flip),
		       mean.d.y = mean(d.y), mean.d.y.flip = mean(d.y.flip)) %>%
		mutate(x = (mean.d.x <= mean.d.x.flip) * x + 
		       	(mean.d.x >  mean.d.x.flip) * (255 - x),
		       y = (mean.d.y <= mean.d.y.flip) * y +
		       	(mean.d.y >  mean.d.y.flip) * (255 - y)) %>%
		ungroup() %>%
		select(-c(x.old, y.old, 
		          d.x, d.x.flip, d.y, d.y.flip,
		          mean.d.x, mean.d.x.flip, mean.d.y, mean.d.y.flip))
}
dat <- read_csv(here::here("data", "train_simplified", "asparagus",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 2))

p1 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("2 original")
p2 <- overlay.plot(dat %>% rotate.vert) + ggtitle("2 rotated")

dat <- read_csv(here::here("data", "train_simplified", "asparagus",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 10))

p3 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("10 original")
p4 <- overlay.plot(dat %>% rotate.vert) + ggtitle("10 rotated")

dat <- read_csv(here::here("data", "train_simplified", "asparagus",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 100))

p5 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("100 original")
p6 <- overlay.plot(dat %>% rotate.vert) + ggtitle("100 rotated")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)
## Warning: Removed 1 rows containing missing values (geom_path).
## Warning: Removed 4 rows containing missing values (geom_path).
￼
Let’s see what happens to the apples if we apply this rotation on them.
dat <- read_csv(here::here("data", "train_simplified", "apple",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 2))

p1 <- overlay.plot(dat) + ggtitle("2 original")
p2 <- overlay.plot(dat %>% rotate.vert) + ggtitle("2 rotated")

dat <- read_csv(here::here("data", "train_simplified", "apple",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 10))

p3 <- overlay.plot(dat) + ggtitle("10 original")
p4 <- overlay.plot(dat %>% rotate.vert) + ggtitle("10 rotated")

dat <- read_csv(here::here("data", "train_simplified", "apple",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 100))

p5 <- overlay.plot(dat) + ggtitle("100 original")
p6 <- overlay.plot(dat %>% rotate.vert) + ggtitle("100 rotated")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)
## Warning: Removed 1 rows containing missing values (geom_path).
￼
Banana
several.samples("banana")
￼
dat <- read_csv(here::here("data", "train_simplified", "banana",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 2))

p1 <- overlay.plot(dat) + ggtitle("2 original")
p2 <- overlay.plot(dat %>% rotate.hori) + ggtitle("2 rotated")

dat <- read_csv(here::here("data", "train_simplified", "banana",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 10))

p3 <- overlay.plot(dat) + ggtitle("10 original")
p4 <- overlay.plot(dat %>% rotate.hori) + ggtitle("10 rotated")

dat <- read_csv(here::here("data", "train_simplified", "banana",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 100))

p5 <- overlay.plot(dat) + ggtitle("100 original")
p6 <- overlay.plot(dat %>% rotate.hori) + ggtitle("100 rotated")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)
## Warning: Removed 1 rows containing missing values (geom_path).
## Warning: Removed 4 rows containing missing values (geom_path).
￼
Let’s flip concave bananas to convex.
flip.banana <- function(data) {
	data %>%
		group_by(key_id) %>%
		mutate()
}
Birthday cake
several.samples("birthday cake")
￼
Blueberry
several.samples("blueberry")
￼
Blackberry
several.samples("blackberry")
￼
Bread
several.samples("bread")
￼
Broccoli
several.samples("broccoli")
￼
Cake
several.samples("cake")
￼
Carrot
several.samples("carrot")
￼
Cookie
several.samples("cookie")
￼
Donut
several.samples("donut")
￼
Grapes
several.samples("grapes")
￼
Hamburger
several.samples("hamburger")
￼
Hot dog
several.samples("hot dog")
￼
Ice cream
several.samples("ice cream")
￼
Lollipop
several.samples("lollipop")
￼
dat <- read_csv(here::here("data", "train_simplified", "lollipop",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 2))

p1 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("2 original")
p2 <- overlay.plot(dat %>% rotate.vert) + ggtitle("2 rotated")

dat <- read_csv(here::here("data", "train_simplified", "lollipop",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 10))

p3 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("10 original")
p4 <- overlay.plot(dat %>% rotate.vert) + ggtitle("10 rotated")

dat <- read_csv(here::here("data", "train_simplified", "lollipop",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 100))

p5 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("100 original")
p6 <- overlay.plot(dat %>% rotate.vert) + ggtitle("100 rotated")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)
## Warning: Removed 6 rows containing missing values (geom_path).
￼
Mushroom
several.samples("mushroom")
￼
Onion
several.samples("onion")
￼
Peanut
several.samples("peanut")
￼
Pear
several.samples("pear")
￼
Peas
several.samples("peas")
￼
Pineapple
several.samples("pineapple")
￼
Pizza
several.samples("pizza")
￼
Popsicle
several.samples("popsicle")
￼
dat <- read_csv(here::here("data", "train_simplified", "popsicle",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 2))

p1 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("2 original")
p2 <- overlay.plot(dat %>% rotate.vert) + ggtitle("2 rotated")

dat <- read_csv(here::here("data", "train_simplified", "popsicle",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 10))

p3 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("10 original")
p4 <- overlay.plot(dat %>% rotate.vert) + ggtitle("10 rotated")

dat <- read_csv(here::here("data", "train_simplified", "popsicle",
		   "US_recognized.csv"),
	    col_types = cols()) %>%
	filter(key_id %in% sample(unique(key_id), 100))

p5 <- overlay.plot(dat %>% center.x %>% center.y) + ggtitle("100 original")
p6 <- overlay.plot(dat %>% rotate.vert) + ggtitle("100 rotated")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)
## Warning: Removed 3 rows containing missing values (geom_path).
￼
Potato
several.samples("potato")
￼
Sandwich
several.samples("sandwich")
￼
Steak
several.samples("steak")
￼
Strawberry
several.samples("strawberry")
￼
String bean
several.samples("string bean")
￼
Watermelon
several.samples("watermelon")
￼
