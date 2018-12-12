### Borrowing from Trang Nguyen
# I've changed the functions so they work on ONE key_id 
# rather than an entire data frame of multiple key_ids

# added a function called scale to fit.
############################################################
### SHIFTING FUNCTIONS
to.bottom <- function(data) {
	data %>%
	mutate(y = y - min(y))
}

to.top <- function(data) {
	data %>%
	mutate(y = y + (255 - max(y)))
}

to.right <- function(data) {
	data %>%
	mutate(x = x + (255 - max(x))) 
}

to.left <- function(data) {
	data %>%
	mutate(x = x - min(x)) 
}

center.x <- function(data) {
	data %>%
	mutate(x = x + (0+255)/2 - ((min(x)+max(x))/2)) 
}

center.y <- function(data) {
	data %>%
	mutate(y = y + (0+255)/2 - ((min(y)+max(y))/2))
}

############################################################
### STRETCHING FUNCTIONS
stretch.x <- function(data) {
	data %>%
	to.left() %>%
	mutate(x = x * 255 / max(x))
}

stretch.y <- function(data) {
	data %>%
	to.bottom() %>%
	mutate(y = y * 255 / max(y))
}

scale.to.width <- function(data) {
	data %>%
	to.bottom() %>%
	to.left() %>%
	mutate(x2 = x * 255 / max(x),
		      y = y * 255 / max(x),
		       x = x2) %>%
	select(-x2)
}

scale.to.height <- function(data) {
	data %>%
		to.bottom() %>%
		to.left() %>%
		mutate(y2 = y * 255 / max(y),
		       x = x * 255 / max(y),
		       y = y2) %>%
		select(-y2)
}

scale.to.fit <- function(data) {
	height = with(data, max(y) - min(y))
	width = with(data, max(x) - min(x))
	if(height > width){
		return(scale.to.height(data))
	}else return(scale.to.width(data))
}
###################################################################
### ROUNDING FUNCTION 
integer.xy <- function(data) {
	data %>%
		mutate(x = as.integer(round(x)),
		       y = as.integer(round(y)))
}

###################################################################
### ROTATION FUNCTIONS
rotate.vert <- function(data) {
	prcomp(data, center = TRUE)$x %>%
	as_tibble %>%
	transmute(x = PC2, y = PC1) %>%
	scale.to.fit %>%
	integer.xy %>%
	mutate(flip = mean(abs(y - data$y)) > mean(abs(255-y - data$y)),
	       y = (255-y) * flip + y*(1-flip)) %>%
	select(-flip)
}