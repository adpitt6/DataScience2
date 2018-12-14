### This file reads in
# (1) a small csv file containing the names of the files you want to process
# below it is labeled "foods"

# (2) from each name in (1) it reads in the first 5,000 rows of the matching csv file 
# from the folder "train_simplified", and it creates a tibble with the columns
# key_id, stroke, x, and y. In this tibble, a single row represents the x,y 
# coordinates from ONE point from ONE stroke from ONE drawing. 
# Hence the variable stroke uniquely identifies each stroke, and each
# drawing is a collection of strokes, uniquely identified by key_id

### This file creates

# (1) a folder called binary data in which

# (2) each category of drawing gets a binary .rdata file. 

### Load Some Packages
library(tidyverse)
library(rjson)
library(jsonlite)
library(purrr)

### Read in the names of foods (this data was hand-typed)
foods <- read_delim("food.txt", delim = "\n",col_names = F)[[1]]

if(!file.exists("binary_data")) dir.create("binary_data")
if(!file.exists("binary_data_test")) dir.create("binary_data_test")

### Build Function for formatting one food
### save data as an R binary file
format.one.food <- function(food){
	df <- 
	read_csv(file.path("train_simplified", paste0(food,".csv")),n_max = 6000)%>%
	select(key_id, drawing) %>%
	mutate(drawing = map(drawing, ~fromJSON(., simplifyMatrix = F))
	       ) %>%
	unnest() %>%
	mutate(drawing = map(drawing, ~do.call(.,what="rbind") %>% 
		         	t %>% 
		         	as.tibble())
	       ) %>%
	unnest(.id = "stroke") %>%
	mutate(x = V1, y = 255-V2) %>%
	select(key_id, stroke, x, y)
	ids <- unique(df$key_id)
	train <- filter(df,key_id %in% ids[1:5000])
	test <- filter(df,key_id %in% ids[5001:6000])
	save(train, file = file.path("binary_data", paste0(food,".rdata")))
	save(test, file = file.path("binary_data_test", paste0(food,".rdata")))
}

### Format each food

for(i in foods){
	cat(paste0("Preparing ", i, "s...\n"))
	format.one.food(i)
	cat(paste0(i, "s ready.\n"))
}
