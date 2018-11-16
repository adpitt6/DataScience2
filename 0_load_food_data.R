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

### Build Function for formatting one food
### save data as an R binary file
format.one.food <- function(food){
	df <- 
	read_csv(file.path("train_simplified", paste0(food,".csv")),n_max = 5000)%>%
	select(key_id, drawing) %>%
	mutate(drawing = map(drawing, fromJSON)
	       ) %>%
	unnest() %>%
	mutate(drawing = map(drawing, ~do.call(.,what="rbind") %>% 
		         	t %>% 
		         	as.tibble())
	       ) %>%
	unnest(.id = "stroke") %>%
	mutate(x = V1, y = -V2) %>%
	select(key_id, stroke, x, y)
	save(df, file = file.path("binary_data", paste0(food,".rdata")))
}

### Attempt one formatting
#format.one.food("banana")

for(i in foods){
	cat(paste0("Preparing ", i, "s...\n"))
	format.one.food(i)
	cat(paste0(i, "s ready.\n"))
}

