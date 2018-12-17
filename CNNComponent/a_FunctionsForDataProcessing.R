#### Required packages ####

library(tidyverse)
library(rjson)
library(keras)


#### Set of edibles considered in this project ####

some.edibles <- c("apple", "asparagus",
                  "banana", "birthday cake", "blackberry", "blueberry",
                  "bread", "broccoli",
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


#### Function to extract raw data files and save in /data/raw/ ####

get_raw_file <- function(category) {
    
    if (!dir.exists(here::here("data"))) { dir.create(here::here("data")) }
    
    if(!file.exists(here::here("data", paste0(category, "_raw.rds")))) {
        dat <- read_csv(unz(here::here("data_download", "train_simplified.zip"),
                            paste0(category,".csv")), 
                        col_types = cols())
        
        saveRDS(dat, file = here::here("data", paste0(category, "_raw.rds")))
    }
}



#### Function to extract raw data ####

# This function extracts, for a specified edible, raw data for 12000 Goolge-recognized drawings or 1000 Google-unrecognized drawings or 1000 Google-mixed drawings regardless of Google-reconition status (it will become clear below why we need these different amounts of data for these types)

# This function keeps the image data (variable 'drawing') and image id ('key_id'), discarding variables indicating time, drawing type, country of origin, and Google-recognition status.

get_images <- function(category, recognized = NULL) {
    
    status <- ifelse(recognized=="yes", "True",
                     ifelse(recognized=="no", "False", NA))
    num    <- ifelse(recognized=="yes",  12000, 1000)
    
    dat <- readRDS(here::here("data", "raw", paste0(category, "_raw.rds")))
    
    if (recognized=="yes" | recognized=="no") {
        dat <- dat %>% filter(recognized==status)  
    }
    
    if (nrow(dat) > num) { dat <- dat %>% sample_n(num) }
    
    dat %>% select(-c(timestamp, word, countrycode, recognized))
}



#### Function to get one image ####

get_one <- function(category, recognized = TRUE) {
    
    status <- ifelse(recognized==TRUE, "True", "False")
    
    readRDS(here::here("data", "raw", paste0(category, "_raw.rds"))) %>%
        filter(recognized==status) %>%
        sample_n(1) %>%
        select(-c(timestamp, word, countrycode, recognized))
}


#### Funtion to turn raw data (resulting from get_images()) into strokes format ####

raw_to_strokes <- function(data) {
    
    data %>%
        mutate(drawing = map(drawing, fromJSON)) %>%
        unnest() %>%
        mutate(drawing = map(drawing, ~ 
                                 do.call(., what="rbind") %>%
                                 t() %>%
                                 as_tibble())) %>%
        unnest(.id = "stroke") %>%
        rename(x = "V1", y = "V2") %>%
        mutate(x = as.integer(x), y = as.integer(y))
}




#### Function to place the drawing at the center of the frame ####

center_xy <- function(data) {
    data %>%
        group_by(key_id) %>%
        mutate(x = x + (0+255)/2 - ((min(x)+max(x))/2),
               y = y + (0+255)/2 - ((min(y)+max(y))/2)) %>%
        ungroup()
}


#### Function to reduce image resolution from 256x256 to 32x32 ####

resolution_256_to_32 <- function(data) {
    data %>%
        mutate(x = floor(x/8),
               y = floor(y/8))
}


#### Function to turn data from strokes format into points format ####

strokes_to_points <- function(data) {
    data <- data %>%
        group_by(key_id) %>%
        mutate(x.lead = lag(x),
               y.lead = lag(y),
               same.stroke = stroke==lag(stroke)) %>%
        ungroup() %>%
        filter(same.stroke == TRUE) %>%
        select(-same.stroke) %>%
        mutate(xdiff = x - x.lead,
               ydiff = y - y.lead)
    
    points.1 <- data %>%
        filter(xdiff == 0) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = .$x,
                      y = seq(.$y.lead, .$y)))
    
    points.2 <- data %>%
        filter(xdiff != 0, ydiff == 0) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = seq(.$x.lead, .$x),
                      y = .$y))
    
    points.3 <- data %>%
        filter(xdiff != 0, ydiff != 0, abs(xdiff) >= abs(ydiff)) %>%
        mutate(slope = ydiff / xdiff) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = seq(.$x.lead, .$x),
                      y = .$y.lead + round(seq(0, .$xdiff) * .$slope)))
    
    points.4 <- data %>%
        filter(xdiff != 0, ydiff != 0, abs(xdiff) < abs(ydiff)) %>%
        mutate(slope = xdiff / ydiff) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      y = seq(.$y.lead, .$y),
                      x = .$x.lead + round(seq(0, .$ydiff) * .$slope))) %>%
        select(key_id, stroke, x, y)
    
    points <- bind_rows(points.1,
                        points.2,
                        points.3,
                        points.4) %>%
        group_by(key_id, stroke) %>%
        distinct() %>%
        ungroup() %>%
        select(-stroke) %>%
        mutate(x = as.integer(x),
               y = as.integer(y))
    points
}


#### Function to turn data from points format (in 32x32 resolution) into flat raster format ####

points_to_flat_raster <- function(data) {
    data %>%
        mutate(x = factor(x, levels = seq(0, 31)),
               y = factor(y, levels = seq(0, 31)),
               value = 1) %>%
        complete(key_id, x, y) %>%
        mutate(value = ifelse(is.na(value), 0, value)) %>%
        mutate(x.y = paste0(formatC(x, width = 2, flag = 0), 
                            formatC(y, width = 2, flag = 0))) %>%
        select(-c(x, y)) %>%
        distinct() %>%
        mutate(value = as.integer(value)) %>%
        spread(x.y, value)
}


#### Function to turn images from flat-raster format to square-raster format for CNN ####

flat_to_square_raster <- function(data) {
    x <- data %>%
        select(-key_id) %>%
        mutate_all(as.integer) %>%
        t()
    dim(x) <- c(32, 32, 1, ncol(x))
    x <- aperm(x, c(4, 1, 2, 3))
    
    list(id = data %>% pull(key_id),
         x  = x)
}




#### Function to turn a dataset that contains key_id, category (string), label (0 to 31) and 32x32 flattened raster data into a combination of feature data, category data (of the form ready to be fed into the training and testing of the classifier) and retain drawing id, category and label ####

extract_xy_data <- function(data) {
    x <- data %>% 
        select(-c(label, category, key_id)) %>%
        mutate_all(as.integer) %>%
        t()
    dim(x) <- c(32, 32, 1, ncol(x))
    x <- aperm(x, c(4, 1, 2, 3))
    
    list(id       = data %>% pull(key_id), 
         category = data %>% pull(category),
         label    = data %>% pull(label),
         y        = data %>% pull(label) %>% keras::to_categorical(),
         x        = x)
}
