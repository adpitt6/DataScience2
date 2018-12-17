#### Required packages ####

library(tidyverse)
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


#### Function to generate model2 ####

create_model <- function() {
    model <- keras_model_sequential() %>%
        layer_conv_2d(filter = 32, padding = "same", kernel_size = c(3,3),
                      input_shape = c(32,32,1)) %>%
        layer_activation("relu") %>%
        layer_batch_normalization() %>%
        
        layer_conv_2d(filter = 32, padding = "same", kernel_size = c(3,3)) %>%
        layer_activation("relu") %>%
        layer_batch_normalization() %>%
        
        layer_max_pooling_2d(pool_size = c(2,2)) %>%
        layer_dropout(.3) %>%
        
        layer_flatten() %>%
        layer_dense(256) %>%
        layer_activation("relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(.3) %>%
        
        layer_dense(31) %>%
        layer_activation("softmax") %>%
        
        compile(loss = "categorical_crossentropy",
                optimizer = optimizer_sgd(lr = 0.001,
                                          decay = 1e-6,
                                          momentum = 0.9,
                                          nesterov = T),
                metrics = list("accuracy"))
}


#### Function to evaluate a model on a test set ####

test_process <- function(model, test = test,
                         class.names = some.edibles) {
    
    evaluate <- model %>% evaluate(test$x, test$y)
    class <- model %>% predict_classes(test$x)
    correct <- 1*(class==test$label)
    
    tmp <- data.frame(label = test$label,
                      class = class %>% to_categorical())
    class_matrix <- NULL
    for(i in unique(test$label)) {
        class_matrix <- rbind(class_matrix,
                              tmp %>% filter(label == i) %>% select(-label) %>%
                                  colMeans())
    }
    rownames(class_matrix) <- unique(test$category)
    colnames(class_matrix) <- class.names
    class_matrix <- class_matrix[class.names,]
    
    acc_specific <- diag(class_matrix)
    
    out <- list(loss = evaluate$loss,
                acc.overall = evaluate$acc,
                acc.specific = acc_specific,
                pred.class = class,
                pred.correct = correct,
                class.matrix = class_matrix)
    c(test, out)
}



#### Function to classify some image(s) ####

classify_images <- function(images, image_type = "raw") {
    
    source(here::here("a_FunctionsForDataProcessing.R"))
    
    if (image_type=="raw") { 
        images <- raw.images %>%
            raw_to_strokes() %>%
            center_xy() %>%
            resolution_256_to_32() %>%
            strokes_to_points() %>%
            points_to_flat_raster() %>%
            flat_to_square_raster()
    }
    
    if (image_type=="strokes.256") {
        images <- raw.images %>%
            center_xy() %>%
            resolution_256_to_32() %>%
            strokes_to_points() %>%
            points_to_flat_raster() %>%
            flat_to_square_raster()
    }
    
    if (image_type=="strokes.32") {
        images <- raw.images %>%
            strokes_to_points() %>%
            points_to_flat_raster() %>%
            flat_to_square_raster()
    }
    
    if (image_type=="points.32") {
        images <- raw.images %>%
            points_to_flat_raster() %>%
            flat_to_square_raster()
    }
    
    if (image_type=="raster.flat") {
        images <- raw.images %>%
            flat_to_square_raster()
    }
    
    model <- create_model() %>%
        load_model_weights_hdf5(here::here("mini", "model2", "weights",
                                           "model2_weights_12-0.63-0.80.hdf5"))
    
    label <- model %>% predict_classes(images$x)
    
    data_frame(key_id = raw.images$id, 
               class  = some.edibles[label+1])
}



