#### Required packages ####

library(tidyverse)
library(keras)



#### Drawing types considered ####

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



#### Function to process output from a test of the CNN classifier ####

# This function extracts loss, overall accuracy, type-specific accuracy, matrix of classification probabilities

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



#### Load data for training the classifier ####

train        <- readRDS(here::here("data", "ready", "train_mini.rds"))
validation   <- readRDS(here::here("data", "ready", "validation.rds"))
recognized   <- readRDS(here::here("data", "ready", "recognized.rds"))
unrecognized <- readRDS(here::here("data", "ready", "unrecognized.rds"))
mix          <- readRDS(here::here("data", "ready", "mix.rds"))



#### Create and compile model ####

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

model <- create_model()



#### Train model and save model parameters at the end of each epoch ####

cp_callback <- callback_model_checkpoint(
    filepath = 
        here::here("mini", "model2", "weights",
                   "model2_weights_{epoch:02d}-{val_loss:.2f}-{val_acc:.2f}.hdf5"),
    save_weights_only = TRUE,
    verbose = 1)


model_history <- model %>% 
    fit(train$x,
        train$y,
        epochs = 20,
        batch_size = 32,
        validation_data = list(validation$x, validation$y),
        verbose = 1,
        callbacks = list(cp_callback))



#### Save the model at the last epoch and the training hostory plot ####

model %>% save_model_hdf5(here::here("mini", "model2", "model", "model2.h5"))

pdf(file = here::here("mini", "model2", "outputs", "model2_history.pdf"), 
    width = 6, height = 4)
plot(model_history)
dev.off()

rm(model, model_history)



#### Test the model at every epoch on Google-recognized and Google-unrecognized data ####

for (i in list.files(here::here("mini", "model2", "weights"))) {
    model <- create_model() %>%
        load_model_weights_hdf5(here::here("mini", "model2", "weights", i))
    
    model_recognized   <- test_process(model = model, test = recognized)
    model_unrecognized <- test_process(model = model, test = unrecognized)
    
    save(model_recognized, model_unrecognized,
         file = here::here("mini", "model2", "performance", 
                           paste0(sub(".hdf5", "", sub("_weights", "", i)),
                                  ".RData")))
    
    rm(model, model_recognized, model_unrecognized)
}

