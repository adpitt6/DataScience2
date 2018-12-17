source(here::here("a_FunctionsForDataProcessing.R"))


#### Get raster data ####

## Obtain, for each edible, a training set ('train') of 10000 Google-recognized images (plus a subset 'train_mini' of size 4000 and a subset 'train_nano' of size 2000), a validation set ('validation') 1000 Google-recognized images, a fair test set of 1000 Google-recognized images ('recognized'), an unfair test set of 1000 Google-unrecognized images ('unrecognized'), and another unfair test set of 1000 Google-mixed images ('mix')

set.seed(1022239211)
data <- lapply(1:length(some.edibles), function(i) {
    
    cat <- some.edibles[i]
    tmp <- get_images(category = cat, recognized = "yes") %>%
        raw_to_strokes() %>%
        center_xy() %>%
        resolution_256_to_32() %>%
        strokes_to_points() %>%
        points_to_flat_raster() %>%
        mutate(category = cat, label = i-1)
    
    rows  <- sample(nrow(tmp), 2000)
    
    validation <- tmp[rows[   1:1000],]
    test       <- tmp[rows[1001:2000],]
    train      <- tmp[-rows,]
    train_mini <- train %>% sample_n(4000)
    train_nano <- train %>% sample_n(2000)
    
    rm(tmp, rows)
    
    unrecognized <- get_images(category = cat, recognized = "no") %>%
        raw_to_strokes() %>%
        center_xy() %>%
        resolution_256_to_32() %>%
        strokes_to_points() %>%
        points_to_flat_raster() %>%
        mutate(category = cat, label = i-1)
    
    mix <- get_images(category = cat, recognized = NULL) %>%
        raw_to_strokes() %>%
        center_xy() %>%
        resolution_256_to_32() %>%
        strokes_to_points() %>%
        points_to_flat_raster() %>%
        mutate(category = cat, label = i-1)
    
    out <- list(train        = train,
                train_mini   = train_mini,
                train_nano   = train_nano,
                validation   = validation,
                recognized   = test,
                unrecognized = unrecognized,
                mix          = mix)
    
    saveRDS(out, file = here::here("data", "uniform", 
                                   paste0(cat, "_uniform.rds")))
    
    out
})

names(data) <- some.edibles


## Combine the different edibles into the same files: train, train_mini, train_nano, validation, recognized, unrecognized, mix

# (The long-haul aim is to train the CNN classifier on the large training set. We will use train_nano as a toy dataset to make sure the code works. We will then use train_mini to train (on a laptop computer) the classifier we end up reporting. The plan is use the larger training set when we figure out the required computing resource.)

train        <- NULL
train_mini   <- NULL
train_nano   <- NULL
validation   <- NULL
recognized   <- NULL
unrecognized <- NULL
mix          <- NULL

for (i in some.edibles) {
    train        <- rbind(train,        data[[i]]$train       )
    train_mini   <- rbind(train_mini,   data[[i]]$train_mini  )
    train_nano   <- rbind(train_nano,   data[[i]]$train_nano  )
    validation   <- rbind(validation,   data[[i]]$validation  )
    recognized   <- rbind(recognized,   data[[i]]$recognized  )
    unrecognized <- rbind(unrecognized, data[[i]]$unrecognized)
    mix          <- rbind(mix,          data[[i]]$mix         )
}

rm(data)




#### Turn the above datasets -- containing key_id, category (string), label (0 to 31) and 32x32 flattened raster data -- into a combination of feature data, category data (of the form ready to be fed into the training and testing of the classifier) and retain drawing id, category and label

train        <- train        %>% extract_xy_data()
train_mini   <- train_mini   %>% extract_xy_data()
train_nano   <- train_nano   %>% extract_xy_data()
validation   <- validation   %>% extract_xy_data()
recognized   <- recognized   %>% extract_xy_data()
unrecognized <- unrecognized %>% extract_xy_data()
mix          <- mix          %>% extract_xy_data()


saveRDS(train,        file = here::here("data", "ready", "train.rds"       ))
saveRDS(train_mini,   file = here::here("data", "ready", "train_mini.rds"  ))
saveRDS(train_nano,   file = here::here("data", "ready", "train_nano.rds"  ))
saveRDS(validation,   file = here::here("data", "ready", "validation.rds"  ))
saveRDS(recognized,   file = here::here("data", "ready", "recognized.rds"  ))
saveRDS(unrecognized, file = here::here("data", "ready", "unrecognized.rds"))
saveRDS(mix,          file = here::here("data", "ready", "mix.rds"         ))


