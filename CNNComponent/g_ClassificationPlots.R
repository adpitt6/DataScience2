#### Source relevant functions ####

source(here::here("a_FunctionsForDataProcessing.R"))



#### Get model results on the Google-recognized and unrecognized test sets ####

load(here::here("mini", "model2", "outputs", "all_final_model2_epoch12.RData"))

class_recognized <- data_frame(key_id = model_recognized$id,
                               truth  = model_recognized$category,
                               class  = some.edibles[model_recognized$class+1])

class_unrecognized <- data_frame(key_id = model_unrecognized$id,
                                 truth  = model_unrecognized$category,
                                 class  = 
                                     some.edibles[model_unrecognized$class+1])



#### Plot eight pairs of edibles that tend to get mixed up with each other ####

# This plot is used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>

pairs <- list(c("birthday cake", "cake"),
              c("hamburger", "sandwich"),
              c("lollipop", "popsicle"),
              c("broccoli", "mushroom"),
              c("blackberry", "grapes"),
              c("cookie", "potato"),
              c("asparagus", "string bean"),
              c("apple", "pear"))

p_pairs <- list()

for (i in 1:length(pairs)) {
    
    a <- pairs[[i]][1]
    b <- pairs[[i]][2]
    
    dat <- bind_rows(
        class_recognized %>% filter(truth==a, class==a) %>% sample_n(1),
        class_recognized %>% filter(truth==a, class==b) %>% sample_n(1),
        class_recognized %>% filter(truth==b, class==a) %>% sample_n(1),
        class_recognized %>% filter(truth==b, class==b) %>% sample_n(1)
    )
    
    tmp <- bind_rows(
        readRDS(here::here("data", "raw", paste0(a, "_raw.rds"))) %>%
            filter(key_id %in% dat$key_id),
        readRDS(here::here("data", "raw", paste0(b, "_raw.rds"))) %>%
            filter(key_id %in% dat$key_id)
    ) %>%
        select(-recognized) %>%
        raw_to_strokes() %>%
        center_x() %>%
        center_y() %>%
        resolution_256_to_32() %>%
        strokes_to_points()
    
    dat <- dat %>%
        mutate(key = paste0(toupper(truth), " -> ", class)) %>%
        right_join(tmp, by = "key_id") 
    
    
    p_pairs[[i]] <- dat %>%
        ggplot(aes(x = x, y = y)) + 
        geom_point() +
        facet_wrap(~ key, ncol = 2) +
        labs(x = "", y = "") +
        theme_bw() +
        xlim(0, 31) + ylim(31, 0)
    
    rm(tmp, dat)
}

png(here::here("mini", "model2", "outputs", "classificationPlots",
               "eight_pairs.png"),
    height = 1400, width = 700)
gridExtra::grid.arrange(p_pairs[[1]], p_pairs[[2]], 
                        p_pairs[[3]], p_pairs[[4]],
                        p_pairs[[5]], p_pairs[[6]], 
                        p_pairs[[7]], p_pairs[[8]], ncol = 2)
dev.off()





#### Plot one sample of each of 30 edibles misclassified as blackberry ####

# This plot is used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>


dat <- NULL
for (i in setdiff(some.edibles, "blackberry")) {
    dat <- rbind(dat,
                 class_unrecognized %>% 
                     filter(truth==i, class=="blackberry") %>% sample_n(1))
}


tmp <- NULL
for (i in setdiff(some.edibles, "blackberry")) {
    tmp <- rbind(tmp,
                 readRDS(here::here("data", "raw", paste0(i, "_raw.rds"))) %>%
                     filter(key_id %in% dat$key_id))
}

tmp <- tmp %>%
    select(-recognized) %>%
    raw_to_strokes() %>%
    center_x() %>%
    center_y() %>%
    resolution_256_to_32() %>%
    strokes_to_points()

dat <- dat %>%
    mutate(key = paste0(toupper(truth), " -> ", class)) %>%
    right_join(tmp, by = "key_id")


png(here::here("mini", "model2", "outputs", "classificationPlots",
               "other_as_blackberry.png"),
    height = 1300, width = 1000)
dat %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~ truth, ncol = 5) +
    labs(x = "", 
         y = "",
         title = "NOT BLACKBERRY -> blackberry") +
    theme_bw() +
    xlim(0, 31) + ylim(31, 0)
dev.off()




#### Plot 30 samples of blackberry correctly classified as blackberry ####

# This plot is used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>


dat <- class_recognized %>% 
    filter(truth=="blackberry", class=="blackberry") %>% sample_n(30)

tmp <- readRDS(here::here("data", "raw", "blackberry_raw.rds")) %>%
    filter(key_id %in% dat$key_id) %>%
    select(-recognized) %>%
    raw_to_strokes() %>%
    center_x() %>%
    center_y() %>%
    resolution_256_to_32() %>%
    strokes_to_points()

dat <- dat %>%
    right_join(tmp, by = "key_id")

png(here::here("mini", "model2", "outputs", "classificationPlots",
               "blackberry_as_blackberry.png"),
    height = 1300, width = 1000)
dat %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~ key_id, ncol = 5) +
    labs(x = "", 
         y = "",
         title = "Google-recognized BLACKBERRY -> blackberry") +
    theme_bw() +
    xlim(0, 31) + ylim(31, 0)
dev.off()





