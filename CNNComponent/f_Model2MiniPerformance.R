#### Source relevant functions etc. ####

source(here::here("e_FunctionsKerasRelated.R"))


#### Additional required packages ####

library(spatstat)


#### Get the results of the final model (epoch 12) on the Google-recognized and Google-unrecognized test sets####

load(here::here("mini", "model2", "performance",
                "model2_12-0.63-0.80.RData"))



#### Get the results of the final model on the Google-mixed test set from epoch 12 ####

# load Google-mixed data

mix <- readRDS(here::here("data", "ready", "mix.rds"))

# load the final model

model <- create_model() %>%
    load_model_weights_hdf5(here::here("mini", "model2", "weights", 
                                       "model2_weights_12-0.63-0.80.hdf5"))

# evaluate the final model on the Google-mixed data

model_mix <- test_process(model = model, test = mix)



#### Save the results of the final model on all three test sets ####

save(model_recognized, model_unrecognized, model_mix,
     file = here::here("mini", "model2", "outputs",
                       "all_final_model2_epoch12.RData"))




#### Compare the model's performance on mixed test data with Google AI ####

# Get data for plot

cnn_google <- NULL
for (i in some.edibles) {
    
    cnn_google <- rbind(cnn_google,
                        readRDS(here::here("data", "raw", 
                                           paste0(i, "_raw.rds"))) %>%
                            mutate(category = i) %>%
                            select(category, recognized)
    )
}

cnn_google <- cnn_google %>%
    group_by(category) %>%
    summarize(count = n(),
              recognized = mean(recognized=="True")) %>%
    left_join(data.frame(category = names(model_mix$acc.specific),
                         accurate = model_mix$acc.specific),
              by = "category")


## Plot the comparison

# This plot is used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>

png(here::here("mini", "model2", "outputs", "cnn_vs_google.png"),
    width = 1200, height = 800, res = 200)
cnn_google %>%
    arrange(accurate) %>%
    gather(classification, probability, -c(category, count)) %>%
    mutate(classification = factor(classification,
                                   levels = c("recognized", "accurate"),
                                   labels = c("recognized by Google AI",
                                              "correctly classified by OurCNN"))) %>%
    mutate(category = factor(category,
                             levels = (cnn_google %>% 
                                           arrange(desc(accurate)) %>% 
                                           pull(category)))) %>%
    ggplot(aes(x = category, y = probability, 
               color = classification)) +
    geom_line(aes(group = category), color = "gray") +
    geom_point() +
    labs(title = "OurCNN correct classification vs. Google AI recognition",
         x = "") +
    scale_color_discrete(name = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
          legend.position = c(.2, .5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylim(0,1)
dev.off()



#### Classification matrix on Google-mixed test set ####

# This is a plot of classification probabilities given true category. It is a matrix whose rows indicate true category and columns indicate classification categories. The matrix is ordered with descending accurate classification rates (on the diagonal).

# This plot is shown on the homepage of our website <https://adpitt6.github.io/DataScience2/index.html>

matrix_mix <- model_mix$class.matrix
order_mix <- names(sort(desc(diag(matrix_mix))))

png(here::here("mini", "model2", "outputs", "classMatrices",
               "mix_ordered_homepage.png"),
    height = 1100, width = 1100)
par(mar = c(7, 8, 1, 1) + 0.1, ps = 20, cex = 1, cex.main = 1.5)
plot(im(matrix_mix[order_mix, order_mix]),
     main = "", zlim = c(0,1))
title("Classification probabilities from the CNN approach",
      adj = 0.2, line = -5)
text(1:ncol(matrix_mix), par("usr")[1] + 1,
     srt = 90, adj = 1, cex = 1, xpd = TRUE,
     labels = order_mix)
text(par("usr")[1] + 1, 1:nrow(matrix_mix),
     adj = 1, cex = 1, xpd = TRUE,
     labels = order_mix)
mtext(text = "TRUTH", side = 2, line = 6.5)
mtext(text = "CLASSIFICATION", side = 1, adj = .4, line = 3)
dev.off()



#### Classification matrix on Google-recognized test set ####

# matrix is ordered from highest to lowest correct classification

# plot is used on website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>


matrix_recognized <- model_recognized$class.matrix
order_recognized <- names(sort(desc(diag(matrix_recognized))))

png(here::here("mini", "model2", "outputs", "classMatrices",
               "recognized_ordered.png"),
    height = 1100, width = 1100)
par(mar = c(7, 8, 1, 1) + 0.1, ps = 20, cex = 1, cex.main = 1.5)
plot(im(matrix_recognized[order_recognized, order_recognized]),
     main = "", zlim = c(0,1))
title("Google-recognized images classification probabilities",
      adj = 0.2, line = -5)
text(1:ncol(matrix_recognized), par("usr")[1] + 1,
     srt = 90, adj = 1, cex = 1, xpd = TRUE,
     labels = order_recognized)
text(par("usr")[1] + 1, 1:nrow(matrix_recognized),
     adj = 1, cex = 1, xpd = TRUE,
     labels = order_recognized)
mtext(text = "TRUTH", side = 2, line = 6.5)
mtext(text = "CLASSIFICATION", side = 1, adj = .4, line = 3)
dev.off()


#### Classification matrix on misclassified Google-recognized images ####

# matrix is ordered so that similar categories appear close to one another

# plot is used on website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>



matrix_wrong_recognized <- matrix_recognized
diag(matrix_wrong_recognized) <- 0
matrix_wrong_recognized <- matrix_wrong_recognized / 
    matrix(rep((1 - diag(matrix_recognized)), 
               ncol(matrix_wrong_recognized)),
           ncol = ncol(matrix_wrong_recognized))

reorder_wrong_recognized <- 
    some.edibles[((1 - matrix_wrong_recognized) %>% 
                      as.dist %>% 
                      hclust())$order]

png(here::here("mini", "model2", "outputs", "classMatrices",
               "wrong_recognized_reordered.png"),
    height = 1100, width = 1100)
par(mar = c(7, 8, 1, 1) + 0.1, ps = 20, cex = 1, cex.main = 1.5)
plot(im(matrix_wrong_recognized[reorder_wrong_recognized, 
                                reorder_wrong_recognized]),
     main = "", zlim = c(0,1))
title("Misclassified Google-recognized images classification probabilities",
      adj = 0, line = -5)
text(1:ncol(matrix_wrong_recognized), par("usr")[1] + 1,
     srt = 90, adj = 1, cex = 1, xpd = TRUE,
     labels = reorder_wrong_recognized)
text(par("usr")[1] + 1, 1:nrow(matrix_wrong_recognized),
     adj = 1, cex = 1, xpd = TRUE,
     labels = reorder_wrong_recognized)
mtext(text = "TRUTH", side = 2, line = 6.5)
mtext(text = "CLASSIFICATION", side = 1, adj = .4, line = 3)
dev.off()



#### Classification matrix on Google-unrecognized test set ####

# matrix is ordered from highest to lowest correct classification

# plot is used on website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>

matrix_unrecognized <- model_unrecognized$class.matrix
order_unrecognized <- names(sort(desc(diag(matrix_unrecognized))))

png(here::here("mini", "model2", "outputs", "classMatrices",
               "unrecognized_ordered.png"),
    height = 1100, width = 1100)
par(mar = c(7, 8, 1, 1) + 0.1, ps = 20, cex = 1, cex.main = 1.5)
plot(im(matrix_unrecognized[order_unrecognized, order_unrecognized]),
     main = "", zlim = c(0,1))
title("Google-unrecognized images classification probabilities",
      adj = 0.2, line = -5)
text(1:ncol(matrix_unrecognized), par("usr")[1] + 1,
     srt = 90, adj = 1, cex = 1, xpd = TRUE,
     labels = order_unrecognized)
text(par("usr")[1] + 1, 1:nrow(matrix_unrecognized),
     adj = 1, cex = 1, xpd = TRUE,
     labels = order_unrecognized)
mtext(text = "TRUTH", side = 2, line = 6.5)
mtext(text = "CLASSIFICATION", side = 1, adj = .4, line = 3)
dev.off()


#### Classification matrix on misclassified Google-unrecognized images ####

# matrix is ordered so that similar categories appear close to one another

# plot is used on website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>



matrix_wrong_unrecognized <- matrix_unrecognized
diag(matrix_wrong_unrecognized) <- 0
matrix_wrong_unrecognized <- matrix_wrong_unrecognized / 
    matrix(rep((1 - diag(matrix_unrecognized)), 
               ncol(matrix_wrong_unrecognized)),
           ncol = ncol(matrix_wrong_unrecognized))

reorder_wrong_unrecognized <- 
    some.edibles[((1 - matrix_wrong_unrecognized) %>% 
                      as.dist %>% 
                      hclust())$order]

png(here::here("mini", "model2", "outputs", "classMatrices",
               "wrong_unrecognized_reordered.png"),
    height = 1100, width = 1100)
par(mar = c(7, 8, 1, 1) + 0.1, ps = 20, cex = 1, cex.main = 1.5)
plot(im(matrix_wrong_unrecognized[reorder_wrong_unrecognized, 
                                  reorder_wrong_unrecognized]),
     main = "", zlim = c(0,1))
title("Misclassified Google-unrecognized images classfctn probabilities",
      adj = 0, line = -5)
text(1:ncol(matrix_wrong_unrecognized), par("usr")[1] + 1,
     srt = 90, adj = 1, cex = 1, xpd = TRUE,
     labels = reorder_wrong_unrecognized)
text(par("usr")[1] + 1, 1:nrow(matrix_wrong_unrecognized),
     adj = 1, cex = 1, xpd = TRUE,
     labels = reorder_wrong_unrecognized)
mtext(text = "TRUTH", side = 2, line = 6.5)
mtext(text = "CLASSIFICATION", side = 1, adj = .4, line = 3)
dev.off()







