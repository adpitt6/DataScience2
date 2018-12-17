#### Source relevant functions ####

source(here::here("a_FunctionsForProcessingData.R"))


#### Data reduction plots ####

# These plots are used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>

for (i in some.edibles) {
    
    image <- get_one(i) %>% raw_to_strokes
    
    image.human <- image %>%
        to_top() %>%
        to_left()
    
    image.cnn <- image %>%
        center_xy() %>%
        resolution_256_to_32() %>%
        strokes_to_points()
    
    
    p1 <- image %>% ggplot(aes(x = x, y = y)) + 
        geom_path(aes(group = stroke)) +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0(i,"\noriginal data"),
             x = "",
             y = "")
    
    p2 <- image.human %>% ggplot(aes(x = x, y = y)) + 
        geom_point() +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0(i,
                            "\ndata used by the MLE classifier"),
             x = "",
             y = "")
    
    p3 <- image.cnn %>% ggplot(aes(x = x, y = y)) + 
        geom_point() +
        theme_bw() +
        xlim(0, 31) + ylim(31, 0) +
        labs(title = paste0(i, "\ndata used by OurCNN"),
             x = "",
             y = "")
    
    png(here::here("mini", "model2", "outputs", "dataPlots",
                   paste0("data_reduction_", i, ".png")),
        height = 340, width = 960)
    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
    dev.off()
}



#### Data subsetting plots ####

# These plots are used on our website <https://adpitt6.github.io/DataScience2/AnUnconsciousApproach.html>


for (i in some.edibles) {
    
    rec   <- get_one(i) %>% raw_to_strokes
    unrec <- get_one(i, recognized = FALSE) %>% raw_to_strokes
    
    rec.human   <- rec   %>% to_top() %>% to_left()
    unrec.human <- unrec %>% to_top() %>% to_left()
    
    rec.cnn <- rec %>% 
        center_xy() %>% 
        resolution_256_to_32() %>%
        strokes_to_points()
    
    
    p1 <- rec %>% ggplot(aes(x = x, y = y)) + 
        geom_path(aes(group = stroke)) +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0("RECOGNIZED ", i, "\noriginal data"),
             x = "",
             y = "")
    
    p2 <- rec.human %>% ggplot(aes(x = x, y = y)) + 
        geom_point() +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0("RECOGNIZED ", i,
                            "\ndata used by the MLE classifier"),
             x = "",
             y = "")
    
    p3 <- rec.cnn %>% ggplot(aes(x = x, y = y)) + 
        geom_point() +
        theme_bw() +
        xlim(0, 31) + ylim(31, 0) +
        labs(title = paste0("RECOGNIZED ", i, "\ndata used by OurCNN"),
             x = "",
             y = "")
    
    p4 <- unrec %>% ggplot(aes(x = x, y = y)) + 
        geom_path(aes(group = stroke)) +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0("UNRECOGNIZED ", i, "\noriginal data"),
             x = "",
             y = "")
    
    p5 <- unrec.human %>% ggplot(aes(x = x, y = y)) + 
        geom_point() +
        theme_bw() +
        xlim(0, 255) + ylim(255, 0) +
        labs(title = paste0("UNRECOGNIZED ", i,
                       "\ndata used by the MLE classifier"),
             x = "",
             y = "")
    
    png(here::here("mini", "model2", "outputs", "dataPlots",
                   paste0("rec_unrec_", i, ".png")),
        height = 680, width = 960)
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, ncol = 3)
    dev.off()
}


