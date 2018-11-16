
# load food data
load("binary_data/apple.rdata")
load(file = file.path("Algorithms","Likelihoods_MLE1.rdata"))

# get observed likelihoods for random sample of 100 drawings.
# this takes a while to merge all of the data frames.
df2 <- 
df %>%
mutate(y = y+255) %>%
select(-stroke) %>%
nest(-key_id) %>%
sample_n(100) %>%
mutate(lik = map(data, ~left_join(.,likelihoods, by = c("x","y"))),
       lik = map(lik, ~select(., -x, -y) %>% colSums %>% sort(decreasing = T)),
       pred1 = map(lik, ~.[1:3])) 

# ahhhhh. it works!!!
save(df2, file = "Algorithms/MLE1_Apple_Predictions.rdata")

df2$pred1
