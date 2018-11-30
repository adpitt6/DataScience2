
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
       pred1 = map(lik, ~names(.[1])),
       pred3 = map(lik, ~names(.[1:3]))) 

df2$pred1 %>% unlist

# ahhhhh. it works!!!
save(df2, file = "Algorithms/MLE1_Apple_Predictions.rdata")

# identify bad predictions
df2 <- 
df2 %>%
mutate(notGood = map(pred1, ~ . != "apple") %>% unlist,
       bad = map(pred3, ~!"apple" %in% .) %>% unlist)

# correctly classified 89 apples
with(df2, table(notGood, bad))

# look at very bad predictions
df3 <- df2 %>% filter(bad) %>% select(pred1) 
df3[[1]]

bad.apples <- 
df2 %>% 
filter(bad) %>%
select(-lik, -bad,  -pred3, -notGood) %>%
mutate(pred1 = unlist(pred1)) %>%
unnest

pdf(file = "Algorithms/Bad_Apples.pdf", width = 4, height = 3)
ggplot(bad.apples) +
geom_path(aes(x=x, y=y)) +
geom_point(aes(x=x, y=y)) +
theme_bw() +
geom_text(aes(x = 100, y = 100, label = pred1), col = "red")
dev.off()

# look at not good predictions
df3 <- df2 %>% filter(notGood & !bad) %>% select(pred1) 
df3[[1]]

bad.apples <- 
	df2 %>% 
	filter(notGood & !bad) %>%
	select(-lik, -bad, -notGood, -pred3) %>%
	mutate(pred1 = unlist(pred1)) %>%
	unnest

pdf(file = "Algorithms/NotGood_Apples.pdf", width = 7, height = 6)
ggplot(bad.apples) +
	geom_path(aes(x=x, y=y)) +
	geom_point(aes(x=x, y=y)) +
	facet_wrap("key_id") +
	theme_bw() +
	geom_text(aes(x = 100, y = 100, label = pred1), col = "red")
dev.off()
