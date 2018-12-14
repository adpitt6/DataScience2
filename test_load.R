df <- 
	read_csv(file.path("train_simplified", "apple.csv"),n_max = 1)%>%
	select(key_id, drawing) %>%
	mutate(drawing = map(drawing, ~fromJSON(., simplifyMatrix = F))
	) %>%
	unnest() %>%
	mutate(drawing = map(drawing, ~do.call(.,what="rbind") %>% 
		         	t %>% 
		         	as.tibble())
	) %>%
	unnest(.id = "stroke") %>%
	mutate(x = V1, y =  255-V2) 
plot(select(df,x,y))
