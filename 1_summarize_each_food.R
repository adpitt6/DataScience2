
### Grab Binary File names
bin.files <- dir("binary_data")

### Create directory for summary files
if(!file.exists("summary_data")) dir.create("summary_data")

### Loop through all full binary files
### generate pixel by pixel summaries
for(i in bin.files){
	load(file.path("binary_data",paste0(i)))
	df.sum <- 
		df %>%
		group_by(x,y) %>%
		summarise(n = n())
	save(df.sum, file = file.path("summary_data",paste0(i)))
}
