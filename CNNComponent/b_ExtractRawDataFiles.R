source(here::here("a_FunctionsForDataProcessing.R"))

# Check if data have been downloaded and placed in directory 'data'
# (no automatic download here b/c data access requires signing in to data site)

if (!dir.exists(here::here("data_download"))) { 
    dir.create(here::here("data_download"))
}
if (!file.exists(here::here("data_download", "train_simplified.zip"))) {
    paste0(c("You need to download the data file train_simplified.zip from\n",
             "https://www.kaggle.com/c/quickdraw-doodle-recognition/data",
             "first!"))
}

for (i in some.edibles) { get_raw_file(i) }



