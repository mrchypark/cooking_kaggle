library(jsonlite)
library(dplyr)

unzip("all.zip", exdir = "data")

tra <- fromJSON("./data/train.json") %>% 
  as_tibble()

tst <- fromJSON("./data/test.json") %>% 
  as_tibble()
