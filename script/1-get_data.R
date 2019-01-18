library(jsonlite)
library(dplyr)

tra <- fromJSON("./data/train.json") %>% 
  as_tibble()

tst <- fromJSON("./data/test.json") %>% 
  as_tibble()
