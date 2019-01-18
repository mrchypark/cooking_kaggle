source("script/1-get_data.R")

library(tidyverse)

tra %>% 
  mutate(cuisine = factor(cuisine, levels = flevel)) -> tra

tra %>% 
  group_by(cuisine) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% 
  .$cuisine -> flevel

ggplot(tra, aes(x = cuisine)) + 
  geom_bar()

# 7th largist cuisine size is elbow. cnt = 1546

tra %>% 
  tidyr::unnest() %>% 
  group_by(ingredients) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) -> ingre

tra %>% 
  tidyr::unnest() %>% 
  group_by(ingredients) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt))  -> flevel

flevel %>% 
  filter(cnt > 1000) %>% 
  .$ingredients -> tar

tra %>% 
  tidyr::unnest() %>% 
  mutate(ingredients = factor(ingredients, levels = flevel$ingredients)) %>% 
  filter(ingredients %in% tar) %>% 
  ggplot(aes(x = ingredients)) + 
  geom_bar() +
  coord_flip() +
  scale_y_reverse()
  
