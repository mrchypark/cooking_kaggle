library(data.table)
library(caret)
library(superml)
library(Metrics)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(text2vec)
library(glmnet)

cuis <- fromJSON("data/train.json") %>% 
  as_tibble() %>% 
  rename(ingre = ingredients) 

set.seed(2019)

split <- createDataPartition(cuis$cuisine, p = 0.8)

given <- cuis[split$Resample1, ]
submittion <- cuis[-split$Resample1, ]

given %>% group_by(cuisine) %>% summarise(n = n()) %>% mutate(per = n/sum(n))
submittion %>% group_by(cuisine) %>% summarise(n = n()) %>% mutate(per = n/sum(n))

# split <- createDataPartition(given$cuisine, p = 0.8)
# 
# train <- given[split$Resample1, ]
# test <- given[-split$Resample1, ]
# 
# train %>% group_by(cuisine) %>% summarise(n=n()) %>% mutate(per = n/sum(n))
# test %>% group_by(cuisine) %>% summarise(n=n()) %>% mutate(per = n/sum(n))


train <- 
  given %>% 
  unnest() %>% 
  unnest_tokens(word, ingre) %>% 
  mutate(word = tolower(word)) %>% 
  mutate(word = gsub("-", "_", word)) %>% 
  mutate(word = gsub("[^a-z0-9_ ]", "", word)) %>% 
  group_by(id, cuisine) %>% 
  summarise(ingre = paste0(word, collapse = " ")) %>% 
  ungroup()

it_train = itoken(train$ingre, 
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer, 
                  ids = train$id, 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train, tfidf)

glmnet_classifier = cv.glmnet(x = dtm_train_tfidf, 
                              y = train[['cuisine']], 
                              family = "multinomial",
                              alpha = 1,
                              type.measure = "class",
                              nfolds = 10,
                              thresh = 1e-3,
                              maxit = 3e3)

plot(glmnet_classifier)
max(glmnet_classifier$cvm)



# library(performanceEstimation)
# 
# submittion %>% 
#   unnest() %>% 
#   unnest_tokens(word, ingre) %>% 
#   mutate(word = tolower(word)) %>% 
#   mutate(word = gsub("-", "_", word)) %>% 
#   mutate(word = gsub("[^a-z0-9_ ]", "", word)) %>% 
#   group_by(id, cuisine) %>% 
#   summarise(ingre = paste0(word, collapse = " ")) %>% 
#   ungroup() -> test
# 
# it_test <- test$cuisine %>% 
#   tolower %>% 
#   word_tokenizer %>% 
#   itoken(ids = test$id, 
#          progressbar = FALSE)
# 
# dtm_test_tfidf <- create_dtm(it_test, vectorizer) %>% 
#   transform(tfidf)

pred <- predict(glmnet_classifier, dtm_test_tfidf, type = 'class')

classificationMetrics(test$cuisine, as.character(pred))






###################################################################

library(caret)
library(xgboost)

itControl <- trainControl(
  method = "cv",
  number = 10,
  sampling = "up")

dmatrix <- xgb.DMatrix(dtm_train_tfidf)

gbmFit1 <- train(x = dmatrix,
                  y = train$cuisine, 
                 method = "xgbTree", 
                 metric = "Accuracy",
                 trControl = fitControl,
                 nrounds = 100,
                 max.depth = 11,
                 objective = "num_class:20")
