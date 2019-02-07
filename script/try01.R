library(caret)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(text2vec)
library(glmnet)
library(textstem)

cuis <- fromJSON("data/train.json") %>% 
  as_tibble() %>% 
  rename(ingre = ingredients) 

set.seed(2019)

split <- createDataPartition(cuis$cuisine, p = 0.8)

given <- cuis[split$Resample1, ]
submission <- cuis[-split$Resample1, ]

given %>% group_by(cuisine) %>% summarise(n = n()) %>% mutate(per = n/sum(n))
submission %>% group_by(cuisine) %>% summarise(n = n()) %>% mutate(per = n/sum(n))

train <- 
  given %>% 
  unnest() %>% 
  unnest_tokens(word, ingre) %>% 
  mutate(word = tolower(word)) %>% 
  mutate(word = gsub("-", "_", word)) %>% 
  mutate(word = gsub("[^a-z0-9_ ]", "", word)) %>% 
  # mutate(word = lemmatize_words(word)) %>%
  group_by(id, cuisine) %>% 
  summarise(ingre = paste0(word, collapse = " ")) %>% 
  ungroup()

it_train = itoken(train$ingre, 
                  tokenizer = word_tokenizer, 
                  ids = train$id,
                  progressbar = FALSE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train, tfidf)

submission %>%
  unnest() %>%
  unnest_tokens(word, ingre) %>%
  mutate(word = tolower(word)) %>%
  mutate(word = gsub("-", "_", word)) %>%
  mutate(word = gsub("[^a-z0-9_ ]", "", word)) %>%
  # mutate(word = lemmatize_words(word)) %>% 
  group_by(id, cuisine) %>%
  summarise(ingre = paste0(word, collapse = " ")) %>%
  ungroup() -> test

it_test = itoken(test$ingre, 
                 tokenizer = word_tokenizer, 
                 ids = test$id,
                 progressbar = FALSE)

dtm_test_tfidf <- create_dtm(it_test, vectorizer) %>%
  transform(tfidf)

glmnet_classifier = cv.glmnet(x = dtm_train_tfidf, 
                              y = train[['cuisine']], 
                              family = "multinomial",
                              type.measure = "class",
                              nfolds = 10,
                              thresh = 1e-3,
                              maxit = 1e3)

saveRDS(glmnet_classifier, "clfr.rds")

glmcl <- readRDS("clfr.rds")

plot(glmnet_classifier)
max(glmnet_classifier$cvm)

pred <- predict(glmnet_classifier, dtm_test_tfidf, type = 'class')

ref <- factor(test$cuisine)

matric <- confusionMatrix(ref, factor(pred, levels = levels(ref)), mode = "everything")
matric


###################################################################

library(text2vec)
library(xgboost)

label_train <- as.factor(train$cuisine)
labels <- levels(label_train)
label_train <- as.integer(label_train) - 1
dtrain <- xgb.DMatrix(dtm_train_tfidf, label = label_train)

label_test <- as.factor(test$cuisine)
label_test <- as.integer(label_test) - 1
dtest <- xgb.DMatrix(dtm_test_tfidf, label = label_test)

xgb_params = list(
  objective = "multi:softmax",
  num_class = 20,
  max.depth = 8)

xgb_fit <-
  xgboost(
    data = dtrain,
    label = label_train, 
    params = xgb_params,
    nrounds = 100,
    stratified = TRUE,
    print_every_n = 10,
    maximize = FALSE
  )

pred <- predict(xgb_fit, newdata = dtm_test_tfidf)

label_data <- as.factor(test$cuisine)
labels <- levels(label_data)
yglabel <- as.integer(label_data) - 1


confusionMatrix(factor(yglabel), factor(pred, levels = 0:19), "everything")
