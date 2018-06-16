library(pacman)
p_load(myScrapers, tidyverse, tidytext, data.table, topicmodels, quanteda, RTextTools, caret, yardstick)
#devtools::install_github("quanteda/quanteda")
#library(quanteda)


healthy_aging <- pubmedAbstractR("healthy aging", end = 2018, n = 10000)

########

dataset <- healthy_aging %>%
  group_by(keyword, DOI) %>%
  mutate(healthy_ageing = ifelse(str_detect(keyword, "Healthy Ag"), 1, 0)) %>%
  summarise(class = mean(healthy_ageing)) %>%
  left_join(healthy_aging) %>%
  ungroup() %>%
  select(-keyword) %>%
  distinct()

ds0 <- dataset %>%
  filter(class == 0) %>%
  sample_n( size = 2000)

ds1 <- dataset %>%
  filter(class == 1)

dstot <- bind_rows(ds0, ds1)

dstot1 <- select(dstot, abstract, DOI, class) %>%
  filter(!is.null(abstract))

dstot1 <- sample_frac(dstot1, 1)

corp <- corpus(dstot1$abstract)

docvars(corp, "class") <- dstot1$class
docvars(corp, "DOI") <- dstot1$DOI

dfm <- dfm(corp, ngrams = 1:3, remove = stopwords("en"), remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE)

table(dstot1$class)

matrix <- create_matrix(cbind(dstot1["abstract"],dstot1["class"]), language="english", 
                        removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)

df <- convert(dfm, to = "data.frame")


container <- create_container(matrix,dstot1$class,trainSize=1:1000, testSize=1001:1584, 
                              virgin=FALSE)

container@

models <- train_models(container, algorithms=c("MAXENT", "SVM"))

models$MAXENT

results <- classify_models(container, models)
analytics <- create_analytics(container, results)

analytics@algorithm_summary
score_summary <- create_scoreSummary(container, results)

summary(analytics)

final <- classify_models(container, models) %>% class %>% cbind(slice(dstot1, (1001:1584)))

filter(final, class == 1) %>%
  View()

#########
filt_aging <- healthy_aging %>%
  select(-keyword) %>%
  distinct() 

filt_aging %>%
  group_by(year, DOI) %>%
  count() %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(title = "Frequency of pubmed articlce with MeSH heading 'healthy aging' by year", 
       caption = "'healthy aging' seems to have be first used as a MeSH heading in 2015")

bi <- healthy_aging %>%
  create_bigrams(., title) 

count_bi <- bi %>%
  group_by(year, bigram) %>%
  count(sort = TRUE) 




count_bi %>%
  filter(n >=25) %>%
  create_network_plot(layout = "fr")


aging_corpus <- corpus(filt_aging$title)
docvars(aging_corpus, "year") <- filt_aging$year
docvars(aging_corpus, "title") <- filt_aging$title
docvars(aging_corpus, "DOI") <- filt_aging$DOI

aging_dfm <- dfm(aging_corpus, remove = c(stopwords("en"), "health", "healthy", "aging", "ageing"), ngrams = 1:2, remove_punct = TRUE)
aging_dfm <- dfm_remove(aging_dfm, c("healthy_aging", "healthy_ageing", "and_healthy"))
aging_topics <- convert(aging_dfm, to = "topicmodels")
aging_topics <- LDA(aging_topics, k = 10)

terms(aging_topics, 15)

tidy(topics(aging_topics)) %>%
  bind_cols(filt_aging) %>%
  ggplot(aes(title, factor(x))) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4))

tidy(topics(aging_topics)) %>%
  bind_cols(filt_aging) %>%
  group_by(journal, x) %>%
  count() %>%
  ggplot(aes(journal, factor(x), fill = n)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  coord_flip()

tidy(topics(aging_topics)) %>%
  bind_cols(filt_aging) %>%
  filter(str_detect(journal, "Ger")) %>%
  View()


data <- NYTimes[sample(1:3100,size=1000,replace=FALSE),]
str(data)
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english", 
                        removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$Topic.Code,trainSize=1:750, testSize=751:1000, 
                              virgin=FALSE)

models <- train_models(container, algorithms=c("MAXENT","SVM", "NNET", "RF", "BAGGING", "SLDA"))

results <- classify_models(container, models)
analytics <- create_analytics(container, results)

analytics@algorithm_summary
score_summary <- create_scoreSummary(container, results)

summary(analytics)
