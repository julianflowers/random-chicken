library(pacman)
p_load(myScrapers, tidyverse, lubridate)


get_phe_catalogue

source("trip_search.R")

end <- year(Sys.Date())

###### Precision public health  

term1 <- "precision  AND public health[MeSH]"
term <- "precision+public+health"

trip_1 <- trip_searchR(term) %>%
  mutate(link = paste("<a href=", link, ">Link</a>")) 

DT::datatable(trip_1, escape = FALSE)

pmed_1 <- pubmedAbstractR(search = term1, end = end,  n = 1000)


pmed_1 %>%
  group_by(keyword, year) %>%
  count(sort = TRUE) %>%
  filter(n <50 & n > 5) %>%
  create_network_plot(layout = "fr")


pmed_1 %>%
  filter(str_detect(title, "[Pp]recision")) %>%
  select(title, DOI, abstract, journal, year) %>%
  distinct() %>%
  DT::datatable()

###### exposome/ phenome

term2 <- "exposome OR phenome"
term3 <- "exposome"
term4 <- "phenome+public+health"

pmed_2 <- pubmedAbstractR(search = term2, end = end,  n = 1015)
trip_2 <- trip_searchR(term3, n = 1000) %>%
  mutate(link = paste("<a href=", link, ">Link</a>")) %>%
  DT::datatable(., escape = FALSE)
trip_3 <- trip_searchR(term4, n = 1000) %>%
  mutate(link = paste("<a href=", link, ">Link</a>")) %>%
  DT::datatable(., escape = FALSE)


pmed_2 %>%
  filter(str_detect(keyword, "[Pp]ublic")) %>%
  select(DOI, title, abstract) %>%
  View()

## AI public health

term5 <- "Artificial Intelligence[MeSH] AND Public Health{MeSH]"

ai_pmed <- pubmedAbstractR(term5, end = end, n = 2017)

ai_pmed <- mutate(ai_pmed, search = term5)

ai_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(title = paste("Annual count of abstracts: ", term5)) 

ai_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  create_bigrams(abstract) %>%
  group_by(year, bigram) %>%
  count() %>%
  filter(n>9) %>%
  create_network_plot()

aidoi <- unique(ai_pmed$DOI)

aidoi

## ML public health

term6 <- "Machine Learning[MeSH] AND Public Health[MeSH]"

ml_pmed <- pubmedAbstractR(term6, end = end, n = 4556)

ml_pmed <- mutate(ml_pmed, search = term6)

head(ml_pmed)

mldoi <- unique(ml_pmed$DOI)

ml_pmed %>%
  filter(keyword %in% c("Machine Learning", "Artificial Intelligence")) %>%
  

#### overlap
ai_ml_pmed <- filter(ml_pmed, DOI %in% intersect(mldoi, aidoi)) 

ai_ml_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  create_bigrams(abstract) %>%
  group_by(year, bigram) %>%
  count() %>%
  filter(n>4, year > 2014, year < 2018) %>%
  create_network_plot(textsize = 2) +
  labs(title = "Network plot of abstracts for AI & machine learning in PH",
       subtitle = paste("Abstracts 2015-17: n = ", length(unique(ai_ml_pmed$DOI))))


ml_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(title = paste("Annual count of abstracts: ", term6)) 

ml_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  create_bigrams(abstract) %>%
  group_by(year, bigram) %>%
  count() %>%
  filter(n>19) %>%
  create_network_plot()

## chat bot   
  
term7 <- "chatbot AND Public Health[MeSH]"

chatbot_pmed <- pubmedAbstractR(term7, end = end, n = 2)

chatbot_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(title = paste("Annual count of abstracts: ", term5)) 

chatbot_pmed %>%
  select(-keyword) %>%
  distinct() %>%
  create_bigrams(abstract) %>%
  group_by(year, bigram) %>%
  count() %>%
  filter(n>0) %>%
  create_network_plot()
  
  
