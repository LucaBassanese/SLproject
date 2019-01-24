library(rtweet)
library(twitteR)
library(devtools)
library("httr")
library("base64enc")
library(lubridate)
library(dplyr)
library(tidyverse)
library(readr)
library(tidytext)
library(stringr)
library(tokenizers)
library(purrr)
library(broom)
library(plotly)
library(tweenr)
library(tm)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggraph)
library(igraph)
library(widyr)

#setwd("D:/Universita/Statistical Learning/Progetto")
#data on github
stopwords <- read_csv("stopwords2.txt", 
                      col_names = "words")

Salvinifinale <- read_csv("Salvinifinaledef.csv")
Salvini <- Salvinifinale %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))
#bigram

remove_reg <- "&amp;|&lt;|&gt;"
bigrams <-  Salvini %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                                   "", text)) %>%
  unnest_tokens(bigram, text, token = c("ngrams"),n=2) 


bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords$words, !word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% stopwords$words,!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt"))



prova <- bigrams_filtered %>% 
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word1,word2) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word1,word2) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word1 %in%"immigrati",word2 %in% "clandestini") 
  
prova
prova2 <- bigrams_filtered %>% 
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word1,word2) %>%
  group_by(screen_name, time_floor) %>%
  top_n(1) %>% 
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word1,word2) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) 

prova2 

p <- prova %>% 
  ggplot(aes(time_floor, count/time_total, color = word2)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts %>%
  filter(word1 %in% c("immigrati", "italiani", "lavoro", "pd")) %>%
  group_by(word1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ word1, scales = "free") +
  coord_flip()
bigram_counts
bigram_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Salvini",
       subtitle = "bigrams",
       x = "", y = "") +
  theme_void()



bigram_counts %>% 
  filter(word1=="elezioni")

tweets <-  Salvini %>% 
  filter(!str_detect(text, "^RT"), is_retweet==F) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))


library(widyr)

# count words co-occuring within sections
word_pairs <- tweets %>%
  pairwise_count(word, created_at, sort = TRUE)

word_pairs

# correlazione prima e dopo le elezioni
word_corsbefore <- tweets %>%
  filter(created_at <= "2018-03-04") %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)

word_corsbefore


word_corsbefore %>%
  filter(item1 %in% c("governo", "lavoro", "sicurezza")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_corsbefore %>%
  filter(item1 == "ong")

word_corsbefore %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

word_corsafter <- tweets %>%
  filter(created_at >= "2018-03-04") %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)

word_corsafter


word_corsafter %>%
  filter(item1 %in% c("governo", "lavoro", "sicurezza")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_corsafter %>%
  filter(item1 == "ong")

word_corsafter %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#trigram


trigrams <-  Salvini %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                     "", text)) %>%
  unnest_tokens(trigram, text, token = c("ngrams"),n=3) 


trigrams %>%
  count(trigram, sort = TRUE)
trigrams_separated <- trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt")) %>% 
  filter(!word3 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word3, "[a-z]"),!str_detect(word3, "01f"),!str_detect(word3, "gt"))



prova <- bigrams_filtered %>% 
  mutate(time_floor = floor_date(created_at, unit = "1 day")) %>%
  count(time_floor, screen_name, word1,word2) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word1,word2) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word1 %in%c("governo"))


p <- prova %>%
  filter(count>=2) %>% 
  ggplot(aes(time_floor, count/time_total, color = word2)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p)

# new bigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigram_counts














Renzi <- read_csv("MRenzitweetsdef.csv")
Renzi <- Renzi %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))


remove_reg <- "&amp;|&lt;|&gt;"
bigrams <-  Renzi %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                     "", text)) %>%
  unnest_tokens(bigram, text, token = c("ngrams"),n=2) 


bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords$words, !word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% stopwords$words,!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt"))


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Salvini",
       subtitle = "bigrams",
       x = "", y = "") +
  theme_void()


tweets <-  Renzi %>% 
  filter(!str_detect(text, "^RT"), is_retweet==F) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# count words co-occuring within sections
word_pairs <- tweets %>%
  pairwise_count(word, created_at, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 %in% c("salvini", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_pairs %>%
  filter(n>10) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  coord_flip()


word_cors <- tweets %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)
word_cors


word_cors %>%
  filter(item1 %in% c("immigrati", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cors %>%
  filter(item1 == "")

word_cors %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

word_corsbefore <- tweets %>%
  filter(created_at <= "2018-03-04") %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)

word_corsbefore


word_corsbefore %>%
  filter(item1 %in% c("governo", "lavoro", "sicurezza")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_corsbefore %>%
  filter(item1 == "ong")

word_corsbefore %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

word_corsafter <- tweets %>%
  filter(created_at >= "2018-03-04") %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)

word_corsafter


word_corsafter %>%
  filter(item1 %in% c("governo", "lavoro", "sicurezza")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_corsafter %>%
  filter(item1 == "ong")

word_corsafter %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()






DiMaiotweets <- read_csv("DiMaiotweetsdef.csv")

DiMaio <- DiMaiotweets %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

bigrams <-  DiMaio %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                     "", text)) %>%
  unnest_tokens(bigram, text, token = c("ngrams"),n=2) 


bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords$words, !word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% stopwords$words,!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt"))


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Salvini",
       subtitle = "bigrams",
       x = "", y = "") +
  theme_void()


tweets <-  DiMaio %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# count words co-occuring within sections
word_pairs <- tweets %>%
  pairwise_count(word, created_at, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 %in% c("salvini", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_pairs %>%
  filter(n>10) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  coord_flip()


word_cors <- tweets %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)
word_cors


word_cors %>%
  filter(item1 %in% c("immigrati", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cors %>%
  filter(item1 == "renzi")

word_cors %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



Berlusconi <- read_csv("Berlusconitweetsdef.csv")

Berlusconi <- Berlusconi %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))


bigrams <-  Berlusconi %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                     "", text)) %>%
  unnest_tokens(bigram, text, token = c("ngrams"),n=2) 


bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords$words, !word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% stopwords$words,!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt"))


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Salvini",
       subtitle = "bigrams",
       x = "", y = "") +
  theme_void()


tweets <-  Berlusconi %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# count words co-occuring within sections
word_pairs <- tweets %>%
  pairwise_count(word, created_at, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 %in% c("salvini", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_pairs %>%
  filter(n>10) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  coord_flip()


word_cors <- tweets %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)
word_cors


word_cors %>%
  filter(item1 %in% c("immigrati", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cors %>%
  filter(item1 == "renzi")

word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


Grillofinale <- read_csv("Grillofinaledef.csv")

Grillo <- Grillofinale %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))



bigrams <-  Grillo %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                     "", text)) %>%
  unnest_tokens(bigram, text, token = c("ngrams"),n=2) 


bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords$words, !word1 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word1, "[a-z]"),!str_detect(word1, "01f"),!str_detect(word1, "gt")) %>%
  filter(!word2 %in% stopwords$words,!word2 %in% str_remove_all(stopwords$words, "'"),
         str_detect(word2, "[a-z]"),!str_detect(word2, "01f"),!str_detect(word2, "gt"))


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Salvini",
       subtitle = "bigrams",
       x = "", y = "") +
  theme_void()


tweets <-  Grillo %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# count words co-occuring within sections
word_pairs <- tweets %>%
  pairwise_count(word, created_at, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 %in% c("salvini", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_pairs %>%
  filter(n>10) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, n)) %>%
  ggplot(aes(item2, n)) +
  geom_bar(stat = "identity") +
  coord_flip()


word_cors <- tweets %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, created_at, sort = TRUE)
word_cors


word_cors %>%
  filter(item1 %in% c("immigrati", "italiani", "lavoro", "governo")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cors %>%
  filter(item1 == "renzi")

word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()




