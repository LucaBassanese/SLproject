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
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(wordcloud)

#setwd("D:/Universita/Statistical Learning/Progetto")

#########Di maio ##############


DiMaiotweets <- read_csv("DiMaiotweetsdef.csv")

DiMaio <- DiMaiotweets %>%
  filter(created_at >= as.Date("2017-01-01"), created_at <= as.Date("2019-01-01"))


stopwords <- read_csv("stopwords2.txt", 
                       col_names = "words")
View(stopwords)

remove_reg <- "&amp;|&lt;|&gt;"
tweets <-  DiMaio %>% 
  filter(!str_detect(text, "^RT"), is_retweet == F ) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))
 
# tweets <-  DiMaio %>% 
#   filter(!str_detect(text, "^RT"))
# tweets$text
# c <- tweets %>% mutate(text = str_remove_all(text, remove_reg))
# c$text
# 
# c <- c %>%  unnest_tokens(word, text, token = "tweets")
# c$word
# 
# c <- c %>% filter(!word %in% stopwords$words,
#                   !word %in% str_remove_all(stopwords$words, "'"),
#                   str_detect(word, "[a-z]"))
# c$word

tweets <- tweets %>% 
  mutate(word=str_remove(word,"#"))

#wirdcloud prima e dopo le elezioni 
tweets %>%
  filter(created_at <= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F, random.order = F))

tweets %>%
  filter(created_at >= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F, random.order = F))

#frequenza parole
frequency <- tweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency


(immigrazione <- frequency %>% filter(word %in% c("immigrazione","immigrati","immigrato")))


(lavoro <- frequency %>% filter(word %in% c("lavoro")))

(UE <- frequency %>% filter(word %in% c("unione","europea","ue")))

(istruzione <- frequency %>% filter(word %in% c("istruzione","scuola")))

(terrorismo <- frequency %>% filter(word %in% c("terrorismo")))

(famiglia <- frequency %>% filter(word %in% c("famiglia")))

(pd <- frequency %>% filter(word %in% c("pd","renzi","gentiloni")) )

#frequenza mensile 
words_by_time <- tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%c("lavoro","europea","istruzione","terrorismo","famiglia","governo","pd"))


nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

p <- words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "luigidimaio") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")

#grafico serie storiche frequenze mensili 

library(plotly)

ggplotly(p)

########Salvini###########

Salvinifinale <- read_csv("Salvinifinaledef.csv")
Salvini <- Salvinifinale %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

remove_reg <- "&amp;|&lt;|&gt;"
tweets <-  Salvini %>% 
  filter(!str_detect(text, "^RT"), is_retweet == F) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# tweets <-  Salvini %>% 
#   filter(!str_detect(text, "^RT"))
# tweets$text
# c <- tweets %>% mutate(text = str_remove_all(text, remove_reg))
# c$text
# 
# c <- c %>%  unnest_tokens(word, text, token = "tweets")
# c$word
# 
# c <- c %>% filter(!word %in% stopwords$words,
#                   !word %in% str_remove_all(stopwords$words, "'"),
#                   str_detect(word, "[a-z]"))
# c$word

tweets <- tweets %>% 
  mutate(word=str_remove(word,"#"))


frequency <- tweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency


pal <- brewer.pal(8,"Dark2")
par(mfrow=c(1,2))

tweets %>%
  filter(created_at <= "2018-03-04",!str_detect(word, "<u"),word != "#salvini") %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F,random.order = F))

tweets %>%
  filter(created_at >= "2018-03-04",!str_detect(word, "<u"),word != "#salvini") %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F,random.order = F))


par(mfrow=c(1,2))




frequencybefore <- tweets %>% 
  filter(created_at <= "2018-03-04" ) %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequencyafter <- tweets %>% 
  filter(created_at >= "2018-03-04" ) %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)



(immigrazione <- frequency %>% filter(word %in% c("immigrazione","immigrati","immigrato")))

(lavoro <- frequency %>% filter(word %in% c("lavoro")))

(UE <- frequency %>% filter(word %in% c("unione","europea","ue")))

(istruzione <- frequency %>% filter(word %in% c("istruzione","scuola")))

(terrorismo <- frequency %>% filter(word %in% c("terrorismo")))

(famiglia <- frequency %>% filter(word %in% c("famiglia")))


words_by_time <- tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%c("lavoro","immigrati","ue","scuola","terrorismo","famiglia","governo","elezioni","pd"))

#prime due parole più usate ogni mese
parolepiuusatemensili <- tweets %>%
  filter(!str_detect(word, "^@"), word != "salvini", !str_detect(word, "<u")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  top_n(2) %>% 
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) 

parolepiuusatemensili

nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

p <- words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "matteosalvinimi") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")



library(plotly)

ggplotly(p) %>% highlight("plotly_selected")


(PD <- frequency %>% filter(word %in% c("pd")))
(M5s <- frequency %>% filter(word %in% c("stelle")))


##########Berlusconi#############

Berlusconi <- read_csv("Berlusconitweetsdef.csv")
Berlusconi <- Berlusconi %>% 
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))
  

remove_reg <- "&amp;|&lt;|&gt;"
tweets <-  Berlusconi %>% 
  filter(!str_detect(text, "^RT"),is_retweet == F) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))
# 
# tweets <-  Berlusconi %>% 
#   filter(!str_detect(text, "^RT"))
# tweets$text
# c <- tweets %>% mutate(text = str_remove_all(text, remove_reg))
# c$text
# 
# c <- c %>%  unnest_tokens(word, text, token = "tweets")
# c$word
# 
# c <- c %>% filter(!word %in% stopwords$words,
#                   !word %in% str_remove_all(stopwords$words, "'"),
#                   str_detect(word, "[a-z]"))
# c$word


tweets %>%
  filter(created_at <= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F,random.order = F))

tweets %>%
  filter(created_at >= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F,random.order = F))

tweets <- tweets %>% 
  mutate(word=str_remove(word,"#"))



frequency <- tweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency


(immigrazione <- frequency %>% filter(word %in% c("immigrazione","immigrati","immigrato")))

(lavoro <- frequency %>% filter(word %in% c("lavoro")))

(UE <- frequency %>% filter(word %in% c("unione","europea","ue")))

(istruzione <- frequency %>% filter(word %in% c("istruzione","scuola")))

(terrorismo <- frequency %>% filter(word %in% c("terrorismo")))

(famiglia <- frequency %>% filter(word %in% c("famiglia")))


words_by_time <- tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%c("lavoro","immigrati","europea","scuola","terrorismo","famiglia","governo","elezioni","pd"))


nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

p <- words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "berlusconi") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")



library(plotly)

ggplotly(p) %>% highlight("plotly_selected")


(PD <- frequency %>% filter(word %in% c("pd")))
(M5s <- frequency %>% filter(word %in% c("m5s")))


############Renzi##############


Renzi <- read_csv("MRenzitweetsdef.csv")

Renzi <- Renzi %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

remove_reg <- "&amp;|&lt;|&gt;"
tweets <-  Renzi %>% 
  filter(!str_detect(text, "^RT"), is_retweet==F) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))

# tweets <-  Renzi %>% 
#   filter(!str_detect(text, "^RT"))
# tweets$text
# c <- tweets %>% mutate(text = str_remove_all(text, remove_reg))
# c$text
# 
# c <- c %>%  unnest_tokens(word, text, token = "tweets")
# c$word
# 
# c <- c %>% filter(!word %in% stopwords$words,
#                   !word %in% str_remove_all(stopwords$words, "'"),
#                   str_detect(word, "[a-z]"))
# c$word


tweets %>%
  filter(created_at <= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 75,colors = pal, random.color = F, random.order = F))

tweets %>%
  filter(created_at >= "2018-03-04",!str_detect(word, "<u")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 3,colors = pal, random.color = F, random.order = F))

tweets <- tweets %>% 
  mutate(word=str_remove(word,"#"))


frequency <- tweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency


(immigrazione <- frequency %>% filter(word %in% c("immigrazione","immigrati","immigrato")))

(lavoro <- frequency %>% filter(word %in% c("lavoro")))

(UE <- frequency %>% filter(word %in% c("unione","europea","ue")))

(istruzione <- frequency %>% filter(word %in% c("istruzione","scuola")))

(terrorismo <- frequency %>% filter(word %in% c("terrorismo")))

(famiglia <- frequency %>% filter(word %in% c("famiglia")))


words_by_time <- tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%c("lavoro","avanti","scuola","famiglia","governo","elezioni","salvini","maio"))

parolepiùusatemensili <- tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  top_n(1) %>% 
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) 

parolepiùusatemensili


nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

p <- words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "matteorenzi") %>%
  ggplot(aes(time_floor, count/time_total, color=word)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency") 

p
library(plotly)

ggplotly(p) %>% highlight("plotly_selected")


(PD <- frequency %>% filter(word %in% c("pd")))
(M5s <- frequency %>% filter(word %in% c("stelle")))


########Grillo#############
Grillofinale <- read_csv("Grillofinaledef.csv")



Grillo <- Grillofinale %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

remove_reg <- "&amp;|&lt;|&gt;"

tweets <-  Grillo %>% 
  filter(!str_detect(text, "^RT"))
tweets$text
c <- tweets %>% mutate(text = str_remove_all(text, remove_reg))
c$text

c <- c %>%  unnest_tokens(word, text, token = "tweets")
c$word

c <- c %>% filter(!word %in% stopwords$words,
                  !word %in% str_remove_all(stopwords$words, "'"),
                  str_detect(word, "[a-z]"))
c$word

c <- c %>% 
  mutate(word=str_remove(word,"#"))


frequency <- c %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(c %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency


(immigrazione <- frequency %>% filter(word %in% c("immigrazione","immigrati","immigrato")))

(lavoro <- frequency %>% filter(word %in% c("lavoro")))

(UE <- frequency %>% filter(word %in% c("unione","europea","ue")))

(istruzione <- frequency %>% filter(word %in% c("istruzione","scuola")))

(terrorismo <- frequency %>% filter(word %in% c("terrorismo")))

(famiglia <- frequency %>% filter(word %in% c("famiglia")))



words_by_time <- c %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%c("lavoro","immigrati","europea","scuola","terrorismo","famiglia","governo","elezioni","salvini","renzi"))


parolepiùusatemensili <- c %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  top_n(1) %>% 
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) 



nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

p <- words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "beppe_grillo") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = "Word frequency")


library(plotly)

ggplotly(p) %>% highlight("plotly_selected")


(PD <- frequency %>% filter(word %in% c("pd")))
(M5s <- frequency %>% filter(word %in% c("stelle")))


Grassotweets <- read_csv("Grassotweetsdef.csv")

Melonitweets <- read_csv("Melonitweetsdef.csv")

Martinatweets <- read_csv("Martinatweetsdef.csv")

Grillo <- read_csv("Grillofinaledef.csv")



Grasso <- Grassotweets %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

Meloni <- Melonitweets %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))

Martina <- Martinatweets %>%
  filter(created_at >= as.Date("2016-12-31"),created_at <= as.Date("2019-01-01"))
Grasso$quoted_created_at %>% str()
Salvini$quoted_created_at %>% str()
Grillo$quoted_created_at %>% str()
Grillo$quoted_created_at <- Grillo$quoted_created_at %>% as.Date()
Grillo$quoted_created_at <- Grillo$quoted_created_at %>% as.POSIXct()
Grillo[1,] %>% str()
Salvini[1,] %>% str()
Grillo$quoted_favorite_count <- Grillo$quoted_favorite_count %>% as.integer()

Grillo$quoted_retweet_count <- Grillo$quoted_retweet_count %>% as.integer()

Grillo$quoted_followers_count <- Grillo$quoted_followers_count %>% as.integer()

Grillo$quoted_friends_count <- Grillo$quoted_friends_count %>% as.integer()

Grillo$quoted_statuses_count <- Grillo$quoted_statuses_count %>% as.integer()

Grillo$quoted_verified <- Grillo$quoted_verified %>% as.logical()

#tweets di tutti i politici
tweets <- bind_rows(DiMaio %>% 
                      mutate(person = "DiMaio"),
                    Berlusconi %>% 
                      mutate(person = "Berlusconi"),
                    Grasso %>% 
                      mutate(person = "Grasso"),
                    Grillo %>% 
                      mutate(person = "Grillo"),
                    Martina %>% 
                      mutate(person = "Martina"),
                    Meloni %>% 
                      mutate(person = "Meloni"),
                    Renzi %>% 
                      mutate(person = "Renzi"),
                    Salvini %>% 
                      mutate(person = "Salvini"))

#numero di tweet mensili
ggplot(tweets, aes(x = created_at, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)



remove_reg <- "&amp;|&lt;|&gt;"

tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         !word %in% str_remove_all(stopwords$words, "'"),
         str_detect(word, "[a-z]"))


frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(person) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

frequency[order(frequency$freq,decreasing = T),]

###Lavoro###
parola = "lavoro"
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in% parola)



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes


p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = person,text=person)) +
  geom_line(size = 0.3) +
  labs(x = NULL, y = "Word frequency")

gg <- ggplotly(p,tooltip = "text")

gg <- gg %>% 
  layout(
         annotations =   list(
                              list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                                   x = 0, y = 1.08, showarrow = F,
                                   text = paste("<b>Uso della parola",parola,"</b>"),
                                   font = list(size = 30, family = "Balto"))))
#grafico con immagini
gg %>% htmlwidgets::onRender("
    function(el, x) {
      // when hovering over an element, do something
      el.on('plotly_hover', function(d) {

        // extract tooltip text
        txt = d.points[0].data.text;
        // image is stored locally
        image_location = 'https://raw.githubusercontent.com/AOngaro3/ProgettoSL/master/Immagini/' + txt    + '.jpg';

        // define image to be shown
        var img = {
          // location of image
          source: image_location,
          // top-left corner
          x: 0,
          y: 1,
          sizex: 0.2,
          sizey: 0.2,
          xref: 'paper',
          yref: 'paper'
        };

        // show image and annotation 
        Plotly.relayout(el.id, {
            images: [img] 
        });
      })
    }
    ")



#GIF per utilizzo parola

  
 words_by_time %>%
      inner_join(slopes, by = c("word", "person")) %>%
      ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = 'Year: {floor_date(frame_time, unit= "1 month")}', x = 'person', y = 'Uso della parola') +
  transition_time(time_floor) +
  ease_aes('linear')


words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = paste('Parola',parola,'nel mese di: {closest_state}'), x = 'Politici', y = 'Uso della parola') +
  theme(plot.title = element_text(hjust = 1, size = 22))+
  transition_states(time_floor,transition_length = 3, state_length =1 ) 
+ease_aes('linear')



p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person,frame=time_floor)) +
  geom_bar(stat='identity',position = "count")

gg <- ggplotly(p)
gg

###Governo###

parola = "governo"

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in% parola)



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes


p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = person)) +
  geom_line(size = 0.3) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p) %>%  layout(
  annotations =   list(
    list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
         x = 0, y = 1.08, showarrow = F,
         text = paste("<b>Uso della parola",parola,"</b>"),
         font = list(size = 30, family = "Balto"))))


words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = 'Month: {closest_state}', x = 'Politici', y = 'Uso della parola') +
  theme(plot.title = element_text(hjust = 1, size = 22))+
  transition_states(time_floor,transition_length = 5, state_length = 5) 
+ease_aes('linear')

###immigrazione####
parola= "immigrati"
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in% parola)



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes


p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = person)) +
  geom_line(size = 0.3) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p) %>% layout(
  annotations =   list(
    list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
         x = 0, y = 1.08, showarrow = F,
         text = paste("<b>Uso della parola",parola,"</b>"),
         font = list(size = 30, family = "Balto"))))

words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = 'Month: {closest_state}', x = 'Politici', y = 'Uso della parola') +
  theme(plot.title = element_text(hjust = 1, size = 22))+
  transition_states(time_floor,transition_length = 5, state_length = 5) 
+ease_aes('linear')


#famiglia
parola = "famiglia"
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%parola)



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes


p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = person)) +
  geom_line(size = 0.3) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p) %>% layout(
  annotations =   list(
    list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
         x = 0, y = 1.08, showarrow = F,
         text = paste("<b>Uso della parola",parola,"</b>"),
         font = list(size = 30, family = "Balto"))))

words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = 'Month: {closest_state}', x = 'Politici', y = 'Uso della parola') +
  theme(plot.title = element_text(hjust = 1, size = 22))+
  transition_states(time_floor,transition_length = 5, state_length = 5) 
  +ease_aes('linear')


###europea###

parola <- "europea"

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word %in%parola)



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes


p <- words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = person)) +
  geom_line(size = 0.3) +
  labs(x = NULL, y = "Word frequency")

ggplotly(p) %>% layout(
  annotations =   list(
    list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
         x = 0, y = 1.08, showarrow = F,
         text = paste("<b>Uso della parola",parola,"</b>"),
         font = list(size = 30, family = "Balto"))))

words_by_time %>%
  inner_join(slopes, by = c("word", "person")) %>%
  ggplot(aes(x=person,y=count/time_total,fill=person)) +
  geom_bar(stat='identity') +
  labs(title = 'Month: {closest_state}', x = 'Politici', y = 'Uso della parola') +
  theme(plot.title = element_text(hjust = 1, size = 22))+
  transition_states(time_floor,transition_length = 5, state_length = 5) 
  +ease_aes('linear')