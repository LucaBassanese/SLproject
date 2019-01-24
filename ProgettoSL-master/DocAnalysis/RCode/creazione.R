library(stringr)
library(lubridate)
library(twitteR)
library(ROAuth)
library(httpuv)
library(rtweet)
library(devtools)
library(rlang)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(syuzhet)
library(fansi)
setwd("C:\\Users\\banab\\Desktop\\R\\Statistical Learning")
##############sentix###############
sentix <- read_delim("dizionari/sentix", 
                     "\t", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
colnames(sentix) <- c("Lemma", "POS", "Wordnet_synset_ID", "positive_score", "negative_score", "polarity", "intensity")

sentix$Lemma <- tolower(sentix$Lemma)
diz <- group_by(sentix, Lemma) %>% 
  summarise(positivi = mean(positive_score), negativi = mean(negative_score), ID = first(Wordnet_synset_ID), POS = first(POS))
intensita <- sqrt(diz$positivi^2 + diz$negativi^2)
theta <- atan(sentix$negative_score / sentix$positive_score )
polarity <- 1- 4* theta / pi
diz$intensita <- intensita

####creo verbi########

are <- c("o", "i", "a","iamo","ate", "ano", "avo", "avi", "ava", "avamo", "avate", 
         "avano", "ai", "asti", "ò", "ammo", "aste","arono", "erò", "erai", "erà",
         "eremo", "erete", "eranno", "iate", "ino", "assi", "asse", "assimo", 
         "assero","erei",  "eresti", "erebbe", "eremmo", "ereste", "erebbero", "ando", 
         "ante", "ato")

ere <- c("o","i", "e", "iamo","ete", "ono", "evo", "evi", "eva", "evamo",
         "evate", "evano", "etti", "ei", "esti", "ette", "é", "emmo", "este",
         "ettero", "erono", "erò", "erai", "erà", "eremo", "erete", "eranno",
         "a", "iate", "ano", "essi",  "esse", "essimo",  "essero", "erei", "eresti",
         "erebbe", "eremmo", "ereste", "erebbero", "endo", "ente", "uto"
)


ire <- c("o", "i", "e", "iamo", "ite", "ono", "ivo", "ivi", "iva", "ivamo", "ivate", "ivano", "ii", "isti", "ì",
         "immo", "iste", "irono", "irò", "irai", "irà", "iremo", "irete", "iranno", "a", "iate", "ano",
         "issi", "isse", "issimo",  "issero", "irei", "iresti", "irebbe", "iremmo", "ireste",
         "irebbero", "endo", "ito", "ente"
         
)

verbi <- (diz %>% filter(POS == "v"))$Lemma
infinitoare <- verbi[str_detect(verbi, "are$")]
infinitoere <- verbi[str_detect(verbi, "ere$")]
infinitoire <- verbi[str_detect(verbi, "ire$")]


radiciare <- str_split_fixed(infinitoare, "are", 2)[,1] 
radiciere <- str_split_fixed(infinitoere, "ere", 2)[,1] 
radiciire <- str_split_fixed(infinitoire, "ire", 2)[,1] 

dec1 <- c(outer(radiciare, are, paste0)) %>% 
  sort()
dec2 <- c(outer(radiciere, ere, paste0)) %>% 
  sort()
dec3 <- c(outer(radiciire, ire, paste0)) %>% 
  sort()
declinati <- c(dec1, dec2, dec3) %>% sort()


a <- diz[diz$Lemma %in% infinitoare,] 
e <- diz[diz$Lemma %in% infinitoere, ]
i <- diz[diz$Lemma %in% infinitoire, ]

I <- i 
for (p in 2:40) {
  I <- rbind(I, i)
}

A <- a
for (i in 2:39){
  A <- rbind(A, a)
}

E <- e 
for (i in 2:43){
  E <- rbind(E, e)
}


A <- A[order(A$Lemma), ]
E <- E[order(E$Lemma), ]
I <- I[order(I$Lemma),] 

A$Lemma <- dec1
E$Lemma <- dec2
I$Lemma <- dec3


diz <- rbind(diz, A, E, I)

conta <- diz$Lemma %>% table()
target <- conta[conta>1]
target %>% length()
caz <- target %>% names()
dizio <- diz %>% filter ( !(Lemma %in% caz & POS == "v"))



diz <- dizio %>% tbl_df()

##########nomi##########
#a, o, e -->  i
#a -> e
nomi <- (diz %>% filter(POS == "n"))$Lemma
adj <- (diz %>% filter(POS == "a" ))$Lemma

a <- nomi[str_detect(nomi, "a$")]
o <- nomi[str_detect(nomi, "o$")]
e <- nomi[str_detect(nomi, "e$")]


radicio <- str_split_fixed( o , "o$", 2)[,1] 
radicie <- str_split_fixed( e , "e$", 2)[,1] 
radicia <- str_split_fixed( a , "a$", 2)[,1]

deco <- paste0(radicio, "i")
dece <- paste0(radicie, "i")
dec1 <- paste0(radicia, "i")
dec2<- paste0(radicia, "e")


O <- diz[diz$Lemma %in% o, ]
E <- diz[diz$Lemma %in% e, ]
A1 <- diz[diz$Lemma %in% a, ]
A2 <- diz[diz$Lemma %in% a, ]

O$Lemma <- deco
E$Lemma <- dece
A1$Lemma <- dec1
A2$Lemma <- dec2

plurali <- rbind(O, E, A1, A2) 
plurali <- group_by(plurali, Lemma) %>% 
  summarise(positivi = mean(positivi), negativi = mean(negativi), ID = first(ID), POS = first(POS), intensita = mean(intensita))



plurali$Lemma %in% diz$Lemma  %>% sum( )
plurali <- plurali[plurali$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, plurali)


########aggettivi#########
#e-> i
#o -> i, a, e
#a -> i, e

a <- adj[str_detect(adj, "a$")]
o <- adj[str_detect(adj, "o$")]
e <- adj[str_detect(adj, "e$")]

radicio <- str_split_fixed( o , "o$", 2)[,1] 
radicie <- str_split_fixed( e , "e$", 2)[,1] 
radicia <- str_split_fixed( a , "a$", 2)[,1]

deco1 <- paste0(radicio, "i")
deco2 <- paste0(radicio, "a")
deco3 <- paste0(radicio, "e")
dece <- paste0(radicie, "i")
deca1 <- paste0(radicia, "i")
deca2<- paste0(radicia, "e")


O1 <- O2 <- O3 <-diz[diz$Lemma %in% o, ]
E <- diz[diz$Lemma %in% e, ]
A1 <- diz[diz$Lemma %in% a, ]
A2 <- diz[diz$Lemma %in% a, ]

O1$Lemma <- deco1
O2$Lemma <- deco2
O3$Lemma <- deco3
E$Lemma <- dece
A1$Lemma <- deca1
A2$Lemma <- deca2

aggettivi <- rbind(O1, O2, O3, E, A1, A2) 
aggettivi <- group_by(aggettivi, Lemma) %>% 
  summarise(positivi = mean(positivi), negativi = mean(negativi), ID = first(ID), POS = first(POS), intensita = mean(intensita))



aggettivi$Lemma %in% diz$Lemma  %>% sum( )
aggettivi <- aggettivi[aggettivi$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, aggettivi)
diz <- group_by(diz, Lemma) %>% 
  summarise(positivi = mean(positivi), negativi = mean(negativi), ID = first(ID), POS = first(POS), intensita = mean(intensita))
theta <- atan(diz$negativi/ diz$positivi )
polarity <- 1- 4* theta / pi
diz$polarity <- polarity
write.csv(diz, "dizsent.csv")


################nrc#########
diz <- get_sentiment_dictionary("nrc", "italian")
diz$sentiment %>% unique()
positivi <- ifelse (diz$sentiment=="positive", 1, 0)
negativi <- ifelse (diz$sentiment=="negative", 1, 0)
anger <- ifelse (diz$sentiment=="anger", 1, 0)
anticipation <- ifelse (diz$sentiment=="anticipation", 1, 0)
disgust <- ifelse (diz$sentiment=="disgust", 1, 0)
fear <- ifelse (diz$sentiment=="fear", 1, 0)
joy <- ifelse (diz$sentiment=="joy", 1, 0)
sadness <- ifelse (diz$sentiment=="sadness", 1, 0)
surprise <- ifelse (diz$sentiment=="surprise", 1, 0)
trust <- ifelse (diz$sentiment=="trust", 1, 0)


diz$positivi <- positivi
diz$negativi <- negativi
diz$anger <- anger
diz$anticipation <- anticipation
diz$disgust <- disgust
diz$fear <- fear
diz$joy <- joy
diz$sadness <- sadness
diz$surprise <- surprise
diz$trust <- trust

diz$word <- tolower(diz$word)
diz <- diz %>% group_by(word) %>% 
  summarise(positivi = sum(positivi), negativi = sum(negativi), anger= sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
            fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), sentiment= paste0(sentiment, collapse = " & "))

nomi <- names(diz)
nomi[1] <- "Lemma"
names(diz) <- nomi
diz <- diz[-4454, ]
############nomi e aggettivi#######
adj <- diz$Lemma
a <- adj[str_detect(adj, "a$")]
o <- adj[str_detect(adj, "o$")]
e <- adj[str_detect(adj, "e$")]

radicio <- str_split_fixed( o , "o$", 2)[,1] 
radicie <- str_split_fixed( e , "e$", 2)[,1] 
radicia <- str_split_fixed( a , "a$", 2)[,1]

deco1 <- paste0(radicio, "i")
deco2 <- paste0(radicio, "a")
deco3 <- paste0(radicio, "e")
dece <- paste0(radicie, "i")
deca1 <- paste0(radicia, "i")
deca2<- paste0(radicia, "e")


O1 <- O2 <- O3 <-diz[diz$Lemma %in% o, ]
E <- diz[diz$Lemma %in% e, ]
A1 <- diz[diz$Lemma %in% a, ]
A2 <- diz[diz$Lemma %in% a, ]

O1$Lemma <- deco1
O2$Lemma <- deco2
O3$Lemma <- deco3
E$Lemma <- dece
A1$Lemma <- deca1
A2$Lemma <- deca2

aggettivi <- rbind(O1, O2, O3, E, A1, A2) 
aggettivi <- group_by(aggettivi, Lemma) %>% 
  summarise(positivi = first(positivi), negativi = first(negativi), anger= first(anger), anticipation = first(anticipation), disgust = first(disgust),
            fear = first(fear), joy = first(joy), sadness = first(sadness), surprise = first(surprise), trust = first(trust), sentiment= first(sentiment))



aggettivi$Lemma %in% diz$Lemma  %>% sum( )
aggettivi <- aggettivi[aggettivi$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, aggettivi)
diz$Lemma %>% unique() %>% length ()


#########verbi########

are <- c("o", "i", "a","iamo","ate", "ano", "avo", "avi", "ava", "avamo", "avate", 
         "avano", "ai", "asti", "ò", "ammo", "aste","arono", "erò", "erai", "erà",
         "eremo", "erete", "eranno", "iate", "ino", "assi", "asse", "assimo", 
         "assero","erei",  "eresti", "erebbe", "eremmo", "ereste", "erebbero", "ando", 
         "ante", "ato")

ere <- c("o","i", "e", "iamo","ete", "ono", "evo", "evi", "eva", "evamo",
         "evate", "evano", "etti", "ei", "esti", "ette", "é", "emmo", "este",
         "ettero", "erono", "erò", "erai", "erà", "eremo", "erete", "eranno",
         "a", "iate", "ano", "essi",  "esse", "essimo",  "essero", "erei", "eresti",
         "erebbe", "eremmo", "ereste", "erebbero", "endo", "ente", "uto"
)


ire <- c("o", "i", "e", "iamo", "ite", "ono", "ivo", "ivi", "iva", "ivamo", "ivate", "ivano", "ii", "isti", "ì",
         "immo", "iste", "irono", "irò", "irai", "irà", "iremo", "irete", "iranno", "a", "iate", "ano",
         "issi", "isse", "issimo",  "issero", "irei", "iresti", "irebbe", "iremmo", "ireste",
         "irebbero", "endo", "ito", "ente"
         
)

verbi <- diz$Lemma
infinitoare <- verbi[str_detect(verbi, "are$")]
infinitoere <- verbi[str_detect(verbi, "ere$")]
infinitoire <- verbi[str_detect(verbi, "ire$")]


radiciare <- str_split_fixed(infinitoare, "are$", 2)[,1] 
radiciere <- str_split_fixed(infinitoere, "ere$", 2)[,1] 
radiciire <- str_split_fixed(infinitoire, "ire$", 2)[,1] 

dec1 <- c(outer(radiciare, are, paste0)) %>% 
  sort()
dec2 <- c(outer(radiciere, ere, paste0)) %>% 
  sort()
dec3 <- c(outer(radiciire, ire, paste0)) %>% 
  sort()
declinati <- c(dec1, dec2, dec3) %>% sort()


a <- diz[diz$Lemma %in% infinitoare,] 
e <- diz[diz$Lemma %in% infinitoere, ]
i <- diz[diz$Lemma %in% infinitoire, ]

I <- i 
for (p in 2:40) {
  I <- rbind(I, i)
}

A <- a
for (i in 2:39){
  A <- rbind(A, a)
}

E <- e 
for (i in 2:43){
  E <- rbind(E, e)
}


A <- A[order(A$Lemma), ]
E <- E[order(E$Lemma), ]
I <- I[order(I$Lemma),] 

A$Lemma <- dec1
E$Lemma <- dec2
I$Lemma <- dec3

verbi <- rbind(A, E, I)
verbi <- group_by(verbi, Lemma) %>% 
  summarise(positivi = first(positivi), negativi = first(negativi), anger= first(anger), anticipation = first(anticipation), disgust = first(disgust),
            fear = first(fear), joy = first(joy), sadness = first(sadness), surprise = first(surprise), trust = first(trust), sentiment= first(sentiment))

verbi$Lemma %in% diz$Lemma  %>% sum( )
verbi <- verbi[verbi$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, verbi)

diz$Lemma %>% unique() %>% length()
write.csv(diz, "diznrc.csv")


#########xml#######
diz <- read_csv("convertcsv.csv")
diz <- diz[, -c(5,8)]
names(diz) <- c("Lemma", "score", "metodo", "polarity", "ID", "POS" )
diz <- group_by(diz, Lemma) %>% 
  summarise(score = mean(score), metodo = first(metodo), polarity = first(polarity), ID = first(ID), POS = first(POS))


###########Verbi##########
are <- c("o", "i", "a","iamo","ate", "ano", "avo", "avi", "ava", "avamo", "avate", 
         "avano", "ai", "asti", "ò", "ammo", "aste","arono", "erò", "erai", "erà",
         "eremo", "erete", "eranno", "iate", "ino", "assi", "asse", "assimo", 
         "assero","erei",  "eresti", "erebbe", "eremmo", "ereste", "erebbero", "ando", 
         "ante", "ato")

ere <- c("o","i", "e", "iamo","ete", "ono", "evo", "evi", "eva", "evamo",
         "evate", "evano", "etti", "ei", "esti", "ette", "é", "emmo", "este",
         "ettero", "erono", "erò", "erai", "erà", "eremo", "erete", "eranno",
         "a", "iate", "ano", "essi",  "esse", "essimo",  "essero", "erei", "eresti",
         "erebbe", "eremmo", "ereste", "erebbero", "endo", "ente", "uto"
)


ire <- c("o", "i", "e", "iamo", "ite", "ono", "ivo", "ivi", "iva", "ivamo", "ivate", "ivano", "ii", "isti", "ì",
         "immo", "iste", "irono", "irò", "irai", "irà", "iremo", "irete", "iranno", "a", "iate", "ano",
         "issi", "isse", "issimo",  "issero", "irei", "iresti", "irebbe", "iremmo", "ireste",
         "irebbero", "endo", "ito", "ente"
         
)

verbi <- (diz %>% filter(POS == "verb"))$Lemma
infinitoare <- verbi[str_detect(verbi, "are$")]
infinitoere <- verbi[str_detect(verbi, "ere$")]
infinitoire <- verbi[str_detect(verbi, "ire$")]


radiciare <- str_split_fixed(infinitoare, "are$", 2)[,1] 
radiciere <- str_split_fixed(infinitoere, "ere$", 2)[,1] 
radiciire <- str_split_fixed(infinitoire, "ire$", 2)[,1] 

dec1 <- c(outer(radiciare, are, paste0)) %>% 
  sort()
dec2 <- c(outer(radiciere, ere, paste0)) %>% 
  sort()
dec3 <- c(outer(radiciire, ire, paste0)) %>% 
  sort()
declinati <- c(dec1, dec2, dec3) %>% sort()


a <- diz[diz$Lemma %in% infinitoare,] 
e <- diz[diz$Lemma %in% infinitoere, ]
i <- diz[diz$Lemma %in% infinitoire, ]

I <- i 
for (p in 2:40) {
  I <- rbind(I, i)
}

A <- a
for (i in 2:39){
  A <- rbind(A, a)
}

E <- e 
for (i in 2:43){
  E <- rbind(E, e)
}


A <- A[order(A$Lemma), ]
E <- E[order(E$Lemma), ]
I <- I[order(I$Lemma),] 

A$Lemma <- dec1
E$Lemma <- dec2
I$Lemma <- dec3


diz <- rbind(diz, A, E, I)

conta <- diz$Lemma %>% table()
target <- conta[conta>1]
target %>% length()
caz <- target %>% names()
diz <- diz %>% filter ( !(Lemma %in% caz & POS == "verb"))
diz$Lemma %>% unique() %>% length()

########plurali#######
nomi <- (diz %>% filter(POS == "noun" ))$Lemma
adj <- (diz %>% filter(POS == "adj" ))$Lemma

#nomi
a <- nomi[str_detect(nomi, "a$")]
o <- nomi[str_detect(nomi, "o$")]
e <- nomi[str_detect(nomi, "e$")]


radicio <- str_split_fixed( o , "o$", 2)[,1] 
radicie <- str_split_fixed( e , "e$", 2)[,1] 
radicia <- str_split_fixed( a , "a$", 2)[,1]

deco <- paste0(radicio, "i")
dece <- paste0(radicie, "i")
dec1 <- paste0(radicia, "i")
dec2<- paste0(radicia, "e")


O <- diz[diz$Lemma %in% o, ]
E <- diz[diz$Lemma %in% e, ]
A1 <- diz[diz$Lemma %in% a, ]
A2 <- diz[diz$Lemma %in% a, ]

O$Lemma <- deco
E$Lemma <- dece
A1$Lemma <- dec1
A2$Lemma <- dec2

plurali <- rbind(O, E, A1, A2) 
plurali <- group_by(plurali, Lemma) %>% 
  summarise(score = mean(score), metodo = first(metodo), polarity = first(polarity), ID = first(ID), POS = first(POS))



plurali$Lemma %in% diz$Lemma  %>% sum( )
plurali <- plurali[plurali$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, plurali)
diz$Lemma %>% unique() %>% length ()

###########aggettivi#########

#e-> i
#o -> i, a, e
#a -> i, e

a <- adj[str_detect(adj, "a$")]
o <- adj[str_detect(adj, "o$")]
e <- adj[str_detect(adj, "e$")]

radicio <- str_split_fixed( o , "o$", 2)[,1] 
radicie <- str_split_fixed( e , "e$", 2)[,1] 
radicia <- str_split_fixed( a , "a$", 2)[,1]

deco1 <- paste0(radicio, "i")
deco2 <- paste0(radicio, "a")
deco3 <- paste0(radicio, "e")
dece <- paste0(radicie, "i")
deca1 <- paste0(radicia, "i")
deca2<- paste0(radicia, "e")


O1 <- O2 <- O3 <-diz[diz$Lemma %in% o, ]
E <- diz[diz$Lemma %in% e, ]
A1 <- diz[diz$Lemma %in% a, ]
A2 <- diz[diz$Lemma %in% a, ]

O1$Lemma <- deco1
O2$Lemma <- deco2
O3$Lemma <- deco3
E$Lemma <- dece
A1$Lemma <- deca1
A2$Lemma <- deca2

aggettivi <- rbind(O1, O2, O3, E, A1, A2) 
aggettivi <- group_by(aggettivi, Lemma) %>% 
  summarise(score = mean(score), metodo = first(metodo), polarity = first(polarity), ID = first(ID), POS = first(POS))



aggettivi$Lemma %in% diz$Lemma  %>% sum( )
aggettivi <- aggettivi[aggettivi$Lemma %in% diz$Lemma == FALSE, ]
diz <- rbind(diz, aggettivi)
diz$Lemma %>% unique() %>% length ()
write.csv(diz, "dizxml.csv")
