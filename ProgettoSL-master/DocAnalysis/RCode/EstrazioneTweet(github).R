library(rtweet)
library(twitteR)
library(devtools)
library("httr")
library("base64enc")
#setwd("D:/Universita/Statistical Learning/Progetto")
#######Estrazione dei dati#######
#personal data
token <- create_token(
  app = "...",
  consumer_key = "...",
  consumer_secret = "...",
  access_token = "...",
  access_secret = "...")

###renzi###
renzi <-  get_timeline("matteorenzi",n=3200) #18/12/2018 10:23
max(renzi$created_at) #"2016-04-05 16:12:10 UTC"

save_as_csv(x=renzi,file_name = "MRenzitweetsdef.csv")

###dimaio####
dimaio <- get_timeline("luigidimaio",n=3200) #18/12/2018 10:30

min(dimaio$created_at) #"2015-01-05 17:00:02 UTC"

save_as_csv(x=dimaio,file_name = "DiMaiotweetsdef.csv")

dimaio$text
####Grillo####

grillo <- get_timeline("beppe_grillo",n=200) #18/12/2018 10:35

min(grillo$created_at)


grillo$created_at
save_as_csv(x=grillo,file_name = "Grillotweets(rtweet).csv")

grillotutto <- read_csv("D:/Grillofinale.csv")
max(grillotutto$created_at)


x <- grillo %>% 
  filter(created_at >= "2018-12-18 10:00:42 UTC")

min(x$created_at)

x$created_at
max(grillotutto$created_at)

dataset <- rbind(grillotutto, x)

dataset <- dataset[order(dataset$created_at,decreasing=T),]

head(dataset$created_at)

save_as_csv(x=dataset,file_name = "Grillofinaledef.csv")


#usare altra funzione
####meloni####

meloni <- get_timeline("GiorgiaMeloni",n=3200) #18/12/2018 10:41

max(meloni$created_at) #"2016-03-07 18:41:50 UTC"
save_as_csv(x=meloni,file_name = "Melonitweetsdef.csv")

###berlusconi####

berlusconi <- get_timeline("berlusconi",n=3200) #???18/12/2018 10:54
berlusconi[2176,"text"]

max(berlusconi$created_at) #"2017-10-17 15:14:29 UTC" (data creazione profilo)

save_as_csv(x=berlusconi,file_name = "Berlusconitweetsdef.csv")

###Grasso###

grasso <- get_timeline("PietroGrasso",n=3200) #18/12/2018 10:57
grasso[1,"text"]

max(grasso$created_at) #"2013-01-08 16:46:21 UTC"

save_as_csv(x=grasso,file_name = "Grassotweetsdef.csv")


####Martina####

martina <- get_timeline("maumartina",n=3200) #18/12/2018 10:59
martina[1,"text"]

max(martina$created_at) #"2014-08-08 13:00:28 UTC"

save_as_csv(x=martina,file_name = "Martinatweetsdef.csv")


#salvo i dataset di Salvini e Grillo anche con Rtweet per sicurezza e 


salvini <- get_timeline("matteosalvinimi",n=500) #18/12/2018 11:05
library(readr)
salvinitutto <- read_csv("Salvinifinale2.csv")
head(salvinitutto$created_at)
x <- salvini %>% 
       filter(created_at >= "2018-12-18 10:17:53 UTC")

min(x$created_at)

x$created_at
max(salvinitutto$created_at)

min(salvini$created_at) #"2018-05-15 07:54:13 UTC"

dataset <- rbind(salvinitutto, x)

dataset <- dataset[order(dataset$created_at,decreasing=T),]

head(dataset$created_at)

save_as_csv(x=dataset,file_name = "Salvinifinaledef.csv")






#ora prendo i tweet di salvini e grillo per tutto l'arco con procedura manuale

#personal data
appname <- "..."
key <- "..."
secret <- "..."

# base64 encoding
kands <- paste(key, secret, sep=":")
base64kands <- base64encode(charToRaw(kands))
base64kandsb <- paste("Basic", base64kands, sep=" ")

# request bearer token
resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                 add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant_type=client_credentials")

# get bearer token
bearer <- content(resToken)
bearerToken <- bearer[["access_token"]]
bearerTokenb <- paste("Bearer", bearerToken, sep=" ")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201805140000 ,\"fromDate\": 201805070000}")

tweetsalvinisett1 <- content(resTweets2)

salviniris1 <- tweetsalvinisett1$results
salviniris1[[20]]$created_at

saveRDS(salviniris1,"settimana14_0507_05")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201805060000 ,\"fromDate\": 201804300000}")

tweetsalvinisett2 <- content(resTweets2)

salviniris2 <- tweetsalvinisett2$results
salviniris2[[30]]$created_at

saveRDS(salviniris2,"settimana06_0530_04")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201804290000 ,\"fromDate\": 201804220000}")

tweetsalvinisett3 <- content(resTweets2)

salviniris3 <- tweetsalvinisett3$results
salviniris3[[40]]$created_at

saveRDS(salviniris3,"settimana29_0422_04")

for (i in 1:100) {
  print(salviniris3[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201804210000 ,\"fromDate\": 201804140000}")

tweetsalvinisett4 <- content(resTweets2)

salviniris4 <- tweetsalvinisett4$results

saveRDS(salviniris4,"settimana21_0414_04")


for (i in 1:100) {
  print(salviniris4[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201804130000 ,\"fromDate\": 201804060000}")

tweetsalvinisett5 <- content(resTweets2)

salviniris5 <- tweetsalvinisett5$results

saveRDS(salviniris5,"settimana13_0406_04")


for (i in 1:100) {
  print(salviniris5[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201804050000 ,\"fromDate\": 201803300000}")

tweetsalvinisett6 <- content(resTweets2)

salviniris6 <- tweetsalvinisett6$results

saveRDS(salviniris6,"settimana05_0430_03")

for (i in 1:100) {
  print(salviniris6[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201803290000 ,\"fromDate\": 201803220000}")

tweetsalvinisett7 <- content(resTweets2)

salviniris7 <- tweetsalvinisett7$results

saveRDS(salviniris7,"settimana29_0322_03")


for (i in 1:100) {
  print(salviniris7[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201803210000 ,\"fromDate\": 201803140000}")

tweetsalvinisett7.5 <- content(resTweets2)

salviniris7.5 <- tweetsalvinisett7.5$results

saveRDS(salviniris7.5,"settimana21_0314_03")

for (i in 1:100) {
  print(salviniris7.5[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201803130000 ,\"fromDate\": 201803060000}")

tweetsalvinisett8 <- content(resTweets2)

salviniris8 <- tweetsalvinisett8$results

saveRDS(salviniris8,"settimana13_0306_03")

for (i in 1:100) {
  print(salviniris8[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201803050000 ,\"fromDate\": 201802270000}")

tweetsalvinisett9 <- content(resTweets2)

salviniris9 <- tweetsalvinisett9$results

saveRDS(salviniris9,"settimana05_0327_02")

for (i in 1:100) {
  print(salviniris9[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201802270000 ,\"fromDate\": 201802200000}")

tweetsalvinisett10 <- content(resTweets2)

salviniris10 <- tweetsalvinisett10$results

saveRDS(salviniris10,"settimana27_0222_02")


for (i in 1:100) {
  print(salviniris10[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201802210000 ,\"fromDate\": 201802140000}")

tweetsalvinisett11 <- content(resTweets2)

salviniris11 <- tweetsalvinisett11$results
saveRDS(salviniris11,"settimana21_0214_02")



for (i in 1:100) {
  print(salviniris11[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201802130000 ,\"fromDate\": 201802060000}")

tweetsalvinisett12 <- content(resTweets2)

salviniris12 <- tweetsalvinisett12$results

for (i in 1:100) {
  print(salviniris12[[i]]$created_at)
}

saveRDS(salviniris12,"settimana13_0206_02")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201802050000 ,\"fromDate\": 201801300000}")

tweetsalvinisett13 <- content(resTweets2)

salviniris13 <- tweetsalvinisett13$results

saveRDS(salviniris13,"settimana05_0230_01")


for (i in 1:100) {
  print(salviniris13[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201801290000 ,\"fromDate\": 201801220000}")

tweetsalvinisett14 <- content(resTweets2)

salviniris14 <- tweetsalvinisett14$results

for (i in 1:100) {
  print(salviniris14[[i]]$created_at)
}

saveRDS(salviniris14,"settimana29_0122_01")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201801210000 ,\"fromDate\": 201801140000}")

tweetsalvinisett15 <- content(resTweets2)

salviniris15 <- tweetsalvinisett15$results

for (i in 1:100) {
  print(salviniris15[[i]]$created_at)
}

saveRDS(salviniris15,"settimana21_0114_01")
#ciao <- readRDS("settimana21_0114_01")



resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201801130000 ,\"fromDate\": 201801060000}")

tweetsalvinisett16 <- content(resTweets2)

salviniris16 <- tweetsalvinisett16$results
saveRDS(salviniris16,"settimana13_0106_01")

for (i in 1:100) {
  print(salviniris16[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201801050000 ,\"fromDate\": 201712300000}")

tweetsalvinisett17 <- content(resTweets2)

salviniris17 <- tweetsalvinisett17$results

for (i in 1:100) {
  print(salviniris17[[i]]$created_at)
}

saveRDS(salviniris17,"settimana05_0130_12")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201712290000 ,\"fromDate\": 201712220000}")

tweetsalvinisett18 <- content(resTweets2)

salviniris18 <- tweetsalvinisett18$results

for (i in 1:100) {
  print(salviniris18[[i]]$created_at)
}

saveRDS(salviniris18,"settimana29_1222_12(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201712210000 ,\"fromDate\": 201712140000}")

tweetsalvinisett19 <- content(resTweets2)

salviniris19 <- tweetsalvinisett19$results

for (i in 1:100) {
  print(salviniris19[[i]]$created_at)
}

saveRDS(salviniris19,"settimana21_1214_12(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201712130000 ,\"fromDate\": 201712060000}")

tweetsalvinisett20 <- content(resTweets2)

salviniris20 <- tweetsalvinisett20$results

for (i in 1:100) {
  print(salviniris20[[i]]$created_at)
}

saveRDS(salviniris20,"settimana13_1206_12(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201712050000 ,\"fromDate\": 201711290000}")

tweetsalvinisett21 <- content(resTweets2)

salviniris21 <- tweetsalvinisett21$results

for (i in 1:100) {
  print(salviniris21[[i]]$created_at)
}

saveRDS(salviniris21,"settimana05_1229_11(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201711290000 ,\"fromDate\": 201711220000}")

tweetsalvinisett22 <- content(resTweets2)

salviniris22 <- tweetsalvinisett22$results

for (i in 1:100) {
  print(salviniris22[[i]]$created_at)
}

saveRDS(salviniris22,"settimana29_1122_11(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201711210000 ,\"fromDate\": 201711140000}")

tweetsalvinisett23 <- content(resTweets2)

salviniris23 <- tweetsalvinisett23$results

for (i in 1:100) {
  print(salviniris23[[i]]$created_at)
}

saveRDS(salviniris23,"settimana21_1114_11(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201711130000 ,\"fromDate\": 201711060000}")

tweetsalvinisett24 <- content(resTweets2)

salviniris24 <- tweetsalvinisett24$results

for (i in 1:100) {
  print(salviniris24[[i]]$created_at)
}

saveRDS(salviniris24,"settimana13_1106_11(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201711050000 ,\"fromDate\": 201710300000}")

tweetsalvinisett25 <- content(resTweets2)

salviniris25 <- tweetsalvinisett25$results

for (i in 1:100) {
  print(salviniris25[[i]]$created_at)
}

saveRDS(salviniris25,"settimana05_1130_10(2017)")



resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201710290000 ,\"fromDate\": 201710220000}")

tweetsalvinisett26 <- content(resTweets2)

salviniris26 <- tweetsalvinisett26$results

for (i in 1:100) {
  print(salviniris26[[i]]$created_at)
}

saveRDS(salviniris26,"settimana29_1022_10(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201710210000 ,\"fromDate\": 201710140000}")

tweetsalvinisett27 <- content(resTweets2)

salviniris27 <- tweetsalvinisett27$results

for (i in 1:100) {
  print(salviniris27[[i]]$created_at)
}

saveRDS(salviniris27,"settimana21_1014_10(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201710130000 ,\"fromDate\": 201710060000}")

tweetsalvinisett28 <- content(resTweets2)

salviniris28 <- tweetsalvinisett28$results

for (i in 1:100) {
  print(salviniris28[[i]]$created_at)
}

saveRDS(salviniris28,"settimana13_1006_10(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201710050000 ,\"fromDate\": 201709290000}")

tweetsalvinisett29 <- content(resTweets2)

salviniris29 <- tweetsalvinisett29$results

for (i in 1:100) {
  print(salviniris29[[i]]$created_at)
}

saveRDS(salviniris29,"settimana05_1029_09(2017)")




resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201709290000 ,\"fromDate\": 201709220000}")

tweetsalvinisett30 <- content(resTweets2)

salviniris30 <- tweetsalvinisett30$results

for (i in 1:100) {
  print(salviniris30[[i]]$created_at)
}

saveRDS(salviniris30,"settimana29_0922_09(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201709210000 ,\"fromDate\": 201709140000}")

tweetsalvinisett31 <- content(resTweets2)

salviniris31 <- tweetsalvinisett31$results

for (i in 1:100) {
  print(salviniris31[[i]]$created_at)
}

saveRDS(salviniris31,"settimana21_0914_09(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201709130000 ,\"fromDate\": 201709060000}")

tweetsalvinisett32 <- content(resTweets2)

salviniris32 <- tweetsalvinisett32$results

for (i in 1:100) {
  print(salviniris32[[i]]$created_at)
}

saveRDS(salviniris32,"settimana13_0906_09(2017)")



resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201709050000 ,\"fromDate\": 201708290000}")

tweetsalvinisett33 <- content(resTweets2)

salviniris33 <- tweetsalvinisett33$results

for (i in 1:100) {
  print(salviniris33[[i]]$created_at)
}

saveRDS(salviniris33,"settimana05_0929_08(2017)")




resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201708290000 ,\"fromDate\": 201708220000}")

tweetsalvinisett34 <- content(resTweets2)

salviniris34 <- tweetsalvinisett34$results

for (i in 1:100) {
  print(salviniris34[[i]]$created_at)
}

saveRDS(salviniris34,"settimana29_0822_08(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201708210000 ,\"fromDate\": 201708140000}")

tweetsalvinisett35 <- content(resTweets2)

salviniris35 <- tweetsalvinisett35$results

for (i in 1:100) {
  print(salviniris35[[i]]$created_at)
}

saveRDS(salviniris35,"settimana21_0814_082017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201708130000 ,\"fromDate\": 201708060000}")

tweetsalvinisett36 <- content(resTweets2)

salviniris36 <- tweetsalvinisett36$results

for (i in 1:100) {
  print(salviniris36[[i]]$created_at)
}

saveRDS(salviniris36,"settimana13_0806_08(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201708050000 ,\"fromDate\": 201707290000}")

tweetsalvinisett37 <- content(resTweets2)

salviniris37<- tweetsalvinisett37$results

for (i in 1:100) {
  print(salviniris37[[i]]$created_at)
}

saveRDS(salviniris37,"settimana05_0829_07(2017)")




resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201707290000 ,\"fromDate\": 201707220000}")

tweetsalvinisett38 <- content(resTweets2)

salviniris38 <- tweetsalvinisett38$results

for (i in 1:100) {
  print(salviniris38[[i]]$created_at)
}

saveRDS(salviniris38,"settimana29_0722_07(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/researchproj.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201707210000 ,\"fromDate\": 201707140000}")

tweetsalvinisett39 <- content(resTweets2)

salviniris39 <- tweetsalvinisett39$results

for (i in 1:100) {
  print(salviniris39[[i]]$created_at)
}

saveRDS(salviniris39,"settimana21_0714_07(2017)")

#personal data
appname <- "..."
key <- "..."
secret <- "..."

# base64 encoding
kands <- paste(key, secret, sep=":")
base64kands <- base64encode(charToRaw(kands))
base64kandsb <- paste("Basic", base64kands, sep=" ")

# request bearer token
resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                 add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant_type=client_credentials")

# get bearer token
bearer <- content(resToken)
bearerToken <- bearer[["access_token"]]
bearerTokenb <- paste("Bearer", bearerToken, sep=" ")



resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201707130000 ,\"fromDate\": 201707060000}")

tweetsalvinisett40 <- content(resTweets2)

salviniris40 <- tweetsalvinisett40$results

for (i in 1:100) {
  print(salviniris40[[i]]$created_at)
}

saveRDS(salviniris40,"settimana13_0706_07(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201707050000 ,\"fromDate\": 201706290000}")

tweetsalvinisett41 <- content(resTweets2)

salviniris41<- tweetsalvinisett41$results

for (i in 1:100) {
  print(salviniris41[[i]]$created_at)
}

saveRDS(salviniris41,"settimana05_0729_06(2017)")




resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201706290000 ,\"fromDate\": 201706220000}")

tweetsalvinisett42 <- content(resTweets2)

salviniris42 <- tweetsalvinisett42$results

for (i in 1:100) {
  print(salviniris42[[i]]$created_at)
}

saveRDS(salviniris42,"settimana29_0622_06(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201706210000 ,\"fromDate\": 201706140000}")

tweetsalvinisett43 <- content(resTweets2)

salviniris43 <- tweetsalvinisett43$results

for (i in 1:100) {
  print(salviniris43[[i]]$created_at)
}

saveRDS(salviniris43,"settimana21_0614_06(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201706130000 ,\"fromDate\": 201706060000}")

tweetsalvinisett44 <- content(resTweets2)

salviniris44 <- tweetsalvinisett44$results

for (i in 1:100) {
  print(salviniris44[[i]]$created_at)
}

saveRDS(salviniris44,"settimana13_0606_06(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201706050000 ,\"fromDate\": 201705290000}")

tweetsalvinisett45 <- content(resTweets2)

salviniris45<- tweetsalvinisett45$results



for (i in 1:100) {
  print(salviniris45[[i]]$created_at)
}

saveRDS(salviniris45,"settimana05_0629_05(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201705290000 ,\"fromDate\": 201705220000}")

tweetsalvinisett46 <- content(resTweets2)

salviniris46<- tweetsalvinisett46$results



for (i in 1:100) {
  print(salviniris46[[i]]$created_at)
}

saveRDS(salviniris46,"settimana29_0522_05(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201705210000 ,\"fromDate\": 201705140000}")

tweetsalvinisett47 <- content(resTweets2)

salviniris47<- tweetsalvinisett47$results



for (i in 1:100) {
  print(salviniris47[[i]]$created_at)
}

saveRDS(salviniris47,"settimana29_0522_05(2017)")





resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201705140000 ,\"fromDate\": 201705070000}")

tweetsalvinisett48 <- content(resTweets2)

salviniris48 <- tweetsalvinisett48$results
for (i in 1:100) {
  print(salviniris48[[i]]$created_at)
}

saveRDS(salviniris48,"settimana14_0507_05(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201705060000 ,\"fromDate\": 201704300000}")

tweetsalvinisett48.5 <- content(resTweets2)

salviniris48.5 <- tweetsalvinisett48.5$results

for (i in 1:100) {
  print(salviniris48.5[[i]]$created_at)
}


saveRDS(salviniris48.5,"settimana06_0530_04(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201704290000 ,\"fromDate\": 201704220000}")

tweetsalvinisett49 <- content(resTweets2)

salviniris49 <- tweetsalvinisett49$results

saveRDS(salviniris49,"settimana29_0422_04(2017)")

for (i in 1:100) {
  print(salviniris49[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201704210000 ,\"fromDate\": 201704140000}")

tweetsalvinisett50 <- content(resTweets2)

salviniris50 <- tweetsalvinisett50$results

saveRDS(salviniris50,"settimana21_0414_04(2017)")


for (i in 1:100) {
  print(salviniris50[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201704130000 ,\"fromDate\": 201704060000}")

tweetsalvinisett51 <- content(resTweets2)

salviniris51 <- tweetsalvinisett51$results

saveRDS(salviniris51,"settimana13_0406_04(2017)")


for (i in 1:100) {
  print(salviniris5[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201704050000 ,\"fromDate\": 201703300000}")

tweetsalvinisett52 <- content(resTweets2)

salviniris52 <- tweetsalvinisett52$results

saveRDS(salviniris52,"settimana05_0430_03(2017)")

for (i in 1:100) {
  print(salviniris52[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201703290000 ,\"fromDate\": 201703220000}")

tweetsalvinisett53 <- content(resTweets2)

salviniris53 <- tweetsalvinisett53$results

saveRDS(salviniris53,"settimana29_0322_03(2017)")


for (i in 1:100) {
  print(salviniris53[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201703210000 ,\"fromDate\": 201703140000}")

tweetsalvinisett54 <- content(resTweets2)

salviniris54 <- tweetsalvinisett54$results

saveRDS(salviniris54,"settimana21_0314_03(2017)")

for (i in 1:100) {
  print(salviniris54[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201703130000 ,\"fromDate\": 201703060000}")

tweetsalvinisett55 <- content(resTweets2)

salviniris55 <- tweetsalvinisett55$results

saveRDS(salviniris55,"settimana13_0306_03(2017)")

for (i in 1:100) {
  print(salviniris55[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201703050000 ,\"fromDate\": 201702270000}")

tweetsalvinisett56 <- content(resTweets2)

salviniris56 <- tweetsalvinisett56$results

saveRDS(salviniris56,"settimana05_0327_02(2017)")

for (i in 1:100) {
  print(salviniris56[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201702270000 ,\"fromDate\": 201702200000}")

tweetsalvinisett57 <- content(resTweets2)

salviniris57 <- tweetsalvinisett57$results

saveRDS(salviniris57,"settimana27_0222_02(2017)")


for (i in 1:100) {
  print(salviniris57[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201702210000 ,\"fromDate\": 201702140000}")

tweetsalvinisett58 <- content(resTweets2)

salviniris58 <- tweetsalvinisett58$results
saveRDS(salviniris58,"settimana21_0214_02(2017)")



for (i in 1:100) {
  print(salviniris58[[i]]$created_at)
}


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201702130000 ,\"fromDate\": 201702060000}")

tweetsalvinisett59 <- content(resTweets2)

salviniris59 <- tweetsalvinisett59$results

for (i in 1:100) {
  print(salviniris59[[i]]$created_at)
}

saveRDS(salviniris59,"settimana13_0206_02(2017)")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201702050000 ,\"fromDate\": 201701300000}")

tweetsalvinisett60 <- content(resTweets2)

salviniris60 <- tweetsalvinisett60$results

saveRDS(salviniris60,"settimana05_0230_01(2017)")


for (i in 1:100) {
  print(salviniris60[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201701290000 ,\"fromDate\": 201701220000}")

tweetsalvinisett61 <- content(resTweets2)

salviniris61 <- tweetsalvinisett61$results

for (i in 1:100) {
  print(salviniris61[[i]]$created_at)
}

saveRDS(salviniris61,"settimana29_0122_01(2017)")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201701210000 ,\"fromDate\": 201701140000}")

tweetsalvinisett62 <- content(resTweets2)

salviniris62 <- tweetsalvinisett62$results

for (i in 1:100) {
  print(salviniris62[[i]]$created_at)
}

saveRDS(salviniris62,"settimana21_0114_01")
#ciao <- readRDS("settimana21_0114_01(2017)")



resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201701130000 ,\"fromDate\": 201701060000}")

tweetsalvinisett63 <- content(resTweets2)

salviniris63 <- tweetsalvinisett63$results
saveRDS(salviniris63,"settimana13_0106_01(2017)")

for (i in 1:100) {
  print(salviniris63[[i]]$created_at)
}

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: matteosalvinimi\",\"maxResults\": 100,\"toDate\":201701060000 ,\"fromDate\": 201701010000}")

tweetsalvinisett64 <- content(resTweets2)

salviniris64 <- tweetsalvinisett64$results
saveRDS(salviniris64,"settimana06_0101_01(2017)")

for (i in 1:100) {
  print(salviniris64[[i]]$created_at)
}


#user name=name , screen_name = screen_name , is_quote_status= is_quote, retweeted= is_retweet, favorite_count, retweet_count,text, hashatags, user$entities$media$type= media_type
#d
salviniris1[[10]]$
salviniris10[[2]]$display_text_range
for (i in 1:64) {
for (k in 1:100) {
  print(paste("salviniris",i,sep="")[[k]]$retweeted)
  print(k)
}
  print(i)
}

MatteoSalvini <- data.frame("name"="", "screen_name"="","created_at"="","text"="","is_quote"="","is_retweet"="","favorite_count"=0,"retweet_count"=0,"hashtags"="","media_type"="",stringsAsFactors = F)
k=1
a <- length(salviniris1)

for (i in 1:a) {
  MatteoSalvini[k,"name"] <- as.character(salviniris1[[1]]$user$name)
  MatteoSalvini[k,"screen_name"] <-as.character(salviniris1[[i]]$user$screen_name)
  MatteoSalvini[k,"created_at"] <- as.character(salviniris1[[i]]$created_at)
  if (salviniris1[[i]]$truncated == T) {
    MatteoSalvini[k,"text"] <- as.character(salviniris1[[i]]$extended_tweet$full_text)
  } else  MatteoSalvini[k,"text"] <- as.character(salviniris1[[i]]$text)
  MatteoSalvini[k,"is_quote"] <- salviniris1[[i]]$is_quote_status
  MatteoSalvini[k,"is_retweet"] <- salviniris1[[i]]$retweeted
  MatteoSalvini[k,"favorite_count"] <- salviniris1[[i]]$favorite_count
  MatteoSalvini[k,"retweet_count"] <- salviniris1[[i]]$retweet_count
  MatteoSalvini[k,"hashtags"] <- ""
  numhash <- length(salviniris1[[i]]$entities$hashtags)
  if (numhash>0){
  for (j in 1:numhash) {
  MatteoSalvini[k,"hashtags"] <-paste(MatteoSalvini[k,"hashtags"],salviniris1[[i]]$entities$hashtags[[j]]$text)
  }
  }
  k=k+1
}
ciao1=5
eval(parse(text = paste("salviniris",1,sep="")))[[1]]$text
MatteoSalvini[4,"hashtags"] <- ""
numhash <- length(salviniris1[[4]]$entities$hashtags)
if (numhash>0){
  for (j in 1:numhash) {
    MatteoSalvini[4,"hashtags"] <-paste(MatteoSalvini[4,"hashtags"],salviniris1[[4]]$entities$hashtags[[j]]$text)
  }
}

salviniris10[[10]]$entities$media[[2]]$type
nomi <- c(1:64)
nomi <- c(nomi,7.5,48.5)
nomi <- sort(nomi)
k <- 1
for (t in nomi){
  a <- length(eval(parse(text = paste("salviniris",t,sep=""))))
for (i in 1:a) {
  MatteoSalvini[k,"name"] <- as.character(eval(parse(text = paste("salviniris",t,sep="")))[[1]]$user$name)
  MatteoSalvini[k,"screen_name"] <-as.character(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$user$screen_name)
  MatteoSalvini[k,"created_at"] <- as.character(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$created_at)
  if (eval(parse(text = paste("salviniris",t,sep="")))[[i]]$truncated == T) {
    MatteoSalvini[k,"text"] <- as.character(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$extended_tweet$full_text)
  } else  MatteoSalvini[k,"text"] <- as.character(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$text)
  MatteoSalvini[k,"is_quote"] <- eval(parse(text = paste("salviniris",t,sep="")))[[i]]$is_quote_status
  MatteoSalvini[k,"is_retweet"] <- eval(parse(text = paste("salviniris",t,sep="")))[[i]]$retweeted
  MatteoSalvini[k,"favorite_count"] <- eval(parse(text = paste("salviniris",t,sep="")))[[i]]$favorite_count
  MatteoSalvini[k,"retweet_count"] <- eval(parse(text = paste("salviniris",t,sep="")))[[i]]$retweet_count
  MatteoSalvini[k,"hashtags"] <- ""
  numhash <- length(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$entities$hashtags)
  if (numhash>0){
    for (j in 1:numhash) {
      MatteoSalvini[k,"hashtags"] <-paste(MatteoSalvini[k,"hashtags"],eval(parse(text = paste("salviniris",t,sep="")))[[i]]$entities$hashtags[[j]]$text)
    }
  }
  k=k+1
}
}
k <- 1
for(t in nomi){
  a <- length(eval(parse(text = paste("salviniris",t,sep=""))))
  for (i in 1:a) {
  if(any(names(eval(parse(text = paste("salviniris",t,sep="")))[[i]]$entities) %in% "media")){
    MatteoSalvini[k,"media_type"] <- eval(parse(text = paste("salviniris",t,sep="")))[[i]]$entities$media[[1]]$type
    }
   else {MatteoSalvini[k,"media_type"] <- ""}
    k=k+1
  }
}
table(MatteoSalvini$media_type)
library(lubridate)
library(dplyr)
library(tidyverse)
str(salvini$created_at)
for( i in 1:nrow(MatteoSalvini)){
a <- MatteoSalvini[i,"created_at"]
montha <- word(a,2)
month.name
match(montha,month.abb)
pezzi <- word(a,c(1:6))
pezzi[2] <- paste(0,match(pezzi[2],month.abb),sep="")
data1 <- paste(pezzi[6],"-",pezzi[2],"-",pezzi[3],sep = "")
MatteoSalvini[i,"created_at"] <- paste(data1, pezzi[4])
}
str(MatteoSalvini)
MatteoSalvini$created_at <- ymd_hms(MatteoSalvini$created_at)
str(ymd_hms(dataeora))
str(salvini[1,"created_at"])
Salvo <- as.data.frame(salvini)
str(Salvo$created_at)
str(salvini$is_quote)
MatteoSalvini$is_quote <- as.logical(MatteoSalvini$is_quote)
MatteoSalvini$is_retweet <- as.logical(MatteoSalvini$is_retweet)
MatteoSalvini$hashtags <- as.list(MatteoSalvini$hashtags)
MatteoSalvini$media_type <- as.list(MatteoSalvini$media_type)
Salvi <- bind_rows(salvini,MatteoSalvini)

save_as_csv(x=Salvi, file_name ="Salvinifinale.csv")


#personal data
appname <- "..."
key <- "..."
secret <- "..."

# base64 encoding
kands <- paste(key, secret, sep=":")
base64kands <- base64encode(charToRaw(kands))
base64kandsb <- paste("Basic", base64kands, sep=" ")

# request bearer token
resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                 add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant_type=client_credentials")

# get bearer token
bearer <- content(resToken)
bearerToken <- bearer[["access_token"]]
bearerTokenb <- paste("Bearer", bearerToken, sep=" ")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201707280000 ,\"fromDate\": 201707140000}")

grillosett1 <- content(resTweets2)

grilloris1 <- grillosett1$results

for (i in 1:100) {
  print(grilloris1[[i]]$created_at)
}

saveRDS(grilloris1,"settimana28_0714_07(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201707140000 ,\"fromDate\": 201707010000}")

grillosett2 <- content(resTweets2)

grilloris2 <- grillosett2$results

for (i in 1:100) {
  print(grilloris2[[i]]$created_at)
}

saveRDS(grilloris2,"settimana14_0707_07(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201707070000 ,\"fromDate\": 201706250000}")

grillosett3 <- content(resTweets2)

grilloris3 <- grillosett3$results

for (i in 1:100) {
  print(grilloris3[[i]]$created_at)
}

saveRDS(grilloris3,"settimana07_0725_06(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201706250000 ,\"fromDate\": 201706100000}")

grillosett4 <- content(resTweets2)

grilloris4 <- grillosett4$results

for (i in 1:100) {
  print(grilloris4[[i]]$created_at)
}

saveRDS(grilloris4,"settimana25_0615_06(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201706150000 ,\"fromDate\": 201706010000}")

grillosett5 <- content(resTweets2)

grilloris5 <- grillosett5$results

for (i in 1:100) {
  print(grilloris5[[i]]$created_at)
}

saveRDS(grilloris5,"settimana15_0601_06(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201706010000 ,\"fromDate\": 201705150000}")

grillosett6 <- content(resTweets2)

grilloris6 <- grillosett6$results

for (i in 1:100) {
  print(grilloris6[[i]]$created_at)
}

saveRDS(grilloris6,"settimana01_0620_05(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201705200000 ,\"fromDate\": 201705090000}")

grillosett7 <- content(resTweets2)

grilloris7 <- grillosett7$results

for (i in 1:100) {
  print(grilloris7[[i]]$created_at)
}

saveRDS(grilloris7,"settimana20_0509_05(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201705090000 ,\"fromDate\": 201705010000}")

grillosett8 <- content(resTweets2)

grilloris8 <- grillosett8$results

for (i in 1:100) {
  print(grilloris8[[i]]$created_at)
}

saveRDS(grilloris8,"settimana09_0504_05(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201705040000 ,\"fromDate\": 201704250000}")

grillosett9 <- content(resTweets2)

grilloris9 <- grillosett9$results

for (i in 1:100) {
  print(grilloris9[[i]]$created_at)
}

saveRDS(grilloris9,"settimana04_0528_04(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201704280000 ,\"fromDate\": 201704180000}")

grillosett10 <- content(resTweets2)

grilloris10 <- grillosett10$results

for (i in 1:100) {
  print(grilloris10[[i]]$created_at)
}

saveRDS(grilloris10,"settimana28_0421_04(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201704210000 ,\"fromDate\": 201704100000}")

grillosett11 <- content(resTweets2)

grilloris11 <- grillosett11$results

for (i in 1:100) {
  print(grilloris11[[i]]$created_at)
}

saveRDS(grilloris11,"settimana21_0418_04(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201704180000 ,\"fromDate\": 201704100000}")

grillosett12 <- content(resTweets2)

grilloris12 <- grillosett12$results

for (i in 1:100) {
  print(grilloris12[[i]]$created_at)
}

saveRDS(grilloris12,"settimana18_0410_04(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201704100000 ,\"fromDate\": 201704010000}")

grillosett13 <- content(resTweets2)

grilloris13 <- grillosett13$results

for (i in 1:100) {
  print(grilloris13[[i]]$created_at)
}

saveRDS(grilloris13,"settimana10_0401_04(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201704010000 ,\"fromDate\": 201703200000}")

grillosett14 <- content(resTweets2)

grilloris14 <- grillosett14$results

for (i in 1:100) {
  print(grilloris14[[i]]$created_at)
}

saveRDS(grilloris14,"settimana01_0425_03(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703250000 ,\"fromDate\": 201703150000}")

grillosett15 <- content(resTweets2)

grilloris15 <- grillosett15$results

for (i in 1:100) {
  print(grilloris15[[i]]$created_at)
}

saveRDS(grilloris15,"settimana25_0322_03(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703220000 ,\"fromDate\": 201703100000}")

grillosett16 <- content(resTweets2)

grilloris16 <- grillosett16$results

for (i in 1:100) {
  print(grilloris16[[i]]$created_at)
}

saveRDS(grilloris16,"settimana22_0319_03(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703190000 ,\"fromDate\": 201703100000}")

grillosett17 <- content(resTweets2)

grilloris17 <- grillosett17$results

for (i in 1:100) {
  print(grilloris17[[i]]$created_at)
}

saveRDS(grilloris17,"settimana19_0314_03(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703140000 ,\"fromDate\": 201703040000}")

grillosett18 <- content(resTweets2)

grilloris18 <- grillosett18$results

for (i in 1:100) {
  print(grilloris18[[i]]$created_at)
}

saveRDS(grilloris18,"settimana14_0310_03(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703100000 ,\"fromDate\": 201703040000}")

grillosett19 <- content(resTweets2)

grilloris19 <- grillosett19$results

for (i in 1:100) {
  print(grilloris19[[i]]$created_at)
}

saveRDS(grilloris19,"settimana10_0306_03(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201703060000 ,\"fromDate\": 201702280000}")

grillosett20 <- content(resTweets2)

grilloris20 <- grillosett20$results

for (i in 1:100) {
  print(grilloris20[[i]]$created_at)
}

saveRDS(grilloris20,"settimana06_0328_04(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702280000 ,\"fromDate\": 201702180000}")

grillosett21 <- content(resTweets2)

grilloris21 <- grillosett21$results

for (i in 1:100) {
  print(grilloris21[[i]]$created_at)
}

saveRDS(grilloris21,"settimana28_0227_02(2017)G")
grilloris21[[50]]$text

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702270000 ,\"fromDate\": 201702180000}")

grillosett22 <- content(resTweets2)

grilloris22 <- grillosett22$results

for (i in 1:100) {
  print(grilloris22[[i]]$created_at)
}

saveRDS(grilloris22,"settimana27_0224_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/politica.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702240000 ,\"fromDate\": 201702180000}")

grillosett23 <- content(resTweets2)

grilloris23 <- grillosett23$results

for (i in 1:100) {
  print(grilloris23[[i]]$created_at)
}

saveRDS(grilloris23,"settimana24_0218_02(2017)G")




#personal data
appname <- "..."
key <- "..."
secret <- "..."

# base64 encoding
kands <- paste(key, secret, sep=":")
base64kands <- base64encode(charToRaw(kands))
base64kandsb <- paste("Basic", base64kands, sep=" ")

# request bearer token
resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                 add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant_type=client_credentials")

# get bearer token
bearer <- content(resToken)
bearerToken <- bearer[["access_token"]]
bearerTokenb <- paste("Bearer", bearerToken, sep=" ")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702180000 ,\"fromDate\": 201702100000}")

grillosett24 <- content(resTweets2)

grilloris24 <- grillosett24$results

for (i in 1:100) {
  print(grilloris24[[i]]$created_at)
}

saveRDS(grilloris24,"settimana18_0216_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702160000 ,\"fromDate\": 201702100000}")

grillosett25 <- content(resTweets2)

grilloris25 <- grillosett25$results

for (i in 1:100) {
  print(grilloris25[[i]]$created_at)
}

saveRDS(grilloris25,"settimana16_0214_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702140000 ,\"fromDate\": 201702080000}")

grillosett26 <- content(resTweets2)

grilloris26 <- grillosett26$results

for (i in 1:100) {
  print(grilloris26[[i]]$created_at)
}

saveRDS(grilloris26,"settimana14_0210_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702100000 ,\"fromDate\": 201702020000}")

grillosett27 <- content(resTweets2)

grilloris27 <- grillosett27$results

for (i in 1:100) {
  print(grilloris27[[i]]$created_at)
}

saveRDS(grilloris27,"settimana10_0208_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702080000 ,\"fromDate\": 201702020000}")

grillosett28 <- content(resTweets2)

grilloris28 <- grillosett28$results

for (i in 1:100) {
  print(grilloris28[[i]]$created_at)
}

saveRDS(grilloris28,"settimana08_0202_02(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201702020000 ,\"fromDate\": 201701250000}")

grillosett29 <- content(resTweets2)

grilloris29 <- grillosett29$results

for (i in 1:100) {
  print(grilloris29[[i]]$created_at)
}

saveRDS(grilloris29,"settimana02_0226_01(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701260000 ,\"fromDate\": 201701180000}")

grillosett30 <- content(resTweets2)

grilloris30 <- grillosett30$results

for (i in 1:100) {
  print(grilloris30[[i]]$created_at)
}

saveRDS(grilloris30,"settimana26_0122_01(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701220000 ,\"fromDate\": 201701150000}")

grillosett31 <- content(resTweets2)

grilloris31 <- grillosett31$results

for (i in 1:100) {
  print(grilloris31[[i]]$created_at)
}

saveRDS(grilloris31,"settimana22_0115_01(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701150000 ,\"fromDate\": 201701080000}")

grillosett32 <- content(resTweets2)

grilloris32 <- grillosett32$results

for (i in 1:100) {
  print(grilloris32[[i]]$created_at)
}

saveRDS(grilloris32,"settimana15_0112_01(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701120000 ,\"fromDate\": 201701080000}")

grillosett33 <- content(resTweets2)

grilloris33 <- grillosett33$results

for (i in 1:100) {
  print(grilloris33[[i]]$created_at)
}

saveRDS(grilloris33,"settimana12_0108_01(2017)G")

resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701080000 ,\"fromDate\": 201701010000}")

grillosett34 <- content(resTweets2)

grilloris34 <- grillosett34$results

for (i in 1:100) {
  print(grilloris34[[i]]$created_at)
}

saveRDS(grilloris34,"settimana08_0102_01(2017)G")


resTweets2 <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/search.json",
                   add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                   body = "{\"query\": \"from: beppe_grillo\",\"maxResults\": 100,\"toDate\":201701020000 ,\"fromDate\": 201701010000}")

grillosett35 <- content(resTweets2)

grilloris35 <- grillosett35$results

for (i in 1:100) {
  print(grilloris35[[i]]$created_at)
}

saveRDS(grilloris35,"settimana02_0102_01(2017)G")


BeppeGrillo <- data.frame("name"="", "screen_name"="","created_at"="","text"="","is_quote"="","is_retweet"="","favorite_count"=0,"retweet_count"=0,"hashtags"="","media_type"="",stringsAsFactors = F)
nomi <- c(1:35)
k <- 1
for (t in nomi){
  a <- length(eval(parse(text = paste("grilloris",t,sep=""))))
  for (i in 1:a) {
    BeppeGrillo[k,"name"] <- as.character(eval(parse(text = paste("grilloris",t,sep="")))[[1]]$user$name)
    BeppeGrillo[k,"screen_name"] <-as.character(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$user$screen_name)
    BeppeGrillo[k,"created_at"] <- as.character(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$created_at)
    if (eval(parse(text = paste("grilloris",t,sep="")))[[i]]$truncated == T) {
      BeppeGrillo[k,"text"] <- as.character(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$extended_tweet$full_text)
    } else  BeppeGrillo[k,"text"] <- as.character(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$text)
    BeppeGrillo[k,"is_quote"] <- eval(parse(text = paste("grilloris",t,sep="")))[[i]]$is_quote_status
    BeppeGrillo[k,"is_retweet"] <- eval(parse(text = paste("grilloris",t,sep="")))[[i]]$retweeted
    BeppeGrillo[k,"favorite_count"] <- eval(parse(text = paste("grilloris",t,sep="")))[[i]]$favorite_count
    BeppeGrillo[k,"retweet_count"] <- eval(parse(text = paste("grilloris",t,sep="")))[[i]]$retweet_count
    BeppeGrillo[k,"hashtags"] <- ""
    numhash <- length(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$entities$hashtags)
    if (numhash>0){
      for (j in 1:numhash) {
        BeppeGrillo[k,"hashtags"] <-paste(BeppeGrillo[k,"hashtags"],eval(parse(text = paste("grilloris",t,sep="")))[[i]]$entities$hashtags[[j]]$text)
      }
    }
    k=k+1
  }
}
k <- 1
for(t in nomi){
  a <- length(eval(parse(text = paste("grilloris",t,sep=""))))
  for (i in 1:a) {
    if(any(names(eval(parse(text = paste("grilloris",t,sep="")))[[i]]$entities) %in% "media")){
      BeppeGrillo[k,"media_type"] <- eval(parse(text = paste("grilloris",t,sep="")))[[i]]$entities$media[[1]]$type
    }
    else {BeppeGrillo[k,"media_type"] <- ""}
    k=k+1
  }
}
library(readr)
grillo <- read_csv("Grillotweets(rtweet).csv")

for( i in 1:nrow(BeppeGrillo)){
  a <- BeppeGrillo[i,"created_at"]
  montha <- word(a,2)
  month.name
  match(montha,month.abb)
  pezzi <- word(a,c(1:6))
  pezzi[2] <- paste(0,match(pezzi[2],month.abb),sep="")
  data1 <- paste(pezzi[6],"-",pezzi[2],"-",pezzi[3],sep = "")
  BeppeGrillo[i,"created_at"] <- paste(data1, pezzi[4])
}
str(BeppeGrillo)
BeppeGrillo$created_at <- ymd_hms(BeppeGrillo$created_at)
str(ymd_hms(dataeora))
str(grillo[1,"created_at"])
Salvo <- as.data.frame(grillo)
str(grillo$is_quote)
BeppeGrillo$is_quote <- as.logical(BeppeGrillo$is_quote)
BeppeGrillo$is_retweet <- as.logical(BeppeGrillo$is_retweet)
BeppeGrillo$hashtags <- as.character(BeppeGrillo$hashtags)
BeppeGrillo$media_type <- as.character(BeppeGrillo$media_type)

Beppe <- bind_rows(grillo,BeppeGrillo)

save_as_csv(x=Beppe, file_name ="Grillofinale.csv")
