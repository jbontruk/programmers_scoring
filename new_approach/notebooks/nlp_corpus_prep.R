# Libraries ----
library(RMySQL)
library(dplyr)

# Clean from html tags ----
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Posts body for Word2Vec ----
getwd()

setwd('./data/PostsBody')
getwd()
list.files()
pb1 <- read.csv2("QueryResults (1).csv", stringsAsFactors = F, sep = ',')
pb2 <- read.csv2("QueryResults (2).csv", stringsAsFactors = F, sep = ',')
pb3 <- read.csv2("QueryResults (3).csv", stringsAsFactors = F, sep = ',')
pb4 <- read.csv2("QueryResults (4).csv", stringsAsFactors = F, sep = ',')
pb5 <- read.csv2("QueryResults (5).csv", stringsAsFactors = F, sep = ',')
pb6 <- read.csv2("QueryResults (6).csv", stringsAsFactors = F, sep = ',')
pb7 <- read.csv2("QueryResults (7).csv", stringsAsFactors = F, sep = ',')
pb8 <- read.csv2("QueryResults (8).csv", stringsAsFactors = F, sep = ',')
pb9 <- read.csv2("QueryResults (9).csv", stringsAsFactors = F, sep = ',')
pb10 <- read.csv2("QueryResults (10).csv", stringsAsFactors = F, sep = ',')
pb11 <- read.csv2("QueryResults.csv", stringsAsFactors = F, sep = ',')
pb <- rbind(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, pb9, pb10, pb11)

setwd('../QuestionsForAnswers')
getwd()
list.files()
q1 <- read.csv2("Questions (1).csv", stringsAsFactors = F, sep = ',')
q2 <- read.csv2("Questions (2).csv", stringsAsFactors = F, sep = ',')
q3 <- read.csv2("Questions (3).csv", stringsAsFactors = F, sep = ',')
q4 <- read.csv2("Questions (4).csv", stringsAsFactors = F, sep = ',')
q5 <- read.csv2("Questions (5).csv", stringsAsFactors = F, sep = ',')
q6 <- read.csv2("Questions (6).csv", stringsAsFactors = F, sep = ',')
q7 <- read.csv2("Questions (7).csv", stringsAsFactors = F, sep = ',')
q8 <- read.csv2("Questions (8).csv", stringsAsFactors = F, sep = ',')
q9 <- read.csv2("Questions.csv", stringsAsFactors = F, sep = ',')
q <- rbind(q1, q2, q3, q4, q5, q6, q7, q8, q9)

setwd('../UsersQuestions')
getwd()
list.files()
uq1 <- read.csv2("UsersQuestions (1).csv", stringsAsFactors = F, sep = ',')
uq2 <- read.csv2("UsersQuestions (2).csv", stringsAsFactors = F, sep = ',')
uq3 <- read.csv2("UsersQuestions (3).csv", stringsAsFactors = F, sep = ',')
uq4 <- read.csv2("UsersQuestions.csv", stringsAsFactors = F, sep = ',')
uq <- rbind(uq1, uq2, uq3, uq4)

corpus <- rbind(pb, q, uq)
corpus$body <- cleanFun(corpus[,2])
corpus <- corpus %>% select(body) %>% filter(nchar(body) != 0)

write.csv2(corpus, "C:/Users/jaroslaw.bontruk/Documents/Repos/ITM.Internal.AdvancedSourcing/data/corpus.csv", row.names = F)

# Tags chains for Word2Vec
db <- dbConnect(dbDriver('MySQL'), user = '###', password = '###', 
                dbname = 'stackoverflow', host = '###', port = 3306)
dbListTables(db)
post_tags <- dbReadTable(db, "posttags")

posts_min2 <- post_tags %>% count(PostId) %>% filter(n > 1)

tags_chains <- post_tags %>%
  filter(PostId %in% posts_min2$PostId) %>%
  group_by(PostId) %>%
  summarise(tags = paste(TagName, collapse = ', '))
tags_chains <- tags_chains %>% select(tags)

write.csv2(tags_chains, "C:/Users/jaroslaw.bontruk/Documents/Repos/ITM.Internal.AdvancedSourcing/data/tags_chains.csv", row.names = F)