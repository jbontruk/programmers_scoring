library(RSQLite)
library(dplyr)
library(plotly)
library(lubridate)

filename <- "C:/Users/jaroslaw.bontruk/Downloads/RealSkill_2020-01-08 (1)/RealSkill.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

dbListTables(db)
users <- dbReadTable(db, "Users")
users <- users[2:nrow(users),]
users$Id <- as.integer(users$Id)
badges <- dbReadTable(db, "Badges")
posts <- dbReadTable(db, "Posts")
post_tags <- dbReadTable(db, "PostTags")
tags <- dbReadTable(db, "Tags")

# Stackoverflow Rating:
#  General_User_Rating: 0.1*Accept_Rate + sum(Bronze_Badges + 10*Silver_Badges + 100*Gold_Badges) + 100*log10(Reputation + 1)
#  User_Expertise_Ratings: sum(Answer_Count + Answer_Score + Question_Count + Question_Score) GROUPBY Tag
#  Overall_User_Rating: General_User_Rating + sum(User_Expertise_Ratings)

# Analiza reputacji
# 50% ma tylko 1 punkt, 25% ma poniżej 100 i 25% powyżej
# Logarytm dziesiętny wydaje się dobrą transformacją
reputation <- users %>% count(reputation = as.numeric(Reputation))

fill <- data.frame(reputation = seq(1, max(reputation$reputation)))
reputation <- left_join(fill, reputation)
reputation[is.na(reputation)] <- 0

reputation$log <- log10(reputation$reputation)
reputation$binned <- cut(reputation$reputation, breaks = c(0, 1, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10^4))

grouped_reputation <- reputation %>% 
  group_by(reputation = binned) %>%
  summarise(n = sum(n))

plot_ly(grouped_reputation, x = ~reputation, y = ~n, type = 'scatter', mode = 'lines')
plot_ly(reputation, x = ~log, y = ~n, type = 'scatter', mode = 'lines')

# Analiza postow
users_posts <- posts %>%
  group_by(Id = as.character(OwnerUserId)) %>%
  summarise(count_posts = n(),
            sum_scores = sum(Score, na.rm = T))

t1 <- left_join(users, users_posts) %>%
  mutate(reputation = as.numeric(Reputation),
         views = as.numeric(Views),
         upvotes = as.numeric(UpVotes),
         downvotes = as.numeric(DownVotes)) %>%
  filter(reputation <= 25000)

plot_ly(t1, x = ~reputation, y = ~count_posts)
plot_ly(t1, x = ~reputation, y = ~sum_scores)
plot_ly(t1, x = ~reputation, y = ~views)
plot_ly(t1, x = ~reputation, y = ~upvotes)
plot_ly(t1, x = ~reputation, y = ~downvotes)

cor(t1$reputation, t1$sum_scores, method = "pearson", use = "complete.obs")
cor.test(t1$reputation, t1$sum_scores, method = "pearson")
cor.test(t1$reputation, t1$count_posts, method = "pearson")
cor.test(t1$reputation, t1$views, method = "pearson")
cor.test(t1$reputation, t1$upvotes, method = "pearson")
cor.test(t1$reputation, t1$upvotes, method = "pearson")

users_questions <- posts %>%
  filter(ParentId == 0) %>%
  group_by(Id = as.character(OwnerUserId)) %>%
  summarise(count = n())
users_answers <- posts %>%
  filter(ParentId != "" ) %>%
  group_by(Id = as.character(OwnerUserId)) %>%
  summarise(count = n())

accepted <- unique(posts$AcceptedAnswerId)

# Analiza tagow
tags_stats <- left_join(post_tags, posts, by = c('PostId' = 'Id'))

t2 <- tags_stats %>%
  mutate(AnswerCount = as.integer(AnswerCount),
         ViewCount = as.integer(ViewCount)) %>%
  group_by(TagName) %>%
  summarise(count_posts = n(),
            sum_answers_count = sum(AnswerCount, na.rm = T),
            sum_score = sum(Score, na.rm = T),
            sum_view_count = sum(ViewCount, na.rm = T)) %>%
  arrange(desc(count_posts))

# Analiza badges
t3 <- badges %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

users_badges <- badges %>%
  group_by(UserId) %>%
  summarise(class_1 = sum(if_else(Class == 1, 1, 0)),
            class_2 = sum(if_else(Class == 2, 1, 0)),
            class_3 = sum(if_else(Class == 3, 1, 0)),
            sum_badges = n())
users_badges <- left_join(users, users_badges, by = c("Id" = "UserId"))
users_badges$sum_badges[is.na(users_badges$sum_badges)] <- 0

sum_badges <- users_badges %>% count(sum_badges)
fill2 <- data.frame(sum_badges = seq(0, max(sum_badges$sum_badges)))
sum_badges <- left_join(fill2, sum_badges)

plot_ly(sum_badges, x = ~sum_badges, y = ~n, type = 'scatter', mode = 'lines')

sum_badges$binned <- cut(sum_badges$sum_badges, breaks = c(-1, 0, 1, 5, 10, 100, 10000))
grouped_badges <- sum_badges %>% 
  group_by(sum_badges = binned) %>%
  summarise(n = sum(n))

plot_ly(grouped_badges, x = ~sum_badges, y = ~n, type = 'scatter', mode = 'lines')