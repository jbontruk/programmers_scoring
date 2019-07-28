# wczytanie bibliotek ----
library(splitstackshape)
library(rMySQL)
library(dbConnect)
library(dplyr)
library(Hmisc)
library(car)

# pobranie danych z mysql localhost ----
polaczSQL <- function(nameDB, kwerenda) {
  con <- dbConnect(RMySQL::MySQL(), host = "localhost",
                   user = "root", password = "######", port = 3306, dbname = nameDB)
  res <- dbSendQuery(con, kwerenda)
  data <- dbFetch(res, n = -1)
  dbDisconnect(con)
  return(data)
}
profiles <- polaczSQL("db","select * from profiles")
repositories <- polaczSQL("db","select * from repositories")

# pobranie danych z psql IP ----
require("RPostgreSQL")
pw <- {"codeboards"}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "codeboards",
                 host = "46.101.227.235", port = 5432,
                 user = "codeboards", password = pw)
rm(pw)
profiles <- dbGetQuery(con, "SELECT * from profiles")
repositories <- dbGetQuery(con, "SELECT * from repositories")

# badanie jezykow ----
profiles1 <- profiles
profiles1 <- cSplit(cSplit(profiles1, "language_statistics", ",", "long"), 
                    "language_statistics", "=>")

profiles1$language_statistics_1<-gsub('"', '', profiles1$language_statistics_1)
profiles1$language_statistics_2<-gsub('"', '', profiles1$language_statistics_2)
profiles1$language_statistics_2<-as.numeric(profiles1$language_statistics_2)

languages <- profiles1 %>%
  group_by(language_statistics_1) %>%
  summarise(count=n(), 
            mean=mean(language_statistics_2, na.rm=TRUE),
            median=median(language_statistics_2, na.rm=TRUE),
            min=min(language_statistics_2, na.rm=TRUE),
            max=max(language_statistics_2,na.rm=TRUE))

write.table(languages, file = "languages.csv", sep = ";", dec = ",")

# wybranie zbioru zmiennych NA PODSTAWIE RAPORTU ----
df <- profiles[,c("id", "github_joined", "github_observers", "github_stargazes")]
df$days_in_github <- as.numeric(Sys.Date() - as.Date(df$github_joined))
temp <- repositories[,c("profile_id", "slug", "stars")]
temp1 <- repositories %>% group_by(profile_id, slug) %>% summarise(stars = mean(stars))
temp2 <- temp1 %>% group_by(profile_id) %>% summarise(sum_of_repos = n(),
                                                      sum_of_repo_stars = sum(stars))
df <- left_join(df, temp2, by = c("id" = "profile_id"))
rm(temp, temp1, temp2)
df[, 3:7][is.na(df[, 3:7])] <- 0
str(df)

# zbadanie korelacji miedzy zmiennymi ----
rcorr(as.matrix(df[,3:7]), type="pearson") 
rcorr(as.matrix(df[,3:7]), type="spearman")

# wykresy rozrzutu, szczegolnie stargazes vs repo_stars ----
plot(df$github_stargazes, df$sum_of_repo_stars, type="p")
scatterplotMatrix(~github_observers+
                    github_stargazes+
                    days_in_github+
                    sum_of_repos+
                    sum_of_repo_stars,
                  data=df,
                  main="All df Variables correlations")
summary(df)

# usuniecie ze zbioru skorelowanej i gorszej zmiennej (stargazes) ----
df <- df[,-4]

# ponowne zbadanie ----
rcorr(as.matrix(df[,3:6]), type="pearson") 
rcorr(as.matrix(df[,3:6]), type="spearman")
scatterplotMatrix(~github_observers+
                    days_in_github+
                    sum_of_repos+
                    sum_of_repo_stars,
                  data=df,
                  main="All df Variables correlations")

# regresja zmiennej observers ----
fit <- lm(formula = github_observers ~
            days_in_github +
            sum_of_repos + 
            sum_of_repo_stars, 
          data =df)
summary(fit)

# rownanie koncowe ratingu ----
attributes(fit)
attributes(summary(fit))
print(paste0("rating = ",
             fit$coefficients[2],
             " * days_in_github + ",
             fit$coefficients[3],
             " * sum_of_repos + ",
             fit$coefficients[4],
             " * sum_of_repo_stars + ",
             1 - (fit$coefficients[2] + fit$coefficients[3] + fit$coefficients[4]),
             " * github_observers"))

# nadanie ratingu i rankingu ----
df$rating <- fit$coefficients[2] * df$days_in_github +
  fit$coefficients[3] * df$sum_of_repos +
  fit$coefficients[4] * df$sum_of_repo_stars +
  (1 - (fit$coefficients[2] + fit$coefficients[3] + fit$coefficients[4])) * df$github_observers
hist(df$rating, breaks = 50)
df$ranking <- rank(-df$rating, ties.method = "min")
plot(sort(df$ranking))
write.table(df, file = "df.csv")

badges <- profiles %>% group_by(stackoverflow_badges) %>% summarise(count = n())
summary(profiles$stackoverflow_badges)