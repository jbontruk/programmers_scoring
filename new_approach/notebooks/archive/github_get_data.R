# Libraries ----
library(tidyverse)
library(httr)
library(httpuv)

 # Get users list ----
getwd()
setwd("C:/Users/jaroslaw.bontruk/Desktop/Poboczne/Advanced Sourcing")
a <- list.files()
a

users <- read.csv2(a[3], sep = ",", stringsAsFactors = F)

# Autorization ----
# Github user: getthisdata1 (na mejlu getthisdata@onet.pl)
# Github pass: jak w itmagination
oauth_endpoints("github")
myapp <- oauth_app(appname = "advanced_sourcing",
                   key = "0ae1490b06b5c0a22644",
                   secret = "d109388701d731562748b67e19c0776ab3f01ee9")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)

# Get data from API ----
user <- users$github_profile[1]

# Get user details
request <- GET(paste0("https://api.github.com/users/",
                      user),
               gtoken)
body <- content(request, "parsed")
user_details <- data.frame(t(sapply(body,c)))

# Get all repos - page by page (30 repos per page)
request <- GET(paste0("https://api.github.com/users/",
                      user,
                      '/repos'),
               gtoken)
body <- content(request, "parsed")
t <- map_dfr(body, ~as_data_frame(t(.)))
repos <- as_data_frame(t)

# Remember to filter t$fork TRUE/FALSE, forks are not his

# Get stargazes of repos
stars <- data.frame(full_name = character(),
                    stargazes = character(),
                    stringsAsFactors = F)
for (i in 1:30) {
  stars[i, 1] = repos$full_name[[i]]
  stars[i, 2] = content(GET(repos$stargazers_url[[i]], gtoken), "text")
}

# Remember to filter out own stars to own repo

# Get list of followers
request <- GET(paste0("https://api.github.com/users/",
                      user,
                      '/followers'),
               gtoken)
body <- content(request, "parsed")
t <- map_dfr(body, ~as_data_frame(t(.)))
followers <- as_data_frame(t)