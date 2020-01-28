library(httr)

main <- 'https://api.github.com'

r <- GET(paste0(main, '/orgs/microsoft/repos'))
t1 <- content(r, "parsed")

r <- GET(paste0(main, '/orgs/microsoft/repos?page=3'))
t2 <- content(r, "parsed")

r <- GET(t[[1]]$stargazers_url)
s <- content(r, "parsed")