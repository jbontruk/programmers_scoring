# Library ----
library(RMySQL)
library(dplyr)

# Read data ----
db <- dbConnect(dbDriver('MySQL'), user = '###', password = '###', 
                dbname = 'stackoverflow', host = '###', port = 3306)
post_tags <- dbReadTable(db, "posttags")

# Generate supertags ----
tags_count <- post_tags %>%
  count(TagName) %>%
  arrange(desc(n))

tags_to_map <- tags_count[101:nrow(tags_count),]

top_tags <- left_join(post_tags, tags_count, by = "TagName") %>%
  arrange(PostId, desc(n)) %>%
  group_by(PostId) %>%
  mutate(supertag = first(TagName)) %>%
  select(PostId, supertag) %>%
  unique()

supertags <- left_join(post_tags, top_tags, by = 'PostId') %>%
  group_by(TagName, supertag) %>%
  filter(TagName != supertag & TagName %in% tags_to_map$TagName) %>%
  summarise(n = n()) %>%
  group_by(TagName) %>%
  mutate(perc = n / sum(n))

supertags_map <- supertags %>%
  arrange(TagName, desc(n)) %>%
  group_by(TagName, supertag) %>%
  mutate(winner = first(supertag)) %>%
  filter(perc > 0.5) %>%
  select(TagName, Supertag = supertag, Probability = perc, N_obs = n) %>%
  data.frame()

# Organize rare supertags ----
rare_supertags <- supertags_map %>%
  group_by(Supertag) %>%
  summarise(n = n()) %>%
  filter(n < 15)

rare_supertags <- left_join(rare_supertags, supertags_map, by = c("Supertag" = "TagName")) %>%
  filter(!is.na(N_obs)) %>%
  select(Supertag, supersupertag = Supertag.y)

supertags_map <- left_join(supertags_map, rare_supertags) %>%
  mutate(Supertag = if_else(is.na(supersupertag),
                            Supertag,
                            supersupertag)) %>%
  select(-supersupertag)

# Add supertag to tag which is supertag ----
supertags_map <- supertags_map %>%
  mutate(Supertag = if_else(TagName %in% supertags_map$Supertag,
                            TagName,
                            Supertag))

already_tagged <- supertags_map %>% 
  filter(TagName %in% supertags_map$Supertag) %>%
  select(TagName)

tags_to_add <- supertags_map %>%
  filter(!(Supertag %in% already_tagged$TagName)) %>%
  select(Supertag) %>%
  unique() %>%
  mutate(TagName = Supertag,
         Probability = 1.0,
         N_obs = NA)

supertags_map <- rbind(supertags_map, tags_to_add)

# Write data ----
db <- dbConnect(dbDriver('MySQL'), user = '###', password = '###', 
                dbname = 'stackoverflow', host = '###', port = 3306)
dbWriteTable(db, "supertags", supertags_map, row.names = F, overwrite = T)

tags <- dbReadTable(db, "tags")
calculated <- supertags_map %>%
  select(TagName, CalculatedTagGroup = Supertag) %>%
  unique()
tags <- left_join(tags, calculated)
dbWriteTable(db, "tags", tags, row.names = F, overwrite = T)