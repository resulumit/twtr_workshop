# note --------------------------------------------------------------------

# this script provides the solutions to exercises, or links to them,
# for the workshop (https://github.com/resulumit/twtr_workshop) on
# working with twitter data in r, by resul umit

# last updated on: 2022-03-06


# load the packages -------------------------------------------------------

library(rtweet)
library(tidyverse)
library(tidytext)


# exercise 1 --------------------------------------------------------------

df_tweets <- search_tweets(q = "#AcademicTwitter",
                           n = 30)

# exercise 3 --------------------------------------------------------------

# see the variable names
names(df_tweets)

# get a better glimpse of your data
tibble::glimpse(df_tweets)


# exercise 5 --------------------------------------------------------------

df_tweets <- search_tweets(q = "\"publish or perish\" academia -phd",
                           n = 10)


# exercise 6 --------------------------------------------------------------

df_tweets <- search_tweets(q = "Switzerland",
                           n = 50,
                           type = "popular",
                           include_rts = FALSE)


# exercise 7 --------------------------------------------------------------

df_tweets <- search_tweets(q = "phd",
                           n = 35000,
                           retryonratelimit = TRUE)


# exercise 8 --------------------------------------------------------------

df_users <- search_users(q = "PhD -rstats",
                         n = 30)


# exercise 9 --------------------------------------------------------------

df_tweets <- search_tweets(q = "PhD -rstats",
                           n = 30)


# exercise 10 -------------------------------------------------------------

View(df_users)
View(df_tweets)


# exercise 11 -------------------------------------------------------------

df_tweets <- search_tweets(q = "computational social science")
df_users <- search_users(q = "computational social science")


# exercise 12 -------------------------------------------------------------

df_limits <- rate_limit()


# exercise 13 -------------------------------------------------------------

limit_before <- rate_limit(query = "search/tweets")$remaining


# exercise 14 -------------------------------------------------------------

df_tweets <- search_tweets(q = "rstats",
                           n = 50,
                           lang = "en")


# exercise 15 -------------------------------------------------------------

# note down the limit after
limit_after <- rate_limit(query = "search/tweets")$remaining

# compare the limits before and after
limit_before - limit_after


# exercise 16 -------------------------------------------------------------

df_tweets <- lookup_tweets(statuses = "1500133572761563136")


# exercise 17 -------------------------------------------------------------

# import data and keep only the head (by default, 6 rows)
ids <- readRDS("data/status_ids.rds") %>%
  head()

# view the data
View(ids)

# look up the tweets
df_tweets <- lookup_tweets(statuses = ids$status_id)


# exercise 18 -------------------------------------------------------------

# import data on mps and keep only the head (by default, 6 rows)
df_mps <- read.csv("data/mps.csv") %>%
  head()

# view the data
View(df_mps)

# look up the users
df_tweets <- lookup_users(users = df_mps$screen_name)


# exercise 19 -------------------------------------------------------------

# import data and keep only the screen_name variable
screen_names <- read.csv("data/mps.csv") %>%
  select(screen_name)

# single out the 10th and 20th mps
mp10 <- screen_names[10, 1]
mp20 <- screen_names[20, 1]

#  check the friendship status of the two
df_friends <- lookup_friendships(source = mp10,
                   target = mp20)


# exercise 20 -------------------------------------------------------------

df_timelines <- get_timeline(user = c("drob", "hadleywickham", "JennyBryan"),
                             n = c(10, 20, 30))


# exercise 21 -------------------------------------------------------------

df_my_timeline <- get_timeline(user = "resulumit",
                               n = 3200)


# exercise 22 -------------------------------------------------------------

# import data and keep only the screen_name variable
screen_names <- read.csv("data/mps.csv") %>%
  select(screen_name)

# single out the first five mps
mps1_5 <- screen_names[1:5, 1]

# get their timelines
df_mp_timelines <- get_timeline(user = mps1_5,
                                n = 3200)


# exercise 23 -------------------------------------------------------------

# search and single out the potential screen names
search_users(q = "Universität Luzern")$screen_name

# get the followers of the account

df_followers <- get_followers(user = "UniLuzern")

# exercise 24 -------------------------------------------------------------

df_friends <- get_friends(users = "UniLuzern")

# exercise 25 -------------------------------------------------------------

df_limits <- rate_limit() %>%
  mutate(difference = limit - remaining) %>%
  arrange(-difference)


# exercise 26 -------------------------------------------------------------

df_favourites <- get_favorites(user = c("drob", "hadleywickham",
                                        "JennyBryan"))

# exercise 27 -------------------------------------------------------------

# find my most retweeted tweet
df_my_tweets <- get_timeline(user = "resulumit") %>%
  filter(retweet_count == max(retweet_count))

# see 100 users who have retweeted it
df_retweets <- get_retweets(status_id = df_my_tweets$status_id)


# exercise 28 -------------------------------------------------------------

df_trend_locations <- trends_available()


# exercise 29 -------------------------------------------------------------

# choose two cities
zurich <- df_trend_locations[which(df_trend_locations$name == "Zurich"), ]$woeid
munich <- df_trend_locations[which(df_trend_locations$name == "Munich"), ]$woeid

# get the trends for them
zurich_trends <- get_trends(woeid = zurich)
munich_trends <- get_trends(woeid = munich)


# exercise 30 -------------------------------------------------------------

oslo <- df_trend_locations[which(df_trend_locations$name == "Oslo"), ]$woeid
oslo_trends <- get_trends(woeid = oslo)


# exercise 31 -------------------------------------------------------------

df_memberships <- lists_memberships(user = "hadleywickham")


# exercise 32 -------------------------------------------------------------

# choose the list highest number of subscribers
hw_listed <- lists_memberships(user = "hadleywickham") %>%
  filter(subscriber_count == max(subscriber_count))

# see the others listed
df_members <- lists_members(list_id = hw_listed$list_id)


# exercise 33 -------------------------------------------------------------

df_statuses <- lists_statuses(list_id = hw_listed$list_id)


# exercise 34 -------------------------------------------------------------

df_subscribers <- lists_subscribers(list_id = hw_listed$list_id)


# exercise 35 -------------------------------------------------------------

df_subscriptions <- lists_subscriptions(user = df_subscribers$user_id[1])


# exercise 36 -------------------------------------------------------------

df_stream <- stream_tweets(q = "",
                           timeout = 30)


# exercise 37 -------------------------------------------------------------

df_stream <- stream_tweets(q = "and",
              timeout = 30)

# exercise 38 -------------------------------------------------------------

df_stream <- stream_tweets(q = "PhD",
                           timeout = 30)

# exercise 39 -------------------------------------------------------------

df_stream <- stream_tweets(q = "elections",
                           timeout = 30)

# exercise 40 -------------------------------------------------------------

# import data
df_tweets <- read_rds("data/tweets.rds")

# remove mentions, save as a new variable
df_tweets %>%
  mutate(no_mentions = str_remove_all(string = text, pattern = "[@][\\w_-]+")) %>%
  select(text, no_mentions)  %>%
  View()


# exercise 41 -------------------------------------------------------------
df_tweets %>%
  mutate(no_hashtags = str_remove_all(string = text, pattern = "[#][\\w_-]+")) %>%
  select(text, no_hashtags)  %>%
  View()


# exercise 42 -------------------------------------------------------------
df_tweets %>%
  group_by(status_id) %>%
  mutate(no_links =
           str_remove_all(string = text,
                          pattern = str_c(unlist(urls_t.co), collapse = "|"))) %>%
  ungroup() %>%
  select(text, no_links)  %>%
  View()

# exercise 43 -------------------------------------------------------------

# pull the help file
?iconv

# remove imojis
df_tweets %>%
  mutate(no_emojis = iconv(x = text, from = "latin1", to = "ASCII", sub = "")) %>%
  select(text, no_emojis)  %>%
  View()


# exercise 44 -------------------------------------------------------------
df_tweets %>%
  mutate(no_mentions = str_remove_all(string = text, pattern = c("[@][\\w_-]+", "[#][\\w_-]+"),
         no_mentions_hashtags = str_remove_all(string = no_mentions, pattern = "[#][\\w_-]+"))) %>%
  group_by(status_id) %>%
  mutate(no_mentions_hashtags_links =
           str_remove_all(string = no_mentions_hashtags,
                          pattern = str_c(unlist(urls_t.co), collapse = "|"))) %>%
  ungroup() %>%
  mutate(all_clean = iconv(x = no_mentions_hashtags_links, from = "latin1", to = "ASCII", sub = "")) %>%
  select(text, all_clean)  %>%
  View()


# exercise 45 -------------------------------------------------------------

df_tweets %>%
  mutate(no_punctuations = str_replace_all(string = text, pattern = "[[:punct:]]", replacement = " ")) %>%
  select(text, no_punctuations)  %>%
  View()

# exercise 46 -------------------------------------------------------------

df_tweets %>%
  mutate(no_whitespace = str_squish(string = text)) %>%
  select(text, no_whitespace)  %>%
  View()

# exercise 47 -------------------------------------------------------------

df_tweets %>%
  mutate(lower_text = str_to_lower(string = text)) %>%
  select(text, lower_text)  %>%
  View()


# exercise 48 -------------------------------------------------------------

mp_level <- df_tweets %>%
  group_by(user_id) %>%
  summarise(merged_tweets = paste0(text, collapse = ". "),
            n_tweets = n(),
            mean_rt = mean(retweet_count),
            mean_like = mean(favorite_count))


# exercise 49 -------------------------------------------------------------

word_level <- df_tweets %>%
  unnest_tokens(output = word, input = text, token = "words")


# exercise 50 -------------------------------------------------------------

hashtag_level <- df_tweets %>%
  group_by(status_id) %>%
  mutate(tidy_hashtags = str_c(unlist(hashtags), collapse = " ")) %>%
  unnest_tokens(output = hashtag, input = tidy_hashtags, token = "words")


# exercise 51 -------------------------------------------------------------

no_stopwords <- df_tweets %>%
  unnest_tokens(output = meaningful_words, input = text, token = "tweets") %>%
  anti_join(stop_words, by = c("meaningful_words" = "word")) %>%
  select(status_id, meaningful_words)


# exercises 52 to 57 ------------------------------------------------------

# answers are available in:
# exercises/users_answers.Rmd


# exercises 63 to 68 ------------------------------------------------------

# answers are available in:
# exercises/tweets_answers.Rmd
