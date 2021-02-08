library(rtweet)

df1 <- search_tweets(q = "academia \"publish or perish\" -phd",
                    type = "recent",
                    n = 200)

df2 <- search_tweets(q = "Luzern",
                     geocode = c("47.05,8.30,25mi"),
                     type = "recent",
                     include_rts = FALSE,
                     n = 200)


df3 <- search_tweets(q = "phd",
                     type = "recent",
                     retryonratelimit = TRUE,
                     n = 35000)


df_users <- search_users(q = "PhD -rstats",
                         n = 300)

df_tweets <- search_tweets(q = "PhD -rstats",
                           n = 300)
