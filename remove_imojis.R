library(rio)
library(tidyverse)

df <- import("data/tweets.rds")

tweet <- df$text[3]

as_string(tweet)


writeLines(tweet)

writeLines(str_remove_all(string = tweet, pattern = "[<].+[>]"))

str_view(tweet, "^\\s*<U\\+\\w+>\\s*")

str_remove_all(string = tweet, pattern = "[\U{1F300}-\u{1F6FF}]")

str_remove_all("<U+0001F5E3><U+FE0F> If you are a vict",
               pattern = "\\s*<U\\+\\w+>\\s*")



