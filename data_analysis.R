# getting ready -----------------------------------------------------------

# install packages that does not need to be loaded
# install.packages("textdata")

# load packages
library(tidytext)
library(rtweet)
library(dotwhisker)
library(tidygraph)
library(ggraph)
library(doc2concrete)
library(tidyverse)

# load the data
mps <- read.csv("data/mps.csv", na.strings = "")
twts <- read_rds("data/tweets.rds")

# user-based analyses -----------------------------------------------------

# who has the most followers?

mps_new <- mps %>%
  filter(!is.na(screen_name))

mps_now <- lookup_users(mps_new$screen_name) %>%
  select(screen_name, followers_count)

left_join(mps_new, mps_now, by = "screen_name") %>%
  arrange(followers_count, desc = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(y = reorder(name, followers_count),
             x = followers_count,
             fill = party)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Top 20 MPs\n") + xlab("\nFollowers") +
  scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c("0", "1M", "2M", "3M")) +
  scale_fill_manual(name = "",
                    breaks = c("Conservative", "Labour", "Green Party",
                               "LibDem", "Independent"),
                    values = c("#0087DC", "#DC241f", "#008066", "#FDBB30", "gray"))

# who do they talk to?

twts %>%
  filter(screen_name != reply_to_screen_name & is_retweet == FALSE &
         !is.na(reply_to_screen_name)) %>%
  count(reply_to_screen_name, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(y = reorder(reply_to_screen_name, n), x = n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Top 20 Replied to Users\n") + xlab("")


# retweet networks

twts_new <- twts %>%
  filter(is_retweet == TRUE & screen_name != retweet_screen_name) %>%
  inner_join(., mps, by = c("retweet_screen_name" = "screen_name")) %>%
  select(screen_name, retweet_screen_name) %>%
  group_by(screen_name) %>%
  count(retweet_screen_name)


rt_network <- as_tbl_graph(twts_new) %>%
  activate(nodes) %>%
  left_join(., mps, by = c("name" = "screen_name")) %>%
  filter(party %in% c("Conservative", "Labour", "SNP", "LibDem"))

ggraph(rt_network) +
  geom_edge_link() +
  geom_node_point(aes(color = party)) +
  theme_graph() +
  scale_color_manual(name = "",
                     breaks = c("Conservative", "Labour", "SNP",
                                "LibDem"),
                     values = c("#0087DC", "#DC241f", "#FFFF00", "#FDBB30"))


# reply networks of LibDem MPs

twts_new <- twts %>%
  filter(!is.na(reply_to_screen_name) & screen_name != reply_to_screen_name) %>%
  inner_join(., mps, by = c("reply_to_screen_name" = "screen_name")) %>%
  select(screen_name, reply_to_screen_name) %>%
  group_by(screen_name) %>%
  count(reply_to_screen_name)


rply_network <- as_tbl_graph(twts_new) %>%
  activate(nodes) %>%
  left_join(., mps, by = c("name" = "screen_name")) %>%
  filter(party %in% c("SNP"))

ggraph(rply_network) +
  geom_edge_link(aes(size = n)) +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  theme_graph()

# measures of centredness

# https://www.data-imaginist.com/2017/introducing-tidygraph/

twts_new <- twts %>%
  filter(is_retweet == TRUE & screen_name != retweet_screen_name) %>%
  inner_join(., mps, by = c("retweet_screen_name" = "screen_name")) %>%
  select(screen_name, retweet_screen_name) %>%
  group_by(screen_name) %>%
  count(retweet_screen_name) %>%
  as_tbl_graph()

twts_new %>%
  mutate(betweenness = centrality_betweenness(),
         authority = centrality_authority()) %>%
  as.data.frame() %>%
  pivot_longer(cols = c("betweenness", "authority"),
               names_to = "centrality_measure", values_to = "centrality_value") %>%
  group_by(centrality_measure) %>%
  filter(rank(desc(centrality_value)) <= 10) %>%
  ungroup() %>%
  ggplot(aes(y = reorder(name, centrality_value),
             x = centrality_value)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(. ~ centrality_measure, scales = "free") +
  ylab("") + xlab("")


# factors correlating with being on twitter

mps_new <- mps %>%
  mutate(on_twitter = if_else(is.na(screen_name), 0, 1))

m1 <- lm(on_twitter ~ age + electorate + female + majority + term,
           data = mps_new)

dwplot(m1) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1))


# factors correlating with tweeting more

twts_new <- twts %>%
  group_by(screen_name) %>%
  summarise(n_tweets = n())

merged <- left_join(mps_new, twts_new, by = "screen_name") %>%
  mutate(sum_tweets = if_else(on_twitter == 1 & is.na(n_tweets),
                              0, as.numeric(n_tweets)))

m2 <- lm(sum_tweets ~ age + electorate + female + majority + term,
         data = merged)

dwplot(m2) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1))

# factors correlating with the number of followers right now

mps_new <- mps %>%
  filter(!is.na(screen_name))

mps_now <- lookup_users(mps_new$screen_name)

merged <- left_join(mps_new, mps_now, by = "screen_name")

m3 <- lm(followers_count ~ age + electorate + female + majority + term,
         data = merged)

dwplot(m3) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = c(-50000, 0, 50000))

# factors correlating with being retweeted more

twts_new <- twts %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name) %>%
  summarise(across(c(retweet_count, followers_count), mean))

merged <- left_join(mps, twts_new, by = "screen_name")

m4 <- lm(retweet_count ~ age + electorate + female + majority + term + followers_count,
         data = merged)

dwplot(m4) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = c(-50000, 0, 50000))



# tweet-based analyses ----------------------------------------------------

# when do they tweet?

twts %>%
  filter(is_retweet == FALSE) %>%
  mutate(hour = format(created_at, tz = "GB", "%H")) %>%
  group_by(hour) %>%
  summarise(n_tweets = n()) %>%
  ggplot(aes(hour, n_tweets)) +
  geom_line(aes(group = 1), size = 1) +
  theme_bw() +
  ylab("Number of Tweets\n") + xlab("\nHours")


twts %>%
  filter(is_retweet == FALSE) %>%
  mutate(day = format(created_at, tz = "GB", "%A"),
         day =factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                     "Friday", "Saturday", "Sunday"))) %>%
  group_by(day) %>%
  summarise(n_tweets = n()) %>%
  ggplot(aes(day, n_tweets)) +
  geom_line(aes(group = 1), size = 1) +
  theme_bw() +
  ylab("Number of Tweets\n") + xlab("\nDays") +
  scale_x_discrete(breaks = c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday"))


# most frequently used hashtags

twts %>%

  filter(is_retweet == FALSE & !is.na(hashtags)) %>%

  # unlist the lists of hashtags to create strings
  group_by(status_id) %>%
  mutate(tidy_hashtags = str_c(unlist(hashtags), collapse = " ")) %>%

  ungroup() %>%
  # split the string, create a new variable called `da_tweets`
  unnest_tokens(output = unnested_hashtags, input = tidy_hashtags, token = "words") %>%

  count(unnested_hashtags) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(y = reorder(unnested_hashtags, n),
             x = n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Top 20 Hashtags\n") + xlab("")


# most frequently used words

twts %>%
  unnest_tokens(output = token_tweets, input = text, token = "tweets") %>%
  anti_join(., stop_words, by = c("token_tweets" = "word")) %>%
  count(token_tweets) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(y = reorder(token_tweets, n),
             x = n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Top 20 Words\n") + xlab("")


twts_new <- twts %>%
  unnest_tokens(output = token_tweets, input = text, token = "tweets") %>%
  anti_join(., stop_words, by = c("token_tweets" = "word")) %>%
  count(token_tweets)

wordcloud(words = twts_new$token_tweets,
          freq = twts_new$n, max.words = 100)


# average share of retweets from specific accounts

mps_new <- mps %>%
  select(screen_name, party)

left_join(twts, mps_new, by = "screen_name") %>%
  filter(!(screen_name %in% c("BorisJohnson", "Keir_Starmer", "EdwardJDavey")) &
         party %in% c("Conservative", "Labour", "SNP", "LibDem")) %>%
  mutate(leader_rt = case_when(party == "Conservative" & retweet_screen_name == "BorisJohnson" ~ 1,
                               party == "Labour" & retweet_screen_name == "Keir_Starmer" ~ 1,
                               party == "SNP" & retweet_screen_name == "NicolaSturgeon" ~ 1,
                               party == "LibDem" & retweet_screen_name == "EdwardJDavey" ~ 1,
                               TRUE ~ 0)) %>%
  group_by(party) %>%
  summarise(share_rt = sum(leader_rt) * 100 / n()) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(party, -share_rt), y = share_rt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Share of Leader Retweets\n") + xlab("") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))


# sentiment analysis

df_sentiments <- get_sentiments("nrc") %>%
  mutate(sentiment_value = 1) %>%
  pivot_wider(names_from = sentiment, values_from = sentiment_value, values_fill = 0)

twts_new <- twts %>%
  unnest_tokens(output = token_tweets, input = text, token = "tweets") %>%
  anti_join(stop_words, by = c("token_tweets" = "word")) %>%
  left_join(., df_sentiments, by = c("token_tweets" = "word"))

tweet_level <- twts_new %>%
  group_by(status_id) %>%
  summarise(positive_tone = sum(positive, na.rm = TRUE) -
                            sum(negative, na.rm = TRUE))

user_level <- twts_new %>%
  group_by(user_id) %>%
  summarise(positive_tone = sum(positive, na.rm = TRUE) -
              sum(negative, na.rm = TRUE))

# sentiments over 15 days, by party


left_join(twts_new, mps, by = "screen_name") %>%
  filter(is_retweet == FALSE &
         party %in% c("Conservative", "Labour", "SNP", "LibDem")) %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(party, date) %>%
  summarise(positive_tone = (sum(positive, na.rm = TRUE) - sum(negative, na.rm = TRUE)) /
                            (sum(positive, na.rm = TRUE) + sum(negative, na.rm = TRUE))) %>%
  ungroup() %>%
  ggplot(aes(date, positive_tone)) +
  geom_line(aes(group = 1), size = 1) +
  theme_bw() +
  facet_wrap(. ~ party) +
  ylab("Average Sentiment\n") + xlab("")


# are replies more likely to be negative?

twts_new <- left_join(twts, tweet_level, by = "status_id") %>%
  filter(is_quote == FALSE, is_retweet == FALSE) %>%
  mutate(is_reply = ifelse(is.na(reply_to_status_id), 0, 1))

summary(lm(positive_tone ~ is_reply, data = twts_new))


# concreteness analysis

mturk_list


twts_new <- twts %>%
  unnest_tokens(output = token_tweets, input = text, token = "tweets") %>%
  anti_join(stop_words, by = c("token_tweets" = "word")) %>%
  left_join(., mturk_list, by = c("token_tweets" = "Word"))

tweet_level <- twts_new %>%
  group_by(status_id) %>%
  summarise(concrete_tone = if_else(all(is.na(Conc.M)), NA_real_, sum(Conc.M, na.rm = TRUE)))

# are tweets less concrete at unsocial times?

twts_new <- left_join(twts, tweet_level, by = "status_id") %>%
  filter(is_quote == FALSE, is_retweet == FALSE) %>%
  mutate(hour = format(created_at, tz = "GB", "%H"))

m6 <- lm(concrete_tone ~ hour, data = twts_new)

dwplot(m6) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = 2)
