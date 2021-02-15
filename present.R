library(rio)


# pipes -------------------------------------------------------------------

twts <- import("data/tweets.rds")
twts2 <- select(twts, screen_name, source)
twts3 <- group_by(twts2, screen_name)
twts4 <- count(twts3, source)
head(twts4)


import("data/tweets.rds") %>%
  select(screen_name, source) %>%
  group_by(screen_name) %>%
  count(source) %>%
  head()



# networks ----------------------------------------------------------------


# create a temporary tweets dataframe about who replies to whom, how often
twts_temp <- twts %>%

  # filter to replies only, filter out self replies
  filter(!is.na(reply_to_screen_name) & screen_name != reply_to_screen_name) %>%

  # filter out those replied to, who are not mps themselves
  # by using the inner_join function, with the mps dataset
  inner_join(., mps, by = c("reply_to_screen_name" = "screen_name")) %>%

  # reduce to the dataset to two variables only: who replies to whom
  select(screen_name, reply_to_screen_name) %>%

  # calculate the retweet counts by mp
  group_by(screen_name) %>%
  count(reply_to_screen_name)



# turn that temporary dataframe into a data structure for tidy graph
# using the `as_tbl_graph` function
rply_network <- as_tbl_graph(twts_temp) %>%

  # add the `party` variable to the nodes
  # by first activating the nodes
  activate(nodes) %>%

  # and then merging the `mps` dataset
  left_join(., mps, by = c("name" = "screen_name")) %>%

  # create a new variable with surnames only with the `str_extract` function
  # this will make graph easier to read
  mutate(surname = str_extract(name.y, pattern = "\\w+$")) %>%

  # filter down to the SNP and LibDem MPs only
  filter(party %in% c("SNP", "LibDem"))



# plot the tidy graph data with the `ggraph` function
ggraph(rply_network, layout = "kk") +
  geom_edge_link(arrow = arrow(length = unit(2, "mm")),
                 end_cap = circle(3, "mm")) +
  geom_node_point(aes(color = party, size = 5)) +
  geom_node_text(aes(label = surname)) +
  theme_graph()

# others: layout = "kk"  layout = "drl"


# centrality scores
rply_network %>%

  # add the `party` and centrality variables to the nodes
  # by first activating the nodes
  activate(nodes) %>%


  # add the centrality variables with the `mutate` function
  mutate(betweenness = centrality_betweenness(),
         power = centrality_power()) %>%

  # turn into a small dataframe
  as.data.frame() %>%
  select(surname, betweenness, power)








df <- twts %>%
  # unlist the lists of hashtags to create strings
  group_by(status_id) %>%
  mutate(tidy_hashtags = str_c(unlist(hashtags), collapse = " ")) %>%
  # split the string, create a new variable called da_tweets
  unnest_tokens(output = da_hashtags, input = tidy_hashtags, token = "words")

















# sentiments --------------------------------------------------------------

install.packages("textdata")   # sentiment dictionary


df_sentiments <- get_sentiments("nrc") %>%

  # pivot your data wider, so that every new word is a new row
  mutate(sentiment_value = 1) %>%
  pivot_wider(names_from = sentiment, values_from = sentiment_value, values_fill = 0)

# create a temporarty dataframe, from the `tweets` dataset
twts_temp <- twts %>%

  # filter out retweets
  filter(is_retweet == FALSE) %>%

  # unnest the remaining tweets into words
  unnest_tokens(output = token_tweets, input = text, token = "tweets") %>%

  select(status_id, token_tweets) %>%


  # join the sentiments dataframe in
  left_join(., df_sentiments, by = c("token_tweets" = "word"))






# concreteness ------------------------------------------------------------

library(doc2concrete)
df_conc <- mturk_list
















































