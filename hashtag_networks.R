# Notes:
# 1. This script uses the package widyr --- install it if you haven't done so
# before. It causes a warning about functions being old, which you can ignore
#
# 2. I have limited the plot to top 20 most frequently occurring pairs of
# hashtags otherwise the graph becomes undrawable/unreadable with large Ns


#load packages
library(tidyverse)     # most data operations
library(tidytext)      # tokenising hashtags
library(widyr)         # pairwise counting of hashtags
library(tidygraph)     # network analysis, data manipulation
library(ggraph)        # network analysis, visualisation


# start with the tweets dataframe
hashtag_network <- read_rds("data/tweets.rds")%>%

  # select the variables that we need
  select(status_id, hashtags) %>%

  # switch to rowwise operations
  rowwise() %>%

  # keep only tweets with multiple hastags
  filter(length(hashtags) > 1) %>%

  # unlist the lists of hashtags
  mutate(hashtags2 = str_c(unlist(hashtags), collapse = " ")) %>%

  # split the string, create a new variable called da_tweets
  unnest_tokens(output = hashtags3, input = hashtags2, token = "words") %>%

  # duplicate the hastag column
  pairwise_count(item = hashtags3, feature = status_id) %>%

  # choose the 20 most frequent combinations of hashtags
  arrange(n, desc = TRUE) %>%
  top_n(20) %>%

  # turn into tidy graph data
  as_tbl_graph()


# plot the tidy graph data with the `ggraph` function
ggraph(hashtag_network, layout = "kk") +
  geom_edge_link(arrow = arrow(length = unit(2, "mm")),
                 end_cap = circle(3, "mm")) +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  theme_graph()
