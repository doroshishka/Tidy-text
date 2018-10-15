# Tidy-text
Tidy text workshop

Tidy Text Mining Textbook: https://www.tidytextmining.com/

library(dplyr)
library(stringr)
library(tidytext)
library(tibble)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(igraph)
library(ggraph)
library(reshape2)

getwd()
setwd("directory")

sotu <- read.delim("SOTU.txt", header = FALSE, sep = "\n") #imports speech

# Ch 1 - The Tidy Text Format 
Associated Chapter: https://www.tidytextmining.com/tidytext.html#the-unnest_tokens-function

tidy_sotu <- data_frame(text = as.character(sotu[[1]])) %>%
  unnest_tokens(word, text)

tidy_sotu[1:20,1]

#data(stop_words) #tidy_text stopwords
#stop_words
#tidy_sotu2 <- anti_join(tidy_sotu, stop_words)

tidy_sotu2 %>%
  count(word, sort = TRUE)

tidy_sotu %>% #graphical representation of word frequencies
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%    #words used 5 times or more
  mutate(word = reorder(word, n)) %>% #orders the words by n, not alphabetically
  ggplot(aes(word, n)) +
  geom_col() + 
  coord_flip()

tidy_sotu %>% #wordcloud
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words =80))

# Ch 4 - n-grams 
Associated Chapter: https://www.tidytextmining.com/ngrams.html

sotu_bigrams_all <- sotu %>%
  unnest_tokens(bigram, V1, token = "ngrams", n = 2)

sotu_bigrams_all %>%
  count(bigram, sort = TRUE) #up to here, bigrams contain all words

bigrams_separated <- sotu_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% #filtered bigram list, one word pre column
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>% #bigram by frequency
  count(word1, word2, sort = TRUE)

sotu_bigrams <- bigrams_filtered %>% #bigram list
  unite(bigram, word1, word2, sep = " ")
sotu_bigrams

bigrams_filtered %>% #most common words mentioned with "people"
  filter(word2 == "people") %>%
  count(word1, sort = TRUE)

#Bigram network
bigrams_count <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigrams_count

bigram_graph <- bigram_count %>% #creates igraph
  filter(n > 1) %>% #only considers bigrams used twice or more
  graph_from_data_frame()
bigram_graph

#basic ggraph out of igraph; requires: nodes, edge, text (provided from igraph)
set.seed(1800) #set the seed to make sure you are always producing the same graph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#modified ggraph, which includes arrows for directionality
set.seed(1800)
a <- grid::arrow(type = "closed", length = unit(0.1, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.1, 'inches')) +
  geom_node_point(color = "red") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) # +

#theme_void() is a common theme for plotting networks
theme_void()
a <- grid::arrow adds arrows to provide directionality (e.g., "trade deals" not "deals trade")
edge_alpha = n means bigrams that are more rare have transparant arrows (n >= 3 has dark arrows)

# Ch 2 - Sentiment Analysis
Associated Chapter: https://www.tidytextmining.com/sentiment.html

get_sentiments("afinn") #-5 to +5 sentiment
get_sentiments("bing") #positive/negative binary
get_sentiments("nrc") #yes/no binaries for 9 categories

#nrc sentiment
nrcjoy <- get_sentiments("nrc")
filter(sentiment == "joy") #filters to search for only joy sentiment words
tidy_sotu %>%
  inner_join(nrcjoy) %>%
  count(word, sentiment, sort = TRUE)

#afinn sentiment
afinn_sotu <- tidy_sotu %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort = TRUE) %>%
  ungroup()
afinn_sotu

#bing sentiment
bing_sotu <- tidy_sotu %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_sotu

bing_sotu %>% #plotting positive and negative
  group_by(sentiment) %>%
  top_n(6) %>% #top 6 words
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", #plots frequency of occurance
       x = NULL) +
  coord_flip()

tidy_sotu %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 200)

# Ch 3 - Term Frequencies
Associated Chapter: https://www.tidytextmining.com/tfidf.html

sotu_words <- tidy_sotu %>%
  count(word, sort = TRUE) %>%
  ungroup()
sotu_words

sotu_total <- rep(summarize(sotu_words, total = sum(n)), length(sotu_words))
sotu_words <- cbind(sotu_words, sotu_total[1])
freq <- sotu_words %>%
  mutate(rank = row_number(),
         'term frequency' = n/total)

#plotting term frequency
rank_subset <- freq %>% 
  filter(rank < 500,
         rank > 5)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_abline(intercept = -1.0613, slope = -0.8956, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
