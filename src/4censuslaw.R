library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(reshape2)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(igraph)
library(ggraph)


#
# Read indata ---------------------------------------------------------------------------
#

# Import
censuslaw <- read_csv("output/censuslaw.csv")


#
# Some descriptives ---------------------------------------------------------------------------
#

# Number of mentions per title
bytitle <- censuslaw %>% group_by(Source, Title) %>% summarize(n = n()) %>% arrange(n)

ggplot(bytitle, aes(x = reorder(Title, n), y = n)) +
  geom_col() +
  labs(title = "Number of mentions per title", x = "Title", y = "Number of mentions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#
# Tidytext ---------------------------------------------------------------------------
#

# Parse
tidylaw <- censuslaw %>%
  unnest_tokens(word, Text)

# Remove stopwords
data(stop_words)
tidylaw <- tidylaw %>% anti_join(stop_words)

# Title is a factor
tidylaw$Title <- as.factor(tidylaw$Title)

# Top words
topwords <- tidylaw %>% count(word, sort = TRUE) 

tidylaw %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most frequently used words (n > 30)", x = "Word", y = "Frequency (n)")

tidylaw %>%
  group_by(Title) %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() + 
  facet_wrap(~Title, scales = "free") +
  labs(title = "Most frequently used words (n > 5)", x = "Frequency") +
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6))

# Word cloud
tidylaw %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 250))

# Tidy n-grams
lawbi <- tidylaw %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)
lawbi %>% count(bigram, sort = TRUE)

lawtri <- tidylaw %>%
  unnest_tokens(trigram, word, token = "ngrams", n = 3)
lawtri %>% count(trigram, sort = TRUE)

bigramfreq <- lawbi %>%
  filter(!is.na(bigram)) %>%
  group_by(Title) %>%
  count(bigram, sort = TRUE) %>%
  arrange(Title)
bigramfreq %>% filter(n > 5) %>% arrange(desc(n))

# Networks
bigrams_separated <- lawbi %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE) %>%
  filter(!is.na(word1) & !is.na(word2))

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Census law bigrams")
