# load libraries ----
library(rtweet)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(topicmodels)
library(ldatuning)
library(tictoc)


rm(list = ls())

# load data
setwd("/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/")
#setwd("/Users/ralfschmaelzle/Documents/GITHUB/nomcomm/PAPERS_WORKING/message_engine2/")

data <- read_csv("./01_AI_generated_messages/Total AI-generated messages_no prompt.csv")
data$text = data$tweets

# select only columns we need for topic modeling
data_clean <- data %>% select(text)

# create idx to keep track of each "document" (tweet)
# we make it a factor for the cast_dtm function below
data_clean <- data_clean %>%  mutate(idx = as.factor(row_number()))

# remove URLs
data_clean$text <- gsub("https\\S*","", data_clean$text)
# remove "@username" tags
data_clean$text <- gsub("@\\w+", "", data_clean$text) 
# remove hashtags
data_clean$text <- gsub('#\\S+', '', data_clean$text)


my_stop_words <- tibble(
  word = c(
    "chinas",
    "china",
    "a2",
    "beijing",
    "javascript"
  ),
  lexicon = "twitter"
)

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)


# put data into tidy text format - note we use 'token = 'tweets'' for twitter-specific text preprocessing
data_tidy <- data_clean %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # remove stop words
  anti_join(all_stop_words)
# stem the words
#mutate(word = SnowballC::wordStem(word))

# get rid of unicode for the & symbol ("amp")
data_tidy <- data_tidy %>% filter(word != "amp")

#this was what caused the error!
data_tidy$idx<-as.character(data_tidy$idx)

data_dtm <- data_tidy %>% 
  # get count of each token in each document
  count(idx, word) %>%
  # create a document-term matrix with all features and tfidf weighting
  cast_dtm(document = idx, term = word, value = n, weighting = tm::weightTf)

# view
data_dtm

# remove sparse terms
data_dtm_trim <- removeSparseTerms(data_dtm, sparse = .99)
data_dtm_trim

# we need to get rid of rows with none of the non-sparse terms
rowTotals <- apply(data_dtm_trim, 1, sum) #Find the sum of words in each Document
data_dtm_final <- data_dtm_trim[rowTotals> 0, ] 


#create topic model with 10 topics
tweets_lda <- LDA(data_dtm_final, k = 7, control = list(seed = 1234))

# get the beta values for each word, which attribute each word a probability for each topic
tweet_topics <- tidy(tweets_lda, matrix = "beta")

# make dataframe showcasing the 10 words with highest beta per topic
tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



# plot top words for each topic
tweet_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Use 'ldatuning' package to find optimum number of topics. Note I'm using quad-core CPU
topic_n <- FindTopicsNumber(dtm = data_dtm_final , topics = seq(from = 2, to = 20, by = 1), 
                            metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),    #"Griffiths2004"
                            method = "Gibbs",
                            control = list(seed = 123),
                            mc.cores = 4L,
                            verbose = TRUE)

FindTopicsNumber_plot(topic_n)



###
ap_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(400, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

library(wordcloud)
library(reshape2)



ap_top_terms %>%
  mutate(topic = paste("topic", topic)) %>%
  acast(term ~ topic, value.var = "beta", fill = 0) %>%
  comparison.cloud(colors = c('red','goldenrod','limegreen','aquamarine3','deepskyblue1','purple1','deeppink2'),
                   random.order=FALSE,,
                   title.size = 0.75,
                   scale = c(3.5,0.5),
                   max.words = 400)



