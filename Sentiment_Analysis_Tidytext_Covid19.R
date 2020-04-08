# Project : Sentiment Analysis of the Corona Virus Tweets
#Dataset : https://www.kaggle.com/smid80/coronavirus-covid19-tweets#Coronavirus%20Tweet%20Corpus.CSV
#Author : Smitakshi Gupta
# Tools : Using Sentiment Analysis in R 


#Installing the libraries
install.packages("tidytext")
library(tidytext)
library(dplyr)
install.packages("data.table", repos = "https://cran.r-project.org")
library(data.table)

get_sentiments("bing")
get_sentiments("nrc")
get_sentiments("afinn")


covid19 = fread("/Users/smitakshigupta/Desktop/Portfolio/Sentiment_Analysis_of_Tweets/Coronavirus Tweet Corpus.csv", 
             strip.white=T, sep=",", header=T, na.strings=c(""," ", "NA","nan", "NaN", "nannan"))

install.packages("bit64")
library(bit64)

colnames(covid19)

str(covid19)

n = nrow(covid19) # n will be ther number of obs. in data
# Creating an index for 30% of obs. by random

trainIndex = sample(1:n,size = round(0.3*n),replace=FALSE)

# Using the index to create train data
covid19_data = covid19[trainIndex,]


#Ensuring that dplyr is installed to run other data manipulation functions

if ("dplyr" %in% installed.packages()[, "Package"]){ 
  cat("'dplyr' is installed.")
} else {
  install.packages("dplyr",dependencies=T)
}
library(dplyr)


#Breaking the text in the tweet into words for further pre-processing
library(tidytext)
tidy_text <- covid19_data %>%
#The function unnest_tokens splits each row such that there is one token (word) in each row of the new data frame
  unnest_tokens(word, text)
tidy_text[1:20]


#Removing the stop words from the data
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE) 

#Creating the visualization for the most commonly used words 
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 50000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Applying tf-idf to the data - creating unique documents based on the unique users in the original data

tweet_words <- covid19_data %>%
  unnest_tokens(word, text) %>%
  count(screen_name, word, sort = TRUE) %>% # It will count the number of words per user
  ungroup()
head(tweet_words)

total_words <- tweet_words %>% 
  group_by(screen_name) %>% 
  summarize(total = sum(n)) # Counts the total number of words used by each user
head(total_words)

tweet_words <- left_join(tweet_words, total_words) 

head(tweet_words)

tweet_words <- tweet_words %>%
  bind_tf_idf(word, screen_name, n)
head(tweet_words)

#Now the sentiment analysis 
#Looking at the NRC lexicon and just filtering the joy words
#All the sentiments are based on unigrams

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_text %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

#Analysing the sentiment with Bing Lexicon
sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

head(sentiment)

#Visualizing the sentiments with the word clouds 
install.packages("wordcloud")
library(wordcloud)


tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Analyzing the positive and negative words 

install.packages("reshape2", repos = "https://cran.r-project.org")

library(reshape2)

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

