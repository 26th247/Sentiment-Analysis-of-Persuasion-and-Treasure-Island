require(stringr)
require(dplyr)
require(lubridate)
require(textdata)
require(tidytext)
require(syuzhet)
require(ggplot2)
require(forcats)
require(wordcloud)
require(reshape2)
require(kableExtra)

persuasion <-read.csv("C:/Users/Harun/Documents/Data Sets/105_Persuasion.csv")
treasure_island <- read.csv("C:/Users/Harun/Documents/Data Sets/120_Treasure Island.csv")

                                        ##### PERSUASION #####

#get rid of the ID variable and keep just text
persuasion <- persuasion %>%    
  select(text)

#check the stop words
stop_words                     

#anti-join to get rid of the stop words in the text, now we have the useful words left

refined_persuasion <- persuasion %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words,  by = "word") %>%
  count(word, sort = TRUE)     

kable(t1) %>%
  kable_styling(full_width = FALSE, position = "float_left")
kable(t2) %>%
  kable_styling(full_width = FALSE, position = "left")

#ggplot to show the most frequent words

head(refined_persuasion, 30) %>%
  arrange(desc(word)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "coral4") +
  coord_flip() +
  ggtitle("Persuasion Most Popular Words") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Count")

#use wordcloud to visualise most frequent words

refined_persuasion %>%          
  anti_join(stop_words) %>%
  count(word, n) %>%
  with(wordcloud(word, n, max.words = 50))

#Start using the sentiments and add it to the world cloud

refined_persuasion %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(stop_words) %>%
  count(word, n, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("tomato3", "royalblue1"), max.words = 100)
  
# put the sentiments into a dataframe for more detailed analysis

persuasion_sentiments <- refined_persuasion %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

positive_sent <- subset(sentiments, sentiment =="positive")
negative_sent <- subset(sentiments, sentiment =="negative")

positive.score <- aggregate(n ~ sentiment)

#count the negative and positive sentiments
persuasion_sentiments %>%
  count(sentiment)

#apply nrc for wider range of analysis

refined_persuasion %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  sample_n(10) %>%

  ggplot(aes(sentiment, n)) +
  geom_col(fill = "slateblue") +
  ggtitle("NRC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("NRC Sentiments") +
  ylab("Count")

persuasion_sentiments <- refined_persuasion %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# CONTEXT FOR THE AMOUNT OF NEGATIVE AND POSITIVE WORDS IN THE LEXICONS
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

                                      ###### TREASURE ISLAND #####

# only select the text and anti_join stop words to keep useful words

treasure_island <- treasure_island %>%
  select(text)

stop_words

refined_treasureisland <- treasure_island %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words,  by = "word") %>%
  count(word, sort = TRUE)


#briefly plot the bar to see most popular words
head(refined_treasureisland, 10) %>%
  arrange(desc(word)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "coral4") +
  coord_flip() +
  ggtitle("Treasure Island Most Popular Words") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Count")

# put the sentiments into a dataframe for more detailed analysis

treasureisland_sentiments <- refined_treasureisland %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)


#count the negative and positive sentiments
treasureisland_sentiments %>%
  count(sentiment)






















