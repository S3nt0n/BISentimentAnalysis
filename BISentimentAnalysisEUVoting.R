#Set Environmentpath
path <- "C:/Users/Simon Cox/Google Drive/Studium FOM/3. Semester/Business Intelligence/Programmierung"
setwd(path)

# Load packages
library("rtweet")
library("dplyr")
library("jsonlite")
library("tm")
library("textstem")
library("hunspell")
library("tidytext")
library("ggplot2")

#Function to clean and import JSON dataframe
fill_No_of_follower_and_clear <- function(politicians) {
  for (row in 1:nrow(politicians))
  {
    columnNumberOfFollower <- politicians[row, "node.trackings.edges"]
    unlistedColumn <- unlist(columnNumberOfFollower)
    if (length(unlistedColumn) == 0)
    {
      politicians[row, "NumberOfFollower"] <- as.integer(0)
    }
    else
    {
      politicians[row, "NumberOfFollower"] <- as.integer(unlistedColumn[1])
    }
  }
  
  politicians$node.trackings.edges <- NULL
  politicians <- filter(politicians, node.type == "Service::Twitter::Profile")
  politicians$node.type <- NULL
  names(politicians)[1] <- "ScreenName"
  names(politicians)[3] <- "NumberOfFollowers"
  politicians <- top_n(politicians, 50)
  return(politicians)
}

# Authentication 
app_name <- "BISentimentAnalysisEUVoting"
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

#Import politicians for each party
cdu_politicians <- jsonlite::fromJSON("Res/cdu_csu.json", flatten = TRUE)
cdu_politicians <- cdu_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
cdu_politicians$party <- "CDU"
cdu_politicians <- fill_No_of_follower_and_clear(cdu_politicians)

spd_politicians <- jsonlite::fromJSON("Res/spd.json", flatten = TRUE)
spd_politicians <- spd_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
spd_politicians$party <- "SPD"
spd_politicians <- fill_No_of_follower_and_clear(spd_politicians)

fdb_politicians <- jsonlite::fromJSON("Res/fdp.json", flatten = TRUE)
fdb_politicians <- fdb_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
fdb_politicians$party <- "FDP"
fdb_politicians <- fill_No_of_follower_and_clear(fdb_politicians)

gruene_politicians <- jsonlite::fromJSON("Res/gruene.json", flatten = TRUE)
gruene_politicians <- gruene_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
gruene_politicians$party <- "Gruene"
gruene_politicians <- fill_No_of_follower_and_clear(gruene_politicians)

afd_politicians <- jsonlite::fromJSON("Res/afd.json", flatten = TRUE)
afd_politicians <- afd_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
afd_politicians$party <- "AFD"
afd_politicians <- fill_No_of_follower_and_clear(afd_politicians)

linke_politicians <- jsonlite::fromJSON("Res/linke.json", flatten = TRUE)
linke_politicians <- linke_politicians[["data"]][["watchlist"]][["profiles"]][["edges"]]
linke_politicians$party <- "Linke"
linke_politicians <- fill_No_of_follower_and_clear(linke_politicians)

######################### API call of twitter to get the latest tweets ######################################

linke_politicians_tweets <- get_timelines(user = as.vector(linke_politicians[['ScreenName']]), n = 3200)
afd_politicians_tweets <- get_timelines(user = as.vector(afd_politicians[['ScreenName']]), n = 3200)
gruene_politicians_tweets <- get_timelines(user = as.vector(gruene_politicians[['ScreenName']]), n = 3200)
spd_politicians_tweets <- get_timelines(user = as.vector(spd_politicians[['ScreenName']]), n = 3200)
cdu_politicians_tweets <- get_timelines(user = as.vector(cdu_politicians[['ScreenName']]), n = 3200)
fdb_politicians_tweets <- get_timelines(user = as.vector(fdb_politicians[['ScreenName']]), n = 3200)

###############################Combining and filtering Tweets and politicians ###########################################
#Combine politicians to one dataframe
all_politicians <- do.call("rbind", list(linke_politicians, fdb_politicians, spd_politicians, cdu_politicians, gruene_politicians, afd_politicians))
#Combine Tweets to one dataframe
all_politicians_tweets <- do.call("rbind", list(cdu_politicians_tweets, spd_politicians_tweets, fdb_politicians_tweets, gruene_politicians_tweets, afd_politicians_tweets, linke_politicians_tweets))

#Filter tweets to election time
electioneering_beginning <- as.POSIXlt("2019-04-11 00:00")
electioneering_ending <- as.POSIXlt("2019-05-23 00:00")
all_politicians_tweets <- filter(all_politicians_tweets, created_at >= electioneering_beginning)
all_politicians_tweets <- filter(all_politicians_tweets, created_at <= electioneering_ending)

#Rename column to "ScreenName" for later join on column
names(all_politicians_tweets)[4] <- "ScreenName"

library("plyr")
#Join Politicians with Tweets to have Party as additional colum
all_politicians_tweets <- join(all_politicians_tweets, all_politicians, by="ScreenName")
#Count tweets by party
all_politicians_tweets  %>% count("party")

#Reduce Width of data for further analysis and safe them in RawData.csv
all_politicians_tweets <- select(all_politicians_tweets, "user_id", "status_id", "name", "created_at", "ScreenName", "text", "party", "NumberOfFollowers")
write.csv(all_politicians_tweets, "RawData.csv")
############################### Pre Processing ############################################################
#Load Stopwords
stopwords_german <- readLines("res/GermanStopWords.txt") 
#all words to lowcase
all_politicians_tweets$text <- tolower(all_politicians_tweets$text) 
#Remove Links
all_politicians_tweets$text <- gsub("http[[:alnum:][:punct:]]*", "", all_politicians_tweets$text) 
#Remove @Users
all_politicians_tweets$text <- gsub("@[[:alnum:][:punct:]]*", "", all_politicians_tweets$text) 
#Remove Stopwords
all_politicians_tweets$text <- removeWords(all_politicians_tweets$text, stopwords_german) 
#Remove Numbers
all_politicians_tweets$text <- removeNumbers(all_politicians_tweets$text) 
#Remove Punctuation
all_politicians_tweets$text <- removePunctuation(all_politicians_tweets$text)
#Remove all non UTF-8 characters
Encoding(all_politicians_tweets$text) <- "UTF-16" 
all_politicians_tweets$text <- stripWhitespace(all_politicians_tweets$text)
all_politicians_tweets <- filter(all_politicians_tweets, text != "")
all_politicians_tweets <- filter(all_politicians_tweets, text != " ")

#Lematization dictionary out of text
#Paste in one text array
lemma_text <- paste(all_politicians_tweets$text)
#Remove Unicodes
lemma_text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "",lemma_text)
#Split words
lemma_text <- strsplit(lemma_text, " ")
#Convert to Vector
lemma_text <- unlist(lemma_text)
#Remove empty elements
lemma_text <- lemma_text[lemma_text != ""]
#Generate lemma_dictionary
lemma_dictionary <- make_lemma_dictionary(lemma_text, lang="de_DE_neu")

#Replace Flexion with words out of lemma_dictionary
all_politicians_tweets$text <- lemmatize_strings(all_politicians_tweets$text, lemma_dictionary)

#Generate data.frame with one word each line
all_politicians_tweets <- unnest_tokens(all_politicians_tweets, input = "text", output = "words")

############################## Analyzing #######################################################
#Load sentiment words
sentiment_words <- c(
  readLines("res/SentiWS_v2.0_Positive.txt",
            encoding = "UTF-8"),
  readLines("res/SentiWS_v2.0_Negative.txt",
            encoding = "UTF-8")
) 

#Split sentiment_words to convert to list with dataframe elements
sentiment_words <- lapply(sentiment_words, function(x) {
  res <- strsplit(x, "\t", fixed = TRUE)[[1]]
  return(data.frame(words = res[1], value = res[2],
                    stringsAsFactors = FALSE))
})
#Convert list to data.frame
sentiment_words <- bind_rows(sentiment_words)
#Replace empty
sentiment_words$words <- gsub("\\|.*", "", sentiment_words$words)
#Convert Words to lowercase
sentiment_words$words <- tolower(sentiment_words$words)
#convert value to numeric value
sentiment_words$value <- as.numeric(sentiment_words$value)

#Join sentiment_words with tweets and filter
all_politicians_tweets_with_sentiment <- left_join(all_politicians_tweets, sentiment_words, by = "words")
#convert value to numeric value
all_politicians_tweets_with_sentiment$value <- as.numeric(all_politicians_tweets_with_sentiment$value)
#Filter to only get data with sentiment
all_politicians_tweets_with_sentiment <- filter(all_politicians_tweets_with_sentiment, !is.na(value))

#Detach plyr because group_by does not work otherwise
detach(package:plyr)

################# Sentiment by party ###################
all_politicians_tweets_with_sentiment %>%
  group_by(party) %>% 
  summarize(sentiment = mean(value))  %>% 
  ggplot(aes(x = party, y = sentiment, fill = party)) +
  geom_col() +
  scale_fill_manual(values = c("#9999CC","grey","yellow","green", "pink", "red")) +
  geom_text(aes(label = round(sentiment, 5), vjust=-0.15))

################ Sentiment by account #####################
#add column with name and party for Analytics
all_politicians_tweets_with_sentiment$Name_and_Party <- paste(all_politicians_tweets_with_sentiment$name, " (", all_politicians_tweets_with_sentiment$party, ") ")

############# Top 5 negative Sentiments ####################
all_politicians_tweets_with_sentiment %>%
  filter(NumberOfFollowers >= 20000) %>% 
  group_by(Name_and_Party) %>% 
  summarize(sentiment = mean(value))  %>%
  top_n(5, sentiment) %>%
  ggplot(aes(x = Name_and_Party, y = sentiment, fill = Name_and_Party)) +
  geom_col() +
  scale_fill_manual(values = c("black","black","red","red", "black")) +
  geom_text(aes(label = round(sentiment, 5), vjust=-0.15))

############# Top 5 negative Sentiments ####################
all_politicians_tweets_with_sentiment %>%
  filter(NumberOfFollowers >= 20000) %>% 
  group_by(Name_and_Party) %>% 
  summarize(sentiment = mean(value))  %>%
  top_n(-5, sentiment) %>%
  ggplot(aes(x = Name_and_Party, y = sentiment, fill = Name_and_Party)) +
  geom_col() +
  scale_fill_manual(values = c("#9999CC","green","Yellow","green", "green")) +
  geom_text(aes(label = round(sentiment, 5), vjust=-0.15))

########### Top -3 Words of CDU #################
all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "CDU")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("grey")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.1))


########### Top -5 Words of SPD #################
all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "SPD")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("red")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.15))

########### Top -5 Words of FDP #################
all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "FDP")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("yellow")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.15))

########### Top -5 Words of AfD #################
g2 <- all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "AFD")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("#9999CC")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.15))

########### Top -5 Words of Gruene #################
all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "Gruene")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("green")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.1))

########### Top -5 Words of Linke #################
g1 <- all_politicians_tweets_with_sentiment %>%
  group_by(words, party) %>% 
  summarize(sentiment = sum(value))%>% 
  ungroup %>%
  filter(party == "Linke")  %>%
  arrange(sentiment) %>%
  top_n(-3, sentiment) %>% 
  ggplot(aes(reorder(words, sentiment), sentiment, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Sentiment") +
  facet_wrap(~party, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("pink")) +
  geom_text(aes(label = round(sentiment, 2), hjust=-0.1))
