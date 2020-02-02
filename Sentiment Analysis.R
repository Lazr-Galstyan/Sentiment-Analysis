# Download the necessary packages

install.packages("igraph")
install.packages("network")
install.packages("intergraph")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("NLP")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("dplyr")
install.packages("purrr")
install.packages("SnowballC")
library(igraph) 
library(network) 
library(intergraph) 
library(twitteR)
library(ROAuth)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(purrr)
library(SnowballC)

# Insert the Values
api_key <-'3w8phVPn1kBLtqPR5OZIBZy4g'
api_secret <- 'OX7N6SOJOboCRWTCMPFHCYnPkTMzcyEwYfAua0d9h9jYx7EaTk'
access_token <- '1181305107851296768-w5GLqJmPYeG6mONPNasI4rIKUCJRwI'
access_token_secret <-'jfQLPV2jaxogHzd0bctJkmd8pdb0oMqYpoyAXUCEu7fMf'

setup_twitter_oauth(api_key,
                    api_secret,
                    access_token,
                    access_token_secret)
1
# Select the tweets
tweets <- searchTwitter("#Huawei", n = 1200, lang = 'en')
tweets

# Covnert lists into Data frame
Huawei <- twListToDF(tweets)

# Create CSV file and save it
write.csv(Huawei, file = '~/Desktop/huawei.csv', row.names= F)
# Reading Data File
Huawei <- read.csv("Huawei.csv")

# Look through the dataset
names(Huawei)
head(Huawei)
dim(Huawei)
summary(Huawei)
str(Huawei)

# Clean the text of special characters such as symbols and emoticons
Huawei$text <- sapply(Huawei$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
head(Huawei$text)


# Building Courpus
corpus <-iconv(Huawei$text, to='utf-8') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five tweets

### Cleaning the data
# Convert to lowercase
corpus <-tm_map(corpus, tolower) #convert all letters to lower case
inspect(corpus[1:5]) #inspect the first five tweets

# Remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five tweets

# Remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five tweets

# Look at the stopwords
stopwords('english')

# Select stopwords(english) to see what words are removed
# Remove stopwords
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])


# Remove URLs (https://etc.); make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x) # Make a function to remove the http:...
cleanset <-tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

# Tweets were pulled using Huawei or Huaweii so we can clean it from the text
cleanset <-tm_map(cleanset, removeWords, c('huawei', 'huaweii', 'huaw', 'huaweis','huaweifacts','huaweieurope',
                                           'huaweiindia'))
inspect(cleanset[1:10])

# Remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:100])

# stemming (remove variations from words)
cleanset <- tm_map(cleanset, stemDocument)
inspect(cleanset[1:100])

# Create matrix of rows and columns
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:20,1:20]
# Calculate how many time a word appears
Number <- rowSums(tdm)
# Select the ones that appear more than 30 times
Number30 <- subset(Number,Number>=30)
barplot(Number30, las = 2, col = rainbow(40))
names(Number30)

# We see that there are several stopwords that we need to remove e.g. "will"
cleanset <-tm_map(cleanset, removeWords, c(stopwords('english'), "will", "dont", "got", "says", "asked", "towards",
                                           "much", "like", "build", "intelligent","connect", "world",
                                           "one","two", "three", "fulli", "first", "put", "can", "wasn't", "get"))

# After adding a stopword, we run the codes again to see what other stopwords we have
# We also remove "building", "intelligent", "connected", and "world" words, becasue that is the slogan of the company
# In total we removed 17 other stopwords

# Create matrix of rows and columns
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:20,1:20]

# Calculate how many time a word appears
Number <- rowSums(tdm)
# Select the ones that appear more than 30 times
Number30 <- subset(Number,Number>=30)
barplot(Number30, las = 2, col = rainbow(40))
names(Number30)

# Create Word Clouds
Number30 <- sort(rowSums(tdm),
                decreasing=TRUE)
set.seed(9999)
wordcloud(words = names(Number30),
          freq=Number30,
          random.order =FALSE)


# Colored Wordclouds
wordcloud(words = names(Number30), 
          freq=Number30,
          random.order=FALSE,
          max.words= 200, 
          min.freq= 5,
          colors = brewer.pal(8, 'Dark2'), 
          scale = c(3, 0.2), 
          rot.per= .3) 


# Get sentiment scores for each of the tweet
tweetaftermap <- purrr::map_chr(tweets, function(x) x $text)
sentiment <- get_nrc_sentiment(tweetaftermap)

# Make a barplot for the sentiments
barplot(colSums(sentiment),
        las = 2,
        ylab = "Total Count",
        main = "Sentiment Scores for Huawei Tweets")


## Network Graphs: SNA
tdm[1:20, 1:20]
tdm[tdm>1] < -1
# whenever our tdm value is more than 1 for a tweet we convert into 1 
# we only need that the term appeared -frequency of terms is not required

termM <- tdm %*% t(tdm) #transpose of tdm matrix; create tweet adjacency matrix using %*%
termM[1:10,1:10]  #term term matrix, alerts appeared in 8 tweets, alerts and nflx appeared in 3 tweets


h.adj <- graph.adjacency(termM, weighted=T, mode ='undirected') 
#convert it into graph, no direction for edges
plot(h.adj)

#remove terms that have loops (going to self) 
h.adj <- simplify(h.adj)


#set labels and degrees of Vertices (V), each word is a vertices
V(h.adj)$label <- V(h.adj)$name #label is name
V(h.adj)$label

V(h.adj)$degree <- degree(h.adj)
V(h.adj)$degree

#Histogram of node degree, use 100 bars (too many words), label of y and x axis
hist(V(h.adj)$degree,
     breaks=100,
     col='green',
     main ='histogram of node degree',
     ylab ='frequency',
     xlab='degree of vertices')

set.seed(9999)
plot(h.adj)


# Visualize words/terms
tdm <- tdm[rowSums(tdm)>50,] ## And run all the codes above again
#include only terms having frequency more than 30 and rerun plot

# Let's run it again
tdm[1:20, 1:20]
tdm[tdm>1] < -1
# whenever our tdm value is more than 1 for a tweet we convert into 1 
# we only need that the term appeared -frequency of terms is not required

termM <- tdm %*% t(tdm) #transpose of tdm matrix; create tweet adjacency matrix using %*%
termM[1:10,1:10]  #term term matrix, alerts appeared in 8 tweets, alerts and nflx appeared in 3 tweets


h.adj <- graph.adjacency(termM, weighted=T, mode ='undirected') 
#convert it into graph, no direction for edges
plot(h.adj)

#remove terms that have loops (going to self) 
h.adj <- igraph::simplify(h.adj)


#set labels and degrees of Vertices (V), each word is a vertices
V(h.adj)$label <- V(h.adj)$name #label is name
V(h.adj)$label

V(h.adj)$degree <- degree(h.adj)
V(h.adj)$degree

#Histogram of node degree, use 100 bars (too many words), label of y and x axis
hist(V(h.adj)$degree,
     breaks=100,
     col='green',
     main ='histogram of node degree',
     ylab ='frequency',
     xlab='degree of vertices')

set.seed(9999)
plot(h.adj)

# Communities
comm <- cluster_edge_betweenness(h.adj)
plot(comm, h.adj)

prop <-cluster_label_prop(h.adj)
plot(prop, h.adj)
# Label propagation

greed <-cluster_fast_greedy(as.undirected(h.adj)) #greedy algorithm for clustering
plot(greed, as.undirected(h.adj))

V(h.adj)$degree <- degree(h.adj)
h.adj <- igraph::simplify(h.adj)

# Show Vertices with Tweets
V(h.adj)$label <- V(h.adj)$name
V(h.adj)$label.cex <- 1
V(h.adj)$label.color <- rgb(0.4, 0,0,0.7)
V(h.adj)$size <- 2
V(h.adj)$frome.colr <- NA
plot(h.adj, vertex.label = NA, vertex.size = 5)


