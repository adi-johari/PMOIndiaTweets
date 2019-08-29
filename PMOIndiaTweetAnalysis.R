#Read File
PMOTweets <- read.csv(file.choose(), header = T)

#Build corpus
library(tm)
corpus <- iconv(PMOTweets$Text, from = "utf-8", to= "ASCII//TRANSLIT")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean Data
corpus<- tm_map(corpus, tolower)
corpus<- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, removeNumbers)
cleanset<- tm_map(corpus, removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

removeN <- function(x) gsub('\\n', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeN))
inspect(cleanset[1:5])

removeS <- function(x) gsub('ã¢â‚¬â„¢s', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeS))

cleanset <- tm_map(cleanset, removeWords, c('will', 'since', 'see', 'meet', 'also', 'many', 'can', 'look', 'just', 'much', 'two', 'make', 'towards', 'called', 'talks', 'live', 'due', 'like', 'now', 'need', 'way', 'ago', 'tomorrow', 'let', 'working', 'want', 'earlier', 'met', 'day', 'help', 'sri', 'parts', 'get', 'done', 'lakh', 'days', 'may', 'came', 'never', 'see', 'meet', 'said', 'know', 'become' ))
cleanset <- tm_map(cleanset, removeWords, c('give', 'last', 'work', 'made', 'create', 'must', 'come', 'via', 'thoughts' ))
cleanset <- tm_map(cleanset, removeWords, c('lost', 'visit', 'lives', 'take', 'today', 'every', 'year', 'even', 'shri', 'worked','long', 'best', 'think', ))

cleanset <- tm_map(cleanset, gsub, pattern = 'narendramodi', replacement = 'modi')
cleanset <- tm_map(cleanset, gsub, pattern = 'narendramodis', replacement = 'modi')
cleanset <- tm_map(cleanset, gsub, pattern = 'namo', replacement = 'modi')
cleanset <- tm_map(cleanset, gsub, pattern = 'talks', replacement = 'ties')
cleanset <- tm_map(cleanset, gsub, pattern = 'coming', replacement = 'come')
cleanset <- tm_map(cleanset, gsub, pattern = 'uttar', replacement = 'uttarpradesh')
cleanset <- tm_map(cleanset, gsub, pattern = 'pradesh', replacement = 'uttarpradesh')
cleanset <- tm_map(cleanset, gsub, pattern = 'cleanliness', replacement = 'clean')
cleanset <- tm_map(cleanset, gsub, pattern = 'gandhi', replacement = 'bapu')
cleanset <- tm_map(cleanset, gsub, pattern = 'indias', replacement = 'india')
cleanset <- tm_map(cleanset, gsub, pattern = 'indians', replacement = 'india')
cleanset <- tm_map(cleanset, gsub, pattern = 'lok', replacement = 'loksabha')
cleanset <- tm_map(cleanset, gsub, pattern = 'sabha', replacement = 'loksabha')
cleanset <- tm_map(cleanset, gsub, pattern = 'modis', replacement = 'modi')
cleanset <- tm_map(cleanset, removeWords, c('modi' ))

cleanset<- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])


#Term Document Matrix
tdm<- TermDocumentMatrix(cleanset)
tdm
tdm<- as.matrix(tdm)
tdm[1:10, 1:20]

#BarPlot
w<- rowSums(tdm)
w<- subset(w, w>=15)
barplot(w, las=2, col= rainbow(50))

#Word Cloud
library(wordcloud)
w<- sort(rowSums(tdm), decreasing=TRUE)
set.seed(222)
wordcloud(words=names(w), freq=w, max.words = 100, random.order = F, min.freq = 5, colors=brewer.pal(8, 'Dark2'), scale=c(7, 0.3), rot.per = 0.3)

library(wordcloud2)
w<- data.frame(names(w), w)
colnames(w)<- c('word', 'freq')
wordcloud2(w, size=0.5, shape = 'circle', rotateRatio = 0.3, minSize = 1)


#Sentiment Analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

PMOTweets <- read.csv(file.choose(), header = T)
tweets<- iconv(PMOTweets$Text, from = "utf-8", to= "ASCII//TRANSLIT")

#Sentiment scores
s<- get_nrc_sentiment(tweets)

#bar plot
barplot(colSums(s), las=2, col=rainbow(10), ylab='Count', main='Sentiment Scores of PMOIndia tweets')

