#Read File
PMOTweets <- read.csv(file.choose(), header = T)

#Build corpus
library(tm)
corpus <- iconv(PMOTweets$Text, to = "utf-8")
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
w




