#Read File
PMOTweets <- read.csv(file.choose(), header = T)

#Build corpus
library(tm)
corpus <- iconv(PMOTweets$Text, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
