## Use sentimentr package to determine the aggregated 
## sentiment in tweet csv dataset
library(dplyr)
library(sentimentr)

path<-paste(getwd(),"/Datasets/tweet.csv",sep="")
tweet<-read.csv(path, header = T)

tweet$text<-as.character(tweet$text)
## Score the sentiment for every text 
## and create the new column called scoring
tweet1<-mutate(tweet, scoring = sentiment_by(text)$ave_sentiment)
tweet2<-select(tweet1,id,scoring)

path1<-paste(getwd(),"/Datasets/tweetsentiment.csv",sep="")
write.csv(tweet2,path1)
