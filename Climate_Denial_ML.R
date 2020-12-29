## Use NLP techniques to assess this dataset
## and train machine learning for detecting
## the climate denial text

library(dplyr)
library(tidyr)
library(tidytext)
library(lexicon) # Use it for English dictionary

## Get and clean the dataset
path<-paste(getwd(),"/Datasets/tweet_climatechange.csv", sep="")
climate_data<-read.csv(path)
climate_data$existence<-as.character(climate_data$existence)
climate_data$tweet<-as.character(climate_data$tweet)
climate_data<-climate_data%>%filter(existence.confidence != "na")%>%
  filter(existence != "na")%>%filter(existence != "N/A")%>%
  mutate(binary=ifelse(existence == "Yes",0,ifelse(existence == 
  "Y",0,ifelse(existence == "No",1,ifelse(existence == "N",1,
  420)))))

## Clean up the natural language
climate_data$tweet<-gsub("\\[[^][]*]", "",
  climate_data$tweet) ##Remove [link]
climate_data$tweet<-gsub(" ?((f|ht)tp(s?)://|www)(.*)[.][a-z]+",
  "",climate_data$tweet) ##Remove url
climate_data$tweet<-gsub(" ?[RT?@]\\w+.", "", 
  climate_data$tweet) ##Remove RT @words
climate_data$tweet<-gsub("[[:punct:][:blank:]]+", 
  " ", climate_data$tweet)
climate_data$tweet<-tolower(climate_data$tweet) ##Decapitalize all strings

for(i in 1:length(climate_data$tweet)){
  separate<-strsplit(climate_data$tweet[i]," ")%>%
    as.data.frame(col.names = "wordlist")
  separate$wordlist<-as.character(separate$wordlist)
  separate<-separate%>%filter(wordlist %in% grady_augmented)
  climate_data$tweet[i]<-separate$wordlist%>%paste(collapse=" ")
}

##Create and use the neat dataset
write.csv(climate_data,paste(getwd(),"/Datasets/neatdataset.csv", sep=""))
path<-paste(getwd(),"/Datasets/neatdataset.csv", sep="")
climate_data<-read.csv(path)

##Vectorization
library(tm)
climate_data<-mutate(climate_data,tweet1 = tolower(enc2utf8(
  as.character(tweet))))
climate_data<-mutate(climate_data,tweet1=stemDocument(
  tweet1)) # remove word stem
climate_data$id <- 1:length(climate_data$tweet1) # Create the id
corpus = Corpus(VectorSource(climate_data$tweet1))
frequencies<-DocumentTermMatrix(corpus)

## Remove the words with a lot of zeroes
sparse = removeSparseTerms(frequencies, 0.99)

## Create the dataframe matrix of word vectors
climate_vector = as.data.frame(as.matrix(sparse))
colnames(climate_vector) = make.names(colnames(climate_vector))
climate_vector$id = climate_data$binary
climate_vector$id = as.factor(climate_vector$id)

## Build the classification machine learnings
library(caret)
library(randomForest)
library(naivebayes)

set.seed(345)
inTrain<-createDataPartition(y=climate_vector$id,p=.6,list=F)
training<-climate_vector[inTrain,] # Training Set (60% of dataset)
valandtest<-climate_vector[-inTrain,] # Validation and Testing

set.seed(345)
partition<-createDataPartition(y=valandtest$id,p=.5,list=F)
validation<-valandtest[partition,] # Validation Set (20% of dataset)
testing<-valandtest[-partition,] # Testing Set (20% of dataset)

random_forest<-randomForest(id~.,data=training,prox=T)
bayesian<-train(id~.,method='naive_bayes',data=training)
r_part<-train(id~.,method="rpart",data=training)
support_vector<-train(id~.,method="svmLinear",data=training)

## Evaluate the classification machine learnings
library(formattable)

rf_result<-confusionMatrix(predict(random_forest,validation),validation$id) ## random forest result
rf_sensitivity<-round(unlist(rf_result[4])[1],4)
rf_specficity<-round(unlist(rf_result[4])[2],4)
rf_accuracy<-round(unlist(rf_result[3])[1],4)

nb_result<-confusionMatrix(predict(bayesian,validation),validation$id) ## bayesian result
nb_sensitivity<-round(unlist(nb_result[4])[1],4)
nb_specficity<-round(unlist(nb_result[4])[2],4)
nb_accuracy<-round(unlist(nb_result[3])[1],4)

rp_result<-confusionMatrix(predict(r_part,validation),validation$id) # decision tree result
rp_sensitivity<-round(unlist(rp_result[4])[1],4)
rp_specficity<-round(unlist(rp_result[4])[2],4)
rp_accuracy<-round(unlist(rp_result[3])[1],4)

svm_result<-confusionMatrix(predict(support_vector,validation),validation$id) # svm result
svm_sensitivity<-round(unlist(svm_result[4])[1],4)
svm_specficity<-round(unlist(svm_result[4])[2],4)
svm_accuracy<-round(unlist(svm_result[3])[1],4)

a<-rbind("Random Forest", "Naive Bayesian", "Decision Tree",
  "Support Vector Machine")%>%as.data.frame()
b<-rbind(rf_sensitivity,nb_sensitivity,rp_sensitivity,svm_sensitivity)%>%
  as.data.frame()
c<-rbind(rf_specficity,nb_specficity,rp_specficity,svm_specficity)%>%
  as.data.frame()
d<-rbind(rf_accuracy,nb_accuracy,rp_accuracy,svm_accuracy)%>%
  as.data.frame()

classification<-bind_cols(a,b,c,d)
names(classification)<-c("Machine Learning",
  "Sensitivity","Specificity","Accuracy")
classification$`Machine Learning`<-as.character(
  classification$`Machine Learning`)
classification$Sensitivity<-as.character(classification$Sensitivity)%>%
  as.numeric()
classification$Specificity<-as.character(classification$Specificity)%>%
  as.numeric()
classification$Accuracy<-as.character(classification$Accuracy)%>%
  as.numeric()                                                          ## Build dataframe

formclassification<-formattable(classification, 
  align = c("l","c","c","c"),
  list(`Machine Learning` = formatter("span",style = ~style(color="grey",font.weight="bold")),
  area(col=2:4) ~ color_tile("#EFBDB8","#C2F9FC")))

## Choose random forest for climate denial detection
result<-confusionMatrix(predict(support_vector,testing),
  testing$id) # One time test, not more!
a<-rbind("Accuracy","Sensitivity","Specificity")%>%
  as.data.frame()
Accuracy<-round(unlist(result[3])[1],4)
Sensitivity<-round(unlist(result[4])[1],4)
Specificity<-round(unlist(result[4])[2],4)
b<-rbind(Accuracy,Sensitivity,Specificity)%>%
  as.data.frame()
test<-bind_cols(a,b)
names(test)<-c("Support Vector Machine","Result")
formattable(test, 
  align = c("l","c"),
  list(`Support Vector Machine` = formatter("span",style = ~style(color="grey",font.weight="bold")),
  area(col=2) ~ color_tile("#EFBDB8","#C2F9FC")))

## Build climate denial detection
climate_denial_detection<-function(x){
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(lexicon)
  library(tm)
  
  x<-tolower(enc2utf8(as.character(x)))
  x<-gsub("\\[[^][]*]", "",
          x) ##Remove [link]
  x<-gsub(" ?((f|ht)tp(s?)://|www)(.*)[.][a-z]+",
          "",x) ##Remove url
  x<-gsub(" ?[RT?@]\\w+.", "", 
          x) ##Remove RT @words
  x<-gsub("[[:punct:][:blank:]]+", 
          " ", x)
  
  word<-select(climate_vector,-id)%>%names()
  
  for(i in 1:length(x)){
    separate<-strsplit(x[i]," ")%>%
      as.data.frame(col.names = "wordlist")
    separate$wordlist<-as.character(separate$wordlist)
    separate<-separate%>%filter(wordlist %in% grady_augmented)%>%
      mutate(wordlist=stemDocument(
        wordlist))%>%filter(wordlist %in% word)
    x[i]<-separate$wordlist%>%paste(collapse=" ")
  }
  k<-1:length(word)%>%as.data.frame()%>%t()%>%as.data.frame()
  names(k)<-c(word)
  for(i in 1:length(x)){
    for(j in 1:length(word)){
      a[j]<-grepl(word[j],x[i])%>%as.numeric()%>%sum()
    }
    a<-a%>%as.data.frame()
    names(a)<-word
    k<-rbind(k,a)
  }
  k<-as.data.frame(k)
  names(k)<-c(word)
  k<-slice(k,2:(length(x)+1))
  finalanswer<-predict(random_forest,k)
  finalanswer
}

## Predict the climate denial language existed in tweet dataset
path<-paste(getwd(),"/Datasets/climatepolitical.csv",sep="")
politics<-read.csv(path)
classified<-climate_denial_detection(politics$text)%>%as.data.frame()
names(classified)<-"Potential_Climate_Denial"
politics<-bind_cols(politics,classified)
politics<-select(politics, id, name, screen_name,Potential_Climate_Denial)

write.csv(politics,paste(getwd(),"/Datasets/climatedenial.csv",sep=""))

## Create another plot
## Getting Data
barchartd<-paste(getwd(),"/Datasets/Bar_Chart.csv",sep="")
ready<-read.csv(barchartd)

ready$Sentiment_Level<-as.factor(ifelse(ready$Sentiment_Level=="true",1,-1))
ready<-arrange(ready,desc(Potential_Climate_Denial))
mydata <- head(ready,10)

plot_ly(mydata,x=~Potential_Climate_Denial,y=~Name,name=~Sentiment_Level,
  type="bar",marker = list(color = c("#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#C9D0C9",
  "#F66161")))%>%
  layout(title="<b>Highest Percentage of Potential Climate Denial Language</b>",
  titlefont = list(family = "Courier",size=16,color="#F2FFFB"),margin = 10, 
  font = list(family = "Courier",size=14),yaxis=list(
  categoryarray=~Sentiment_Level,
  categoryorder="array",title="",color="#F2FFFB"),xaxis=list(
  title="Percentage in decimal form",color="#F2FFFB"),showlegend=T,
  plot_bgcolor='#FFFFFF',paper_bgcolor="#77B09E",legend=list(
  font = list(color = "#F2FFFB")))

## Build the formattable for mean and standard deviation
column_name<-c("Category","Mean","Standard Deviation","Sample Size")
firstrow<-c("Control",-0.26,0.96,1418)
secondrow<-c("Treatment",-0.13,0.99,166)
thetable<-rbind(firstrow,secondrow)%>%as.data.frame()
rownames(thetable)<-NULL
names(thetable)<-column_name

formattable(thetable, 
  align = c("l","c","c","c"),
  list(`Category` = formatter("span",style = ~style(color="grey",font.weight="bold")),
  area(col=4) ~ color_tile("#EFBDB8","#C2F9FC")))

