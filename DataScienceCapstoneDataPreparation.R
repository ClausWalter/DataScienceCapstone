library(tm)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(data.table)
library(stringr)

set.seed(256)
Sys.setlocale(category = "LC_ALL", locale = "US")

corpus<-VCorpus(DirSource("C:/Users/claus/Documents/Capstone/final/en_US/sample/"))

## Now the real cleansing and transforming using standard tm functionlity;
## skipping some standard steps for now:

##        corpus<-tm_map(corpus, removePunctuation)
##        textCorpus<-tm_map(corpus, removeWords, stopwords("english"))
##        corpus<-tm_map(corpus, removeWords, c("placeholder for bad words"))
corpus<-tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, stemDocument)

tdm <- function(corpus, n){
        ngramTokens <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokens))
        tdm
}

## Calculating the n-grams and saving as csv files for later use in text prediction

matrixTdm1<-as.matrix(tdm(corpus,1))
write.table(matrixTdm1, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm1.csv", sep=";", col.names=TRUE)
matrixTdm2<-as.matrix(tdm(corpus,2))
write.table(matrixTdm2, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm2.csv", sep=";", col.names=TRUE)
matrixTdm3<-as.matrix(tdm(corpus,3))
write.table(matrixTdm3, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm3.csv", sep=";", col.names=TRUE)
matrixTdm4<-as.matrix(tdm(corpus,4))
write.table(matrixTdm4, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm4.csv", sep=";", col.names=TRUE)
matrixTdm5<-as.matrix(tdm(corpus,5))
write.table(matrixTdm5, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm5.csv", sep=";", col.names=TRUE)

## For the later string split to predict values based on input:
temp<-c("1")
temp<-as.data.frame(temp)
dataframeTdm2 <- as.data.frame(matrixTdm2)
setDT(dataframeTdm2, keep.rownames = TRUE)
for (i in 1:6){temp[i,1]<-str_replace_all(temp[i,1], "[[^a-zA-Z0-9]]", " ")}
temp2<-str_split_fixed(temp$rn, " ", 2)
temp3<-cbind(temp, temp2)