library(tm)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(data.table)
library(stringr)
library(qdap)

set.seed(256)
Sys.setlocale(category = "LC_ALL", locale = "US")

## Load data and create data sample, unless already done so. Note that I use 
## absolute paths due to knitr restrictions. Kind of quick-and-dirty approach,
## but good enough for the assignment. Please adjust to your needs in case
## you want to run it on your computer.

path<-getwd()
url <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filename<- "SwiftkeyDB.zip"
filenameNpath<-file.path(path, filename)
if (!file.exists(path)) {dir.create(path)}
if (!file.exists(filenameNpath)){
        download.file(url, destfile = filenameNpath, mode = "wb")
        unzip(zipfile=filenameNpath, exdir="./Capstone")
}
if(length(list.files("C:/Users/claus/Documents/Capstone/final/en_US/sample"))<3){
        blogs <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.blogs.txt")
        news <- readLines(file("C:/Users/claus/Documents/Capstone/final/en_US/en_US.news.txt"))
        twitter <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.twitter.txt")
        
        blogs<-blogs[rbinom(length(blogs)* 0.05, length(blogs), 0.5)]
        news<-news[rbinom(length(news)*0.05, length(news), 0.5)]
        twitter<-twitter[rbinom(length(twitter)*0.05, length(twitter), 0.5)]
        
        write.csv(blogs, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleBlogs.txt", row.names=FALSE, col.names = FALSE)
        write.csv(news, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleNews.txt", row.names=FALSE, col.names = FALSE)
        write.csv(twitter, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleTwitter.txt", row.names=FALSE, col.names = FALSE)
}

if(length(list.files("C:/Users/claus/Documents/Capstone/DataScienceCapstone/FileStats"))<2){
        blogs <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.blogs.txt")
        news <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.News.txt")
        twitter <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.Twitter.txt")
        blogsSample <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleBlogs.txt")
        newsSample <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleNews.txt")
        twitterSample <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleTwitter.txt")

        ## Some file statistics (first original files, then sample files):

        fileStats <- matrix(c(1:9),ncol=3,byrow=TRUE)
        colnames(fileStats) <- c("Approx. File Size in MB","Lines", "Non-unique Words")
        rownames(fileStats) <- c("Blog","News","Twitter")
        fileStats[1,2]<-length(blogs)
        fileStats[2,2]<-length(news)
        fileStats[3,2]<-length(twitter)
        fileStats[1,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/en_US.blogs.txt")/1024^2)
        fileStats[2,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/en_US.news.txt")/1024^2)
        fileStats[3,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/en_US.twitter.txt")/1024^2)
        fileStats[1,3]<-sum(sapply(gregexpr("\\W+", blogs), length) + 1)
        fileStats[2,3]<-sum(sapply(gregexpr("\\W+", news), length) + 1)
        fileStats[3,3]<-sum(sapply(gregexpr("\\W+", twitter), length) + 1)
        write.csv(fileStats, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/FileStats/fileStats.txt", row.names=FALSE, col.names = FALSE)
        
        ## Removing the large temporary objects:

        rm(blogs, news, twitter)
        gc()

        fileStats[1,2]<-length(blogsSample)
        fileStats[2,2]<-length(newsSample)
        fileStats[3,2]<-length(twitterSample)
        fileStats[1,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleBlogs.txt")/1024^2)
        fileStats[2,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleNews.txt")/1024^2)
        fileStats[3,1]<-as.integer(file.size("C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleTwitter.txt")/1024^2)
        fileStats[1,3]<-sum(sapply(gregexpr("\\W+", blogsSample), length) + 1)
        fileStats[2,3]<-sum(sapply(gregexpr("\\W+", newsSample), length) + 1)
        fileStats[3,3]<-sum(sapply(gregexpr("\\W+", twitterSample), length) + 1)
        fileStats   
        write.csv(fileStats, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/FileStats/fileStatsSample.txt", row.names=FALSE, col.names = FALSE)
        ## Creating the corpus and removing more temporary objects:        

        rm(blogsSample, newsSample, twitterSample)
        gc()
}        
print("Stats of the original files (English only):")
fileStats<-read.csv("C:/Users/claus/Documents/Capstone/DataScienceCapstone/FileStats/fileStats.txt")
fileStats
print("Stats of the sample files (English only):")
fileStats<-read.csv("C:/Users/claus/Documents/Capstone/DataScienceCapstone/FileStats/fileStatsSample.txt")
fileStats

corpus<-VCorpus(DirSource("C:/Users/claus/Documents/Capstone/final/en_US/sample/"))

removeMostPunctuation<-function(text, keep=c("'", ";", "-"))
{
        m<-sub_holder(keep, text)
        m$unhold(strip(m$output))
}

## Now the real cleansing and transforming using standard tm functionlity;
## skipping some standard steps for now:

##      corpus<-tm_map(corpus, removePunctuation)
##      corpus<-tm_map(corpus, removeMostPunctuation)
##      corpus<-tm_map(corpus, removeWords, stopwords("english"))
##      corpus<-tm_map(corpus, removeWords, c("placeholder for bad words"))
##      corpus<-tm_map(corpus, content_transformer(tolower))
##      corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, stemDocument)

print("Summary corpus:")
str(corpus)

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

## Unique ngram statistics per tdm matrix:

tdmStats <- matrix(c(1:5),ncol=5,byrow=TRUE)
row.names(tdmStats) <- c("Unique ngrams:")
colnames(tdmStats) <- c("Unigram","Bigram","Trigram", "Quadgram", "Pentagram")
tdmStats[1,1]<-nrow(matrixTdm1)
tdmStats[1,2]<-nrow(matrixTdm2)
tdmStats[1,3]<-nrow(matrixTdm3)
tdmStats[1,4]<-nrow(matrixTdm4)
tdmStats[1,5]<-nrow(matrixTdm5)
tdmStats

## Preparing the graphical output:

output <- function(ngramMatrix){
        ngramFreq<-data.frame(Ngram=rownames(ngramMatrix), 
                              blogs=ngramMatrix[,1], 
                              news=ngramMatrix[,2], 
                              twitter=ngramMatrix[,3])
        ngramFreq["sum"]<- rowSums(ngramFreq[,2:4])
        ngramFreq<-ngramFreq[order(-ngramFreq$sum),]
        ngramFreqBlogs<-ngramFreq[order(-ngramFreq[,2]), c(1,2,5)]
        ngramFreqBlogs["Type"]=c("blogs")
        colnames(ngramFreqBlogs)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqNews<-ngramFreq[order(-ngramFreq[,3]), c(1,3,5)]
        ngramFreqNews["Type"]=c("news")
        colnames(ngramFreqNews)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqTwitter<-ngramFreq[order(-ngramFreq[,4]), c(1,4,5)]
        ngramFreqTwitter["Type"]=c("twitter")
        colnames(ngramFreqTwitter)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqRep<-rbind(ngramFreqBlogs, ngramFreqNews, ngramFreqTwitter)
        colnames(ngramFreqRep)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqRep<-ngramFreqRep[order(-ngramFreqRep[,3], ngramFreqRep[,1]),]
        
        wordcloud(ngramFreqRep[1:102,]$Ngram, ngramFreqRep[1:102,]$Frequency, colors=brewer.pal(6,"Dark2"))
        
        plot<-ggplot(ngramFreqRep[1:102,], aes(x= reorder(Ngram, Frequency), y=Frequency, fill=Type)) +
                theme(axis.text=element_text(size=12)) +
                scale_x_discrete(name="Ngram ordered by Frequency") +
                geom_bar(stat="identity") +
                coord_flip()
        plot
}

## And the final overview as highlight of the exploratory data analysis

output(matrixTdm1)
output(matrixTdm2)
output(matrixTdm3)
output(matrixTdm4)
output(matrixTdm5)