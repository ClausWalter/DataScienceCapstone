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

removeMostPunctuation<-function(text, keep=c("'", ";", "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
{
        m<-sub_holder(keep, text)
        m$unhold(strip(m$output))
}

temp<-readLines("C:/Users/claus/Documents/Capstone/DataScienceCapstone/testfile.csv")
head(temp)
temp2<-removeMostPunctuation(temp[1:15])
temp2


## removeMostPunctuation("Whatever it's @# §- worth")