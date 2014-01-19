# Talkin' baseball (on induction day)

# http://baseballwithr.wordpress.com/2014/01/20/talkin-baseball-on-induction-day

# required packages: tm, wordcloud, ggplot2
# speeches files available on this GitHub

# read speech file
speech = readLines("your/path/to/HOF/speeches/1939GeorgeHermanRuth.txt")

# load text mining package
library(tm)

# places speech in a Corpus
myCorpus = Corpus(VectorSource(speech))
#remove punctuation
myCorpus = tm_map(myCorpus, removePunctuation)

# length of speech
dtm = DocumentTermMatrix(myCorpus)
sum(dtm)

# prepare data.frame with filename, count
speechLength = data.frame(speech=character(0), words=numeric(0))
# read speeches file names
speechFiles = list.files(path="your/path/to/HOF/speeches/")

# loop through speech and count words
for(speechFile in speechFiles){
  speech = readLines(paste("your/path/to/HOF/speeches/", speechFile, sep=""))  
  myCorpus = Corpus(VectorSource(speech))
  myCorpus = tm_map(myCorpus, removePunctuation)
  dtm = DocumentTermMatrix(myCorpus)
  speechLine = data.frame(speech = speechFile, words=sum(dtm))
  speechLength = rbind(speechLength, speechLine)
}

# add speech year
speechLength$year = as.numeric(substr(speechLength$speech, 1, 4))

# plot it
library(ggplot2)

ggplot(data=speechLength, aes(x=year, y=words)) +
  geom_point() +
  geom_smooth()

######### words
speech = readLines("your/path/to/HOF/speeches/1971LeroyRobertPaige.txt")
myCorpus = Corpus(VectorSource(speech))

# set text to lower caps, remove punctuantion, numbers and stopwords
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

# package for wordclouds
library(wordcloud)

# generate wordcloud
set.seed(1234)
wordcloud(myCorpus, min.freq = 3, rot.per=0, scale=c(3,.3))
