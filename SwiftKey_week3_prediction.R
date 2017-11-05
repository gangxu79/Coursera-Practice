library(tm)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tidytext)
library(tidyr)
library(tidytext)
library(dplyr)

folder <- "C:/Users/stran/Dropbox/Work/Programming/R/Coursera/Data Specialization/Course 10 Capstone/"
setwd(folder)
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#download.file(url = url, destfile = "Coursera-SwiftKey.zip")
#unzip(zipfile = "Coursera-SwiftKey.zip", exdir = "./data")

# text folder
folder <- "./data/final/en_US/"

# read all lines
en_US.blogs <- readLines(con = "./data/final/en_US/en_US.blogs.txt", skipNul = TRUE)
en_US.news <- readLines(con = "./data/final/en_US/en_US.news.txt", skipNul = TRUE)
en_US.twitter <- readLines(con = "./data/final/en_US/en_US.twitter.txt", skipNul = TRUE)

# define parameters
chunk_size <- 100000

# preprocessing
# remove non-English word
filter_char <- function (text) {
    splittext <- unlist(strsplit(text, split = " "))
    nonassic_idx <- which(is.na(iconv(splittext, "latin1", "ASCII")))
    if (length(nonassic_idx) > 0) {
        converted <- paste(splittext[-nonassic_idx], collapse = " ")
    } else {
        converted <- text
    }
    return(converted)
}
# unique term with sum of count
uniqueterm <- function (x) {
    u <- distinct(x)
    for (i in 1:nrow(u)) {
        u$count[i] <- sum(x$count[which(x$term==u$term[i])])
    }
    return(u)
}
# remove URL
removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
# remove hash tag
removeHashTags <- function(x) gsub("#\\S+", "", x)
# remove twitter handles
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)
# remove documents less than 4 words

# define n-gram tokenizer
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# blogs
blogs <- sapply(en_US.blogs, filter_char)
names(blogs) <- NULL
blogs <- removeURL(blogs)
blogs <- removeHashTags(blogs)
blogs <- removeTwitterHandles(blogs)

print("blogs start")
print(Sys.time())
randomperm <- sample(1:length(blogs))
n <- ceiling(length(blogs)/chunk_size)
blogs.df.fourgram <- vector(mode = "list", length = n)
blogs.df.trigram <- vector(mode = "list", length = n)
blogs.df.bigram <- vector(mode = "list", length = n)
for (i in 1:n) {
    cp <- VCorpus(VectorSource(blogs[randomperm[((i-1)*chunk_size+1):min(length(randomperm),(i*chunk_size))]]))
    cp <- tm_map(cp, removePunctuation)
    cp <- tm_map(cp, removeNumbers)
    cp <- tm_map(cp, content_transformer(tolower))
    # corpus.blogs[[i]] <- tm_map(corpus.blogs[[i]], removeWords, stopwords('english'))
    cp <- tm_map(cp, stripWhitespace)
    cp <- tm_map(cp, PlainTextDocument)
    
    # 4 gram
    fourgram <- TermDocumentMatrix(cp, control=list(tokenize = FourgramTokenizer))
    blogs.df.fourgram[[i]] <- tidy(fourgram) %>% select(term, count) %>% filter(count > 1)
    # 3 gram
    trigram <- TermDocumentMatrix(cp, control=list(tokenize = TrigramTokenizer))
    blogs.df.trigram[[i]] <- tidy(trigram) %>% select(term, count) %>% filter(count > 1)
    # 2 gram
    bigram <- TermDocumentMatrix(cp, control=list(tokenize = BigramTokenizer))
    blogs.df.bigram[[i]] <- tidy(bigram) %>% select(term, count) %>% uniqueterm() %>% filter(count > 1)
    
    print(Sys.time())
}
print("blogs stop")
print(Sys.time())

# news
news <- sapply(en_US.news, filter_char)
names(news) <- NULL
news <- removeURL(news)
news <- removeHashTags(news)
news <- removeTwitterHandles(news)

print("news start")
print(Sys.time())
randomperm <- sample(1:length(news))
n <- ceiling(length(news)/chunk_size)
news.df.fourgram <- vector(mode = "list", length = n)
news.df.trigram <- vector(mode = "list", length = n)
news.df.bigram <- vector(mode = "list", length = n)
for (i in 1:n) {
    cp <- VCorpus(VectorSource(news[randomperm[((i-1)*chunk_size+1):min(length(randomperm),(i*chunk_size))]]))
    cp <- tm_map(cp, removePunctuation)
    cp <- tm_map(cp, removeNumbers)
    cp <- tm_map(cp, content_transformer(tolower))
    # corpus.blogs[[i]] <- tm_map(corpus.blogs[[i]], removeWords, stopwords('english'))
    cp <- tm_map(cp, stripWhitespace)
    cp <- tm_map(cp, PlainTextDocument)
    
    # 4 gram
    fourgram <- TermDocumentMatrix(cp, control=list(tokenize = FourgramTokenizer))
    news.df.fourgram[[i]] <- tidy(fourgram) %>% select(term, count) %>% filter(count > 1)
    # 3 gram
    trigram <- TermDocumentMatrix(cp, control=list(tokenize = TrigramTokenizer))
    news.df.trigram[[i]] <- tidy(trigram) %>% select(term, count) %>% filter(count > 1)
    # 2 gram
    bigram <- TermDocumentMatrix(cp, control=list(tokenize = BigramTokenizer))
    news.df.bigram[[i]] <- tidy(bigram) %>% select(term, count) %>% uniqueterm() %>% filter(count > 1)
    
    print(Sys.time())
}
print("news stop")
print(Sys.time())


# twitter
twitter <- sapply(en_US.twitter, filter_char)
names(twitter) <- NULL
twitter <- removeURL(twitter)
twitter <- removeHashTags(twitter)
twitter <- removeTwitterHandles(twitter)

print("twitter start")
print(Sys.time())
randomperm <- sample(1:length(twitter))
n <- ceiling(length(twitter)/chunk_size)
twitter.df.fourgram <- vector(mode = "list", length = n)
twitter.df.trigram <- vector(mode = "list", length = n)
twitter.df.bigram <- vector(mode = "list", length = n)
for (i in 1:n) {
    cp <- VCorpus(VectorSource(twitter[randomperm[((i-1)*chunk_size+1):min(length(randomperm),(i*chunk_size))]]))
    cp <- tm_map(cp, removePunctuation)
    cp <- tm_map(cp, removeNumbers)
    cp <- tm_map(cp, content_transformer(tolower))
    # corpus.blogs[[i]] <- tm_map(corpus.blogs[[i]], removeWords, stopwords('english'))
    cp <- tm_map(cp, stripWhitespace)
    cp <- tm_map(cp, PlainTextDocument)
    
    # 4 gram
    fourgram <- TermDocumentMatrix(cp, control=list(tokenize = FourgramTokenizer))
    twitter.df.fourgram[[i]] <- tidy(fourgram) %>% select(term, count) %>% filter(count > 1)
    # 3 gram
    trigram <- TermDocumentMatrix(cp, control=list(tokenize = TrigramTokenizer))
    twitter.df.trigram[[i]] <- tidy(trigram) %>% select(term, count) %>% filter(count > 1)
    # 2 gram
    bigram <- TermDocumentMatrix(cp, control=list(tokenize = BigramTokenizer))
    twitter.df.bigram[[i]] <- tidy(bigram) %>% select(term, count) %>% uniqueterm() %>% filter(count > 1)
    
    print(Sys.time())
}
print("twitter stop")
print(Sys.time())

