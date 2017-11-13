library(tm)
library(RWeka)
library(tidyr)
library(tidytext)
library(dplyr)

rm(list = ls())
folder <- "C:/Users/liduan/Documents/"
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
FivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

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
blogs.df.fivegram <- vector(mode = "list", length = n)
blogs.df.fourgram <- vector(mode = "list", length = n)
blogs.df.trigram <- vector(mode = "list", length = n)
blogs.df.bigram <- vector(mode = "list", length = n)

for (i in 1:n) {
  cp <- VCorpus(VectorSource(blogs[randomperm[((i-1)*chunk_size+1):min(i*chunk_size,length(randomperm))]])) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  # 5 gram
  blogs.df.fivegram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = FivegramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>%
    summarise(count = sum(count)) %>% 
    filter(count > 0)
  
  # 4 gram
  blogs.df.fourgram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = FourgramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 1)
  # 3 gram
  
  blogs.df.trigram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = TrigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 2)
  # 2 gram
  blogs.df.bigram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = BigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 3)
  
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
news.df.fivegram <- vector(mode = "list", length = n)
news.df.fourgram <- vector(mode = "list", length = n)
news.df.trigram <- vector(mode = "list", length = n)
news.df.bigram <- vector(mode = "list", length = n)

for (i in 1:n) {
  cp <- VCorpus(VectorSource(news[randomperm[((i-1)*chunk_size+1):min(i*chunk_size,length(randomperm))]])) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  # 5 gram
  news.df.fivegram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = FivegramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 0)
  # 4 gram
  news.df.fourgram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = FourgramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 1)
  # 3 gram
  news.df.trigram[[i]] <- cp %>% 
    TermDocumentMatrix(control=list(tokenize = TrigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 2)
  # 2 gram
  news.df.bigram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = BigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 3)
  
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
twitter.df.fivegram <- vector(mode = "list", length = n)
twitter.df.fourgram <- vector(mode = "list", length = n)
twitter.df.trigram <- vector(mode = "list", length = n)
twitter.df.bigram <- vector(mode = "list", length = n)

for (i in 1:n) {
  cp <- VCorpus(VectorSource(twitter[randomperm[((i-1)*chunk_size+1):min(i*chunk_size,length(randomperm))]])) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  # 5 gram
  twitter.df.fivegram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = FivegramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>%
    filter(count > 0)
  # 4 gram
  twitter.df.fourgram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = FourgramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>%
    filter(count > 1)
  # 3 gram
  twitter.df.trigram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = TrigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>%
    filter(count > 2)
  # 2 gram
  twitter.df.bigram[[i]] <- cp %>%
    TermDocumentMatrix(control=list(tokenize = BigramTokenizer)) %>%
    tidy() %>% 
    select(term, count) %>% 
    group_by(term) %>% 
    summarise(count = sum(count)) %>% 
    filter(count > 3)
}

print(Sys.time())

print("twitter stop")
print(Sys.time())

nBlogs <- length(blogs.df.bigram)
nNews <- length(news.df.bigram)
ntwitter <- length(twitter.df.bigram)

print("combine bigram")
# bigram
df.bigram <- news.df.bigram[[1]]
for (i in 1:nBlogs) {
  df.bigram <- blogs.df.bigram[[i]] %>%
    rbind(df.bigram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

for (i in 1:ntwitter) {
  df.bigram <- twitter.df.bigram[[i]] %>%
    rbind(df.bigram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

print("combine trigram")
# trigram
df.trigram <- news.df.trigram[[1]]
for (i in 1:nBlogs) {
  df.trigram <- blogs.df.trigram[[i]] %>%
    rbind(df.trigram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

for (i in 1:ntwitter) {
  df.trigram <- twitter.df.trigram[[i]] %>%
    rbind(df.trigram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

print("combine fourgram")
# fourgram
df.fourgram <- news.df.fourgram[[1]]
for (i in 1:nBlogs) {
  df.fourgram <- blogs.df.fourgram[[i]] %>%
    rbind(df.fourgram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

for (i in 1:ntwitter) {
  df.fourgram <- twitter.df.fourgram[[i]] %>%
    rbind(df.fourgram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

print("combine fivegram")
# fivegram
df.fivegram <- news.df.fivegram[[1]]
for (i in 1:nBlogs) {
  df.fivegram <- blogs.df.fivegram[[i]] %>%
    rbind(df.fivegram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

for (i in 1:ntwitter) {
  df.fivegram <- twitter.df.fivegram[[i]] %>%
    rbind(df.fivegram) %>%
    group_by(term) %>%
    summarise(count = sum(count))
}

save(df.bigram, df.trigram, df.fourgram, df.fivegram, file = "ngrams_df_1111.RData")
bigramterms <- separate(df.bigram, term, into = c("term1", "term2"), sep = " ")
trigramterms <- separate(df.trigram, term, into = c("term1", "term2", "term3"), sep = " ")
fourgramterms <- separate(df.fourgram, term, into = c("term1", "term2", "term3", "term4"), sep = " ")
fivegramterms <- separate(df.fivegram, term, into = c("term1", "term2", "term3", "term4", "term5"), sep = " ")

save(bigramterms, trigramterms, fourgramterms, fivegramterms, file = "ngrams_df_1111_sep.RData")
