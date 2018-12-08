#Importing the libraries
library("plyr")
library("stringr")
library("tm")
library("SnowballC")
library("lda")
library("LDAvis")

setwd("C:/Users/harsh/Google Drive/Fall'18/GEOG 594/Assignments/Ass8")


#load Twitter data sample
Twitter<-read.csv("TwitterSample.csv")

Tweets_corpus <- Corpus(VectorSource(Twitter$TWEET_TEXT))


Tweets_corpus <- tm_map(Tweets_corpus, tolower)

# remove punctuation
Tweets_corpus <- tm_map(Tweets_corpus, removePunctuation)
# remove numbers
Tweets_corpus <- tm_map(Tweets_corpus, removeNumbers)


# remove URLs
Tweets_corpus <- tm_map(Tweets_corpus, function(x) gsub("http[[:alnum:]]*","", x))
# remove NonASCII characters
Tweets_corpus <- tm_map(Tweets_corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))

# remove stopwords
Tweets_corpus <- tm_map(Tweets_corpus, removeWords,stopwords("SMART"))

Tweets_corpus <- tm_map(Tweets_corpus, removeWords,c("london", "im","ive", "dont", "didnt"))

Tweets_corpus <- tm_map(Tweets_corpus, stripWhitespace)

Tweets_corpus <- tm_map(Tweets_corpus, PlainTextDocument)

Tweets_corpus <- tm_map(Tweets_corpus, stemDocument)

# unlist the text corpus
Tweet_Clean<-as.data.frame(unlist(sapply(Tweets_corpus[[1]]$content,'[')), stringsAsFactors=F)
# remove extra whitespace in text
Tweet_Clean <- lapply(Tweet_Clean[,1], function(x) gsub("^ ", "", x)) #multiple spaces 
Tweet_Clean <- lapply(Tweet_Clean, function(x) gsub("^[[:space:]]+", "", x)) #space at the begining 
Tweet_Clean <- lapply(Tweet_Clean, function(x) gsub("[[:space:]]+$", "", x)) #space at the end

# bind clean text with Twitter data
Twitter$Tweet_Clean<-Tweet_Clean
# check the first 10 Tweets
Twitter[1:10,]

doc.list <- strsplit(unlist(Tweet_Clean), "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 3 times:

term.table <- term.table[term.table>3]

vocab <- names(term.table)

# put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

# Compute some statistics related to the data set: D <- length(documents) # number of documents
W <- length(vocab) # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document N <- sum(doc.length) # total number of tokens in the data
term.frequency <- as.integer(term.table) # frequencies of terms in the corpus
documents <- lapply(doc.list, get.terms)


### fit LDA model
# parameters
K <- 20
G <- 1000
alpha <- 0.1
eta <- 0.1
t1 <- print(Sys.time())
lda_fit <- lda.collapsed.gibbs.sampler (documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta)
t2 <- print(Sys.time())
t2-t1

top_words<-top.topic.words(lda_fit$topics,20,by.score=TRUE)

theta <- t(apply(lda_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(lda_fit$topics) + eta, 2, function(x) x/sum(x)))

Tweet_Topics <- list(phi = phi, theta = theta, doc.length = doc.length, vocab = vocab, term.frequency = term.frequency)

Tweet_Topics_json <- with(Tweet_Topics,createJSON(phi, theta, doc.length, vocab, term.frequency))

serVis(Tweet_Topics_json)

doc_topic <- apply(lda_fit$document_sums, 2, function(x) which(x == max(x))[1])
Twitter$topic<-doc_topic


TwitterDf <- data.frame(lapply(Twitter, as.character), stringsAsFactors=FALSE)

write.csv(TwitterDf,"TwitterWithTopic.csv")
