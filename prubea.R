library(tm)
library('wordcloud')

setwd("C:/Users/User/Desktop/Lab5/DS-Lab-5-TP/Text Files")

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

url <- file.path("C:/Users/User/Desktop/Lab5/DS-Lab-5-TP/Text Files/")   
 
docs <- VCorpus(DirSource(url))   


## lower 
docs <- tm_map(docs, tolower)   
## removes punctuation
docs <- tm_map(docs,removePunctuation)   
## remove numbers 
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, PlainTextDocument)
##remove stopwrods
docs <- tm_map(docs, removeWords, stopwords("english"))   


removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

removeURL(docs)



dtm <- DocumentTermMatrix(docs)   
dtm

tdm <- TermDocumentMatrix(docs)   
tdm

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
wordcloud(names(freq), freq,max.words=100)   



freq_df <- function(tdm){
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_df <- data.frame(word=names(freq), freq=freq)
  return(freq_df)
}

total_n1 <- freq_df(tdm) # Total words before sparsity filter


unigram <- removeSparseTerms(tdm, 0.9999)
unigram_freq <- freq_df(unigram)

#### Cumulative sum
unigram_cs <- cumsum(unigram_freq["freq"])
unigram_cs_relative <- unigram_cs/sum(total_n1["freq"])


# How many words are we using now?
data.frame(Metric = c("Total words", "Non-sparse words", "Unique words", 
                      "Non-sparse unique words", "Words for 50 % of text", "Words for 90 % of text"),
           Value  = c(sum(total_n1["freq"]), sum(unigram_freq["freq"]), length(unlist(total_n1["word"])),
                      length(unlist(unigram_freq["word"])), sum(unigram_cs_relative < 0.5), sum(unigram_cs_relative < 0.9))
)


# Function to find bigrams in a corpus and tokenize them.
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# Function to find trigrams in a corpus and tokenize them.
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


# Extract bigrams from text into a term-document matrix
bigram <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer)), 0.9999)
bigram_freq <- freq_df(bigram)

# Extract trigrams from text into a term-document matrix
trigram <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer)), 0.9999)
trigram_freq <- freq_df(trigram)



# tokenize into tri-grams
trigram.twitterTdm <- tm::TermDocumentMatrix(text.corpus["twitter"], control = list(tokenize = TrigramTokenizer))
# put into data frame
freq.trigram.twitter <- data.frame(word = trigram.twitterTdm$dimnames$Terms, frequency = trigram.twitterTdm$v)
# reorder by descending frequency
freq.trigram.twitter <- plyr::arrange(freq.trigram.twitter, -frequency)
