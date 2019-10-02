library(tm)
library('wordcloud')

setwd("C:/Users/User/Desktop/Prueba/ejemplo")

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

url <- file.path("C:/Users/User/Desktop/Prueba/ejemplo")   

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
removeHashTags(docs)
removeTwitterHandles(docs)



dtm <- DocumentTermMatrix(docs)   
dtm

tdm <- TermDocumentMatrix(docs)   
tdm


library(ggplot2)
##Nos crea una tabla con todas las palabras y que tan frecuentes estan 
freq.docs <- data.frame(word = tdm$dimnames$Terms, frequency = tdm$v)
n<- 25L
top <- freq.docs[1:n, ]

top$word <- reorder(top$word, top$frequency)
g.top <- ggplot(top, aes(x = word, y = frequency))
g.top <- g.top + geom_bar(stat = "identity") + coord_flip() +
  labs(title = "Most Frequent: Blog")
g.top

##Crea una imagen con las palabras mas utilizadas
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
wordcloud(names(freq), freq,max.words=100)   


## extraido de https://rpubs.com/SViaene/TextPredictionReport1
# Function to find bigrams in a corpus and tokenize them.
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# Function to find trigrams in a corpus and tokenize them.
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

bigram <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer)), 0.9999)
bigram_freq <- freq_df(bigram)
trigram <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer)), 0.9999)
trigram_freq <- freq_df(trigram)



plot <- ggplot(x=trigram_freq$word, y= trigram_freq$freq)
plot <- plot + geom_bar(stat = "identity") + coord_flip() +
  labs(title = "trigama")
plot


