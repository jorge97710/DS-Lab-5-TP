twitterdata <- readLines("en_US.twitter.txt",skipNul = TRUE) 
blogdata <- readLines("en_US.blogs.txt", skipNul = TRUE) 
noticiasdata <- file("en_US.news.txt", 'rb') 
noticias <- readLines(noticiasdata)  #newsdata 
close(noticiasdata)
rm(noticiasdata) 
library("tm")
library(dplyr)
#limpieza
twitterdata <- Corpus(VectorSource(twitterdata))                     
twitterdata = tm_map(twitterdata, content_transformer(tolower))       
twitterdata = tm_map(twitterdata, removePunctuation)                 
twitterdata <- tm_map(twitterdata, stripWhitespace)                   
matrizdocumentacion <- TermDocumentMatrix(twitterdata)                               

#se repite lo mismo para cada uno de los casos 
value1<-matrizdocumentacion[sample(1:matrizdocumentacion$nrow,0.5*matrizdocumentacion$nrow),]
value2<- as.matrix(value1)
value3 <- sort(rowSums(value2),decreasing=TRUE)
d <- data.frame(word = names(value3),freq=value3)

findFreqTerms(matrizdocumentacion, lowfreq = 4)

#frecuencia de palabras
hist(d[1:50,]$freq, las = 2,  main ="en_US.twitter.txt")
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, main ="en_US.twitter.txt")
#blogs
blogdata <- Corpus(VectorSource(blogdata))                      
blogdata = tm_map(blogdata, content_transformer(tolower))       
blogdata = tm_map(blogdata, removePunctuation)                  
blogdata = tm_map(blogdata, removeWords, stopwords("english"))  
blogdata <- tm_map(blogdata, stripWhitespace)                   
matrizdocumentacion <- TermDocumentMatrix(blogdata)   

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, main ="en_US.blogs")
hist(d[1:50,]$freq, las = 2,  main ="en_US.blogs")
#noticias
noticias <- Corpus(VectorSource(noticias))                      
noticias = tm_map(noticias, content_transformer(tolower))       
noticias = tm_map(noticias, removePunctuation)                  
noticias = tm_map(noticias, removeWords, stopwords("english"))  
noticias <- tm_map(noticias, stripWhitespace)                   
matrizdocumentacion <- TermDocumentMatrix(noticias)                               

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, main ="en_US.news")
hist(d[1:50,]$freq, las = 2,  main ="en_US.news")
#prediccion
predicciondetexto <- function(textoobtenido, thecorpus = thebigcorpus, minimum.n = 2, npredobtenidas = 3){
  ngrama <- unnest_tokens(corpus, ngram, 
                               corpus,
                               token = "ngrams",
                               n = minimum.n + 1)
  
  palabras_clave <- ultimaspalabras(workingtext, minimum.n) 
  palabras_clave <- paste0("^",palabras_clave, collapse = "") 
  ngrama <- filter(ngrama, grepl(palabras_clave,ngram)) 
  tablaprediccion <- sort(table(sapply(ngrama$ngram,ultimaspalabras, USE.NAMES = FALSE)), decreasing = TRUE)
  tablaprediccion <- round(tablaprediccion / sum(tablaprediccion),2) 
  if(length(tablaprediccion) < npredobtenidas){  
    npredobtenidas <- length(tablaprediccion)}
  return(tablaprediccion[1:npredobtenidas])
  
}