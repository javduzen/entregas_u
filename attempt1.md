library(tidyverse)
library(readr)
library(qdap)
library(tm)
data <- read.csv(file='C:/Users/Javiera/Documents/sanguchez.csv',sep=";")
df <- data.frame(data)
# head(df)
#eliminar datos NA
#df2 <- na.omit(data)
#eliminar columnas irrelevantes
df3 <- subset(data, select = -c(texto, url, Local, Direccion, Precio))

head(df3)
#summary(df3) indica que hay 8 NA's en nota, se eliminan los NA
df3 <- df3[complete.cases(df3), ]
#summary(df3)
#guerdar datos solo de los que tienen nota 5 o 4
#QUIZAS CAMBIAR A SOLO 5
#df4 <- filter(df3, nota==5 | nota==4)
df4 <- filter(df3, nota==5)
#isolate ingredientes
ing_text <- df4$Ingredientes
#print(ing)

ing_source <- VectorSource(ing_text)
# Make a volatile corpus: tweets_corpus
#144 documents
myCorpus <- VCorpus(ing_source)
myCorpusCopy <- myCorpus

#CLEANING
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
 # corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  corpus <- tm_map(corpus, removeWords, "pan")
  return(corpus)
}
# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(myCorpus)

#clean_corp[[27]][1]
#df4$Ingredientes[27]  DIFERENCIA DEL TEXTO

corp_text <- as.data.frame(clean_corp, 30)

frequent_terms <- freq_terms(corp_text)
#plot(frequent_terms)
#Al visualizar los datos se pueden distinguir algunas fallas, 

#transformacion del queso
clean_corp <- tm_map(clean_corp, content_transformer(function(x) gsub("(queso).(\\w+)", "\\1\\2", x, ignore.case = TRUE)))
clean_corp <- tm_map(clean_corp, content_transformer(function(x) gsub("(salsa).(\\w+)", "\\1\\2", x, ignore.case = TRUE)))
#clean_corp <- tm_map(clean_corp, content_transformer(function(x) gsub("(cebolla).(\\w+)", "\\1\\2", x, ignore.case = TRUE)))
corp_text <- as.data.frame(clean_corp)
(frequent_terms <- freq_terms(corp_text,100))

#juegos de palabras
#queso, quesos
#quesomozarella y mozarella
#caramelizada y caramelizadas
#

#eliminar pan
corp_text <- as.data.frame(clean_corp)
frequent_terms <- freq_terms(corp_text,30)
plot(frequent_terms)
















