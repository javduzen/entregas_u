---
title: "Proyecto 1: Analisis Sanguchez"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
```
En el siguiente reporte se utilizará la base de datos sanguchez.csv, en donde se almacen datos de las evaluaciones de distintos usuarios sobre ciertos sandwiches, en las columnas encontramos [url, Local, Direccion, Precio, Ingredientes, nota, texto].
El objetivo es, basado en estos datos, poder determinar cuales son los ingredientes que componen a los sandwiches con mayor puntuación.
Se utilizará el método "bag of words", de esta manera se generará un corpus de texto con los datos de la columna ingredientes, tomando en cuenta aquellas filas que tienen nota 5. Posteriormente se limpiará el corpus con el fin de poder extraer solo las palabras que sean ingredientes y luego se evaluarán las frecuencias de estas palabras. 
El análisis se dividirá en tres partes: Carga de datos, limpieza de datos, análisis de datos

## 1. Carga de datos
Carga de librerías necesarias
```{r}
library(tidyverse)
library(readr)
library(qdap)
library(tm)
```
```

```
Cargar el csv desde la carpeta donde está guardada 
```{r}
data <- read.csv(file='C:/Users/Javiera/Documents/sanguchez.csv',sep=";")
#df <- data.frame(data)
```
Como los datos de interes son las notas e ingredientes, se eliminarán las demás columnas

```{r}
data <- subset(data, select = -c(texto, url, Local, Direccion, Precio))
head(data)
summary(data)
```
## 2. Limpieza de datos
Summary indica que hay notas NA, estos datos no sirve entonces se eliminan. Como solo nos interesa analizar aquellos sandwiches que tienen nota 5, se eliminan los demas
```{r}
data <- data[complete.cases(data), ]
data <- filter(data, nota==5)
head(data)
```
Ahora se eliminarán las columnas innecesarias, como solo interesan los ingredientes, se dejará solo la columna Ingredientes y se almacenarán en la variable ing_text
```{r}
ing_text <- data$Ingredientes
```
Para generar el corpus de texto con el fin de crear la bolsa de palabras, debemos unir toda la columna de ingredientes en un solo texto, para esto se deberá crear un vector con la funcion VectorSource y luego el corpus con la funcion VCorpus.
```{r}
ing_source <- VectorSource(ing_text)
corpus_ing <- VCorpus(ing_source)
#copia para posterior comparaciones
corpuscopy <- corpus_ing
#un vistaza a como se ve el corpus
corpus_ing[[1]]$content
```
Siguiendo con la limpieza, ahora se eliminarán caracteres y palabras que no aportan al análisis, como spanish stopwords, mayusculas, numeros, signos de puntuacion y otras palabras no relevantes. Para esto se creará una función llamada clean_corpus
```{r}
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
corpus_ing <- clean_corpus(corpus_ing)
#como se ve ahora el mismo ejemplo anterior pero dsps de pasar por la funcion clean_corp
corpus_ing[[1]]$content

```
## 3. Primer análisis
Para poder visualizar los datos que tenemos hasta el momento se transformará el corpus para luego calcular la frecuencia de palabras y graficar, con el fin de poder tener una primera apreciación de los datos
```{r}
#
corp_text <- as.data.frame(corpus_ing)
(frequent_terms <- freq_terms(corp_text,top = 30))
plot(frequent_terms)
```

Hay palabras que no son ingredientes sino que adjetivos, ejemplo: mozzarella viene de queso mozzarella, quesos es lo mismo que queso, cheddar es queso cheddar.

## 4. Segunda limpieza
Como se puede ver en el análisis anterior hay juegos de palabras que deberán considerarse como 1 sola, lo haremos con queso y salsa, ya que en el caso de la cebolla son demasiadas las variables de adjetivos para esta y el proceso de limpieza era más complejo.
```{r}
corpus_ing <- tm_map(corpus_ing, content_transformer(function(x) gsub("(queso).(\\w+)", "\\1\\2", x, ignore.case = TRUE)))
corpus_ing <- tm_map(corpus_ing, content_transformer(function(x) gsub("(salsa).(\\w+)", "\\1\\2", x, ignore.case = TRUE)))
corp_text <- as.data.frame(corpus_ing)
(frequent_terms <- freq_terms(corp_text,top = 30))
plot(frequent_terms)
```

Despues de esta limpieza ya podemos ver los ingredientes con mayor frecuencia más claramente, aun así hay algunos errores que se podrían corregir.

## 5 Análisis final y conclusiones

Partiendo de la suposición de que un sandwich esta compuesto por alrededer de 5 ingredientes, se extraerán los 5 ingredientes con mayor frecuencia de la bolsa de palabras.
```{r}
(frequent_terms_final <- freq_terms(corp_text,5))
```
**Siendo los ingredientes ideales: CEBOLLA (21), MAYONESA (19), TOMATE (17), LECHUGA (12) y TOCINO (12).**

Si lo comparamos con la realidad, es muy similar, todos los restaurantes de comida rápida tienen en sus menus algun sandwich con una combinación similar.
Aun así, hay varias cosas que podrían mejorarse en el análisis siendo estas:
* Considerar aquellos sandwiches que tengan nota 4, ya que también es considerada una nota alta
* Lograr separar a las variantes de cebolla en el corpus, ya que hay varios tipos de cebolla 
* Como hay muchos adjetivos de los ingredientes (ej: frita, morada, caramelizada, crocante, casera), buscar la forma de crear unirlas a la palabra ingrediente y así conformar el ingrediente más específico. 


