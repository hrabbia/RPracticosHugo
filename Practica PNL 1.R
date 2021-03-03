################# Práctico PLN 1ª Parte #######################################

#Como en todo ejercicio de análisis de texto a través de lenguaje natural, vamos a explorar el df con el 
#objetivo puesto en entender de qué trata este corpus de texto. Por eso, en principio, no damos nunguna pista de 
#qué contenidos tiene este corpus y partimos de la base de que hay que descubrirlo.

#Cargamos el df

library(readr)
library(tidytext)
library(tibble)
library(funModeling)
library(forcats)
library(ggplot2)
library(dplyr)

library(tidyverse)
library(igraph)
library(ggraph)


df <- read.csv(practicopln) #en realidad esto no me sirve de nada, porque lo que hice fue importarlo desde la columna a la derecha abajo, y luego pedí importar objeto. Lo mismo con my_stopwords.

#1)Superficialmente, ¿De què trata este corpus? Utilizar funciones exploratorias

summary(practicopln) #me indica que es un corpus con tuits y detalles de los mismos
head(practicopln) #está integrado por 90 variables o columnas, como ser user_id, lang, scree_name text, reply_to_status, entre muchas otras.
#Hay variables character (chr), dttm, lgl, double (dbl)
row(practicopln)
glimpse(practicopln)



#2) ¿En qué otros idiomas fueron escritos estos tweets? Me quedo con aquellos tweets que solo han sido escritos
#en español

#exploro los idiomas en los que se ha escrito
summary(practicopln$lang)
unique(practicopln$lang)
freq(practicopln$lang)

# Se ha escrito en los siguientes idiomas: es"  "pt"  "und" "en"  "it"  "tl"  "ca"  "in"  "et"  "eu"  "da"  "pl"  "lt"  "fr"  "ro"  "tr"  "ht"  "no"  "fi" "cy"  "sv"  "nl

#La gran mayoría está en "es" (casi 95%) y solo 4.56% en "pt". Esto me da pauta de que 

#lo primero que voy a hacer es filtrar por los que están en "es"

PNL2 <- practicopln %>% 
  filter(lang == "es")

#Ahora me voy a quedar específicamente con la columna "text" dado que allí figuran los tuits propiamente dichos, y nos interesa hacer un análisis de contenido de las discusiones.

PNL2 <- PNL2 %>% 
  select(c(text))

# noto que las palabras re, http y otra es t.co no deberían figurar. Tendría que agregarlas a my_stopwords) A posteriori (en bigrams) tambi�n noto otras palabras que deber�n incluirse en my_stopwords.

word <-  data.frame(word= c("t.co", "https", "re", "sos", "vos","te"))

my_stopwords <- bind_rows(my_stopwords, word)

#tokenizo el dataset para identificar también palabras

palabras <- PNL2 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_stopwords) %>%
  count(word, sort = TRUE)


#3)¿Cuáles son las 50 palabras más frecuentes para estos tweets?

unique(palabras)


top_50 <- palabras %>% top_n(50, n)

freq(top_50)



#4)  Ahora, podemos hacernos una idea de lo que tratan estos tweets, pero podríamos ir un poco más allá con 
# otras herramientas. También podríamos explorar n-grams. Aquí podríamos explorar el número de n-grams que 
#quisiéramos, pero lo recomendable en PLN es explorar hasta 3-grams y no más ya que suele ser infructuoso. 
#Veamos entonces ¿cuáles son los 10 bigramas y los 10 trigramas más comunes? Recordemos que aquí la estrategia
#para limpiar palabras vacías no es anti_join
  

#bigrams

feminismos_bigram <- PNL2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

feminismos_bigram_filtered <- PNL2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)  %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word)


feminismos_bigram_filtered  %>% top_n(10)


#a partir de ac� lo viejo de HUGO, que es lo que estaba mal:

PNLbigrams <- PNL2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)



PNLbigrams <- PNL2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

PNLbigram1 <- PNL_bigram_united  %>%
  unnest_tokens(bigram, col, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>%
  count(word1, word2, sort = TRUE)

PNLbigrams <- PNL2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  anti_join(my_stopwords)
  
bigrams_separados <- PNLbigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

filtered_PNL <- bigrams_separados %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word)

head(filtered_PNL)

PNL_bigram_united <- filtered_PNL %>%
  unite(bigram, word1, word2, sep = " ")

PNL_bigram_united %>% top_n(50)


united_PNLbigrams <- filtered_PNL %>%
  unite(bigram, word1, word2, sep = " ")

united_PNLbigrams %>% top_n(20,n)

#HASTA AC� llega el moco de HUGO...

#trigrams

trigram_PNL <- PNL2 %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word,
         !word3 %in% my_stopwords$word) %>%
  count(word1, word2, word3, sort = TRUE)

trigram_PNL %>% top_n(20)


  

#5) ¿Qué nodos de significado existen y cuáles son son los núcleos más importantes? Lo graficamos a través de 
#redes de n.gramas. Recordar que para graficar estas redes, los biogrmas tienen que estar separados.


summary(bigrams_separados)
head(bigrams_separados)
row(bigrams_separados)
is.na(bigrams_separados)


summary(feminismos_bigram_filtered)
glimpse(feminismos_bigram_filtered)

library(igraph)
library(ggraph)
set.seed(2017)

#lo primero que har� es seleccionar del dataset todos los tuits que hagan referencia a feministas, feminismos y derivados lexicales directos. No puedo trabajar con toda la data porque se me traba la computadora (ya lo prob� como 5 veces, y lo dej� corriendo toda la noche, pero no hubo caso)
# En este sentido, podr�amos pensar que estamos explorando qu� discursos circulan en redes respecto a los feminismos
# a su vez solo voy a seleccionar los bigrams que tengan al menos 1000 frecuencias.



feminismo <-  data.frame(word= c("feminismo", "feminismos", "feminista"))

feminismo_bigrams <- feminismos_bigram_filtered %>% 
  filter(word1, word2 >= 1000, word1 %in% c("feminismo","feminismos","feminista"))

feminismo_filtrado <- feminismos_bigram_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(n >= 1000) %>% 
  filter(!word1 %in% c("feminismo","feminismos","feminista"))%>%
  filter(!word2 %in% c("feminismo","feminismos","feminista"))

PNL_bigram_graph <- feminismo_filtrado %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(feminismo_filtrado, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)






  