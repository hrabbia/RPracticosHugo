
## INICIO DE PRACTICA GUIADA ---------------------------------------------------

## En el siguiente script vamos a iniciar un analisis exploratorio y de visualizacion de un dataset que ya conocemos, delitos! La idea es poner a prueba todo lo que vimos en las clases y desafiarnos con preguntas que requieren una busqueda particular. Esta pensado para que exploremos libremente, intenten ser creativos y ambiciosos a la hora del analisis y sumen cualquier conocimiento adicional que puedan compartir con el resto. 

## Introduccion ----------------------------------------------------------------

## 1. Vamos a iniciar creando un proyecto nuevo en una carpeta para alojar todos los documentos de este trabajo. Recuerden poner un nombre acorde y setear el espacio de trabajo. 

## 2. Cargen todas las librerias y datasets a utilizar. Instalen las librerias que no tengan descargadas y luego importenlas en su espacio de trabajo.      

library(skimr)
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(lubridate)
library(summarytools)
library(dplyr)
library(ggsci)
library(ggplot2)

delitos_2019 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")

delitos_2018 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")

delitos_2017 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")


## 3. Como pueden ver importamos las librerias desde una pagina web. Estas paginas suelen ser una gran fuente de datos cuando queremos inciar un analisis. Pueden revisarlo en el siguiente link: https://mapa.seguridadciudad.gob.ar/ 

## Limpieza de los datos -------------------------------------------------------

## 4. Unifica los tres datasets en uno solo de tal modo que quede uno debajo del otro. En el caso de que aparezca un error, que sucede con la variable franja horaria? Transformala y luego uni los datasets.  

delitosCABA <- bind_rows(delitos_2017,delitos_2018,delitos_2019)


#inspecciono tipo de variables por el error en franja_horaria

class(delitos_2017$franja_horaria)
class(delitos_2018$franja_horaria)
class(delitos_2019$franja_horaria)

#identifico que en delitos_2017 es character en vez de numeric como en las otras dos bases

#modifico la variable a numeric

delitos_2017 <- delitos_2017 %>% 
  mutate(franja_horaria= as.double(franja_horaria))

#me indica un problema que no sé qué es pero parece que tiene que ver con NA, así que inspecciono los NA

is.na(delitos_2017$franja_horaria)

#Pero parece que no es ese el problema
#Vuelvo a unificar y creo dataset delitosCABA

delitosCABA <- bind_rows(delitos_2017,delitos_2018,delitos_2019)

## 5.Cuantas filas tiene cada dataset? Y cuantas columnas? 
#inspecciono filas y columnas del dataset


glimpse(delitos_2017)
#rows = 120,564
#col =10

glimpse(delitos_2018)
#rows = 123,733
#col =10

glimpse(delitos_2019)
#rows = 117,661
#col =10

glimpse(delitosCABA)
#rows = 361,958
#col =10

## 6. Que tipo de datos contiene el dataset? 

#con el análisis anterior puedo ver que tiene datos números y de caracteres, desde id, fecha, pasando por tipo de delito, barrio, lat, long, etc.



## 7. Cuantos valores faltantes se registran en cada variable? 

#exploro NA y otros parametros del dataset
summary(delitosCABA)

#la mayoría de valores perdidos se presentan en comuna, lat y long, aunque también hay 43 NA en franja_horaria
#quiero ver si hay NA también en las variables character
anyNA(delitosCABA$tipo_delito) 
anyNA(delitosCABA$subtipo_delito)
anyNA(delitosCABA$barrio)

#en tipo_delito es FALSE por lo que no habría NA, mientras que en subtipo_delitos y barrio es TRUE, así que habría NA

#exploro cuántos NA hay para cada una de estas dos variables:
sum(is.na(delitosCABA$subtipo_delito))  #son NA 311570 en esta variable
sum(is.na(delitosCABA$barrio))   #son NA 7738 en esta variable



## 8. Que sucede con la variable cantidad registrada? Explora los valores unicos, cuales son los valores mas frecuentes y saca conclusiones al respecto. Puede que tengas que buscar sobre tablas de frecuencia.  

unique(delitosCABA$cantidad_registrada) #1, 2, 3, 4 SON VALORES UNICOS
freq(delitosCABA$cantidad_registrada) #pero la gran mayoría corresponde a 1, y 2,3 y 4 tienen porcentajes marginales



## 9. Cual es la relacion entre tipo de delito y subtipo de delito? Describir. Puede que tengas que buscar sobre tablas de contingencia

library(dplyr)
library(janitor)

table1 <- table(delitosCABA$tipo_delito,delitosCABA$subtipo_delito) #supuestamente me arroja una tabla, aparece en Values, pero no la puedo ver...

#busco otra alternativa y encuentro esto

table2 <- delitosCABA %>% 
  select(tipo_delito, subtipo_delito) %>% 
  table()

#NO puedo leer las tablas, no sé por qué.

library(ggplot2)
library(janitor)

print(table2) #no me aparecía, por eso hice esto:
#Lesiones en siniestro vial son los subtipos de delito más frecuentes, seguido de Hurto (sin violencia) de automotor y robo con violencia de automotor

#me gustaría poder diferenciar entre homicidio doloso y homocidio vial en un nuevo dataset, pero he buscado como hacerlo y no lo puedo ecnontrar. Sería una forma de ser más preciso con esta info, pero voy a proseguir con el dataset como está.

## 10. Hace el grafico pertinente para mostrar los tipos de delitos existentes y sus frecuencias. No olvides incluir titulo, nombres a los ejes y colores.  



tab2 <- delitosCABA %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))

#en la tab2 nos muestra que el delito más comun es robo con violencia, seguido de hurto (sin violencia). El menos frecuente es homicidio con 729 casos en 3 años.

plot1 <- ggplot(data = tab2, aes(x = tipo_delito, y = perc))+
  geom_bar(stat = "identity", color="red", fill="red", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  theme_minimal()+
  labs(title = "Frecuencia de tipo de delitos CABA (2017-2019)",
       x = "",y = "Porcentaje",
       caption = "Intendencia de Buenos Aires")

print(plot1)

## 11. Hace el grafico pertinente para mostrar como se distribuye la variable franja horaria. No olvides incluir titulo, nombres a los ejes y colores.  


#realicé un histograma para ver su distribución
ggplot(data = delitosCABA) +
  geom_histogram(mapping = aes(x=franja_horaria), color= "blue", fill="blue", bins=24)+
  theme_minimal()+
  labs(title="Cantidad de Delitos de CABA (2017-2019) según Franja Horaria",
       caption = "Intendencia de Buenos Aires",
       y="Cantidad de Delitos",
       x="Franja Horaria")






## 12. Incorporaremos al grafico anterior una segmentacion por tipo de delito y un filtro para quedarnos con los delitos que hayan ocurrido especialmente en Puerto Madero. 

#selecciono específicamente el barrio que me interesa trabajar y genero un nuevo objeto datobarrio

datobarrio <- delitosCABA %>% 
  filter(barrio == "Puerto Madero")

#ahora hago un gráfico de densidad según tipo delito

plot2 <- datobarrio %>% 
  geom_histogram(mapping = aes(x=franja_horaria), color= "blue", fill="blue", bins=24)+ 
  theme_minimal()+
  labs(title="Puerto Madero: Cantidad de Delitos según Franja Horaria (2017-2019)",
       y="Cantidad de Delitos",
       x="Franja horaria",
       caption = "Intendencia de Buenos Aires")





plot3 <- ggplot(data = datobarrio)+
  geom_histogram(mapping = aes(x=franja_horaria), color= "blue", fill="blue", bins=27)+ 
  theme_minimal()+
  labs(title="Puerto Madero: Cantidad de Delitos según Franja Horaria (2017-2019)",
       y="Cantidad de Delitos",
       x="Franja horaria",
       caption = "Intendencia de Buenos Aires")

print(plot3)


#los datos me muestran que habría horas en particular donde no acontecen delitos, aunque sospecho que puede ser un error mío también de cómo traté los datos.












