#importando as bibliotecas

library(readr)
library(dplyr)
library(Amelia)
library(pscl)

#importando as bases de dados train e test de NYCTaxi

traintaxinyc <-read_csv("~/train.csv")

testtaxinyc <-read_csv("~/test.csv")

#verificando as estruturas

str(traintaxinyc)

str(testtaxinyc)

#sumários

summary(traintaxinyc)

summary(testtaxinyc)

#visualizando as colunas

View(traintaxinyc)

View(testtaxinyc)

#Usando Kmeans para verificar se houve a gravação dos dados da viagem ou não

set.seed(20)

traintaxinyc <-na.omit(traintaxinyc) #omitindo os dados NA

NYCTripCluster <- kmeans(traintaxinyc[, c(2,5)], 2, nstart = 20)

NYCTripCluster 

base::table(NYCTripCluster$cluster,traintaxinyc$store_and_fwd_flag)

#Preparando os dados para montar os modelos e calcular a distancia manhattan

#unindo as colunas de train e test com bind_rows

full <- bind_rows(traintaxinyc, testtaxinyc)

#verificando a estrutura de full

str(full)

#utilizando a função sapply para verificar os casos de na e o length das colunas

sapply(traintaxinyc, function(x) sum(is.na(x)))

sapply(traintaxinyc, function(x) length(unique(x)))


