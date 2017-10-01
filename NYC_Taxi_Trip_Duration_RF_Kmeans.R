#Importando as bibliotecas

library(readr)
library(data.table)
library(lubridate)
library(tidyverse)
library(randomForest)

#Definindo as classes das colunas das bases de train e test

train_classes <- c("factor", "factor", "character", "character", "integer",
                   "numeric", "numeric", "numeric", "numeric", 
                   "factor", "numeric")

test_classes <- c("factor", "factor", "character", "integer",
                  "numeric", "numeric", "numeric", "numeric", "factor")

#Importando as bases de dados train e test com fread, passando as colunas das classes

traintaxinyc <- fread("datasets/train.csv", colClasses = train_classes)

testtaxinyc <- fread("datasets/test.csv", colClasses = test_classes)

#Verificando as estruturas de train e test

str(traintaxinyc)

str(testtaxinyc)

#sumários de train e test

summary(traintaxinyc)

summary(testtaxinyc)

#visualizando as colunas de train e test

View(traintaxinyc)

View(testtaxinyc)

#Usando Kmeans para verificar se houve a gravação dos dados da viagem ou não

set.seed(20)

#omitindo os dados NA

traintaxinyc <-na.omit(traintaxinyc) #

#Criando o cluster com Kmeans

NYCTripCluster <- kmeans(traintaxinyc[, c(2,5)], 2, nstart = 20)

#verificando o cluster

NYCTripCluster 

#Criando a tabela para visualizar o resultado do Cluster com Kmeans

base::table(NYCTripCluster$cluster,traintaxinyc$store_and_fwd_flag)

#Preparando os dados para montar os modelos e calcular a distancia manhattan

set.seed(412)

#Pegando uma amostra de train pra demonstrar o pickup_time pela trip_duration

sample <- train %>% mutate(pickup_hour = hour(ymd_hms(pickup_datetime))) %>% sample_n(10000)

#desenhando o grafico com o ggplot 

ggplot(sample, aes(x = pickup_hour, y = log(trip_duration))) + geom_point(position = "jitter", alpha = 0.25) + geom_smooth() +labs(x = "Hour of Pickup", y = "Log of Trip Duration", title = "Trip Duration by Pickup Hour")

#Criando um modelo com randomForest com ntree = 100 para fazer a predição da trip_duration

rf_trip_duration <- randomForest(trip_duration ~ vendor_id + passenger_count + pickup_longitude + pickup_latitude,data = sample,ntree = 100)

#fazendo a predição do modelo para trip_duration

rf_prediction <- predict(rf_trip_duration, test, type = "response") 

#Preparando o arquivo de submissão em dataframe com a predição com id e trip_duration

submission_file <- data.frame(id = test$id, trip_duration = rf_prediction)

#Escrevendo o arquivo.csv para a submissão

write.csv(submission_file, "randomForest_submission.csv", row.names=F)


