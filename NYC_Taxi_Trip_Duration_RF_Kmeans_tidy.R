#Importando as bibliotecas


source('PreparacaoDeDados.R')

set.seed(20)

NYCTripCluster <- kmeans(train[, 2:6], 2, nstart = 20)

#verificando o cluster

NYCTripCluster 

#Criando a tabela para visualizar o resultado do Cluster com Kmeans

base::table(NYCTripCluster$cluster,train$recording)

#Preparando os dados para montar os modelos e calcular a distancia manhattan

set.seed(412)



#Criando um modelo com randomForest com ntree = 100 para fazer a predição da trip_duration com vendor_id + passenger_count + pickup_longitude + pickup_latitude 

rf_trip_duration <- randomForest(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic ,data = train,ntree = 100)

#fazendo a predição do modelo para trip_duration

rf_prediction <- predict(rf_trip_duration, test, type = "response") 

#Preparando o arquivo de submissão em dataframe com a predição com id e trip_duration

submission_file <- data.frame(id = test$id, trip_duration = rf_prediction)

#Escrevendo o arquivo.csv para a submissão

write.csv(submission_file, "randomForest_tidy.csv", row.names=F)

