
library(nnet)
library(caret)

source('PreparacaoDeDados.R')

set.seed(500)
head(train)
t<-train[,c(2,3,4,5,7)]
cor(t)
t <- t[sample(1:nrow(t),13000),]
sapply(train, is.numeric)
inTrain <- createDataPartition(y=t$trip_duration, p=0.8, list=FALSE) 
strain <- train[inTrain,]
stest <- train[-inTrain,]
#plot(strain$distance,strain$trip_duration)

#strain$passenger_count <- scale(strain$passenger_count)
#strain$distance <- scale(strain$distance)
#strain$traffic <- scale(strain$traffic)
#strain$trip_duration <- scale(strain$trip_duration)

model <- train(trip_duration ~  passenger_count + distance , t, method='nnet', linout=TRUE, trace = FALSE)
ps <- predict(model, test)
ps
plot(strain$trip_duration,ps)

#plot(ir.nn)

#ir.nn <- nnet(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic, data = train, size = 30, rang = 0.2, decay = 5e-04, maxit = 10000, trace = T)

Estimated <- compute(ir.nn, test[,2:6])
Estimated <- Estimated$net.result
Estimated



#Preparando o arquivo de submissão em dataframe com a predição com id e trip_duration

submission_file <- data.frame(id = test$id, trip_duration = ps)

#Escrevendo o arquivo.csv para a submissão

write.csv(submission_file, "randomNeuralNetsCaretSample.csv", row.names=F)
