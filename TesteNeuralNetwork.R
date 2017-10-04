
library(nnet)
library(caret)

source('PreparacaoDeDados.R')

set.seed(500)
head(train)
t<-train[,c(2,3,4,5,7)]

#cor(t)
t <- t[sample(1:nrow(t),130000),]

model <- train(trip_duration ~  passenger_count + distance , t, method='nnet', linout=TRUE, trace = FALSE)
ps <- predict(model, test)
ps
plot(ps)



#Preparando o arquivo de submissão em dataframe com a predição com id e trip_duration

submission_file <- data.frame(id = test$id, trip_duration = ps)

#Escrevendo o arquivo.csv para a submissão

write.csv(submission_file, "randomNeuralNetsCaretSample.csv", row.names=F)
