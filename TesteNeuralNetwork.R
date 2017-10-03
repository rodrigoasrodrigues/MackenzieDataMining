
library(nnet)
library(neuralnet)

source('PreparacaoDeDados.R')

set.seed(500)
head(train)

strain <- sample(train,500)
ir.nn <- neuralnet(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic, data = strain,  hidden = c(5,4,3,2), threshold = 0.5, lifesign.step = 100)


#ir.nn <- nnet(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic, data = train, size = 30, rang = 0.2, decay = 5e-04, maxit = 10000, trace = T)

nnet_prediction <- predict(ir.nn, test[,2:6]) 
