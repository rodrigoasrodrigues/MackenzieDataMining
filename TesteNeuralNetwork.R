
library(nnet)
library(neuralnet)

source('PreparacaoDeDados.R')

set.seed(500)
head(train)

strain <- train[sample(1:nrow(train),500),]

plot(strain$distance,strain$trip_duration)

strain$passenger_count <- scale(strain$passenger_count)
strain$distance <- scale(strain$distance)
strain$traffic <- scale(strain$traffic)
strain$trip_duration <- scale(strain$trip_duration)

ir.nn <- neuralnet(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic, data = strain,  hidden = c(5,3),lifesign.step = 0.1)
plot(ir.nn)

#ir.nn <- nnet(trip_duration ~ vendor_id + passenger_count + recording + distance + traffic, data = train, size = 30, rang = 0.2, decay = 5e-04, maxit = 10000, trace = T)

Estimated <- compute(ir.nn, test[,2:6])
Estimated <- Estimated$net.result
Estimated <- round(Estimated)
Estimated
