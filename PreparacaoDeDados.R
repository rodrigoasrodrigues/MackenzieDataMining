source('installLibs.R')
source('Distancia.R')

# descompacta arquivos do dataset
unzip(zipfile = 'datasets/test.zip',exdir = 'datasets')
unzip(zipfile = 'datasets/train.zip',exdir = 'datasets')

train.original <- read_csv("datasets/train.csv")
test.original <- read_csv("datasets/test.csv")

#para ser mais rápido, se o arquivo csv processado existir importa ele
if(file.exists('datasets/tidy_train.csv') && file.exists('datasets/tidy_test.csv'))
{
  
  train <- read_csv("datasets/tidy_train.csv")
  test <- read_csv("datasets/tidy_test.csv")
} else
{
  
  #limpa durações maiores do que duas horas
  train.original <- train.original[train.original$trip_duration<=60*60*2,]
  

  
#analisa horários de pico
rush <- hour(train.original$pickup_datetime)
rush.hist <- hist(rush,  breaks = 23, plot = FALSE)
rush.mean <- mean(rush.hist$counts) #media
rush.sd <- sd(rush.hist$counts) #desvio padrão
hcol <- rep('orange',24) #padrão laranja (transito moderado)
hcol[rush.hist$counts<rush.mean] <- 'green' #transito abaixo da média
hcol[rush.hist$counts>rush.mean+rush.sd] <- 'red' #transito Intenso (media + desvio padrão)
hist(rush,  breaks = 23, plot = TRUE, freq = TRUE,main = 'Histograma dos horários (Rush)', xlab = 'Hora', ylab='Viagens', col = hcol)
abline(rush.mean,0, lty=2)
abline(rush.mean+rush.sd,0, col='red', lty=2)

cat('Calculando distâncias, limpando dados e classificando horários de pico.\nEssa operação pode demorar alguns minutos\n')

train <- transmute(train.original, id, vendor_id, passenger_count, recording=store_and_fwd_flag=='Y',
       distance = distancia(pickup_latitude,pickup_longitude,dropoff_latitude, dropoff_longitude), 
       traffic=ifelse(is.na(pickup_datetime),rush.mean,rush.hist$counts[hour(pickup_datetime)]) , trip_duration)

cat('Fazendo o mesmo para a base de teste\n\n')

test <- transmute(test.original, id, vendor_id, passenger_count, recording=store_and_fwd_flag=='Y',
                  distance = distancia(pickup_latitude,pickup_longitude,dropoff_latitude, dropoff_longitude), 
                  traffic=ifelse(is.na(pickup_datetime),rush.mean,rush.hist$counts[hour(pickup_datetime)]))

#remove distancias absurdas
qnt <- quantile(train$distance, probs=c(.25, .75))
H <- 1.5 * IQR(train$distance)
dist <- train$distance
train<- train[dist <= (qnt[2] + H),]

cat('Exportando CSV\n\n')
write.csv(train, "datasets/tidy_train.csv", row.names=F)
write.csv(test, "datasets/tidy_test.csv", row.names=F)


}
