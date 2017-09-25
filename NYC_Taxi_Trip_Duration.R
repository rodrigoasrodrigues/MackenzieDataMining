#importando as bibliotecas

library(readr)
library(dplyr)
library(Amelia)
library(pscl)

#importando as bases de dados train e test de NYCTaxi

traintaxinyc <-read_csv("datasets/train.csv")

testtaxinyc <-read_csv("datasets/test.csv")


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

#fazendo um subset para selecionar as colunas de traintaxinyc

data <- subset(traintaxinyc, select = c(2, 5, 6, 7, 8, 10, 11))

#verificando os NAs das colunas selecionadas no subset e passando os complete cases de data

sapply(data, function(x) sum(is.na(x)))

data[complete.cases(data), ] 

#definindo rownames de data iguais a null

rownames(data) <- NULL 

#montando um modelo de regressão logística para achar valores para trip_duration

model <- glm(trip_duration~ ., family = binomial(link = "logit"),
             data = traintaxinyc)

#fazendo o sumário do modelo

summary(model)

#definindo uma amostra de data para usar na predição do modelo

train <- sample_frac(data, 0.7)

sid <- as.numeric(rownames(train)) 

test <- data[-sid, ]

#utilizando anova para o modelo

anova(model, test = "Chisq")

#definindo a variavel fitted.results para predizer o modelo

fitted.results <- predict(model, data = subset(test ,select=c(2,3,4,5,6,7,8)),type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$trip_duration)

print(paste('Acurácia:',1-misClasificError))


#modelo 2 utilizando a distancia manhattan e levando em conta a angulação de 60º de Manhattan
#para o cálculo do tempo de trip_duration

#obs: precisamos tentar resolver o problema do tamanho e de repente tentar uma maneira 
#pegar uma amostra menor, pois o modelo de regressão logistica esta gerando um vetor de 15.852.2GB
#bugando o R para gerar o modelo.








