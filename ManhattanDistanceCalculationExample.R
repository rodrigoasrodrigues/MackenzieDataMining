#importando as bibliotecas


installLibs <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


installLibs(c('readr','dplyr','Amelia','pscl'))


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


