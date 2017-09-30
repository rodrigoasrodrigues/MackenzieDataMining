
# Converte graus para rads para usar as funções matemáticas
# (Cordenadas são em graus)
grausParaRadianos <- function(graus){
  graus * (pi/180)
}

#calcula distancia em graus e converte para metros
distanciaEuclidiana <- function(coords){
  lat1 <- coords[1]
  long1 <- coords[2]
  lat2 <- coords[3]
  long2 <- coords[4]
  x <- lat2 - lat1
  y <- long2 - long1
  h <- sqrt(x*x+y*y)
  r <- 6371000 #raio da terra em m
  rad <- grausParaRadianos(h)
  d<- rad*r
  d
} 

distanciaManhatan <- function(coords){
  lat1 <- coords[1]
  long1 <- coords[2]
  lat2 <- coords[3]
  long2 <- coords[4]
  d1 <- distanciaEuclidiana(c(lat1,0,lat2,0))
  d2 <- distanciaEuclidiana(c(0,long1,0,long2))
  d <- d1+d2
  d
}

# a cidade de manhatan tem uma inclinação de 29º em relação ao norte
rotacionaCoordenadas <- function(coords){
  lat1 <- coords[1]
  long1 <- coords[2]
  lat2 <- coords[3]
  long2 <- coords[4]
  x1 <- lat2 - lat1
  y1 <- long2 - long1
  angulo <- grausParaRadianos(29)
  x2 <- x1 * cos(angulo) - y1 * sin(angulo)
  y2 <- x1 * sin(angulo) + y1 * cos(angulo)

  x <- x2 + lat1
  y <- y2 + long1
  
  c(lat1,long1,x,y)
}

#calcula a distancia utilizando a distancida de manhattan com cordenadas rotacionas em 29º
distancia <- function(lat1, long1, lat2, long2){
  d <- rep(0,length(lat1))
  for(i in 1:length(lat1)){
    
    a <- c(lat1[i],long1[i],lat2[i],long2[i])
    
    d[i] <- rotacionaCoordenadas(a) %>% distanciaManhatan
  }
  d
}
