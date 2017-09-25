#How to calculate Manhattan Distance in R?
print("How to calculate Manhattan Distance in R?")

fa <- c(5,2,1,4)
fb <- c(5,5,4,2)

fm <- matrix(c(fa,fb), byrow=T, nrow=2)

col_names_vectors <- c("U1", "U2", "U3", "U4")
row_names_vectors <- c("FilmA","FilmB")

colnames(fm) <- col_names_vectors
rownames(fm) <- row_names_vectors

fm


#Manhatan Distance based on vectors
#dv1 <- abs(fa[1]-fa[4]) + abs(fb[1]-fb[4])
#dv1

#Manhattan Distance based on matrix

#Distance from user1 (U1) to the new user
d1 <- abs(fm[1,1]-fm[1,4]) + abs(fm[2,1]-fm[2,4])
d1

#Distance from user2 (U2) to the new user 
d2 <- abs(fm[1,2]-fm[1,4]) + abs(fm[2,2]-fm[2,4])
d2

#Distance from user3 (U3) to the new user 
d3 <- abs(fm[1,3]-fm[1,4]) + abs(fm[2,3]-fm[2,4])
d3

distance_vector <- c(d1,d2,d3)
#Checking which distance is the smallest one
min(distance_vector)
#For the new user, system should recommend user1 (U1)