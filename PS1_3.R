Pascal_triangle <- function(n){ 
  Pas_result <- matrix(0,nrow = n,ncol = n) 
  Pas_result[,1] <- 1   
  for (i in 2:n){ 
    for(j in 2:i){ 
      Pas_result[i,j] <- Pas_result[i-1,j-1] + Pas_result[i-1,j] 
    } 
  } 
  print(Pas_result[n,])  
} 

Pascal_triangle(100) 
Pascal_triangle(200)