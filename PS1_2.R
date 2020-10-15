#Create matrices
M1 <- matrix(sample(0:50,50),nrow = 10,ncol = 5,byrow = TRUE)
print(M1)
M2 <- matrix(sample(0:50,50),nrow = 5,ncol = 10,byrow = TRUE)
print(M2)

#matrix multiplication
Matrix_multip<-function(Mat1,Mat2){
  Mat1_rows <- nrow(Mat1)
  Mat1_cols <- ncol(Mat1)
  Mat2_cols <- ncol(Mat2)
  
  Mat_out = matrix(nrow = Mat1_rows, ncol = Mat2_cols) 
  
  for(i in 1:Mat1_rows) {
    for(j in 1:Mat2_cols){
      Mat_value <- 0
      for(k in 1:Mat1_cols){
        val_row <- Mat1[i,k]
        val_col <- Mat2[k,j]
        
        Mat_value <- Mat_value + val_row*val_col
        Mat_out[i,j] <- Mat_value
      }
    }
  }
  print(Mat_out)
}

Matrix_multip(M1,M2)
print(M1%*%M2)
#GOOD WORK
