Print_values <- function(a,b,c) {
  if(a>b) 
    if(b>c) Result<-c(a,b,c)
    else {
      if(a>c) Result<-c(a,c,b)
      else Result<-c(c,a,b)
    }
      
  else{
    if(b>c)
      if(a>c) Result<-c(b,a,c)
      else Result<-c(b,c,a)
    else Result<-c(c,b,a)
  }
  print(Result)
}
Print_values(10,8,22)
Print_values(1,82,2)
Print_values(10,3,9)
Print_values(10,31,9)
Print_values(10,31,91)
Print_values(10,31,9)
# GOOD WORK
