set.seed(42)
n <- 1000000
x <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_greedy <- function(x,W,fast=FALSE){
  if ( is.data.frame(x) != TRUE ) {
    stop("x is not a data.frame")
  }
  if ( (ncol(x) != 2) || all(colnames(x)==c("w","v"))==FALSE  ) {
    stop("x dont have two variables w and v")
  }
  if ( is.numeric(W) != TRUE || W < 0  ) {
    stop("Not numeric and/or positive W")
  }
  if ( any(x$w < 0)  || any(x$v < 0)) {
    stop("Not only positive values in data.frame")
  }
  if (fast==TRUE){
    cppFunction(code = 'NumericVector temp(NumericVector value, NumericVector weight, int n){
  NumericVector valuePW(n);
    for(int i = 0; i <= n; i++){
       valuePW[i] = value[i]/weight[i];
    }
  return valuePW;}')
    valuePW <- temp(x$v,x$w,length(x$v))
  } else {
    #used function instead of loop, 0/0.05
    f <- function(x) x$v/x$w
    valuePW <- f(x)
  }
  
  sorted <- matrix(c(x$v, x$w, valuePW), ncol=3)
  sorted <- sorted[order(-sorted[,3]),]
  value<-0
  test<-numeric()
  weight<-0
  elements<-numeric()
  i<-1
  
  while ((weight+sorted[i,2])<W) {
    weight <- weight+sorted[i,2]
    value <- value+sorted[i,1]
    elements <- append(elements, which(x$v %in% sorted[i,1]))
    
    i<-i+1
  } 
  return(list(value=round(value), elements=elements))
}
