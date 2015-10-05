knapsack_greedy <- function(x,W){
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
  
  valuePW <- numeric(length(x$v))
  for (i in seq(length(x$v))) {
    valuePW[i] <- x$v[i]/x$w[i]
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
