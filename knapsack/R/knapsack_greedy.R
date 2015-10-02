knapsack_greedy <- function(x,W){
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
