set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


# Use intToBits() instead


knapsack_brute_force <- function(x,W){
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
  n <- length(x$v)
  m <- matrix(0,nrow=W+1,ncol=dim(x)[1]+1)
  m_item <- matrix(rep(0,(n+1)*(W+1)), W+1,n+1)
  for(i in 1:dim(x)[1]) {
    for(j in 1:W) {
      if( x$w[i] > j ) {
        m[j+1,i+1]<-m[j+1,i]
        m_item[j+1,i+1]<-m_item[j+1,i]
      } else {
        if( m[j+1,i] >= m[j+1-x$w[i],i]+x$v[i] ) {
          m[j+1,i+1]<-m[j+1,i]
          m_item[j+1,i+1]<-m_item[j+1,i]          
        } else {
          m[j+1,i+1]<-m[j+1-x$w[i],i]+x$v[i]
          m_item[j+1,i+1]<-i
        }
      }
    }
  }
  W <- dim(m)[1]
  items <- c()
  col <- dim(m)[2]
  selected_item <- m_item[W,col]
  while(selected_item!=0) {
    selected_item <- m_item[W,col]
    if(selected_item!=0) {
      selected_item_value <- x[as.numeric(rownames(x)) == selected_item,]
      if(-m[W - selected_item_value$w,col-1]+m[W,col]==selected_item_value$v) {
        W <- W - selected_item_value$w
        items <- c(items,selected_item)
      }
      col <- col - 1
    }
  }
  return(list(value=round(sum(x[items,]$v)), elements=sort(items)))
}


knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)

