n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000))

knapsack_dynamic <- function(x,W){
  n <- length(x$v)
  weight <- 0
  prevWeight <-0
  
  m <- matrix(rep(0,(n+1)*(W+1)), n+1, W+1)
 #m <- matrix(rep(0,length(x$w))*length(x$v), length(x$w), length(x$v))
  
  for (i in 1:n) { #hoppar över raden med n=0 element
    for (j in 1:W) { #hoppar över kolumnen med W=0
      if (x$w[i] <= j) {
        m[i+1,j+1] <- max(m[i,j+1],(m[i,j-x$w[i]+1]+x$v[i])) 
      } else {
        m[i+1,j+1] <- m[i, j+1]
      }
    }
  }
 return(m[nrow(m),ncol(m)])
}


