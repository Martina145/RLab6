set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000))

knapsack_dynamic <- function(x,W){
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
  elements <- rep(0, n)
  m <- matrix(rep(0,(n+1)*(W+1)), n+1, W+1)
  items <- n
  weight <- W
  
  #probably a bottlekneck!
  for (i in 1:n) { #hoppar över raden med n=0 element
    for (j in 1:W) { #hoppar över kolumnen med W=0
      if (x$w[i] <= j) {
        m[i+1,j+1] <- max(m[i,j+1],(m[i,j-x$w[i]+1]+x$v[i]))
      } else {
        m[i+1,j+1] <- m[i, j+1]
      }
    }
  }
  
  value <- m[nrow(m), ncol(m)]
  temp <- value
  binary <- rep(0, n)
  items <- n
  weight <- W
  
  while(temp > 0) {
    while(m[items+1,weight+1] == temp) {
      items <- items - 1
    }
    items <- items + 1
    binary[items] <- 1
    weight <- weight - x$w[items]
    items <- items - 1
    temp <- m[items+1,weight+1]
  }
  
  elements <- numeric()
  for (i in seq(length(binary))) {
    if (binary[i]==1) {
      elements <- append(elements, i)
    }
  }
  return(list(value=round(value), elements=elements))
}

# install rtools, load it
# install.packages("devtools")
# library(devtools)
# devtools::install_github("hadley/lineprof")

#source(find_ex(knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)))

#profile <- lineprof(knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000))
#shine(profile)
