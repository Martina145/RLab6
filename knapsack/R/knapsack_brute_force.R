set.seed(42)
n <- 5
knapsack_objects <-
  data.frame(
    item=c(1:n),
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )



knapsack_brute_force <- function(x,W){
  K<-matrix(NA,nrow=W+1,ncol=dim(x)[1]+1)
  0->K[1,]->K[,1]
  matrix_item<-matrix(0,nrow=W+1,ncol=dim(x)[1]+1)
  for(j in 1:dim(x)[1]) {
    for(w in 1:W) {
      wj<-x$w[j]
      item<-j
      value<-x$v[j]
      if( wj > w ) {
        K[w+1,j+1]<-K[w+1,j]
        matrix_item[w+1,j+1]<-matrix_item[w+1,j]
      } else {
        if( K[w+1,j] >= K[w+1-wj,j]+value ) {
          K[w+1,j+1]<-K[w+1,j]
          matrix_item[w+1,j+1]<-matrix_item[w+1,j]      		
        } else {
          K[w+1,j+1]<-K[w+1-wj,j]+value
          matrix_item[w+1,j+1]<-item
        }
      }
    }
  }
  
  knapsack <- list(K=K,Item=matrix_item)
  W <- dim(knapsack$K)[1]
  itens <- c()
  col <- dim(knapsack$K)[2]
  selected_item <- knapsack$Item[W,col]
  while(selected_item!=0) {
    selected_item<-knapsack$Item[W,col]
    if(selected_item!=0) {
      selected_item_value<-x[x$item == selected_item,]
      if(-knapsack$K[W - selected_item_value$w,col-1]+knapsack$K[W,col]==selected_item_value$v) {
        W <- W - selected_item_value$w
        itens<-c(itens,selected_item)
      }
      col <- col - 1
    }
  }
  
  return(itens)
}

knapsack_brute_force(knapsack_objects, 3000)
