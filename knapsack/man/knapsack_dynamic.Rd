\name{knapsack_dynamic}
\alias{knapsack_dynamic}

\title{
Dynamic programming
}
\description{
Knapsack problem solved by Dynamic programming
}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
  \item{x}{data frame with two variables, "w"=weigth and "v"=value for the items. }
  \item{W}{limited weight for the knapsack.}
}

\value{
Returns a list with the items packed in the knapsack and their summed value.
}

\examples{
set.seed(42)
n <- 8
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_dynamic(x = knapsack_objects, W = 3500)
}




\keyword{Dynamic}


