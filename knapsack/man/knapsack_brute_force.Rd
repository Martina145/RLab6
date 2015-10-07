\name{knapsack_brute_force}
\alias{knapsack_brute_force}

\title{
Brute Force Search
}
\description{
Knapsack problem solved by Brute Force search
}
\usage{
knapsack_brute_force(x, W)
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
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
}




\keyword{Brute_force}

