set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

test_that("Returns the right values", {
  expect_that(knapsack_greedy(x = knapsack_objects[1:40,], W = 3500),equals(list(value=40976,elements=c(37,35,25,8,18,22,14))))
  expect_that(knapsack_greedy(x = knapsack_objects[1:40,], W = 2000),equals(list(value=34244,elements=c(37,35,25,8,18,22))))
})

test_that("Returns an error", {
  expect_that(knapsack_greedy(x = 5, W = 3500),
              throws_error("x is not a data.frame"))
  
  k1 <- data.frame(w=c(1,2,3))
  expect_that(knapsack_greedy(x = k1, W = 3500),
              throws_error("x dont have two variables w and v"))
  
  k2 <- data.frame(w=c(1,2,3),w2=c(4,5,6))
  expect_that(knapsack_greedy(x = k2, W = 2000),
              throws_error("x dont have two variables w and v"))
  
  expect_that(knapsack_greedy(x = knapsack_objects[1:8,], W = -5),
              throws_error("Not numeric and/or positive W"))
  
  expect_that(knapsack_greedy(x = knapsack_objects[1:8,], W = "W"),
              throws_error("Not numeric and/or positive W"))
  
  k3 <- data.frame(w=c(1,-2,3),v=c(4,5,6))
  expect_that(knapsack_greedy(x = k3, W = 3500),
              throws_error("Not only positive values in data.frame"))
  
  k4 <- data.frame(w=c(1,2,3),v=c(4,-5,6))
  expect_that(knapsack_greedy(x = k4, W = 3500),
              throws_error("Not only positive values in data.frame"))
})


