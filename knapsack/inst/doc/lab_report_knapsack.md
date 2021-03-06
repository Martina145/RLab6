---
title: "Knapsack"
author: "Caroline Svahn & Martina Sandberg"
date: "2015-10-07"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Answers to questions

1) To run `knapsack_brute_force()` with W=3500 and n=16 before optimization took 3 seconds.
 
2) `knapsack_dynamic()` took 83.24 seconds with W=3500 and n=500 and

3) `knapsack_greedy()` with W=3500 and n=1000000 required 86.10 seconds.

4) Trying to improve the code performance without `C++` did not have much effect - at least not consistant. Functions within the functions were used to avoid looping over matrices and vectors and efforts to maximize the effectiveness of the remaining loops were made but the results are not definite since `system.time()`returned various results. Instead, we relied on implementing `C++`.


5) After using `Rcpp` and `C++` in `knapsack_greedy()` it takes 44.94 seconds with W=3500 and n=1000000. So the performance gain is 41.16 seconds.

