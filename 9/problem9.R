library(tidyverse)

input <- readLines("9/input") %>% str_split(" ") %>% map(as.numeric)

input[[1]] %>% diff() %>% diff()

sums <- c()
for (ins in seq_along(input)) {
  .in <- input[[ins]]
  res <- list()
  step <- 1
  while (TRUE) {
    res[[step]] <- .in
    if (all(.in==0)) break
    .in <- diff(.in)
    step <- step+1
  }
  res

  # reverse order
  res <- rev(res)
  #res2 <- list()
  #i <- 1
  currdiff <- 0
  sumpred <- 0
  for (i in seq_along(res)) {
    tmp <- res[[i]]
    pred <- last(tmp)+currdiff
    currdiff <- pred
    if (i == length(res)) sumpred <- sumpred+pred
    #res2[[i]] <- c(tmp, pred)
  }
  sums <- c(sums, sumpred)

}
sums
sum(sums)

# part 2


#ins <- 3
sums <- c()
for (ins in seq_along(input)) {
  .in <- input[[ins]]
  res <- list()
  step <- 1
  while (TRUE) {
    res[[step]] <- .in
    if (all(.in==0)) break
    .in <- diff(.in)
    step <- step+1
  }
  res

  # reverse order
  res <- rev(res)
  res2 <- list()
  #i <- 5
  currdiff <- 0
  sumpred <- 0
  for (i in seq_along(res)) {
    tmp <- res[[i]]
    pred <- first(tmp)-currdiff
    currdiff <- pred
    if (i == length(res)) sumpred <- sumpred+pred
    res2[[i]] <- c(pred, tmp)
  }
  sums <- c(sums, sumpred)

}

sum(sums)
