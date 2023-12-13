library(tidyverse)

input <- readr::read_file("13/ex")

d <- str_split(input, "\\r\\n\\r\\n") |>
  map(~str_split(.x, "\\r\\n"))
d <- d[[1]]

# test
x <- d[[1]] |> str_split("")
X <- unlist(x) |>
  matrix(nrow=length(x), ncol=length(x[[1]]), byrow = T)

X[X=="."] <- 0
X[X=="#"] <- 1
X <- matrix(as.numeric(X), ncol=length(x[[1]]))

X
xx <- colSums(X)
# 4 2 5 2 3 3 2 5 2
# splitta mellan 3 och 3 och fortsätt splitta
# om alla har en match är odet ok
sort(xx)
rowSums(X)

