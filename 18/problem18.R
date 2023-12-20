library(tidyverse)


input <- readLines("18/ex")

# assume start is (0,0)
X <- matrix(nrow = 0, ncol = 2)
colnames(X) <- c("x", "y")
X <- rbind(X, c(0, 0))
colors <- c()

i <- 1
for (i in seq_along(input)) {
  currin <- input[i] |> str_split_1(" ")
  direction <- currin[1]
  steps <- as.numeric(currin[2])
  color <- currin[3] |> str_remove_all("[\\(\\)]")

  curr_pos <- X[nrow(X), 1:2]

  if (direction == "R") {
    curr_pos["x"] <- curr_pos["x"]+steps
  }
  if (direction == "L") {
    curr_pos["x"] <- curr_pos["x"]-steps
  }
  if (direction == "D") {
    curr_pos["y"] <- curr_pos["y"]-steps
  }
  if (direction == "U") {
    curr_pos["y"] <- curr_pos["y"]+steps
  }
  X <- rbind(X, c(curr_pos))
  colors <- c(colors, color)

}

X



gcd_math <- function(x, y) {
  if (x == 0){return(y)}
  else if (y == 0) {return(x)}
  else if (x > y) {return(gcd_math(x %% y, y))}
  else {return(gcd_math(x, y %% x))}
}


X
X <- abs(X)
gcd_math(6,5)
gcd_math()

Reduce(gcd_math, X)

sum(map2_dbl(X[,"x"], X[,"y"], ~ Reduce(gcd_math, )))
?reduce



# calculate area with shoelace formula
area <- 0
for (i in 1:(nrow(X)-1)) {
  curr <- X[i,]
  nextc <- X[i+1,]
  dx = nextc["x"] + curr["x"]
  dy = nextc["y"] - curr["y"]
  area <- area + ((1/2)*(dx)*(dy))  # shoelace formula
}
area <- abs(area)

# Picks theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
# THe area of polygon A =
# interior points + boundary points/2 -1
# A = i + b / 2 - 1
# so
# i = A - b / 2 + 1

abs(area)-(round(nrow(X)/2))+1
24
62-38

38+24

colSums(X)
X
rowSums(X) |> sum()
