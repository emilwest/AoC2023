library(tidyverse)


#input <- readLines("18/ex")
input <- readLines("18/input")

# assume start is (0,0)
X <- matrix(nrow = 0, ncol = 2)
colnames(X) <- c("x", "y")
X <- rbind(X, c(0, 0))
colors <- c()

borders <- 0
#i <- 1
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
  borders <- borders+steps

}

X
borders

# calculate area with shoelace formula
area <- 0
for (i in 1:(nrow(X)-1)) {
  curr <- X[i,]
  nextc <- X[i+1,]
  dx = nextc["x"] + curr["x"]
  dy = nextc["y"] - curr["y"]
  area <- area + ((1/2)*(dx)*(dy))  # shoelace formula
}
area <- abs(area) |> unname()

# Picks theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
# THe area of polygon A =
# interior points + boundary points/2 -1
# A = i + b / 2 - 1
# so
# i = A - b / 2 + 1

interior_points <- area-borders/2+1

borders+interior_points
# 45159


# part 2
# The first five hexadecimal digits encode the distance in meters as a five-digit
# hexadecimal number. The last hexadecimal digit encodes the direction to dig:
# 0 means R, 1 means D, 2 means L, and 3 means U.

#70c710 = R 461937
#0dc571 = D 56407
#5713f0 = R 356671
#d2c081 = D 863240
#59c680 = R 367720
#411b91 = D 266681
#8ceee2 = L 577262
#caa173 = U 829975
#1b58a2 = L 112010
#caa171 = D 829975
#7807d2 = L 491645
#a77fa3 = U 686074
#015232 = L 5411
#7a21e3 = U 500254

base::strtoi("70c71", base=16L)

colors2 <- colors |> str_remove("#")
newsteps <- colors2 |> substring(1,5) |> strtoi(base = 16L)
directions <- colors2 |> substring(6) |>
  str_replace_all(c("0"="R", "1"="D", "2"="L", "3"="U"))


# assume start is (0,0)
X <- matrix(nrow = 0, ncol = 2)
colnames(X) <- c("x", "y")
X <- rbind(X, c(0, 0))

borders <- 0
#i <- 1
for (i in seq_along(newsteps))  {
  direction <- directions[i]
  steps <- newsteps[i]

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
  borders <- borders+steps

}
X

# calculate area with shoelace formula
area <- 0
for (i in 1:(nrow(X)-1)) {
  curr <- X[i,]
  nextc <- X[i+1,]
  dx = nextc["x"] + curr["x"]
  dy = nextc["y"] - curr["y"]
  area <- area + ((1/2)*(dx)*(dy))  # shoelace formula
}
area <- abs(area) |> unname()
interior_points <- area-borders/2+1

options(scipen=999)
borders+interior_points
# 134549294799713
