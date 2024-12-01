library(tidyverse)

input <- readLines("11/input")

xlen <- nchar(input[[1]])
ylen <- length(input)

M <- matrix(0, nrow = length(input), ncol = xlen)
A <- matrix(nrow=0, ncol = 3)
colnames(A) <- c("x", "y", "rank")

counter <- 0
for (y in seq_along(input)) {
  tmp <- strsplit(input[[y]], "")[[1]]
  for (x in seq_along(tmp)) {
    currchar <- tmp[x]
    if (str_detect(currchar, "#")) {
      counter <- counter+1
      A <- rbind(A, matrix(c(x,y,counter), nrow=1))
      M[y, x] <- 1
    }
  }
}

# Expand universe
emptyrows <- which(rowSums(M)==0)
emptycols <- which(colSums(M)==0)

# at each step the next coordinate will shift one step to the right
# add offset 1,2,3,4,5, etc for each added empty row
addera <- cumsum(rep(1,xlen))
rowinds <- emptyrows+addera[1:length(emptyrows)]
colinds <- emptycols+addera[1:length(emptycols)]

# New matrix 'M2' with all zeros,
# No. of rows = original matrix rows + number new rows to be added
M2 <- matrix(0, nrow=ylen+length(emptyrows), ncol=xlen+length(emptycols))
M2[-rowinds, -colinds] <- M

# saved coordinates also need to be adjusted
A2 <- which(M2 != 0, arr.ind = T)
A2 <- A2[order(A2[, 1]),  ] # reorder

numgalaxies <- nrow(A2)
numpairs <- choose(numgalaxies, 2)

# create id to create combinations from
A2 <- cbind(A2, id= 1:nrow(A2))
combs <- combn(A2[,3], 2, simplify = F)

if(length(combs)!=numpairs) stop("Calculation is wrong")

distres <- c()
for (i in seq_along(combs)) {
  print(i)

  currcomb <- combs[[i]]
  firstcoord <- A2[currcomb[1], ]
  secondcoord <- A2[currcomb[2], ]


  dy <- abs(firstcoord[1] - secondcoord[1])
  dx <- abs(firstcoord[2] - secondcoord[2])

  if(i %% 1000==0) {
    cat(paste0("iteration: ", i, "\n"))
  }
  res <- dy+dx
  distres <- c(distres, unname(res))

}
sum(distres)

# part 2
# expand universe empty rows by 1 000 000 each
# im an idiot, i actually didnt need to add empty rows to matrix, a can just add it to the coordinates
# however it was good to visualize

# Expand universe
emptyrows <- which(rowSums(M)==0)
emptycols <- which(colSums(M)==0)

adjust <- 10^6-1

# copy of A
B <- A
addera <- cumsum(rep(adjust, xlen))

# first empty x/y coordinate is as-is ie no offset, but then it will increase cumulatively
offset_y <- c(0, addera[1:(length(emptyrows)-1)])
offset_x <- c(0, addera[1:(length(emptycols)-1)])

for (j in 1:length(emptyrows)) {
  currempty <- emptyrows[j]
  currempty <- currempty+offset_y[j]
  # all remaining y values after current empty shift by adjustment

  # subset rows where y >= new y after adding empty rows
  # and add adjustmend
  newy <- B[B[, "y"] >= currempty, "y"]
  newy <- newy+adjust
  B[B[, "y"] >= currempty, "y"] <- newy
}

for (i in 1:length(emptycols)) {
  currempty <- emptycols[i]
  currempty <- currempty+offset_x[i]

  newy <- B[B[, "x"] >= currempty, "x"]
  newy <- newy+adjust
  B[B[, "x"] >= currempty, "x"] <- newy
}


combs <- combn(B[,"rank"], 2, simplify = F)
numgalaxies <- nrow(B)
numpairs <- choose(numgalaxies, 2)

if(length(combs)!=numpairs) stop("Calculation is wrong")

distres2 <- c()
for (i in seq_along(combs)) {
  print(i)

  currcomb <- combs[[i]]
  firstcoord <- B[currcomb[1], ]
  secondcoord <- B[currcomb[2], ]

  dy <- abs(firstcoord[1] - secondcoord[1])
  dx <- abs(firstcoord[2] - secondcoord[2])

  if(i %% 25000==0) {
    cat(paste0("iteration: ", i, "\n"))
  }
  res <- dy+dx
  distres2 <- c(distres2, unname(res))

}
sum(distres2)

