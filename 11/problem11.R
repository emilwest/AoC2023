library(tidyverse)

input <- readLines("11/ex")

xlen <- nchar(input[[1]])
ylen <- length(input)

M <- matrix(0, nrow = length(input), ncol = xlen)
A <- matrix(nrow=0, ncol = 3)
colnames(A) <- c("x", "y", "rank")

# y <- 1
# x <- 1
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
A


numgalaxies <- nrow(A2)
numpairs <- choose(numgalaxies, 2)

# create id to create combinations from
A2 <- cbind(A2, id= 1:nrow(A2))
combs <- combn(A2[,3], 2, simplify = F)

if(length(combs)!=numpairs) stop("Calculation is wrong")



combs

i <- 3

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

# x_old <- A[,1]
# x_new <- A2[,2]
# difff <- x_new-x_old
# newdiff <- difff + 10^2-1
#
# A2[,2] <- x_old+newdiff
#
# y_old <- A[,2]
# y_new <- A2[,1]
# difff <- y_new-y_old
# newdiff <- difff + 10^2-1
#
# A2[,1] <- y_old+newdiff


adjust <- 10^6-1
adjust <- 100

# Expand universe
emptyrows <- which(rowSums(M)==0)
emptycols <- which(colSums(M)==0)

# at each step the next coordinate will shift one step to the right
# add offset 1,2,3,4,5, etc for each added empty row
addera <- cumsum(rep(1,xlen))+adjust
rowinds
rowinds <- emptyrows+addera[1:length(emptyrows)]
colinds <- emptycols+addera[1:length(emptycols)]

# which x coordinates are around emptyrows?
emptyrows[1]

A2 <- as_tibble(A) %>%
  mutate(y = ifelse(y > emptycols[1], y + adjust, y),
         x = ifelse(x > emptyrows[1], x + adjust, x)
         )

# create id to create combinations from
A2 <- as.matrix(
A2
)
combs <- combn(A2[,3], 2, simplify = F)

if(length(combs)!=numpairs) stop("Calculation is wrong")


distres2 <- c()
for (i in seq_along(combs)) {
  print(i)

  currcomb <- combs[[i]]
  firstcoord <- A2[currcomb[1], ]
  secondcoord <- A2[currcomb[2], ]


  dy <- abs(firstcoord[1] - secondcoord[1])
  dx <- abs(firstcoord[2] - secondcoord[2])

  if(i %% 10000==0) {
    cat(paste0("iteration: ", i, "\n"))
  }
  res <- dy+dx
  distres2 <- c(distres2, unname(res))

}
sum(distres2)*2




