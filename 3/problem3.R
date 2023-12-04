library(tidyverse)

input <- readLines("3/ex")
input %>% nchar()
# 140x140 matris


numvec <- c()
l <- 1
for (l in 1:length(input)) {
  chars <- str_split_1(input[l],"")

  x <- ""
  for (char in 1:length(chars)) {
    currchar <- chars[char]
    lastcharx <- substr(x, nchar(x), nchar(x))
    isnum <- str_detect(currchar, "[0-9]")

    if (isnum) {
      x <- str_c(x,currchar)
    }
    else if (!isnum & lastcharx != ",") {
      x <- str_c(x,",")
    }
    else if (!isnum & lastcharx == ",") {
      next
    }


  }

  extracted_nums <- as.numeric(str_split_1(substr(x, 1, nchar(x)-1), ","))
  numvec <- c(numvec, extracted_nums)

  print("newline")
}





