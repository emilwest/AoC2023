library(tidyverse)

lines <- readLines("1/input1.txt")

nummer <- str_extract_all(lines, "[0-9]+") |> map(str_c, collapse="") |> unlist()

nummer |>
  enframe() |>
  mutate(l = str_length(value)) |>
  mutate(num = ifelse(l==1,
                      as.numeric(str_c(value,value)),
                      as.numeric(str_c(
                        str_extract(value, "^[0-9]"),
                        str_extract(value, "[0-9]$")
                      ))
                      )) |>
  summarise(sum(num))

# part 2


one_digits <- c("zero"="0", "one"="1", "two"="2", "three"="3", "four"="4", "five"="5",
                   "six"="6", "seven"="7", "eight"="8", "nine"="9")


# line <- "eighthreeseveninefiveeightoneeightthreeeight"

numvec <- c()
numvec2 <- c()
for (line in lines) {
  resvec <- c()
  strbuilder <- c()
  lastchar <- c()
  for (char in str_split_1(line, pattern = "")) {

    if (str_detect(char, "[0-9]")) {
      resvec <- c(resvec, char)
      next
    }

    # tar hand om fall som eighthreesevenine
    if (!is.null(lastchar)) {
      strbuilder <- str_c(strbuilder, lastchar, char, collapse = "")
      lastchar <- c()
      next
    } else {
      strbuilder <- str_c(strbuilder, char, collapse = "")
    }

   # print(strbuilder)

    if (str_detect(strbuilder, "zero|one|two|three|four|five|six|seven|eight|nine|ten")) {
      #print("match!!!")
      num <- str_replace_all(strbuilder, one_digits)
      num <- str_remove_all(num, "[a-zA-Z]")
      #print(num)
      resvec <- c(resvec, num)
      strbuilder <- c()
      lastchar <- char
    }
  }

  if (length(resvec)==1) {
    resvec <- rep(resvec, 2)
  }

  full <- as.numeric(str_c(resvec, collapse=""))
  temp <- as.numeric(str_c(resvec[1], last(resvec), collapse=""))
  numvec <- c(numvec, temp)
  numvec2 <- c(numvec2, full)
  temp <- c()
}

sum(numvec)

