library(tidyverse)

# ascii <- rvest::read_html("https://en.wikipedia.org/wiki/ASCII#Printable_characters") |>
#   rvest::html_table()
# ascii <- ascii[[4]]
# names(ascii) <- c("bin","oct","dec","hex","63","65","67")
# write_csv2(ascii |> slice(-1), "15/ascii.csv")

ascii <- read_csv2("15/ascii.csv", col_types = cols(.default = "c"))
ascii <- ascii |> select(code=dec, char=`67`) |> mutate(char=str_replace(char, "space", " "))

input <- readLines("15/input")
# input <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
chars <- input |> str_split_1(",")


# Determine the ASCII code for the current character of the string.
# Increase the current value by the ASCII code you just determined.
# Set the current value to itself multiplied by 17.
# Set the current value to the remainder of dividing itself by 256.

get_ascii <- function(.char) {
  code <- ascii |> filter(char==.char) |> pull(code) |> as.numeric()
  if (is.null(code) | is.na(code)) stop(str_glue("char {.char} does not exist"))
  code
}

calc_ascii <- function(.char, value) {
  code <- get_ascii(.char)
  value <- value + code
  value <- value*17
  value <- value%%256
  return(value)
}

calc_hash <- function(.chars) {
  resvec <- 0
  for (char in .chars) {
    resvec <- calc_ascii(char, resvec)
    #print(resvec)
  }
  resvec
}


summa <- chars |>
  str_split("") |>
  map_dbl(calc_hash) |>
  sum()
summa


# part 2


xx <- chars |>
  enframe() %>%
  mutate(token = str_extract(value, "^([a-z]+)(=|-)", group = 1 ),
         operator = str_extract(value, "(=|-)"),
         number = str_extract(value, "\\=([0-9]+)$", group = 1),
         operator_meaning = ifelse(operator == "=", "add focal length", "remove lens"),
         box = token %>% str_split("") %>% map_dbl(calc_hash)
         )

# pre-allocate output vectors/matrices
n_boxes <- 256
boxes <- vector("list", length=n_boxes)
X <- matrix(nrow=0, ncol=2)
colnames(X) <- c("lab", "length")
for (i in 1:n_boxes) {
  boxes[[i]] <- X
}


xx
#i <- 151
for (i in seq_len(nrow(xx))) {
  tmp <- xx[i,]
  b <- as.numeric(tmp$box)

  currbox <- boxes[[b+1]]
  currbox <- matrix(currbox, ncol=2)
  colnames(currbox) <- c("label", "length")

  if (tmp$operator == "=") {
    isempty <- all(is.na(currbox))

    if (!isempty & any(currbox[,1] == tmp$token)) {
      # replace length
      currbox[which(currbox[,1] == tmp$token), 2] <- tmp$number
    } else {
      currbox <- rbind(currbox, matrix(c(tmp$token, tmp$number), ncol=2))
    }

    boxes[[b+1]] <- currbox
  }
  if (tmp$operator == "-") {
    # check if label exist in box
    if (any(currbox[,1] == tmp$token)) {
      if (!is.matrix(currbox)) stop("nej1")
      remove_ind <- which(currbox[,1] == tmp$token)
      currbox <- currbox[-remove_ind, ]
      currbox <- matrix(currbox, ncol=2)
      colnames(currbox) <- c("label", "length")
      if (!is.matrix(currbox)) stop("nej2")
      boxes[[b+1]] <- currbox
    }


  }

}

boxes

map(boxes, ~as_tibble(.x) %>% mutate(across(everything(), as.character))) %>%
  bind_rows(.id = "box") %>%
  group_by(box) %>%
  mutate(slot = row_number()) %>%
  ungroup() %>%
  mutate(
    across(-label, as.numeric),
    product = box*slot*length) %>%
  summarise(summa = sum(product, na.rm=T))


# 248279
