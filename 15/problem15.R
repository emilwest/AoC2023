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
  map(calc_hash) |>
  unlist() |> sum()
summa


# part 2


chars |>
  str_split("")

