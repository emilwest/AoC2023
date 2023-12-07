library(tidyverse)

d <- readLines("7/input") |> enframe()
d <- d |>
  separate(value, into = c("card", "bid")) |>
  mutate(bid=as.numeric(bid))

d

# Five of a kind, where all five cards have the same label: AAAAA
# Four of a kind, where four cards have the same label and one card has a different label: AA8AA
# Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
# Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
# Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
# One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
# High card, where all cards' labels are distinct: 23456

str_unique(c("c", "c", "b")) |> str_count()

d <- d |>
  mutate(charvec = str_split(card, ""))
  # mutate(highcard = map(charvec, ~ str_unique(.x) |> table()
  #                       ))
  #


count_n <- function(x, n) {
  a <- x |> unlist() |> str_count(n)
  length(a[which(a==1)])
}



chartable
i <- 1
resvec <- c()
for (i in seq_len(nrow(chartable))) {
  tmp <- chartable[i,] |> select(where(~!is.na(.x)))
  txt <- ""
  if (count_n(tmp, "5")==1) txt <- "five of a kind"
  if (count_n(tmp, "4")==1) txt <- "four of a kind"
  if (count_n(tmp, "3")==1 & count_n(tmp, "2")==1 ) txt <- "full house"
  if (count_n(tmp, "3")==1 & count_n(tmp, "1")==2 ) txt <- "three of a kind"
  if (count_n(tmp, "2")==2) txt <- "two pair"
  if (count_n(tmp, "2")==1 & count_n(tmp, "1") == 3) txt <- "one pair"
  if (count_n(tmp, "1")==5) txt <- "high card"
  resvec <- c(resvec, txt)
}




chartable <- map(d$charvec, table) |> bind_rows()
help(package="dplyr")
chartable |>
  filter(if_any(.cols = everything(), .fns = ~ .x == 5))

chartable |>
  mutate(x = if_any(.cols = everything(), .fns = ~ .x == 5) ))

chartable |>
  mutate(across(everything(), ))

# d |>
#   bind_cols(chartable) |>
#   mutate(across(-c(name,card,bid,chavec),
#                 ~
#                 ))


