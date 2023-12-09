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

d <- d |>
  mutate(charvec = str_split(card, ""))

count_n <- function(x, n) {
  a <- x |> unlist() |> str_count(n)
  length(a[which(a==1)])
}

chartable <- d$charvec |> map(table) |> bind_rows()

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

d$kind <- resvec

kinds <- c("high card", "one pair", "two pair", "three of a kind",
  "full house", "four of a kind", "five of a kind")


get_minimum <- function(.charvec, n=1) {
  x <- .charvec |> map(n) |> unlist() |> str_replace_all(
    c("T"="10", "J"="11", "Q"="12", "K"="13", "A"="14")
  ) |> as.numeric()

  .charvec[which(x==min(x))]
}


#k <- 1
tmp <- d %>% filter(kind == kinds[1])

cardsorted <- c()
for (k in kinds) {
  tmp <- d |> filter(kind==k)
  #print(tmp)
  while (nrow(tmp) > 0) {
    curr_vec <- tmp$charvec
    for (i in 1:5) {
      curr_vec <- get_minimum(curr_vec, i)
      #print(curr_vec)

      if (length(curr_vec)==1) {
        curr_vec <- str_c(curr_vec[[1]],collapse="")
        cardsorted <- c(cardsorted, curr_vec)
        print(str_glue("added {curr_vec}"))
        #currrank <- currrank+1
        break

      }
    }
    tmp <- tmp %>% filter(card!=curr_vec)
  }
}

d
xx <- enframe(cardsorted, name="rank", value="card") %>%
  left_join(d %>% select(card,bid), by = "card") %>%
  mutate(prod=rank*bid)
sum(xx$prod)


# -------------------------
# PART 2

to_num <- function(s) {
  str_replace_all(s,c("T"="10", "Q"="12", "K"="13", "A"="14", "J"="1"))
}
to_char <- function(s) {
  str_replace_all(s,c("10"="T", "12"="Q", "13"="K", "14"="A", "1"="J"))
}
get_max_char <- function(.charvec) {
  y <- to_num(names(.charvec)[names(.charvec)!="J"])
  to_char(y[which(y==max(as.numeric(y)))])
}


classify <- function(.charvec) {
  txt <- ""
  if (count_n(.charvec, "5")==1) txt <- "five of a kind"
  if (count_n(.charvec, "4")==1) txt <- "four of a kind"
  if (count_n(.charvec, "3")==1 & count_n(.charvec, "2")==1 ) txt <- "full house"
  if (count_n(.charvec, "3")==1 & count_n(.charvec, "1")==2 ) txt <- "three of a kind"
  if (count_n(.charvec, "2")==2) txt <- "two pair"
  if (count_n(.charvec, "2")==1 & count_n(.charvec, "1") == 3) txt <- "one pair"
  if (count_n(.charvec, "1")==5) txt <- "high card"
  txt
}

d %>% mutate(x=str_count(card,"J")) %>% filter(x==3)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="four of a kind" & x>0)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="three of a kind" & x==3)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="two pair" & x>1)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="one pair" & x==2)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="one pair" & x==1)
d %>% mutate(x=str_count(card,"J")) %>% filter(kind=="high card")

# i <- 102
resvec <- c()
for (i in seq_len(nrow(chartable))) {
  tmp <- chartable[i,] |> select(where(~!is.na(.x)))
  txt <- classify(tmp)

  if (any(names(tmp)=="J")) {
    num_j <- tmp[names(tmp)=="J"][[1]]

    if (txt=="four of a kind")  txt <- "five of a kind"
    else if (txt=="full house") txt <- "five of a kind"
    else if (txt=="three of a kind") txt <- "four of a kind"
    else if (txt=="two pair" & num_j==1) txt <- "full house"
    else if (txt=="two pair" & num_j==2) txt <- "four of a kind"
    else if (txt=="one pair") txt <- "three of a kind"
    #else if (txt=="one pair" & num_j==2) txt <- "one pair"
    else if (txt=="high card") txt <- "one pair"
  }

  resvec <- c(resvec, txt)
}

d$kind2 <- resvec


get_minimum2 <- function(.charvec, n=1) {
  x <- .charvec |> map(n) |> unlist() |> str_replace_all(
    c("T"="10", "J"="1", "Q"="12", "K"="13", "A"="14")
  ) |> as.numeric()

  .charvec[which(x==min(x))]
}

# tmp <- d %>% filter(kind2 == kinds[1])




cardsorted2 <- c()
for (k in kinds) {
  tmp <- d |> filter(kind2==k)
  #print(tmp)

  while (nrow(tmp) > 0) {
    curr_vec <- tmp$charvec
    i <- 1
    for (i in 1:5) {
      curr_vec <- get_minimum2(curr_vec, i)

      if (length(curr_vec)==1) {
        curr_vec <- str_c(curr_vec[[1]],collapse="")
        cardsorted2 <- c(cardsorted2, curr_vec)
        print(str_glue("added {curr_vec}"))
        #currrank <- currrank+1
        break

      }
    }
    tmp <- tmp %>% filter(card!=curr_vec)
  }
}

cardsorted2
cardsorted

xx2 <- enframe(cardsorted2, name="rank", value="card") %>%
  left_join(d %>% select(card,bid), by = "card") %>%
  mutate(prod=rank*bid)


sum(xx2$prod)
# 254412181
