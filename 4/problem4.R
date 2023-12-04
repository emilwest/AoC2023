library(tidyverse)

#input <- readLines("4/ex2")
input <- readLines("4/input")
kort <- input |>
  str_split("\\s+\\|\\s+") |>
  enframe() |>
  unnest(value)


facit <- kort |>
  filter(str_detect(value, "Card")) |>
  mutate(value = str_remove(value, "Card\\s+[0-9]+:\\s+"))
dragning <- kort |>
  filter(!str_detect(value, "Card")) |>
  dplyr::rename("dragning"="value")

x <- facit |>
  left_join(dragning, by="name") |>
  mutate(
    value= str_split(value, "\\s+") |> map(as.numeric),
    dragning= str_split(dragning, "\\s+") |> map(as.numeric)
         )


x



pow <- function(.l) {
  if (.l == 0) return(0)
    sekvens <- seq(from = 0, to = (.l-1))
    res <- 2^sekvens
    res[.l]
}

pow(0)
pow(2)
pow(3)
pow(4)
pow(12)

points <- c()
for (i in seq_len(nrow(x))) {
  tmp <- x[i,]
  drag <- tmp$dragning[[1]]
  val <- tmp$value[[1]]
  vinnare <- drag[which(drag %in% val)]

  points[i] <- pow(length(vinnare))
}

sum(points)


# part 2

#i <- 5
# number of instances of cards (originals+copies)
copy_memory <- c()
for (i in seq_len(nrow(x))) {
  print(i)
  tmp <- x[i,]
  drag <- tmp$dragning[[1]]
  val <- tmp$value[[1]]
  vinnare <- drag[which(drag %in% val)]

  l <- length(vinnare)
  copy_memory <- c(copy_memory, i) # add original first
  tab <- table(copy_memory) # keep track of number of copies

  if (l>0) {
    copy_seq <- seq(from=i+1, to = l+i) # copies won
    # multiplier = current i has multiple copies, copy_seq will be multiplied by this
    multiplier <- unname(tab[names(tab)==i])

    if (multiplier==0) stop(print(i))
    # repeat copies won by multiplier
    copy_memory <- c(copy_memory, rep(copy_seq, multiplier))
    #tab <- table(copy_memory)

    #print(tab)
  }


}

tab
sum(tab)
names(tab)
tab[1][1] <- tab[1][1]+1

X <- matrix(ncol=nrow(x))
X[1,1] <- 11

X + matrix(c(1,2,3,4), nrow=1)
