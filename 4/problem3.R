library(tidyverse)

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

  l <- length(vinnare)
  points[i] <- pow(l)

}

points
sum(points)
pow(4)







