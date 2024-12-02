library(tidyverse)

dat <- readLines("2024/2/input.txt") |> str_split("\\s+") |> map(as.numeric)

# So, a report only counts as safe if both of the following are true:
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.

check_all_incrasing_or_decreasing <- function(.vec) {
  x <- sign(diff(.vec)) # +1 positiv, -1 negativa nummer
  return(all(x) == 1 | all(x) == -1)
}

is_diff_ok <- function(.vec) {
  return(all(between(abs(diff(.vec)), 1, 3)))
}

between(c(1,1,2,3,3,3), 1, 3)

xx <- dat |> map(diff) |> map(~all(sign(.x) == 1 | sign(.x) == -1)) |> unlist()
dat[which(xx==FALSE)] |> map(diff) |> map(.x > 3)
dat[which(xx==FALSE)] |> map(check_all_incrasing_or_decreasing)
dat |> map(check_all_incrasing_or_decreasing)
yy <- dat |> map(is_diff_ok) |> unlist()

dat[which(yy==FALSE)] |> map(diff)


dat |>
  keep(~check_all_incrasing_or_decreasing(.x) & is_diff_ok(.x) ) |>
  length()

