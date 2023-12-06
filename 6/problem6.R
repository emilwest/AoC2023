library(tidyverse)

input <- readLines("6/input")

time <- input[1] |> str_split_1("\\s+")
dist <- input[2] |> str_split_1("\\s+")

time <- as.numeric(time[-1])
dist <- as.numeric(dist[-1])

time
dist

get_dist <- function(time, dur) {
  remaining_time <- time-dur
  remaining_time*dur
}

res <- map(
  1:length(time),
  ~ get_dist(time[.x], 1:time[.x])
)

(which(res[[1]]>dist[1]) |> length()) *
(which(res[[2]]>dist[2]) |> length()) *
(which(res[[3]]>dist[3]) |> length()) *
(which(res[[4]]>dist[4]) |> length())

#map2(.x = res, .y = 1:length(res), ~ which(.x[[.y]] > dist[.y] ))
#map2(.x = res, .y = dist, ~ which(.x > dist ))


#part 2

time <- as.character(time) |> str_c(collapse="") |> as.numeric()
dist <- as.character(dist) |> str_c(collapse="") |> as.numeric()

res2 <- get_dist(time, 1:time)
which(res2>dist) |> length()

