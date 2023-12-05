library(tidyverse)

input <- readr::read_file("5/input") %>% str_split_1("\\r\\n\\r\\n") %>% str_split("\\r\\n")
seeds <- input[[1]] %>% str_remove("seeds: ") %>% str_split_1(" ") %>% as.numeric()
input <- input[-1]
n <- input %>% map(1) %>% unlist()
lista <- input %>% map(tail, -1) %>%  map(str_split, " ") %>% set_names(n) %>% map(~.x %>% map(as.numeric) )
lista <- lista %>% map(., discard, .p = rlang::is_na)

# destination range start, the source range start, and the range length
# get_map <- function(x) {
#   .range <- x[3]-1
#   dest <- seq(from=x[1], to=x[1]+.range)
#   .source <- seq(from=x[2], to=x[2]+.range)
#
#   set_names(dest, .source)
# }
#
#
# get_dest <- function(.map, .source) {
#   index <- which(.map==.source)
#   if (identical(index, integer(0))) return(.source)
#   .map[[index]]
# }

#get_map(inner) %>% get_dest(50)

# i <- 1
# j <- 1
# pre-allocate map memory


# mapmemory <- list()
# for (i in seq_along(lista)) {
#   tmp <- lista[[i]]
#
#   # print("----")
#   # print(i)
#
#   allvec <- c()
#   for (j in seq_along(tmp)) {
#     print(j)
#     inner <- tmp[[j]]
#
#     check_between(inner, 79)
#
#     v <- get_map(inner)
#
#     allvec <- c(allvec, v)
#
#     allvec <- allvec[unique(names(allvec))]
#   }
#   mapmemory[[n[i]]] <- allvec
# }
#


# i <- 1
j <- 25
# s <- 14
s <- seeds[1]

finalloc <- c()
for (s in seeds) {
 # print(s)
  currsource <- s

  for (i in seq_along(lista)) {
    print(str_glue("i={i} {n[i]} currsource: {currsource} s {s}"))

    tmp <- lista[[i]]

    mapmemory <- c()
    for (j in seq_along(tmp)) {
      #print(j)
      inner <- tmp[[j]]
      .range <- inner[3]-1

      # a check here to speed up and ignore all that are not in range to begin with
      if (dplyr::between(currsource, left = inner[2], right = inner[2]+.range)) {
        print(str_glue("IN RANGE at {j}"))

        sourceseq <- seq(from=inner[2], to=inner[2]+.range)
        dest <- seq(from=inner[1], to=inner[1]+.range)

        index <- which(sourceseq %in% currsource )
        mapmemory <- dest[index]

        #mapmemory <- get_map(inner)
        break
      }
    }

    if (is.null(mapmemory)) mapmemory <- currsource

    #currsource <- get_dest(mapmemory, currsource)
    currsource <- mapmemory
    #print(str_glue("{n[i]} {currsource}"))
  }

 # print(str_glue("currsource: {currsource}"))
  finalloc <- c(finalloc, currsource)
}

min(finalloc)





