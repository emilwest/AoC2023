library(tidyverse)

input <- readr::read_file("5/ex") %>% str_split_1("\\r\\n\\r\\n") %>% str_split("\\r\\n")
seeds <- input[[1]] %>% str_remove("seeds: ") %>% str_split_1(" ") %>% as.numeric()
input <- input[-1]
n <- input %>% map(1) %>% unlist()
lista <- input %>% map(tail, -1) %>%  map(str_split, " ") %>% set_names(n) %>% map(~.x %>% map(as.numeric) )
lista <- lista %>% map(., discard, .p = rlang::is_na)



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

# part 2
#seeds <- c(20, 5, 50, 4) # test

udda <- seq(from=1, to = length(seeds), by =2)
jamna <- seq(from=1, to = length(seeds))[-udda]






# MapEntry = tuple[int, int, int]
# Range = tuple[int, int]
#
# class _Problem(MultiLineProblem[int], ABC):
#   def __init__(self):
#   seeds, *maps = split_at(self.lines, lambda x: x == '')
# self.seeds = [int(i) for i in seeds[0][7:].split()]
# items = [[[int(i) for i in vals.split()] for vals in m[1:]] for m in maps]
# self.maps = [sorted((s, s + o, d - s) for d, s, o in f) for f in items]
#
# class Problem1(_Problem):
#   def solution(self) -> int:
#   def get_next_value(item: int, m: list[MapEntry]) -> int:
#   for start, end, offset in m:
#   if start <= item < end:
#   return item + offset
# return item
# return min(reduce(get_next_value, self.maps, seed) for seed in self.seeds)
#
# class Problem2(_Problem):
#   def solution(self) -> int:
#   def get_next_ranges(ranges: Iterable[Range], m: list[MapEntry]) -> Iterator[Range]:
#   for r in ranges:
#   s, e = r
# for start, end, offset in m:
#   if s < start:
#   yield s, min(e, start)
# if start >= e:
#   break
# if s >= end:
#   continue
# yield max(s, start) + offset, min(end, e) + offset
# s = end
# else:
#   if s < e:
#   yield s, e
# seed_ranges: Iterable[Range] = ((s, s + o) for s, o in batched(self.seeds, 2))
# return min(s for s, _ in reduce(get_next_ranges, self.maps, seed_ranges))
#
#






udda
jamna



expand_seeds <- function(i) {
  u <- seeds[udda][i]
  .range <- seeds[jamna][i]-1

  seq(from = u, to = u+.range)
}

length(seeds)/2
expand_seeds(1)
all((seeds[1]:(seeds[2]-1)) == expand_seeds(1))

expand_seeds(2)
#
# x <-  map(1:(length(seeds)/2), expand_seeds)
# vec <- x[[1]]
#
# split_to_range <- function(vec) {
#   middle_ind <- (length(vec)/2)
#   left <- c(vec[1], vec[middle_ind])
#   right <- c(vec[middle_ind+1], vec[length(vec)])
#   c(left, right)
# }
#
# # TODO
#
# split_to_range(x[[1]])
#

ex <- c(79, 14, 55, 13)
#
# source_start <- ex[1]
# source_end <- ex[2]-1
# lista[[1]]
#
#
#
# M
#
#
#
# destinations <- list()
# sources <- c()
# finalloc <- c()
#
# i <- 1
# for (i in seq_along(udda)) {
#   # print(s)
#   currsource_start <- seeds[udda[i]]
#   currsource_end <- currsource_start + (seeds[jamna[i]]-1)
#
#   M <- matrix(c(currsource_start, currsource_end), nrow=1)
#   # M <- M[1:(nrow(M)-1),]
#   # lastval <- M[-1,]
#
#   destinations[[i]] <- M
#
#   for (j in seq_along(lista)) {
#     print(str_glue("i={i} {n[i]} "))
#
#     sources <- destinations[[j]]
#     destinations <- list()
#
#     tmp <- lista[[j]]
#     while(nrow(sources) > 0) {
#       print(sources)
#
#       if (nrow(sources)==1) curr <-sources else curr <- sources[-1,]
#       #sources <- sources[1:(nrow(M)-1),]
#
#       currsource_start <- curr[,1]
#       currsource_end <- curr[,2]
#
#       sources
#
#       #print(j)
#       inner <- tmp[[j]]
#       .range <- inner[3]-1
#       .source <- inner[2]
#       .source_end <- .source+.range
#       .dest <- inner[1]
#
#       if (currsource_start >= .source & currsource_end <= .source_end) {
#         destinations <- append(destinations,  matrix(c(.source, .source_end), nrow=1))
#         break
#       }
#
#       if (currsource_end < .source | currsource_start > .source_end ) {
#         next
#       }
#
#       if (currsource_start < .source) {
#         sources <- append(sources, matrix(c(currsource_start, .source-1), nrow=1))
#         sources <- append(sources, matrix(c(.source, currsource_end), nrow=1))
#         break
#       }
#
#       if (currsource_end > .source_end) {
#         sources <- append(sources, matrix(c(currsource_start, currsource_end), nrow=1))
#         sources <- append(sources, matrix(c(.source_end+1, currsource_end), nrow=1))
#         break
#       }
#
#     }
#
#   }
#
#   # print(str_glue("currsource: {currsource}"))
#   #finalloc <- c(finalloc, currsource)
# }
#
#
#
# #
# #
# #
# # splitseed <- split_to_range(x[[1]])
# # finalloc2 <- c()
# #
# # for (s in splitseed) {
# #   # print(s)
# #   currsource <- s
# #
# #   if (currsource == min(finalloc))
# #
# #
# #   for (i in seq_along(lista)) {
# #     print(str_glue("i={i} {n[i]} currsource: {currsource} s {s}"))
# #
# #     tmp <- lista[[i]]
# #
# #     mapmemory <- c()
# #     for (j in seq_along(tmp)) {
# #       #print(j)
# #       inner <- tmp[[j]]
# #       .range <- inner[3]-1
# #
# #       # a check here to speed up and ignore all that are not in range to begin with
# #       if (dplyr::between(currsource, left = inner[2], right = inner[2]+.range)) {
# #         print(str_glue("IN RANGE at {j}"))
# #
# #         sourceseq <- seq(from=inner[2], to=inner[2]+.range)
# #         dest <- seq(from=inner[1], to=inner[1]+.range)
# #
# #         index <- which(sourceseq %in% currsource )
# #         mapmemory <- dest[index]
# #
# #         #mapmemory <- get_map(inner)
# #         break
# #       }
# #     }
# #
# #     if (is.null(mapmemory)) mapmemory <- currsource
# #
# #     #currsource <- get_dest(mapmemory, currsource)
# #     currsource <- mapmemory
# #     #print(str_glue("{n[i]} {currsource}"))
# #   }
# #
# #   # print(str_glue("currsource: {currsource}"))
# #   finalloc <- c(finalloc, currsource)
# # }
#
#
#
#
#
#
# #
# # # Part 2 ------------------------------------------------------------------
# # lista
# #
# #
# # section = strsplit(almanac_raw[[1]], ":")[[1]]
# # name = section[1]
# # seeds = matrix(as.numeric(strsplit(trimws(section[2]), "\\s+")[[1]]), ncol=2, byrow = T)
# # seeds[,2] = seeds[,1]+seeds[,2]-1
# # seeds = seeds[order(seeds[,1]),]
# # seeds = cbind(NA, NA, seeds)
# #
# # almanac = setNames(list(seeds), name)
# #
# # invisible(sapply(seq_along(almanac_raw)[-1], function(section_id){
# #   section = strsplit(almanac_raw[[section_id]], ":")
# #   name = section[[1]]
# #   map = strsplit(trimws(section[-1]), "\\s+")
# #
# #   map = Reduce(rbind, lapply(map, function(ranges){
# #     ranges = as.numeric(ranges)
# #
# #     sourceRange = cbind(ranges[2], ranges[2]+ranges[3]-1)
# #     destinationRange = cbind(ranges[1], ranges[1]+ranges[3]-1)
# #
# #     range = cbind(sourceRange, destinationRange)
# #   }))
# #
# #   map = map[order(map[,1]),]
# #
# #   if(nrow(map) == 0){
# #     map = cbind(Inf, Inf, Inf, Inf)
# #   } else {
# #     map = rbind(rep(c(-Inf, min(map[,1:2])-1), 2),
# #                 map,
# #                 rep(c(max(map[,1:2])+1, Inf), 2))
# #
# #     newRanges = Reduce(rbind, lapply(2:nrow(map), function(row){
# #       start = map[row-1,2]+1
# #       end = map[row,1]-1
# #       if(start > end) return()
# #
# #       cbind(start, end, start, end)
# #     }))
# #
# #     map = rbind(map, newRanges)
# #   }
# #
# #   almanac <<- append(almanac, setNames(list(map), name))
# #   return()
# # }))
# #
# # almanac2 = almanac
# #
# # invisible(sapply(seq_along(almanac_raw)[-1], function(section_id){
# #   relevantSources = almanac2[[section_id-1]][,3:4]
# #
# #   map = almanac2[[section_id]]
# #   map = Reduce(rbind, apply(map, 1, function(ranges){
# #     matches = cbind(pmax(relevantSources[,1], ranges[1]), pmin(relevantSources[,2], ranges[2]))
# #     matches = matches[matches[,2] - matches[,1] > 0,,drop=F]
# #
# #     relevantIndizes = matches - ranges[1]
# #
# #     rangeStart = matches
# #     rangeEnd = matches
# #
# #     if(all(relevantIndizes != Inf)){
# #       rangeEnd = matrix(c(ranges[3] + relevantIndizes[,1], ranges[3] + relevantIndizes[,2]), ncol=2)
# #     }
# #
# #     if(prod(dim(rangeStart),dim(rangeEnd)) != 0 && all(dim(rangeStart) == dim(rangeEnd))) {
# #       return(cbind(rangeStart, rangeEnd))
# #     }
# #
# #     return(NULL)
# #   }))
# #
# #   map = map[order(map[,1]),]
# #
# #   almanac2[[section_id]] <<- map
# #   return()
# # }))
# #
# # min(almanac2$`humidity-to-location map`[,3:4])
# #
# #
