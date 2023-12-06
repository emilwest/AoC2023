library(tidyverse)

# skapar grupper baserat på obrutna sekvenser:
create_groups_by_unbroken_sequence <- function(x) {
  rr <- rle(x - seq_along(x))
  rr$values <- seq_along(rr$values)
  s <- split(x, inverse.rle(rr))
  s |> enframe(name="grupp", value="id") |> unnest(id) |> pull(grupp) |> as.numeric()
}


input <- readLines("3/input")
input %>% nchar()
# 140x140 matris

# symbols
M <- matrix(NA, nrow=1, ncol = 2)
colnames(M) <- c("x", "y")
C <- matrix(NA, nrow=1, ncol = 3)
colnames(C) <- c("x", "y", "value")
for (l in 1:length(input)) {
  chars <- str_split_1(input[l],"")
  for (char in 1:length(chars)) {
    currchar <- chars[char]
    isnum <- str_detect(currchar, "[0-9]")
    issymbol <- str_detect(currchar, "[^0-9\\.]")
   #  print(str_glue("issymbol {issymbol} currchar {currchar}"))
    if (issymbol) {
      M <- rbind(M, matrix(c(l, char), nrow = 1))
    }
    if (isnum) {
      C <- rbind(C, matrix(c(l, char, as.numeric(currchar)), nrow = 1))
    }
   }
}



M
C


# y <- c(1:5, 8:10, 13:15)
# create_groups_by_unbroken_sequence(y)

cd <- as_tibble(C[-1,]) |> group_by(x) |> mutate(id = create_groups_by_unbroken_sequence(y)) |> ungroup()
md <- as_tibble(M[-1,])

cd
md

allextracts <- tibble()

# curr_sym <- md[1,]
# curr_sym <- md[2,]
# curr_sym <- md[3,]
# curr_sym <- md[4,]
# curr_sym <- md[5,]
# curr_sym <- md[6,]

for (i in seq_len(nrow(md))) {
  curr_sym <- md[i,]

  # check current symbol between symbol x row below and above
  cd_sub <- cd |> filter(between(x,
                                 ifelse(curr_sym$x == 1, 1, curr_sym$x-1),
                                 ifelse(curr_sym$x == length(input), length(input), curr_sym$x+1)
  ))

  x <- cd_sub |>
    filter(between(y,
                   curr_sym$y-1,
                   curr_sym$y+1
    )) |>
    select(x, id) |>
    distinct()

  extracted <- x |>
    left_join(cd_sub, by = join_by(x, id)) |>
    select(x,y)

  allextracts <- allextracts |> bind_rows(extracted)
}

ex <- allextracts |>
  left_join(cd) |>
  group_by(x, id) |>
  summarise(v = str_c(value, collapse = "") |>  as.numeric()) |>
  ungroup()

sum(ex$v)


# -----------------------------------------------------------------------------
# PART2



# symbols
M <- matrix(NA, nrow=1, ncol = 2)
colnames(M) <- c("x", "y")
C <- matrix(NA, nrow=1, ncol = 3)
colnames(C) <- c("x", "y", "value")
for (l in 1:length(input)) {
  chars <- str_split_1(input[l],"")
  for (char in 1:length(chars)) {
    currchar <- chars[char]
    isnum <- str_detect(currchar, "[0-9]")
    issymbol <- str_detect(currchar, "[\\*]")
    #  print(str_glue("issymbol {issymbol} currchar {currchar}"))
    if (issymbol) {
      M <- rbind(M, matrix(c(l, char), nrow = 1))
    }
    if (isnum) {
      C <- rbind(C, matrix(c(l, char, as.numeric(currchar)), nrow = 1))
    }
  }
}


cd <- as_tibble(C[-1,]) |> group_by(x) |> mutate(id = create_groups_by_unbroken_sequence(y)) |> ungroup()
md <- as_tibble(M[-1,])

cd
md

allextracts <- tibble()
i <- 1

options(dplyr.summarise.inform = FALSE)
prods <- c()

for (i in seq_len(nrow(md))) {
  curr_sym <- md[i,]

  # check current symbol between symbol x row below and above
  cd_sub <- cd |> filter(between(x,
                                 ifelse(curr_sym$x == 1, 1, curr_sym$x-1),
                                 ifelse(curr_sym$x == length(input), length(input), curr_sym$x+1)
  ))

  x <- cd_sub |>
    filter(between(y,
                   curr_sym$y-1,
                   curr_sym$y+1
    )) |>
    select(x, id) |>
    distinct()

  if (nrow(x) > 1) {
    #print("ja")
    extracted <- x |>
      left_join(cd_sub, by = join_by(x, id)) |>
      select(x,y)

    ex <- extracted |>
      left_join(cd, by = join_by(x, y)) |>
      group_by(x, id) |>
      summarise(v = str_c(value, collapse = "") |>  as.numeric()) |>
      ungroup() |>
      pull(v) |>
      prod()
    #print(ex)

    prods <- c(prods,ex)
  }


  #allextracts <- allextracts |> bind_rows(extracted)
}


prods
sum(prods)
