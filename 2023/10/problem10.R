library(tidyverse)

input <- readLines("10/input") %>% str_split("")

# försökte generera alla kombinationer på ett snyggt sätt men gjorde det
# manuellt sen istället
# directions <-
#   tribble(
#     ~"val", ~"North", ~"West", ~"South", ~"East",
#     "|", 1, 0, 1, 0,
#     "-", 0, 1, 0, 1,
#     "L", 1, 0, 0, 1,
#     "J", 1, 1, 0, 0,
#     "7", 0, 1, 1, 0,
#     "F", 0, 0, 1, 1
#   )
#
# dir_long <- directions %>%
#   pivot_longer(-val, names_to = "direction")

y_len <- length(input[[1]])
x_len <- length(input)

X <- matrix(nrow=0, ncol=3)
colnames(X) <- c("x", "y", "val")
X

# input
# i <- 1
# j <- 1
for (i in seq_along(input)) {
  tmp <- input[[i]]
  for (j in seq_along(tmp)) {
    currchar <- tmp[[j]]
    is_pipe <- str_detect(currchar, "[\\-SLJ\\|7F]")
    if (is_pipe) X <- rbind(X, matrix(c(j, i, currchar), nrow = 1))
  }
}

d <- as_tibble(X) %>% mutate(across(x:y, as.numeric)) %>% arrange(x,y)
start <- d %>% filter(val=="S")
#start %>% bind_rows(d %>% filter(val!="S"))

g
# get grid around current X and Y coordinates
get_coords_around <- function(current_pos) {
  below_y <- current_pos$y+1
  above_y <- current_pos$y-1
  right_x <- current_pos$x+1
  left_x <- current_pos$x-1
  d %>%
    filter(between(x,
                   ifelse(current_pos$x == 1, 1, current_pos$x-1),
                   ifelse(current_pos$x == x_len, x_len, current_pos$x+1)
    )) %>%
    filter(between(y,
                   current_pos$y-1,
                   current_pos$y+1
    )) %>%
    mutate(
      direction = ifelse(below_y==y & current_pos$x==x, "South", NA),
      direction = ifelse(above_y==y & current_pos$x==x, "North", direction),
      direction = ifelse(right_x==x & current_pos$y==y, "East", direction),
      direction = ifelse(left_x==x & current_pos$y==y, "West", direction),
    ) %>%
    filter(!is.na(direction)) #%>%
    # join on valid directions
    #left_join(dir_long, by = join_by(val, direction))

}

# if a pipe can connect is a function of next direction relative to current char
can_connect <- function(currchar, .comp, direction) {
  case_when(
    # SOUTH/NORTH
    direction == "South" & currchar == "|" & .comp == "L" ~ TRUE,
    direction == "South" & currchar == "|" & .comp == "J" ~ TRUE,
    direction == "South" & currchar == "|" & .comp == "|" ~ TRUE,
    direction == "North" & currchar == "|" & .comp == "7" ~ TRUE,
    direction == "North" & currchar == "|" & .comp == "F" ~ TRUE,
    direction == "North" & currchar == "|" & .comp == "|" ~ TRUE,

    # NORTH/EAST
    direction == "East"  & currchar == "L" & .comp == "J" ~ TRUE,
    direction == "East"  & currchar == "L" & .comp == "7" ~ TRUE,
    direction == "East"  & currchar == "L" & .comp == "-" ~ TRUE,
    direction == "North" & currchar == "L" & .comp == "|" ~ TRUE,
    direction == "North" & currchar == "L" & .comp == "F" ~ TRUE,
    direction == "North" & currchar == "L" & .comp == "7" ~ TRUE,

    # NORTH/WEST
    direction == "North" & currchar == "J" & .comp == "F" ~ TRUE,
    direction == "North" & currchar == "J" & .comp == "7" ~ TRUE,
    direction == "North" & currchar == "J" & .comp == "|" ~ TRUE,
    direction == "West" & currchar == "J" & .comp == "F" ~ TRUE,
    direction == "West" & currchar == "J" & .comp == "-" ~ TRUE,
    direction == "West" & currchar == "J" & .comp == "L" ~ TRUE,

    #SOUTH/EAST
    direction == "South" & currchar == "F" & .comp == "J" ~ TRUE,
    direction == "South" & currchar == "F" & .comp == "|" ~ TRUE,
    direction == "South" & currchar == "F" & .comp == "L" ~ TRUE,
    direction == "East" & currchar == "F" & .comp == "-" ~ TRUE,
    direction == "East" & currchar == "F" & .comp == "J" ~ TRUE,
    direction == "East" & currchar == "F" & .comp == "7" ~ TRUE,

    # EAST/WEST
    direction == "East" & currchar == "-" & .comp == "J" ~ TRUE,
    direction == "East" & currchar == "-" & .comp == "7" ~ TRUE,
    direction == "East" & currchar == "-" & .comp == "-" ~ TRUE,
    direction == "West" & currchar == "-" & .comp == "L" ~ TRUE,
    direction == "West" & currchar == "-" & .comp == "F" ~ TRUE,
    direction == "West" & currchar == "-" & .comp == "-" ~ TRUE,

    # WEST/SOUTH
    direction == "West" & currchar == "7" & .comp == "L" ~ TRUE,
    direction == "West" & currchar == "7" & .comp == "F" ~ TRUE,
    direction == "West" & currchar == "7" & .comp == "-" ~ TRUE,
    direction == "South" & currchar == "7" & .comp == "|" ~ TRUE,
    direction == "South" & currchar == "7" & .comp == "L" ~ TRUE,
    direction == "South" & currchar == "7" & .comp == "J" ~ TRUE,
    .default = FALSE

  )

}


# curr_grid <- get_coords_around(start)

begin <- start
counter <- 0
restib <- tibble() %>% bind_rows(start)

while(TRUE) {
  currchar <- begin$val

  curr_grid <- get_coords_around(begin)
  # remove previous
  curr_grid <- anti_join(curr_grid, restib, by = join_by(x, y, val))

  if (nrow(curr_grid)==0) {
    print("Found S at last")
    break
  }

  if (counter==0) {
    # if (nrow(begin)!=2) stop("warning, first two chars are not close to S")

    # try with first
    # specialare
    begin <- curr_grid %>% filter(direction=="South")
  } else if (nrow(curr_grid)==1) {
    begin <- curr_grid
  } else {
    begin <- curr_grid %>%
      mutate(connect = can_connect(currchar = currchar, .comp = val, direction)) %>%
      filter(connect)
  }

  if (nrow(begin)==0) {
    print(str_glue("currchar {currchar}, curr_grid:"))
    print(curr_grid)
    stop("encountered no match")
  }

  # print(str_glue("currchar {currchar}, curr_grid:"))
  # print(begin)
  # print("----------")
  if(counter %% 1000==0) {
    # Print on the screen some message
    cat(paste0("iteration: ", counter, "\n"))
  }

  counter <- counter+1
  restib <- restib %>% bind_rows(begin)


}


rr <- restib %>% filter(val!="S")
furthestpoint <- round((nrow(rr))/2)
furthestpoint

# -------------------------
# PART 2

# calculate area of the loop:
# https://en.wikipedia.org/wiki/Shoelace_formula
# https://artofproblemsolving.com/wiki/index.php/Shoelace_Theorem

# starting point must be included twice in shoelace
XX <- restib %>% bind_rows(restib %>% slice(1))

area <- 0
#i <- 1
for (i in 1:(nrow(XX)-1)) {
  curr <- XX[i,]
  nextc <- XX[i+1,]
  dx = nextc$x + curr$x
  dy = nextc$y - curr$y
  area <- area + ((1/2)*(dx)*(dy))  # shoelace formula
}
area <- abs(area)


# Picks theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
# THe area of polygon A =
# interior points + boundary points/2 -1
# A = i + b / 2 - 1
# so
# i = A - b / 2 + 1

abs(area)-(nrow(restib)/2)+1
# 579
