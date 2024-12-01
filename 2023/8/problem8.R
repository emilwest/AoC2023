library(tidyverse)

input <- readLines("8/input")
instructions <- input[1]
instructionsvec <- str_split_1(instructions,"")
input <- input[-c(1:2)]
d <- input |>
  enframe() |>
  separate(value, into = c("origin", "inst"), sep = " = ") |>
  separate(inst, into = c("L", "R"), sep = ", ") |>
  mutate(across(L:R, ~ str_remove_all(.x, "[\\(\\)]")))


d
instructions
str_length(instructions)


currname <- 1
instruction_counter <- 1
numsteps <- 0

#final <- d$origin[nrow(d)]
x <- "AAA"
while (x != "ZZZ") {

  tmp <- d |> filter(origin==x)
  curr_instr <- instructionsvec[instruction_counter]
  # get next name
  x <- tmp[which(names(tmp)==curr_instr)][[1]]
  #currname <- d |> filter(origin == x) |> pull(name)


  # if not reached end for all instructions, starts from beginning of instructions again
  if (instruction_counter==length(instructionsvec)) {
    # print("restarted")
    # print(str_glue("numsteps {numsteps}. currname = {currname}"))
    instruction_counter <- 1
  } else {
    instruction_counter <- instruction_counter+1
  }
  numsteps <- numsteps+1

 print(str_glue("x={x}, final={final}, numsteps={numsteps}, curr_instr={curr_instr}"))

}


# part 2
# Simultaneously start on every node that ends with A.
# How many steps does it take before you're only on nodes that end with Z?

# https://stackoverflow.com/questions/62681308/create-a-function-for-least-common-multiple-for-a-vector-in-r-with-helper-a

gcd_math <- function(x, y) {
  if (x == 0){return(y)}
  else if (y == 0) {return(x)}
  else if (x > y) {return(gcd_math(x %% y, y))}
  else {return(gcd_math(x, y %% x))}
}

lcm_math <- function(x,y){
  return(x * y / gcd_math(x,y))
}



#x <- c()
starts <- d |> filter(str_detect(origin, "A$"))
x <- starts$origin
out <- list()
for (start_pos in x) {
  position <- start_pos
  instruction_counter <- 1
  numsteps <- 0
  print(str_glue("pos={position}"))

  while (!str_detect(position, "Z$")) {
    tmp <- d |> filter(origin == position)
    curr_instr <- instructionsvec[instruction_counter]
    # get next name
    position <- tmp[which(names(tmp)==curr_instr)][[1]]
    if (instruction_counter==length(instructionsvec)) {
      instruction_counter <- 1
    } else {
      instruction_counter <- instruction_counter+1
    }
    # print(str_glue("numsteps={numsteps}"))
    numsteps <- numsteps+1
  }

  out[[start_pos]] <- numsteps
}

options(scipen = 999)
# Reduce kör f(f(f(f(f(a, b), c), d), e), f) där f=lcm_math och a-f är numsteps per start
(ans_2 <- out %>% unlist %>% Reduce(lcm_math, .))



