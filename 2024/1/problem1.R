library(tidyverse)

dat <- readLines("2024/1/input.txt")[-1] %>% str_split("\\s+") %>% map(as.numeric)

v1 <- v %>% map(~.x[1]) %>% unlist()
v2 <- v %>% map(~.x[2]) %>% unlist()

# part1
sum(abs(sort(v1)-sort(v2)))

# part2
tibble(a=v1, b=v2) %>%
  rowwise() %>%
  mutate(aib = sum(v2 %in% a),
         sim = a*aib) %>%
  ungroup() %>%
  summarise(s=sum(sim))
