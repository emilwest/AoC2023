library(tidyverse)

input <- readLines("12/ex")
d <- input %>%
  enframe() %>%
  separate(value, into = c("s", "n"), sep = " ") %>%
  mutate(l = str_length(s),

         nsep = str_split(n,","),

         n_dam = map_dbl(nsep, ~sum(as.numeric(.x))),
         n_unk = str_count(s, "\\?"),
         n_op = str_count(s, "\\."),

         n_l = map_dbl(nsep, length),
         .diff = l-n_l
         )

d$s[[1]] %>% strsplit("\\.")
