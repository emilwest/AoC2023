library(tidyverse)

lines <- readLines("2/input")

rgb <- lines %>%
  str_split(";") %>%
  enframe() %>%
  unnest(value) %>%
  mutate(value=str_squish(value) %>%
           str_remove_all("Game [0-9]+: ")) %>%
  separate_wider_delim(value, names=c("a","b","c"), delim = ",", names_sep = "_", too_many = "merge", too_few = "align_start") %>%
  mutate(across(everything(), str_squish)) %>%
  separate_wider_delim(cols = -name, delim = " ", names_sep = "_") %>%
  pivot_longer(cols = -name, names_sep = "_", names_to = c("v","letter", "i")) %>%
  drop_na(value) %>%
  select(-v) %>%
  #mutate(color = ifelse(i==2, value, NA))
  rowid_to_column() %>%
  pivot_wider(names_from = i, values_from = value, values_fn = unique) %>%
  fill(`1`, .direction = "down") %>%
  drop_na(`2`) %>%
  select(-rowid)


rgb <- distinct(rgb)

rgb <- rgb %>%
  group_by(name) %>%
  mutate(lid = case_when(letter == "a" ~1, letter=="b"~2, letter == "c" ~ 3)) %>%
  mutate(innergroup = cumsum(c(-1, diff(lid)) < 0)) %>%
  ungroup()



correct <- tibble(maximum = c(12,13,14), `2` = c("red", "green", "blue"))


rgb %>%
  select(-letter, -lid) %>%
  rowid_to_column() %>%
  pivot_wider(names_from = `2`, values_from = `1`, values_fn = unique)


rgb2 <- rgb %>%
  left_join(correct) %>%
  mutate(toomuch = as.numeric(`1`)>maximum)


toomuchs <- rgb2 %>%
  select(name, toomuch) %>%
  distinct() %>%
  filter(toomuch) %>%
  pull(name)

# correct ans
rgb2 %>%
  filter(!(name %in% toomuchs)) %>%
  pull(name) %>%
  unique() %>%
  as.numeric() %>%
  sum()



# part 2


rgb2 %>%
  group_by(name, `2`) %>%
  summarise(x=max(as.numeric(`1`))) %>%
  ungroup() %>%
  group_by(name) %>%
  summarise(p = prod(x)) %>%
  ungroup() %>%
  summarise(sum(p))


