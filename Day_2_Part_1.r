library(tidyverse)

## Part 1

cubes <- as_tibble((readLines("./Day_2/Part_1/input.txt")))


limits <- tibble(
  limits = c(12, 13, 14),
  color = c("red", "green", "blue")
)

cubes %>% 
  separate(value, into = c("game", "colors"), sep = ":") %>% 
  separate_rows(colors, sep = ";") %>% 
  group_by(game) %>% 
  mutate(shows = row_number()) %>% 
  separate_rows(colors, sep = ",") %>% 
  mutate(colors = trimws(colors)) %>% 
  separate(colors, into = c("count", "color"), sep = " ", convert = T) %>% 
  separate(game, into = c("game", "game_num"), sep = " ", convert = T) %>% 
  left_join(limits) %>% 
  mutate(good = count<=limits) %>% 
  group_by(game_num) %>% 
  summarise(valid = all(good)) %>% 
  filter(valid) %>% 
  pull(game_num) %>% 
  sum()

## Part 2


cubes %>% 
  separate(value, into = c("game", "colors"), sep = ":") %>% 
  separate_rows(colors, sep = ";") %>% 
  group_by(game) %>% 
  mutate(shows = row_number()) %>% 
  separate_rows(colors, sep = ",") %>% 
  mutate(colors = trimws(colors)) %>% 
  separate(colors, into = c("count", "color"), sep = " ", convert = T) %>% 
  separate(game, into = c("game", "game_num"), sep = " ", convert = T) %>% 
  group_by(game_num, color) %>% 
  summarise(valid_max = max(count)) %>% 
  group_by(game_num) %>% 
  summarise(power = prod(valid_max)) %>% 
  pull(power) %>% 
  sum()
  
  