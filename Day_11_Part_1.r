library(tidyverse)

# Part 1

input <- readLines("./Day_11/Part_1/input.txt")

data <- 
  data.frame(map = input) %>% 
  rowid_to_column() %>%
  separate_rows(map, sep="", convert = T) %>% 
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(rowid, colid, sep=",")) 

empty_row_adjust <- 
  data %>% 
  group_by(rowid) %>% 
  summarise(empty = all(map == ".")) %>% 
  mutate(increase = cumsum(lag(empty,1, FALSE)))

adjusted_data <-   
  data %>% 
  left_join(empty_row_adjust %>% select(rowid, increase)) %>% 
  mutate(rowid = rowid+increase) %>% 
  select(-increase)

empty_col_adjust <- 
  data %>% 
  group_by(colid) %>% 
  summarise(empty = all(map == ".")) %>% 
  mutate(increase = cumsum(lag(empty,1, FALSE)))

adjusted_data <-   
  adjusted_data %>% 
  left_join(empty_col_adjust %>% select(colid, increase)) %>% 
  mutate(colid = colid+increase) %>% 
  select(-increase)

galaxies <- 
  adjusted_data %>% 
  filter(map == "#") %>% 
  select(map, rowid, colid, key)

complete <- galaxies %>% full_join(galaxies, by = "map") %>% filter(key.x != key.y) %>% mutate(key = paste(key.x, key.y, sep = ":"))

comb_keys <- as.data.frame(t(combn(galaxies$key, 2))) %>% mutate(key_unique = paste(V1, V2, sep = ":"))
  
complete %>% 
  filter(key %in% comb_keys$key_unique) %>% 
  mutate(distance = abs(rowid.y-rowid.x)+abs(colid.y-colid.x)) %>% 
  pull(distance) %>% 
  sum()

## Part 2
## Just change the cumsum multiplier


empty_row_adjust <- 
  data %>% 
  group_by(rowid) %>% 
  summarise(empty = all(map == ".")) %>%  
  mutate(increase = (1000000-1)*cumsum(lag(empty,1, FALSE)))  ##Only change

adjusted_data <-   
  data %>% 
  left_join(empty_row_adjust %>% select(rowid, increase)) %>% 
  mutate(rowid = rowid+increase) %>% 
  select(-increase)

empty_col_adjust <- 
  data %>% 
  group_by(colid) %>% 
  summarise(empty = all(map == ".")) %>% 
  mutate(increase = (1000000-1)*cumsum(lag(empty,1, FALSE)))  ##Only change

adjusted_data <-   
  adjusted_data %>% 
  left_join(empty_col_adjust %>% select(colid, increase)) %>% 
  mutate(colid = colid+increase) %>% 
  select(-increase)

galaxies <- 
  adjusted_data %>% 
  filter(map == "#") %>% 
  select(map, rowid, colid, key)

complete <- galaxies %>% full_join(galaxies, by = "map") %>% filter(key.x != key.y) %>% mutate(key = paste(key.x, key.y, sep = ":"))

comb_keys <- as.data.frame(t(combn(galaxies$key, 2))) %>% mutate(key_unique = paste(V1, V2, sep = ":"))

complete %>% 
  filter(key %in% comb_keys$key_unique) %>% 
  mutate(distance = abs(rowid.y-rowid.x)+abs(colid.y-colid.x)) %>% 
  pull(distance) %>% 
  sum()
