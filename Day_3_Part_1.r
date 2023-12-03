library(tidyverse)

## Part 1

input <- as_tibble((readLines("./Day_3/Part_1/input.txt")))

create_grid <- function(df){
  
  df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = T) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(rowid, colid, sep=", ")) %>% 
    select(value, rowid, colid, key)
}

data <- create_grid(input) 

data %>% 
  filter(!is.na(as.numeric(value))) %>% 
  group_by(rowid) %>% 
  mutate(start = (colid-1)!=lag(colid, 1, default =-1)) %>% 
  ungroup() %>% 
  mutate(number_id = cumsum(start)) %>% 
  mutate(
    top_left = paste(rowid - 1, colid - 1, sep = ", "),
    top = paste(rowid - 1, colid, sep = ", "),
    top_right = paste(rowid - 1, colid + 1, sep = ", "),
    left = paste(rowid, colid - 1, sep = ", "),
    right = paste(rowid, colid + 1, sep = ", "),
    bottom_left = paste(rowid + 1, colid - 1, sep = ", "),
    bottom = paste(rowid + 1, colid, sep = ", "),
    bottom_right = paste(rowid + 1, colid + 1, sep = ", ")
  ) %>%
  pivot_longer(cols = c(top_left:bottom_right), values_to = "location", names_to = "type") %>% 
  left_join(data %>% filter(is.na(as.numeric(value) & value != ".")) %>%  select(special_value = value, key), by = c("location" = "key")) %>% 
  mutate(special_value = !is.na(special_value)) %>% 
  group_by(number_id) %>% 
  mutate(reading = any(special_value)) %>% 
  filter(reading) %>% 
  group_by(number_id, value, key) %>% 
  slice(1) %>% 
  group_by(number_id) %>% 
  arrange(rowid, desc(colid)) %>% 
  mutate(base_10 = as.numeric(value) * 10^(row_number()-1)) %>% 
  pull(base_10) %>% 
  sum()

## Part 2

data %>% 
  filter(!is.na(as.numeric(value))) %>% 
  group_by(rowid) %>% 
  mutate(start = (colid-1)!=lag(colid, 1, default =-1)) %>% 
  ungroup() %>% 
  mutate(number_id = cumsum(start)) %>% 
  group_by(number_id) %>% 
  arrange(rowid, desc(colid)) %>% 
  mutate(base_10 = as.numeric(value) * 10^(row_number()-1)) %>% 
  mutate(number_value = sum(base_10)) %>% 
  mutate(
    top_left = paste(rowid - 1, colid - 1, sep = ", "),
    top = paste(rowid - 1, colid, sep = ", "),
    top_right = paste(rowid - 1, colid + 1, sep = ", "),
    left = paste(rowid, colid - 1, sep = ", "),
    right = paste(rowid, colid + 1, sep = ", "),
    bottom_left = paste(rowid + 1, colid - 1, sep = ", "),
    bottom = paste(rowid + 1, colid, sep = ", "),
    bottom_right = paste(rowid + 1, colid + 1, sep = ", ")
  ) %>%
  pivot_longer(cols = c(top_left:bottom_right), values_to = "location", names_to = "type") %>% 
  left_join(data %>% filter(is.na(as.numeric(value) & value != ".")) %>%  select(special_value = value, key), by = c("location" = "key")) %>% 
  mutate(special_value = !is.na(special_value)) %>% 
  filter(special_value) %>% 
  group_by(number_id, number_value, location) %>% 
  summarise(count = n()) %>% 
  arrange(location) %>% 
  group_by(location) %>% 
  mutate(n = n()) %>% 
  filter(n>1) %>% 
  summarise(gear_ratio = prod(number_value)) %>% 
  pull(gear_ratio) %>% 
  sum()



