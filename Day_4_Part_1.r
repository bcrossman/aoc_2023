library(tidyverse)

input <- as_tibble((readLines("./Day_4/Part_1/input.txt")))

str_to_list <- function(str) {
  str_split(str, " ")[[1]] %>%
    map(~ .x[.x != ""]) %>%
    unlist() %>%
    map_int(~ as.integer(.x))
}

## Part 1
games <- 
input %>%
  rowwise() %>%
  mutate(
    parts = list(str_split(value, ":|\\|")[[1]]),
    Card = str_trim(parts[[1]]),
    winning = list(str_to_list(str_trim(parts[[2]]))),
    played = list(str_to_list(str_trim(parts[[3]])))
  ) %>%
  select(-value, -parts) %>% 
  ungroup() %>% 
  mutate(matches =  map2_int(winning, played, ~ length(intersect(.x, .y)))) %>% 
  mutate(won = if_else(matches>0, 2^(matches-1), 0))

games %>% # View()
  pull(won) %>% 
  sum()

## Part 2

games$card_count = 1

for(i in 1:nrow(games)){
  #i <- 5
  if(games$matches[i]==0){next}
  games$card_count[(i+1):(i+games$matches[i])] <- games$card_count[(i+1):(i+games$matches[i])]+games$card_count[i]
}

games %>% 
  pull(card_count) %>% 
  sum()
