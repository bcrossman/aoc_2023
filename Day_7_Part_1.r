library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_7/Part_1/input.txt")


## Part 1
processed_file_start <- 
  unglue_data(file, 
              c("{hand} {bet}"),
              convert = T) %>% 
  rowid_to_column() %>% 
  select(rowid, bet, hand) 


processed_file_start %>%
  separate_rows(hand,sep =  "") %>% 
  filter(hand != "") %>% 
  group_by(rowid, bet) %>% 
  mutate(num_groups = n_distinct(hand)) %>% 
  group_by(rowid, bet, hand) %>% 
  mutate(n_in_group = n()) %>% 
  ungroup() %>% 
  mutate(value_card = case_when(
    hand == "A" ~ 14,
    hand == "K" ~ 13,
    hand == "Q" ~ 12,
    hand == "J" ~ 11,
    hand == "T" ~ 10,
    TRUE ~ as.numeric(hand)
  )) %>% 
  group_by(rowid) %>% 
  mutate(length_biggest_group = max(n_in_group)) %>% 
  mutate(order = row_number()) %>% 
  mutate(order = factor(order, levels = c("1", "2", "3", "4", "5"))) %>% 
  complete(order = order, fill = list(value_card = NA)) %>%
  select(-hand, -n_in_group) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(rowid, bet,length_biggest_group, num_groups), names_from = "order", values_from = "value_card") %>% 
  arrange(num_groups, desc(length_biggest_group), desc(`1`), desc(`2`), desc(`3`), desc(`4`), desc(`5`)) %>% 
  drop_na(bet) %>% 
  mutate(rank = (1+n()-row_number())) %>%
  left_join(processed_file_start) %>%
  mutate(won = bet*rank) %>% 
  pull(won) %>% 
  sum()

## Part 2

what_joker_should_be <-   
  processed_file_start %>%
  separate_rows(hand,sep =  "") %>% 
  filter(hand != "") %>% 
  group_by(rowid, bet, hand) %>% 
  summarise(n_in_group = n()) %>%
  mutate(num_groups = n()) %>% 
  group_by(rowid) %>% 
  mutate(value_card = case_when(
    hand == "A" ~ 14,
    hand == "K" ~ 13,
    hand == "Q" ~ 12,
    hand == "J" ~ NA,
    hand == "T" ~ 10,
    TRUE ~ as.numeric(hand)
  )) %>% 
  mutate(length_biggest_group = max(n_in_group)) %>% 
  arrange(rowid, desc(n_in_group), desc(value_card)) %>% 
  mutate(importance = row_number()) %>% #View()
  mutate(importance = factor(importance, levels = c("1", "2", "3", "4", "5"))) %>% 
  complete(importance = importance, fill = list(value_card = NA)) %>%
  select(-hand, -n_in_group) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(rowid, bet,length_biggest_group, num_groups), names_from = "importance", values_from = "value_card") %>% 
  arrange(num_groups, desc(length_biggest_group), desc(`1`), desc(`2`), desc(`3`), desc(`4`), desc(`5`)) %>% 
  drop_na(bet) %>% 
  mutate(rank = (1+n()-row_number())) %>%
  left_join(processed_file_start) %>%
  mutate(best_card = coalesce(`1`, `2`, `3`, `4`, `5`)) %>% 
  mutate(best_card_card = case_when(
    best_card == 14 ~ "A",
    best_card == 13 ~ "K",
    best_card == 12 ~ "Q",
    best_card == 10 ~ "T",
    is.na(best_card) ~ as.character(0),
    TRUE ~ as.character(best_card)
  )) %>% 
  select(hand, best_card_card) %>% 
  distinct_all()


joined_start <- 
  processed_file_start %>% 
  left_join(what_joker_should_be)

for(i in 1:nrow(joined_start)){
  joined_start$hand[i] <- gsub("J", replacement = joined_start$best_card_card[i], x = joined_start$hand[i])
}

joined_start %>%
  select(-best_card_card) %>% 
  separate_rows(hand,sep =  "") %>% 
  filter(hand != "") %>% 
  group_by(rowid, bet) %>% 
  mutate(num_groups = n_distinct(hand)) %>% 
  group_by(rowid, bet, hand) %>% 
  mutate(n_in_group = n()) %>% 
  group_by(rowid, bet) %>% 
  summarise(num_groups = first(num_groups),
            length_biggest_group = max(n_in_group)) %>%
  left_join(processed_file_start) %>% 
  separate_rows(hand,sep =  "") %>% 
  filter(hand != "") %>% 
  ungroup() %>% 
  mutate(value_card = case_when(
    hand == "A" ~ 14,
    hand == "K" ~ 13,
    hand == "Q" ~ 12,
    hand == "J" ~ 0,  
    hand == "T" ~ 10,
    TRUE ~ as.numeric(hand)
  )) %>% 
  group_by(rowid) %>% 
  mutate(order = row_number()) %>% 
  mutate(order = factor(order, levels = c("1", "2", "3", "4", "5"))) %>% 
  complete(order = order, fill = list(value_card = NA)) %>%
  select(-hand) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(rowid, bet,length_biggest_group, num_groups), names_from = "order", values_from = "value_card") %>% 
  arrange(num_groups, desc(length_biggest_group), desc(`1`), desc(`2`), desc(`3`), desc(`4`), desc(`5`)) %>% 
  drop_na(bet) %>% 
  mutate(rank = (1+n()-row_number())) %>%
  left_join(processed_file_start) %>%
  mutate(won = bet*rank) %>% 
  pull(won) %>% 
  sum()
