library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_5/Part_1/input.txt")

input <- 
  unglue_data(file, 
              c("seeds: {seeds}",
                "{source}-to-{destination} map:",
                "{destination_start} {source_start} {range}"),
              convert = TRUE) 

seed_vec <- as.numeric(str_split(input$seeds[[1]]," ")[[1]])

mapping <- 
  input %>% 
  fill(source, destination, .direction = "down") %>% 
  drop_na(source_start) %>% 
  select(-seeds) %>% 
  mutate(across(where(is.numeric), as.numeric)) %>% 
  mutate(movement = destination_start - source_start) %>% 
  mutate(source_end = source_start + (range-1))

result <- c()
for(i in seed_vec){
  # i <- seed_vec[[1]]
  current_number <- i
  type = "seed"
  
  while(type != "location"){
    
    active_map <- 
      mapping %>% 
      filter(source == type) 
    
    type <- active_map$destination[[1]]
    
    final_map <- 
      active_map %>% 
      filter(source_start<=current_number,
             source_end>=current_number) 
    if(nrow(final_map)!=0){
      current_number= current_number+ final_map$movement[[1]]
    }
  }
  result = min(result, current_number)
}

min(result)

## Part 2

df <- matrix(seed_vec, ncol = 2, byrow = TRUE)
df <- as.data.frame(df)
colnames(df) <- c("destination_start", "range")

start_df <- 
  df %>% 
  mutate(destination_end = destination_start + (range-1),
         destination = "seed") %>% 
  arrange(destination_start) %>% 
  select(-range)

type = start_df$destination[[1]]

while(type != "location"){
  
  active_map <-
    mapping %>%
    filter(source == type) %>% 
    arrange(source_start)
  
  missing <- list()
  if(min(active_map$source_start !=0)){
    missing[["a"]] <- data.frame("source_start" = 0, "source_end" = min(active_map$source_start-1), movement = 0)}
  
  for(i in 1:(length(active_map$source_start)-1)){
    if(active_map$source_end[i]+1 != active_map$source_start[i+1]){
      missing[[as.character(l)]] <- data.frame("source_start" = active_map$source_end[i]+1, "source_end" = active_map$source_start[i+1], movement = 0)
    }
  }
  missing_df <- bind_rows(missing)
  
  final_map <- 
    active_map %>% 
    bind_rows(missing_df) %>% 
    fill(destination, .direction = "updown") %>% 
    arrange(source_start)
  
  type <- final_map$destination[[1]]
  
  new_start_df <-
    start_df %>% 
    bind_rows(final_map %>% select(destination_end = source_end, movement)) %>% 
    arrange(destination_end) %>% 
    fill(movement, .direction = "up") %>% 
      mutate(movement = if_else(is.na(movement),0,movement)) %>% 
    filter(!(is.na(destination_start)&is.na(destination)&(row_number() == n()))) %>% 
    fill(destination_start, .direction = "up") %>% 
    filter(destination_start<destination_end) %>% 
    mutate(destination_start = if_else(row_number()!=1&is.na(lag(destination,1)), lag(destination_end,1)+1, destination_start)) %>% 
    filter(destination_start>=start_df$destination_start[1]) %>% 
      fill(destination, .direction = "up") %>% 
    drop_na(destination) %>%
    drop_na(destination_start) %>%
    mutate(destination_start = destination_start+movement,
           destination_end = destination_end+movement,
           destination = type) %>% 
    select(-movement) %>% 
    arrange(destination_end)
  new_start_df
  start_df <- new_start_df
}
start_df
min(start_df$destination_start)
