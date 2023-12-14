library(tidyverse)

# Prep

input <- readLines("./Day_14/Part_1/input.txt")


## Part 1
data.frame(map = input) %>% 
  mutate(weight = n()-row_number()+1) %>%
  mutate(rowid = row_number()) %>% 
  separate_rows(map, sep="", convert = T) %>% 
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  arrange(colid, rowid) %>% 
  group_by(colid) %>% 
  mutate(roadblock = cumsum(map == "#")) %>% 
  group_by(colid, roadblock) %>% 
  mutate(start_value = max(weight)) %>% 
  group_by(colid, roadblock, map) %>%
  mutate(weight = start_value - row_number()+if_else(roadblock==0,1,0)) %>% 
  filter(map == "O") %>% 
  pull(weight) %>% 
  sum()



## Part 2

start_data <- 
  data.frame(map = input) %>% 
  mutate(rowid = row_number()) %>% 
  separate_rows(map, sep="", convert = T) %>% 
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  select(rowid,colid,map)

weights <- c()
states <- list(start_data)
in_list <- FALSE
count <- 1
while(!in_list){
  print(count)
  #North
  north <- 
    start_data %>% 
    arrange(colid, rowid) %>% 
    group_by(colid) %>% 
    mutate(roadblock = cumsum(map == "#")) %>% 
    group_by(colid, roadblock) %>% 
    mutate(start_value = min(rowid)) %>% 
    group_by(colid, roadblock, map) %>%
    mutate(rowid = if_else(map=="O",start_value + row_number()+if_else(roadblock==0,-1,0), rowid)) %>% 
    filter(map != ".") %>% 
    ungroup()
  
  # north %>% 
  #   mutate(weight = max(rowid)-rowid+1) %>%
  #   filter(map == "O") %>% 
  #   pull(weight) %>% 
  #   sum()
  
  west <- 
    start_data %>% 
    select(rowid,colid) %>% 
    left_join(north %>% select(rowid, colid, map)) %>% 
    mutate(map = replace_na(map, ".")) %>%
    mutate(new_rowid = colid,
           new_colid =max(rowid) - rowid +1,
           rowid = new_rowid,
           colid = new_colid) %>% 
    arrange(colid, rowid) %>% 
    group_by(colid) %>% 
    mutate(roadblock = cumsum(map == "#")) %>% 
    group_by(colid, roadblock) %>% 
    mutate(start_value = min(rowid)) %>% 
    group_by(colid, roadblock, map) %>%
    mutate(rowid = if_else(map=="O",start_value + row_number()+if_else(roadblock==0,-1,0), rowid)) %>% 
    filter(map != ".") %>% 
    ungroup()
  
  # west %>% 
  #   mutate(weight = max(rowid)-rowid+1) %>%
  #   filter(map == "O") %>% 
  #   pull(weight) %>% 
  #   sum()
  
  south <- 
    start_data %>% 
    select(rowid,colid) %>% 
    left_join(west %>% select(rowid, colid, map)) %>% 
    mutate(map = replace_na(map, ".")) %>%
    mutate(new_rowid = colid,
           new_colid =max(rowid) - rowid +1,
           rowid = new_rowid,
           colid = new_colid) %>% 
    arrange(colid, rowid) %>% 
    group_by(colid) %>% 
    mutate(roadblock = cumsum(map == "#")) %>% 
    group_by(colid, roadblock) %>% 
    mutate(start_value = min(rowid)) %>% 
    group_by(colid, roadblock, map) %>%
    mutate(rowid = if_else(map=="O",start_value + row_number()+if_else(roadblock==0,-1,0), rowid)) %>% 
    filter(map != ".") %>% 
    ungroup()
  
  # south %>% 
  #   mutate(weight = max(rowid)-rowid+1) %>%
  #   filter(map == "O") %>% 
  #   pull(weight) %>% 
  #   sum()
  
  east <- 
    start_data %>% 
    select(rowid,colid) %>% 
    left_join(south %>% select(rowid, colid, map)) %>% 
    mutate(map = replace_na(map, ".")) %>%
    mutate(new_rowid = colid,
           new_colid =max(rowid) - rowid +1,
           rowid = new_rowid,
           colid = new_colid) %>% 
    arrange(colid, rowid) %>% 
    group_by(colid) %>% 
    mutate(roadblock = cumsum(map == "#")) %>% 
    group_by(colid, roadblock) %>% 
    mutate(start_value = min(rowid)) %>% 
    group_by(colid, roadblock, map) %>%
    mutate(rowid = if_else(map=="O",start_value + row_number()+if_else(roadblock==0,-1,0), rowid)) %>% 
    filter(map != ".") %>% 
    ungroup()
  
  # east %>% 
  #   mutate(weight = max(rowid)-rowid+1) %>%
  #   filter(map == "O") %>% 
  #   pull(weight) %>% 
  #   sum()
  
  start_data <- 
    start_data %>% 
    select(rowid,colid) %>% 
    left_join(east %>% select(rowid, colid, map)) %>% 
    mutate(map = replace_na(map, ".")) %>%
    mutate(new_rowid = colid,
           new_colid =max(rowid) - rowid +1,
           rowid = new_rowid,
           colid = new_colid) %>% 
    select(rowid,colid,map) %>% 
    arrange(rowid, colid)
  
  result <- 
    start_data %>% 
    mutate(weight = max(rowid)-rowid+1) %>%
    filter(map == "O") %>% 
    pull(weight) %>% 
    sum()
  print(result)
  weights <- c(weights, result)
  
  in_list <- FALSE
  state_count <- 1
  for(df in states) {
    if(all.equal(df, start_data) == TRUE) {
      in_list <- TRUE
      break
    }
    state_count = state_count+1
  }
  states[[as.character(count)]] <- start_data
  count=count+1
}

cycle <- count-state_count
last_cycle <- weights[(length(weights)-cycle+1):length(weights)]

end_point <- 1e9
excess <- (end_point - length(weights)) %% cycle
last_cycle[excess]


