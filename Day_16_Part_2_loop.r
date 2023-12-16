library(tidyverse)

# Prep
input <- readLines("./Day_16/Part_1/input.txt")
input <- gsub("\\", "B", input, fixed = TRUE)

map <- data.frame(map = input) %>%
  rowid_to_column() %>%
  separate_rows(map, sep = "", convert = TRUE) %>%
  drop_na(map) %>%
  filter(map != "") %>%
  group_by(rowid) %>%
  mutate(colid = row_number()) %>%
  ungroup() %>%
  mutate(key = paste(rowid, colid, sep = ",")) %>%
  select(rowid, colid, map)

starts <- 
  map %>% 
  filter(rowid == max(rowid) | colid == max(colid) | rowid == min(rowid) | colid == min(colid)) %>% 
  left_join(data.frame(rowid = 1, direction_1 = "v")) %>% 
  left_join(data.frame(colid = 1, direction_2 = ">"))%>% 
  left_join(data.frame(colid = max(map$colid), direction_3 = "<")) %>% 
  left_join(data.frame(rowid = max(map$rowid), direction_4 = "^")) %>%
  pivot_longer(
    cols = starts_with("direction"),
    names_to = "info",
    values_to = "direction"
  ) %>% 
  drop_na(direction) %>% 
  select(-map)

results <- c()
for(i in 1:nrow(starts)){
  print(i)
stack <- list(list(rowid = starts$rowid[i], colid = starts$colid[i], direction = starts$direction[i]))
visited <- character() # Initialize an empty set

while (length(stack) > 0) {
  current <- stack[[1]]
  stack <- stack[-1]
  
  rowid <- current$rowid
  colid <- current$colid
  direction <- current$direction
  
  key <- paste(rowid, colid, direction, sep = ",")
  if (rowid < 1 || rowid > max(map$rowid) || colid < 1 || colid > max(map$colid) || key %in% visited) {
    next
  }
  
  visited <- union(visited, key)
  
  current_item <- map[map$rowid == rowid & map$colid == colid, "map"][[1]]
  
  
  if (current_item == ".") {
    new_rowid <- rowid + (direction == "v") - (direction == "^")
    new_colid <- colid + (direction == ">") - (direction == "<")
    stack <- append(stack, list(list(rowid = new_rowid, colid = new_colid, direction = direction)))
  } 
  
  if (current_item == "B") {
    new_direction <- switch(direction, ">" = "v", "<" = "^", "v" = ">", "^" = "<")
    new_rowid <- rowid - (direction == "<") + (direction == ">")
    new_colid <- colid - (direction == "^") + (direction == "v")
    stack <- append(stack, list(list(rowid = new_rowid, colid = new_colid, direction = new_direction)))
  }
  
  if (current_item == "/") {
    new_direction <- switch(direction, ">" = "^", "<" = "v", "v" = "<", "^" = ">")
    new_rowid <- rowid - (direction == ">") + (direction == "<")
    new_colid <- colid - (direction == "v") + (direction == "^")
    stack <- append(stack, list(list(rowid = new_rowid, colid = new_colid, direction = new_direction)))
  }
  if (current_item == "|") {
    if(direction %in% c("<", ">")){
      stack <- append(stack, list(list(rowid = rowid+1, colid = colid, direction = "v")))
      stack <- append(stack, list(list(rowid = rowid-1, colid = colid, direction = "^")))
    }else{
      new_rowid <- rowid + (direction == "v") - (direction == "^")
      stack <- append(stack, list(list(rowid = new_rowid, colid = colid, direction = direction)))
    }
  }
  if (current_item == "-") {
    if(direction %in% c("^", "v")){
      stack <- append(stack, list(list(rowid = rowid, colid = colid+1, direction = ">")))
      stack <- append(stack, list(list(rowid = rowid, colid = colid-1, direction = "<")))
    }else{
      new_colid <- colid + (direction == ">") - (direction == "<")
      stack <- append(stack, list(list(rowid = rowid, colid = new_colid, direction = direction)))
    }
  }
}

result <- 
  data.frame(visited = visited) %>%
  separate(visited, into = c("rowid", "colid", "direction"), sep=",") %>% 
  distinct(rowid,colid) %>% 
  nrow()

results <- c(results, result)
}

max(results)