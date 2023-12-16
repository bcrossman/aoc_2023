library(tidyverse)

# Prep

input <- readLines("./Day_16/Part_1/input.txt", )
input <- gsub("\\", "B", input, fixed = TRUE)

map <- 
  data.frame(map = input) %>% 
  rowid_to_column() %>%
  separate_rows(map, sep = "", convert = TRUE) %>%  
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(rowid, colid, sep=",")) %>% 
  select(rowid,colid,map)

library(memoise)
navigate_map <- function(rowid, colid, direction, visited = list() ) {
  # rowid <- 69
  # colid <- 91
  # direction <- ">"
  # visited <- list()
  path_history <<- c(path_history, list(c(rowid, colid, direction)))
  
  if (rowid < 1 || rowid > max(map$rowid) || colid < 1 || colid > max(map$colid) || 
      paste(rowid, colid, direction) %in% visited) {
    return(list())
  }
  visited <- c(visited, paste(rowid, colid, direction))
  current_item <- map[(map$rowid==rowid&map$colid==colid), "map"][[1]]
  paths <- list(c(rowid, colid))
  if (current_item == ".") {
    print(paste(rowid, colid, current_item, direction))
    rowid <- rowid + (direction == "v") - (direction == "^")
    colid <- colid + (direction == ">") - (direction == "<")
    new_paths <- navigate_map(rowid, colid, direction, visited)
    paths <- c(paths, new_paths)
    return(paths)
  }
  if (current_item == "B") {
    print(paste(rowid, colid, current_item, direction))
    new_direction <- switch(direction, ">" = "v", "<" = "^", "v" = ">", "^" = "<")
    rowid <- rowid - (direction == "<") + (direction == ">")
    colid <- colid - (direction == "^") + (direction == "v")
    direction <- new_direction
    new_paths <- navigate_map(rowid, colid, direction, visited)
    paths <- c(paths, new_paths)
    return(paths)
  }
  
  if (current_item == "/") {
    print(paste(rowid, colid, current_item, direction))
    new_direction <- switch(direction, ">" = "^", "<" = "v", "v" = "<", "^" = ">")
    rowid <- rowid - (direction == ">") + (direction == "<")
    colid <- colid - (direction == "v") + (direction == "^")
    direction <- new_direction
    new_paths <- navigate_map(rowid, colid, direction, visited)
    paths <- c(paths, new_paths)
    return(paths)
  }
  if (current_item == "|") {
    if(direction %in% c("<", ">")){
      print(paste(rowid, colid, current_item, direction))
      paths <- c(paths, navigate_map(rowid - 1, colid, "^", visited),
                 navigate_map(rowid + 1, colid, "v", visited))
      return(paths)
    }else{
      print(paste(rowid, colid, current_item, direction))
      rowid <- rowid + (direction == "v") - (direction == "^")
      colid <- colid + (direction == ">") - (direction == "<")
      new_paths <- navigate_map(rowid, colid, direction, visited)
      paths <- c(paths, new_paths)
      return(paths)
    }
  }
  if (current_item == "-") {
    if(direction %in% c("^", "v")){
      print(paste(rowid, colid, current_item, direction))
      paths <- c(paths, navigate_map(rowid, colid-1, "<", visited),
                 navigate_map(rowid, colid+1, ">", visited))
      return(paths)
    }else{
      print(paste(rowid, colid, current_item, direction))
      rowid <- rowid + (direction == "v") - (direction == "^")
      colid <- colid + (direction == ">") - (direction == "<")
      new_paths <- navigate_map(rowid, colid, direction, visited)
      paths <- c(paths, new_paths)
    }
  }
}

navigate_map <- memoise::memoise(navigate_map)
result <- navigate_map(1, 1, ">", visited = list())
length(unique(result))

