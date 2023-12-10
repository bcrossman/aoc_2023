library(tidyverse)

# Prep

input <- readLines("./Day_10/Part_1/input.txt")

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


#Allowed
rules <- list()
rules[["|1"]] <- data.frame(map = "|", direction = "up", allowed = c("7", "F", "|", "S"))
rules[["|2"]] <- data.frame(map = "|", direction = "down", allowed = c("L", "J", "|", "S"))
rules[["-1"]] <- data.frame(map = "-", direction = "left", allowed = c("L", "F", "-", "S"))
rules[["-2"]] <- data.frame(map = "-", direction = "right", allowed = c("7", "J", "-", "S"))
rules[["L1"]] <- data.frame(map = "L", direction = "up", allowed = c("7", "F", "|", "S"))
rules[["L2"]] <- data.frame(map = "L", direction = "right", allowed = c("7", "J", "-", "S"))
rules[["J1"]] <- data.frame(map = "J", direction = "up", allowed = c("7", "F", "|", "S"))
rules[["J2"]] <- data.frame(map = "J", direction = "left", allowed = c("F", "L", "-", "S"))
rules[["71"]] <- data.frame(map = "7", direction = "down", allowed = c("J", "F", "|", "S"))
rules[["72"]] <- data.frame(map = "7", direction = "left", allowed = c("L", "F", "-", "S"))
rules[["F1"]] <- data.frame(map = "F", direction = "down", allowed = c("L", "J", "|", "S"))
rules[["F2"]] <- data.frame(map = "F", direction = "right", allowed = c("7", "J", "-", "S"))
rules[["S1"]] <- data.frame(map = "S", direction = "up", allowed = c("7", "F", "|", "S"))
rules[["S2"]] <- data.frame(map = "S", direction = "down", allowed = c("L", "J", "|", "S"))
rules[["S3"]] <- data.frame(map = "S", direction = "left", allowed = c("L", "F", "-", "S"))
rules[["S4"]] <- data.frame(map = "S", direction = "right", allowed = c("7", "J", "-", "S"))

rules_df <- bind_rows(rules)

##  Join edges
edge_points <- 
  data %>% 
  left_join(data %>% mutate(rowid = rowid-1),
            by = c("rowid","colid"),
            suffix = c("", "_down")) %>% 
  left_join(data %>% mutate(rowid = rowid+1),
            by = c("rowid","colid"),
            suffix = c("", "_up")) %>% 
  left_join(data %>% mutate(colid = colid+1),
            by = c("rowid","colid"),
            suffix = c("", "_left")) %>% 
  left_join(data %>% mutate(colid = colid-1),
            by = c("rowid","colid"),
            suffix = c("", "_right")) %>% 
  rename(key_start= key,
         map_start = map) %>% 
  pivot_longer(cols = -c(rowid,colid),
               names_to = c(".value","direction"),
               names_sep = "_") %>% 
  filter(direction != "start") %>% 
  rename(end = key,
         map_end = map) %>%
  mutate(start = paste(rowid, colid, sep=",")) %>% 
  left_join(data %>% select(key, map) , by = c("start"="key")) %>% 
  select(start, end, map, map_end, direction) %>% 
  drop_na(map_end) %>% 
  left_join(rules_df) %>% 
  filter(map_end == allowed) 

edge_list <- 
  edge_points %>% 
  select(start, end)

beg <- edge_points %>% filter(map == "S") %>% pull(start) %>% .[[1]]
ends <- edge_points %>% filter(map_end == "S") %>% pull(start) 

library(igraph)
g <- graph_from_data_frame(d = edge_list %>% filter(end != beg) %>% filter(start != beg), directed = FALSE)

shortest_path <- get.shortest.paths(g, from = ends[2], to = ends[1])

path <- shortest_path$vpath[[1]]
vertex_names <- V(g)$name
path_names <- vertex_names[path]

result <- data.frame(start = c(beg, path_names, beg)) %>% left_join(edge_points %>% group_by(start, map) %>% slice(1)) 

(nrow(result)-1)/2

## Part 2
library(sf)
points_sf <- st_as_sf(data %>% select(rowid, colid), coords = c("colid", "rowid"))
boundary_df <- result %>% select(start) %>% separate(start, into = c("rowid", "colid"), sep = ",", convert = T)
boundary_loop <- st_polygon(list(as.matrix(boundary_df[, c("colid", "rowid")])))
boundary_sf <- st_sfc(boundary_loop)
points_within <- st_within(points_sf, boundary_sf, sparse = FALSE)
sum(points_within)

