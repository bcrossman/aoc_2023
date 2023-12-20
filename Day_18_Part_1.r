library(tidyverse)

# Prep

file <- readLines("./Day_18/Part_1/input.txt")

input <- 
  data.frame(value = file) %>% 
  mutate(value = trimws(value)) %>% 
  separate(value, into = c("direction", "distance", "color"), sep = " ", convert = T)

data <- 
  input %>%
  mutate(
    rowid_start = lag(cumsum(ifelse(direction == 'U', distance, ifelse(direction == 'D', -distance, 0))), default = 0),
    colid_start = lag(cumsum(ifelse(direction == 'R', distance, ifelse(direction == 'L', -distance, 0))), default = 0),
    rowid_end = cumsum(ifelse(direction == 'U', distance, ifelse(direction == 'D', -distance, 0))),
    colid_end = cumsum(ifelse(direction == 'R', distance, ifelse(direction == 'L', -distance, 0)))
  )


all_points <- expand_grid(
  rowid = seq(min(data$rowid_end, na.rm = TRUE), max(data$rowid_end, na.rm = TRUE)),
  colid = seq(min(data$colid_end, na.rm = TRUE), max(data$colid_end, na.rm = TRUE))
)

library(sf)
points_sf <- st_as_sf(all_points, coords = c("colid", "rowid"))
path <- 
  bind_rows(
    data %>% select(rowid = rowid_end, colid = colid_end) %>% slice(n()),
    data %>% select(rowid = rowid_end, colid = colid_end))
boundary_df <- path
boundary_loop <- st_polygon(list(as.matrix(boundary_df[, c("colid", "rowid")])))
boundary_sf <- st_sfc(boundary_loop)
points_within <- st_intersects(points_sf, boundary_sf, sparse = FALSE)
sum(points_within)


ggplot() +
  geom_point(data = all_points, aes(x = colid, y = rowid), color = "gray", size = 1) +
  geom_path(data = path, aes(x = colid, y = rowid), color = "blue", size = 1) +
  coord_fixed() +
  labs(title = "Path", x = "Column ID", y = "Row ID")

## Part 2

hex_to_instruction <- function(hex_code) {
  
  
  # Returning a list of direction and distance
  list(direction = direction, distance = distance)
}


input <- 
  data.frame(value = file) %>% 
  mutate(value = trimws(value)) %>% 
  separate(value, into = c("direction", "distance", "color"), sep = " ", convert = T) %>% 
  mutate(  
    distance_hex = substr(color, 3, 7),
    direction_hex = substr(color, 8, 8),
    distance = as.numeric(strtoi(distance_hex, base=16)),
    direction = case_when(
      direction_hex == "0" ~ "R",
      direction_hex == "1" ~ "D",
      direction_hex == "2" ~ "L",
      direction_hex == "3" ~ "U",
      TRUE ~ NA_character_
    )
  )

data <- 
  input %>%
  mutate(
    rowid_start = lag(cumsum(ifelse(direction == 'U', distance, ifelse(direction == 'D', -distance, 0))), default = 0),
    colid_start = lag(cumsum(ifelse(direction == 'R', distance, ifelse(direction == 'L', -distance, 0))), default = 0),
    rowid_end = cumsum(ifelse(direction == 'U', distance, ifelse(direction == 'D', -distance, 0))),
    colid_end = cumsum(ifelse(direction == 'R', distance, ifelse(direction == 'L', -distance, 0)))) %>% 
  rowwise() %>%
  mutate(
    rowid_seq = ifelse(direction %in% c('U', 'D'), list(seq(rowid_start, rowid_end)), list(rep(rowid_start,distance+1))),
    colid_seq = ifelse(direction %in% c('R', 'L'),  list(seq(colid_start, colid_end)), list(rep(colid_start,distance+1)))
  ) %>%
  ungroup()

data_long <-
  data %>%
  select(-direction, -distance, -rowid_start, -colid_start, -rowid_end, -colid_end) %>%
  unnest(rowid_seq, colid_seq)


# all_points <- expand_grid(
#   rowid = seq(min(data$rowid_end, na.rm = TRUE), max(data$rowid_end, na.rm = TRUE)),
#   colid = seq(min(data$colid_end, na.rm = TRUE), max(data$colid_end, na.rm = TRUE))
# ) NOPE

library(sf)
path <- 
  bind_rows(
    data %>% select(rowid = rowid_end, colid = colid_end) %>% slice(n()),
    data %>% select(rowid = rowid_end, colid = colid_end))
boundary_df <- path
boundary_loop <- st_polygon(list(as.matrix(boundary_df[, c("colid", "rowid")])))
boundary_sf <- st_sfc(boundary_loop)
st_area(boundary_loop)+nrow(data_long %>% distinct(rowid_seq, colid_seq))/2+1


ggplot() +
  geom_point(data = all_points, aes(x = colid, y = rowid), color = "gray", size = 1) +
  geom_path(data = path, aes(x = colid, y = rowid), color = "blue", size = 1) +
  coord_fixed() +
  labs(title = "Path", x = "Column ID", y = "Row ID")
