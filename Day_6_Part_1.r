library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_6/Part_1/input.txt")


## Part 1

unglue_data(file, 
            c("Time: {time}",
              "Distance: {distance}")
) %>% 
  fill(time, .direction = "down") %>% 
  slice(-1) %>% 
  separate_rows(c(time,distance), sep = "\\s+") %>%
  slice(-1) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  mutate(distance = distance +1) %>% 
  mutate(max_hold_win = floor(time/2 + sqrt(time^2-(4*distance))/2),
         min_hold_win = ceiling(time/2 - sqrt(time^2-(4*distance))/2)) %>% 
  mutate(combinations =(max_hold_win - min_hold_win)+1) %>% 
  pull(combinations) %>% 
  prod()

## Part 3


unglue_data(file, 
            c("Time: {time}",
              "Distance: {distance}")
) %>% 
  fill(time, .direction = "down") %>% 
  slice(-1) %>% 
  mutate(time = gsub(" ","",time)) %>% 
  mutate(distance = gsub(" ","",distance)) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  mutate(distance = distance +1) %>% 
  mutate(max_hold_win = floor(time/2 + sqrt(time^2-(4*distance))/2),
         min_hold_win = ceiling(time/2 - sqrt(time^2-(4*distance))/2)) %>% 
  mutate(combinations =(max_hold_win - min_hold_win)+1) %>% 
  pull(combinations)


