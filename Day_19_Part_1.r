library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_19/Part_1/input.txt")
file <- gsub(pattern = "\\{", replacement = "[", x = file)
file <- gsub(pattern = "\\}", replacement = "]", x = file)
file <- gsub(pattern = "^\\[", replacement = "(", x = file)

input <- 
  unglue_data(file, 
              c("{workflow}[{stuff}]",
                "(x={x},m={m},a={a},s={s}]"),
              convert = TRUE) 

parts <- input %>% drop_na(x) %>% select(-workflow, -stuff) %>% mutate(position = "in")
workflow <- 
  input %>% 
  drop_na(workflow) %>% 
  select(workflow, stuff) %>% 
  mutate(
    stuff = gsub(":",",",stuff),
    stuff = gsub("([A-Z])","\'\\1\'",stuff),
    stuff = gsub("([a-z]{2,})","\'\\1\'",stuff),
    stuff = gsub("([a-z])([<>])","if_else(\\1\\2",stuff)) %>% 
  mutate(num_parentheses = nchar(gsub("[^\\(]", "", stuff))) %>% 
  rowwise() %>% 
  mutate(stuff = paste0(stuff, paste0(rep(x = ")", num_parentheses), collapse = ""))) %>% 
  ungroup() %>% 
  select(-num_parentheses)

accepted <- list()
counter <- 1
while(nrow(parts)>0){

parts <- 
  parts %>% 
  left_join(workflow, by = c("position" = "workflow")) %>% 
  rowwise() %>%
  mutate(result = eval(parse(text = stuff))) %>%
  ungroup() %>% 
  mutate(position = result) %>% 
  select(-result, -stuff)

accepted[[counter]] <- parts %>% filter(position == "A")
parts <- parts %>% filter(position != "A") %>% filter(position != "R")
counter <- counter+1
}

accepted %>% bind_rows() %>% mutate(total = x+m+a+s) %>% pull(total) %>% sum()
