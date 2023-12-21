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


parts <- data.frame(x_start = 1,
                    x_end = 4000,
                    m_start = 1,
                    m_end = 4000,
                    a_start = 1,
                    a_end = 4000,
                    s_start = 1,
                    s_end = 4000) %>% 
  mutate(position = "in")


parse_first_if_else <- function(cond_str) {
  pattern <- "^if_else\\((.)([<>]=?)([0-9]+),'([^']+)',"
  
  matches <- regexpr(pattern, cond_str)
  components <- regmatches(cond_str, matches)
  
  variable <- sub(pattern, "\\1", components)
  operator <- sub(pattern, "\\2", components)
  number <- as.numeric(sub(pattern, "\\3", components))
  true_result <- sub(pattern, "\\4", components)
  
  false_part_start <- matches + attr(regexpr(pattern, cond_str), "match.length")
  false_result <- substr(cond_str, false_part_start, nchar(cond_str))
  
 ## dumb cleanup
  false_result <- gsub("\\)\\)$", ")", false_result)
  if (!grepl("\\(", false_result)) {
    false_result <- gsub("\\)$", "", false_result)
  }
  false_result <- gsub("^'|'$", "", false_result)
  true_result <- gsub("^'|'$", "", true_result)
  
  list(variable = variable,
       operator = operator,
       number = number,
       true_result = true_result,
       false_result = false_result)
}

split_dataframe <- function(df, parsed_conditions) {
  
  var <- parsed_conditions$variable
  operator <- parsed_conditions$operator
  number <- parsed_conditions$number
  true_result <- parsed_conditions$true_result
  false_result <- parsed_conditions$false_result
  
  start_col <- paste0(var, "_start")
  end_col <- paste0(var, "_end")

  new_row <- original_row <- df[1, ]
  
  if (operator == "<") {
    # Update values for the case when operator is "<"
    new_row[[start_col]] <- number
    original_row[[end_col]] <- number - 1
    new_row[["position"]] <- false_result
    original_row[["position"]] <- true_result
  } else if (operator == ">") {
    original_row[[end_col]] <- number
    new_row[[start_col]] <- number + 1
    original_row[["position"]] <- false_result
    new_row[["position"]] <- true_result
  }

  rbind(original_row, new_row)
}

accepted <- list()
counter <- 1

while(nrow(parts)>0){
  
  df <- 
    parts %>% 
    left_join(workflow, by = c("position" = "workflow")) %>% 
    mutate(stuff = if_else(is.na(stuff), position, stuff))
  
  parts_pieces <- list()
  for(i in 1:nrow(df)){
    parts_row <- df[i,]
    parsed_conditions <- parse_first_if_else(parts_row$stuff[[1]])
    parts_rows <- split_dataframe(parts_row, parsed_conditions)
    parts_pieces[[i]] <- parts_rows
  }
  parts <- 
    bind_rows(parts_pieces) %>% 
    select(-stuff)
  
  accepted[[counter]] <- parts %>% filter(position == "A")
  parts <- parts %>% filter(position != "A") %>% filter(position != "R")
  counter= counter+1
}

accepted %>% 
  bind_rows() %>% 
  mutate(total = (x_end-x_start+1)*(m_end-m_start+1)*(a_end-a_start+1)*(s_end-s_start+1)) %>% 
  pull(total) %>% 
  sum() %>% 
  scales::comma()
# 
# 167409079868000 %>% scales::comma()
