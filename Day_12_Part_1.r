library(tidyverse)
library(unglue)

# Part I

file <- readLines("./Day_12/Part_1/input.txt")

input <- 
  data.frame(value = file) %>% 
  mutate(value = trimws(value)) %>% 
  separate(value, into = c("reading", "result"), sep = " ")

generate_combinations <- function(s, partial = "") {
  if (nchar(s) == 0) {
    return(partial)
  } else {
    first_char <- substr(s, 1, 1)
    rest <- substr(s, 2, nchar(s))
    if (first_char == "?") {
      return(c(generate_combinations(rest, paste0(partial, ".")),
               generate_combinations(rest, paste0(partial, "#"))))
    } else {
      return(generate_combinations(rest, paste0(partial, first_char)))
    }
  }
}

pattern_count <- c()
for(i in 1:nrow(input)){
  # i <- 3
combinations <- data.frame(string_comb = generate_combinations(input$reading[[i]]))
pattern_needed <- input$result[[i]]

row_result <- 
combinations %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  separate_rows(string_comb, sep = "") %>% 
  filter(string_comb != "") %>% 
  group_by(rowname) %>% 
  mutate(group_num = cumsum(string_comb != lag(string_comb, 1,"."))) %>% #View()
  filter(string_comb == "#") %>%  
  group_by(rowname, group_num) %>% 
  summarise(n = n()) %>% 
  group_by(rowname) %>% 
  summarise(pattern_result = paste(n, collapse = ",")) %>% 
  filter(pattern_result==pattern_needed) %>% 
  nrow()

pattern_count <- c(pattern_count, row_result)
}
sum(pattern_count)


