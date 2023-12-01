library(tidyverse)

calibration_readings <- as_vector((readLines("./Day_1/Part_1/input.txt")))

## Part 1

total_readings <- 
  function(readings){
    sum_readings <- 0
    for(i in readings){
      # print(i)
      for(j in strsplit(i,"")[[1]]){
        # print(j)
        if(!is.na(as.numeric(j))){
          digit_one <- j
          # print(digit_one)
          break()
        }
      }
      for(k in rev(strsplit(i,"")[[1]])){
        if(!is.na(as.numeric(k))){
          digit_two <- k
          # print(digit_two)
          break()
        }
      }
      new_sum <- as.numeric(paste0(digit_one, digit_two))
      # print(new_sum)
      sum_readings = sum_readings + new_sum
    }
    return(sum_readings)
  }

result <- total_readings(calibration_readings)
print(result)

## Part 2

calibration_readings <- gsub("one","one1one", calibration_readings)
calibration_readings <- gsub("two","two2two", calibration_readings)
calibration_readings <- gsub("three","three3three", calibration_readings)
calibration_readings <- gsub("four","four4four", calibration_readings)
calibration_readings <- gsub("five","five5five", calibration_readings)
calibration_readings <- gsub("six","six6six", calibration_readings)
calibration_readings <- gsub("seven","seven7seven", calibration_readings)
calibration_readings <- gsub("eight","eight8eight", calibration_readings)
calibration_readings <- gsub("nine","nine9nine", calibration_readings)

calibration_readings <- gsub("one","", calibration_readings)
calibration_readings <- gsub("two","", calibration_readings)
calibration_readings <- gsub("three","", calibration_readings)
calibration_readings <- gsub("four","", calibration_readings)
calibration_readings <- gsub("five","", calibration_readings)
calibration_readings <- gsub("six","", calibration_readings)
calibration_readings <- gsub("seven","", calibration_readings)
calibration_readings <- gsub("eight","", calibration_readings)
calibration_readings <- gsub("nine","", calibration_readings)


result <- total_readings(calibration_readings)
print(result)

