options(tidyverse.quiet = TRUE)
library(broom)
library(dplyr)
library(ellipse)
library(ggplot2)
library(ggrepel)
library(kableExtra)
library(metafor)
library(stringr)
library(tidyr)
library(writexl)

data_location <- "https://github.com/jeremymiles/adhd_git/raw/refs/heads/main/data/diagnosis_data.csv"

d <- read.csv(data_location)

### TEMP - REMOVE
#d <- d %>%
#  dplyr::filter(ID != "Harrison, 2019{#351}") 

capitalize_first <- function(s) {
  if (is.na(s) || s == "") { #Handle NAs and empty strings
    return(s)
  }
  first_letter <- str_sub(s, 1, 1)
  rest_of_string <- str_sub(s, 2)
  return(str_c(str_to_upper(first_letter), rest_of_string))
}


categories <- c("self_report", "peer_rating", "neuropsycho_tests", "neuroimaging", 
                "clinician_interview", "combination", "biomarker", "EEG")
