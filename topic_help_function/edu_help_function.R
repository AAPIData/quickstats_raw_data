
#load required packages
library(shiny)
library(tidycensus)
library(tidyverse)
library(readr)

#install census key
census_api_key("4e9d7fd959555208210856aaa5061b593c3722af", install = T, overwrite = T)

#create label for the year of the update
label <- load_variables(2016, "acs5", cache = TRUE)
label <- label %>%
  select(-concept) %>% 
  mutate(name = sub('E$', '',name), # get rid of E at the end of each name
         label = sub('Estimate!!', '',label)) %>%
  rename(variable = name)

########################
#load data for edu
table <- get_acs(table = "C15002D", geography = geography, year = year, summary_var = "C15002D_001")

table <- table %>% select(NAME,variable, estimate,moe, summary_est, summary_moe)

table <- table %>% left_join(label) %>% select(label, everything()) # join table

edu_cat4 <- c("Total!!Male!!Less than high school diploma", 
              "Total!!Female!!Less than high school diploma", 
              "Total!!Male!!High school graduate (includes equivalency)",
              "Total!!Female!!High school graduate (includes equivalency)",
              "Total!!Male!!Some college or associate's degree",
              "Total!!Female!!Some college or associate's degree",
              "Total!!Male!!Bachelor's degree or higher",
              "Total!!Female!!Bachelor's degree or higher")
