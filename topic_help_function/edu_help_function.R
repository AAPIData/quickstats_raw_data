
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
#categories for edu
edu_cat4 <- c("Total!!Male!!Less than high school diploma", 
              "Total!!Female!!Less than high school diploma", 
              "Total!!Male!!High school graduate (includes equivalency)",
              "Total!!Female!!High school graduate (includes equivalency)",
              "Total!!Male!!Some college or associate's degree",
              "Total!!Female!!Some college or associate's degree",
              "Total!!Male!!Bachelor's degree or higher",
              "Total!!Female!!Bachelor's degree or higher")

#load data for edu
table1 <- get_acs(table = "C15002B", geography = "county", year = 2016, summary_var = "C15002B_001")
table2 <- get_acs(table = "C15002C", geography = "county", year = 2016, summary_var = "C15002C_001")
table3 <- get_acs(table = "C15002D", geography = "county", year = 2016, summary_var = "C15002D_001")
table4 <- get_acs(table = "C15002E", geography = "county", year = 2016, summary_var = "C15002E_001")
table5 <- get_acs(table = "C15002H", geography = "county", year = 2016, summary_var = "C15002H_001")
table6 <- get_acs(table = "C15002I", geography = "county", year = 2016, summary_var = "C15002I_001")
#generating group info
table1 <- table1 %>% mutate(group="Black")
table2 <- table2 %>% mutate(group="AIAN")
table3 <- table3 %>% mutate(group="AA")
table4 <- table4 %>% mutate(group="NHPI")
table5 <- table5 %>% mutate(group="NH-wite")
table6 <- table6 %>% mutate(group="Latino")
#merge into one
table <- table1 %>% left_join(table2) %>% 
  left_join(table3) %>% left_join(table4) %>% 
  left_join(table5) %>% left_join(table6) %>% 
  select(NAME,group, variable, estimate,moe, summary_est, summary_moe) %>% 
  left_join(label) %>% select(label, everything()) # join table

tbl_count <- table %>% 
  filter(label %in% edu_cat4) %>% 
  mutate(topic_type = case_when(
    label=="Total!!Male!!Less than high school diploma" ~"Less than HS",
    label=="Total!!Female!!Less than high school diploma" ~"Less than HS",
    label=="Total!!Male!!High school graduate (includes equivalency)" ~"HS or GED",
    label=="Total!!Female!!High school graduate (includes equivalency)" ~"HS or GED",
    label=="Total!!Male!!Some college or associate's degree" ~"Some College or AA",
    label=="Total!!Female!!Some college or associate's degree" ~"Some College or AA",
    label=="Total!!Male!!Bachelor's degree or higher" ~"BA or higher",
    label=="Total!!Female!!Bachelor's degree or higher" ~"BA or higher")) %>% 
  mutate(topic = "edu", estimate_type = "count") %>% 
  group_by(NAME, topic_type) %>% 
  mutate(estimate = sum(estimate),
         est_moe = sum(moe)) %>% 
  select(NAME, group, topic, topic_type, 
         estimate_type, estimate, est_moe, summary_est, 
         summary_moe) %>% unique() %>% 
  ungroup() %>% 
  mutate(estimate = case_when(
    est_moe <= .25*estimate ~estimate,
    TRUE ~NA_real_)) %>% 
  mutate(prop = case_when(
    estimate = 0 ~ 0,
    summary_est = 0 ~0,
    TRUE ~estimate / summary_est)) %>% 
  mutate(prop = case_when(
    is.na(estimate)=T ~NA_real_.
    TRUE ~prop))
  ))
