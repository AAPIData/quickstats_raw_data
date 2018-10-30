Library(glue)

final_merger <- function(update_topics){
# Load Helper Function for Poverty
source("topic_help_function/poverty_help_function.R")

# Each helper function needs to output with following columns: geo, NAME, group, topic, topic_type, estimate_type, estimate
# geo values = [state, county, district]
# NAME= [raw name from tidycensus, we will disagg later after we split by geo]
# group = [Asian Alone, NHPI Alone, Black Alone, Hispanic Alone, White Alone]
# topic = [poverty, education, etc.]
# topic_type = [Below Poverty, BA or Higher, etc.]
# estimate_type = [count, prop]
# estimate = [the actual value]


# Conditionally update each topic -----------------------------------------
# NEED SOME CODE HERE TO RUN HELPER FUNCTION IF IT"S INCLUDED IN "update_topics"
# Right now I coded the poverty function to just automatically update everything (all geographies + all racial groups, but we might want to alter this)

if(update_topics %in% "poverty"){
  print("Looks like you want to update Poverty!")
  temp_df <- poverty_total_updater()
}

# else if(update_topics %in% "education"){
#   temp_df2 <- education_total_updater()
# }


# Combine them ------------------------------------------------------------
### SOME CODE HERE
combined_df <- temp_df


rm(temp_df) #Delete and clear space
gc()
# combined_df will have all the topics +geos? 

# Disaggregate by geography -----------------------------------------------

# This should take the combined_df and subset it by geography

# State-Level data --------------------------------------------------------
print("Subsetting data for State-level File \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="state") %>% 
  select(-geo) %>% 
  rename(State=NAME) %>% 
  write_rds("cleaned_data/dta_state.rds")


# County ------------------------------------------------------------------
print("Subsetting data for County-level File. \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="county") %>% 
  select(-geo) %>% 
  separate(NAME, into = c("County","State"),sep=", ") %>% 
  write_rds("cleaned_data/dta_county.rds")


# Congressional district --------------------------------------------------
print("Subsetting data for District-level File \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="district") %>% 
  select(-geo) %>% 
  separate(NAME, into = c("District","State"),sep=", ") %>% 
  mutate(District = str_remove(District,"Congressional ")) %>% 
  mutate(District = str_remove(District, " \\(115th Congress\\)")) %>% 
  write_rds("cleaned_data/dta_district.rds")
}

