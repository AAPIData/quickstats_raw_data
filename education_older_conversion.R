temp_counts <- edu_state %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="count") %>%
  select(State,`Group Population`,topic_type, estimate) %>%
  rename(`Asian Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("Asian Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_state %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("Asian Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate)
temp_asian <- left_join(temp_counts,temp_prop)

rm(temp_counts,temp_prop)
temp_counts <- edu_state %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="count") %>%
  select(State,`Group Population`,topic_type, estimate) %>%
  rename(`NHPI Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("NHPI Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_state %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("NHPI Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate) 
temp_nhpi <- left_join(temp_counts,temp_prop)

temp_final <- left_join(temp_asian,temp_nhpi)
temp_final %>% colnames()
temp_final %>% select(State, `Asian Alone Total`, `Asian Alone Less than HS`, `Asian Alone HS or GED`,
                      `Asian Alone Some College or AA`,`Asian Alone BA or higher`, `Asian Alone Share Less than HS`, `Asian Alone Share HS or GED`,`Asian Alone Share Some College or AA`,`Asian Alone Share BA or higher`,
                      `NHPI Alone Total`, `NHPI Alone Less than HS`, `NHPI Alone HS or GED`,
                      `NHPI Alone Some College or AA`,`NHPI Alone BA or higher`, `NHPI Alone Share Less than HS`, `NHPI Alone Share HS or GED`,`NHPI Alone Share Some College or AA`,`NHPI Alone Share BA or higher`) %>%  
  write_csv("old_quickstats/state_edu_all.csv",na="")


# COUNTY ------------------------------------------------------------------

temp_counts <- edu_county %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="count") %>%
  select(County,State,`Group Population`,topic_type, estimate) %>%
  rename(`Asian Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("Asian Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_county %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(County,State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("Asian Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate)
temp_asian <- left_join(temp_counts,temp_prop)

rm(temp_counts,temp_prop)
temp_counts <- edu_county %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="count") %>%
  select(County,State,`Group Population`,topic_type, estimate) %>%
  rename(`NHPI Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("NHPI Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_county %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(County,State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("NHPI Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate) 
temp_nhpi <- left_join(temp_counts,temp_prop)

temp_final <- left_join(temp_asian,temp_nhpi)
temp_final %>% colnames()
temp_final %>% select(County,State, `Asian Alone Total`, `Asian Alone Less than HS`, `Asian Alone HS or GED`,
                      `Asian Alone Some College or AA`,`Asian Alone BA or higher`, `Asian Alone Share Less than HS`, `Asian Alone Share HS or GED`,`Asian Alone Share Some College or AA`,`Asian Alone Share BA or higher`,
                      `NHPI Alone Total`, `NHPI Alone Less than HS`, `NHPI Alone HS or GED`,
                      `NHPI Alone Some College or AA`,`NHPI Alone BA or higher`, `NHPI Alone Share Less than HS`, `NHPI Alone Share HS or GED`,`NHPI Alone Share Some College or AA`,`NHPI Alone Share BA or higher`) %>%  
  write_csv("old_quickstats/county_edu_all.csv",na="")



# DISTRICT ----------------------------------------------------------------

temp_counts <- edu_district %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="count") %>%
  select(District,State,`Group Population`,topic_type, estimate) %>%
  rename(`Asian Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("Asian Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_district %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(District,State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("Asian Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate)
temp_asian <- left_join(temp_counts,temp_prop)

rm(temp_counts,temp_prop)
temp_counts <- edu_district %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="count") %>%
  select(District,State,`Group Population`,topic_type, estimate) %>%
  rename(`NHPI Alone Total`=`Group Population`) %>% 
  mutate(topic_type= paste("NHPI Alone",topic_type,sep = " ")) %>% 
  spread(topic_type,estimate)

temp_prop <- edu_district %>% 
  rename(`Group Population`=summary_est) %>% 
  filter(group == "NHPI Alone") %>% 
  filter(estimate_type=="prop") %>%
  select(District,State,topic_type, estimate) %>%
  mutate(estimate = round(estimate *100,2)) %>% 
  mutate(topic_type= paste("NHPI Alone Share ",topic_type,sep = "")) %>% 
  spread(topic_type,estimate) 
temp_nhpi <- left_join(temp_counts,temp_prop)

temp_final <- left_join(temp_asian,temp_nhpi)
temp_final %>% colnames()
temp_final %>% select(District,State, `Asian Alone Total`, `Asian Alone Less than HS`, `Asian Alone HS or GED`,
                      `Asian Alone Some College or AA`,`Asian Alone BA or higher`, `Asian Alone Share Less than HS`, `Asian Alone Share HS or GED`,`Asian Alone Share Some College or AA`,`Asian Alone Share BA or higher`,
                      `NHPI Alone Total`, `NHPI Alone Less than HS`, `NHPI Alone HS or GED`,
                      `NHPI Alone Some College or AA`,`NHPI Alone BA or higher`, `NHPI Alone Share Less than HS`, `NHPI Alone Share HS or GED`,`NHPI Alone Share Some College or AA`,`NHPI Alone Share BA or higher`) %>%  
  write_csv("old_quickstats/district_edu_all.csv",na="")

