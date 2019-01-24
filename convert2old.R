library(purrrlyr)
library(purrr)
#Need to Take raw_output of Detailed Pop Helper Function + Finalmerger and re-shape to Old version rdy

# Table Setup for old Quickstats:
detail_pop_state <- read_rds("raw_cleanup/dta_state.rds")
detail_pop_county <- read_rds("raw_cleanup/dta_county.rds")
detail_pop_district <- read_rds("raw_cleanup/dta_district.rds")

state_tbl_generator <- function(){
  # State Level Updating
  detail_pop_state %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="count") %>%
    select(State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/state_detail_aa_pop.csv",na="")
  detail_pop_state %>% 
    filter(group == "Asian Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/state_detail_aacombo_pop.csv",na="")
  detail_pop_state %>% 
    filter(group == "NHPI Alone") %>% 
    filter(estimate_type=="count") %>%
    select(State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/state_detail_nhpi_pop.csv",na="")
  detail_pop_state %>% 
    filter(group == "NHPI Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/state_detail_nhpicombo_pop.csv",na="")
 #Proportions
   detail_pop_state %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="prop") %>%
     mutate(`Total Asian Alone`=summary_est) %>% 
    select(State, `Total Population`, `Total Asian Alone`, topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/state_detail_aa_popshare.csv",na="")
   detail_pop_state %>% 
     filter(group == "NHPI Alone") %>% 
     filter(estimate_type=="prop") %>%
     mutate(`Total NHPI Alone`=summary_est) %>% 
     select(State, `Total Population`, `Total NHPI Alone`, topic_type, estimate) %>%
     spread(topic_type,estimate) %>% 
     write_csv("old_quickstats/state_detail_nhpi_popshare.csv",na="")
  
}
county_tbl_generator <- function(){
  # State Level Updating
  detail_pop_county %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="count") %>%
    select(County, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_aa_pop.csv",na="")
  detail_pop_county %>% 
    filter(group == "Asian Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(County, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_aacombo_pop.csv",na="")
  detail_pop_county %>% 
    filter(group == "NHPI Alone") %>% 
    filter(estimate_type=="count") %>%
    select(County, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_nhpi_pop.csv",na="")
  detail_pop_county %>% 
    filter(group == "NHPI Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(County, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_nhpicombo_pop.csv",na="")
# PROPORTION
  detail_pop_county %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="prop") %>%
    mutate(`Total Asian Alone`=summary_est) %>% 
    select(County,State, `Total Population`, `Total Asian Alone`, topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_aa_popshare.csv",na="")
  detail_pop_county %>% 
    filter(group == "NHPI Alone") %>% 
    filter(estimate_type=="prop") %>%
    mutate(`Total NHPI Alone`=summary_est) %>% 
    select(County,State, `Total Population`, `Total NHPI Alone`, topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/county_detail_nhpi_popshare.csv",na="")
  
  
  }
district_tbl_generator <- function(){
  # State Level Updating
  detail_pop_district %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="count") %>%
    select(District, State, `Total Population`,topic_type, estimate,) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_aa_pop.csv",na="")
  detail_pop_district %>% 
    filter(group == "Asian Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(District, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_aacombo_pop.csv",na="")
  detail_pop_district %>% 
    filter(group == "NHPI Alone") %>% 
    filter(estimate_type=="count") %>%
    select(District, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_nhpi_pop.csv",na="")
  detail_pop_district %>% 
    filter(group == "NHPI Alone or in Combo") %>% 
    filter(estimate_type=="count") %>%
    select(District, State, `Total Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_nhpicombo_pop.csv",na="")
  # Proportin
  detail_pop_district %>% 
    filter(group == "Asian Alone") %>% 
    filter(estimate_type=="prop") %>%
    mutate(`Total Asian Alone`=summary_est) %>% 
    select(District,State, `Total Population`, `Total Asian Alone`, topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_aa_popshare.csv",na="")
  detail_pop_district %>% 
    filter(group == "NHPI Alone") %>% 
    filter(estimate_type=="prop") %>%
    mutate(`Total NHPI Alone`=summary_est) %>% 
    select(District,State, `Total Population`, `Total NHPI Alone`, topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    write_csv("old_quickstats/district_detail_nhpi_popshare.csv",na="")
}

state_tbl_generator()
county_tbl_generator()
district_tbl_generator()


# Education Tables --------------------------------------------------------
edu_state <- read_rds("raw_cleanup/dta_state.rds")
edu_county <- read_rds("raw_cleanup/dta_county.rds")
edu_district <- read_rds("raw_cleanup/dta_district.rds")

edu_state_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="count") %>%
    select(State,`Group Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>%
    select(State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/state_edu_",filename,"_count.csv",sep=""),na="")
  
  temp_prop <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="prop") %>%
    select(State,`Group Population`,topic_type, estimate) %>%
    mutate(estimate = round(estimate *100,2)) %>% 
    spread(topic_type,estimate) %>% 
    select(State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/state_edu_",filename,"_prop.csv",sep=""),na="")
  
  
}
edu_county_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="count") %>%
    select(County,State,`Group Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    select(County,State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/county_edu_",filename,"_count.csv",sep=""),na="")
  
  temp_prop <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="prop") %>%
    select(County,State,`Group Population`,topic_type, estimate) %>%
    mutate(estimate = round(estimate *100,2)) %>% 
    spread(topic_type,estimate) %>% 
    select(County,State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/county_edu_",filename,"_prop.csv",sep=""),na="")
  
  
}
edu_district_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="count") %>%
    select(District,State,`Group Population`,topic_type, estimate) %>%
    mutate(estimate = round(estimate *100,2)) %>% 
    spread(topic_type,estimate) %>% 
    select(District,State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/district_edu_",filename,"_count.csv",sep=""),na="")
  
  temp_prop <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="prop") %>%
    select(District,State,`Group Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    select(District,State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/district_edu_",filename,"_prop.csv",sep=""),na="")
  
  
}

dta_list1 <- list(edu_state)
dta_list2 <- list(edu_county)
dta_list3 <- list(edu_district)
group_name <- c("Asian Alone", "NHPI Alone")
filename <- c("aa","nhpi")
arg_list1 <- list(dta_list1,group_name,filename)
arg_list2 <- list(dta_list2,group_name,filename)
arg_list3<- list(dta_list3,group_name,filename)

# cross3(dta_list, group_name, filename)

pmap(arg_list1,edu_state_composer)
pmap(arg_list2,edu_county_composer)
pmap(arg_list3,edu_district_composer)


# Insurance ---------------------------------------------------------------
ins_state <- read_rds("raw_cleanup/dta_state.rds")
ins_county <- read_rds("raw_cleanup/dta_county.rds")
ins_district <- read_rds("raw_cleanup/dta_district.rds")

ins_state_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    select(State,`Group Population`,topic_type,estimate_type, estimate)  %>%
    spread(estimate_type,estimate) %>% 
    rename(`No Health INS`=count,
           `Share No Health INS`=prop) %>%
    select(-topic_type) %>% 
    write_csv(paste("old_quickstats/state_ins_",filename,".csv",sep=""),na="")
}
ins_county_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    select(County,State,`Group Population`,topic_type,estimate_type, estimate)  %>%
    spread(estimate_type,estimate) %>% 
    rename(`No Health INS`=count,
           `Share No Health INS`=prop) %>%
    select(-topic_type) %>% 
    write_csv(paste("old_quickstats/county_ins_",filename,".csv",sep=""),na="")
  
}
ins_district_composer <- function(dta,group_name,filename){
  temp_counts <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    select(District,State,`Group Population`,topic_type,estimate_type, estimate)  %>%
    spread(estimate_type,estimate) %>% 
    rename(`No Health INS`=count,
           `Share No Health INS`=prop) %>%
    select(-topic_type) %>% 
    write_csv(paste("old_quickstats/district_ins_",filename,".csv",sep=""),na="")
  

  
  
}

dta_list1 <- list(ins_state)
dta_list2 <- list(ins_county)
dta_list3 <- list(ins_district)
group_name <- c("Asian Alone", "NHPI Alone")
filename <- c("aa","nhpi")
arg_list1 <- list(dta_list1,group_name,filename)
arg_list2 <- list(dta_list2,group_name,filename)
arg_list3<- list(dta_list3,group_name,filename)

pmap(arg_list1,ins_state_composer)
pmap(arg_list2,ins_county_composer)
pmap(arg_list3,ins_district_composer)

str(ins_state)
ins_state_composer(ins_state,"Asian Alone","aa")


# AAPI Population ---------------------------------------------------------

# Table Setup for old Quickstats:
aapi_pop_state <- read_rds("raw_cleanup/dta_state.rds")
aapi_pop_county <- read_rds("raw_cleanup/dta_county.rds")
aapi_pop_district <- read_rds("raw_cleanup/dta_district.rds")

temp1 <- aapi_pop_state %>% 
  filter(estimate_type=="count") %>% 
  select(-topic,-estimate_type) %>% 
  spread(topic_type, estimate) %>% 
  rename(`Total Population`=summary_est) %>% 
  arrange(State)
temp2 <- aapi_pop_state %>% 
  filter(estimate_type=="prop") %>% 
  select(-topic,-estimate_type,-summary_est) %>% 
  mutate(topic_type= paste("Share ",topic_type,sep="")) %>% 
  spread(topic_type, estimate) %>% 
  arrange(State)
temp_final <- left_join(temp1,temp2)  %>% 
  select(State, `Total Population`, `Asian Alone`,`Share Asian Alone`,
         `Asian Combo`, `Share Asian Combo`, `NHPI Alone`, `Share NHPI Alone`,
         `NHPI Combo`, `Share NHPI Combo`)  %>% 
  write_csv("old_quickstats/state_aapi_pop.csv",na="")
rm(temp1,temp2, temp_final)


temp1 <- aapi_pop_county %>% 
  filter(estimate_type=="count") %>% 
  select(-topic,-estimate_type) %>% 
  spread(topic_type, estimate) %>% 
  rename(`Total Population`=summary_est) %>% 
  arrange(State,County)
temp2 <- aapi_pop_county %>% 
  filter(estimate_type=="prop") %>% 
  select(-topic,-estimate_type,-summary_est) %>% 
  mutate(topic_type= paste("Share ",topic_type,sep="")) %>% 
  spread(topic_type, estimate) %>% 
  arrange(State,County)
temp_final <- left_join(temp1,temp2)  %>% 
  select(County,State, `Total Population`, `Asian Alone`,`Share Asian Alone`,
         `Asian Combo`, `Share Asian Combo`, `NHPI Alone`, `Share NHPI Alone`,
         `NHPI Combo`, `Share NHPI Combo`)  %>% 
  write_csv("old_quickstats/county_aapi_pop.csv",na="")
rm(temp1,temp2, temp_final)
temp1 <- aapi_pop_district %>% 
  filter(estimate_type=="count") %>% 
  select(-topic,-estimate_type) %>% 
  spread(topic_type, estimate) %>% 
  rename(`Total Population`=summary_est) %>% 
  arrange(State,District)
temp2 <- aapi_pop_district %>% 
  filter(estimate_type=="prop") %>% 
  select(-topic,-estimate_type,-summary_est) %>% 
  mutate(topic_type= paste("Share ",topic_type,sep="")) %>% 
  spread(topic_type, estimate) %>% 
  arrange(State,District)
temp_final <- left_join(temp1,temp2)  %>% 
  select(District,State, `Total Population`, `Asian Alone`,`Share Asian Alone`,
         `Asian Combo`, `Share Asian Combo`, `NHPI Alone`, `Share NHPI Alone`,
         `NHPI Combo`, `Share NHPI Combo`)  %>% 
  write_csv("old_quickstats/district_aapi_pop.csv",na="")
rm(temp1,temp2, temp_final)

# LEP ---------------------------------------------------------------------
LEP_state <- read_rds("raw_cleanup/dta_state.rds")
LEP_county <- read_rds("raw_cleanup/dta_county.rds")
LEP_district <- read_rds("raw_cleanup/dta_district.rds")
#STATE LEVEL
state_lep_aa <- LEP_state %>% 
  filter(group=="Asian Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`Asian Alone LEP` = count,
         `LEP Share Asian Alone`= prop,
         `Asian Alone Total`=`Group Population`) 
state_lep_nhpi <- LEP_state %>% 
  filter(group=="NHPI Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`NHPI Alone LEP` = count,
         `LEP Share NHPI Alone`= prop,
         `NHPI Alone Total`=`Group Population`) 
state_lep_final <- left_join(state_lep_aa,state_lep_nhpi)
state_lep_final %>% write_csv("old_quickstats/state-lep-all-alone.csv",na="")

#County LEVEL
county_lep_aa <- LEP_county %>% 
  filter(group=="Asian Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(County,State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`Asian Alone LEP` = count,
         `LEP Share Asian Alone`= prop,
         `Asian Alone Total`=`Group Population`) 
county_lep_nhpi <- LEP_county %>% 
  filter(group=="NHPI Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(County,State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`NHPI Alone LEP` = count,
         `LEP Share NHPI Alone`= prop,
         `NHPI Alone Total`=`Group Population`) 
county_lep_final <- left_join(county_lep_aa,county_lep_nhpi)
county_lep_final %>% write_csv("old_quickstats/county-lep-all-alone.csv",na="")


#District LEVEL
district_lep_aa <- LEP_district %>% 
  filter(group=="Asian Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(District,State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`Asian Alone LEP` = count,
         `LEP Share Asian Alone`= prop,
         `Asian Alone Total`=`Group Population`) 
district_lep_nhpi <- LEP_district %>% 
  filter(group=="NHPI Alone") %>% 
  # filter(estimate_type=="count") %>% 
  # select(-prefix, -ending,) %>% 
  spread(estimate_type, estimate) %>% 
  select(District,State, `Group Population`, count,prop) %>% 
  mutate(prop=round(prop*100,2)) %>% 
  rename(`NHPI Alone LEP` = count,
         `LEP Share NHPI Alone`= prop,
         `NHPI Alone Total`=`Group Population`) 
district_lep_final <- left_join(district_lep_aa,district_lep_nhpi)
district_lep_final %>% write_csv("old_quickstats/district-lep-all-alone.csv",na="")

# NATIVITY + POVERTY NOT WORKING, did it manually :(
# # POVERTY
# ins_state <- read_rds("raw_cleanup/dta_state.rds")
# ins_county <- read_rds("raw_cleanup/dta_county.rds")
# ins_district <- read_rds("raw_cleanup/dta_district.rds")
# 
# 
# # NATIVITY
# nativity_state <- read_rds("raw_cleanup/dta_state.rds")
# nativity_county <- read_rds("raw_cleanup/dta_county.rds")
# nativity_district <- read_rds("raw_cleanup/dta_district.rds")
# 
# #NATIVITY STATE
# temp1 <- nativity_state %>% 
#   filter(estimate_type=="count") %>% 
#   filter(group=="Asian Alone") %>% 
#   select(-topic,-estimate_type) %>% 
#   spread(topic_type, estimate) %>% 
#   rename(`Total Population`=summary_est) %>% 
#   arrange(State)
# temp2 <- nativity_state %>% 
#   filter(estimate_type=="prop") %>% 
#   select(-topic,-estimate_type,-summary_est) %>% 
#   mutate(topic_type= paste("Share ",topic_type,sep="")) %>% 
#   spread(topic_type, estimate) %>% 
#   arrange(State)
# temp_final <- left_join(temp1,temp2)  %>% 
#   select(State, `Total Population`, `Asian Alone`,`Share Asian Alone`,
#          `Asian Combo`, `Share Asian Combo`, `NHPI Alone`, `Share NHPI Alone`,
#          `NHPI Combo`, `Share NHPI Combo`)  %>% 
#   write_csv("old_quickstats/state_aapi_pop.csv",na="")
# 
