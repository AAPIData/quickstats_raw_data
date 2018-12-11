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
    filter(estimate_type=="count") %>%
    select(State,`Group Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) 
    write_csv(paste("old_quickstats/state_ins_",filename,"_count.csv",sep=""),na="")
  
  temp_prop <- dta %>% 
    rename(`Group Population`=summary_est) %>% 
    filter(group == group_name) %>% 
    filter(estimate_type=="prop") %>%
    select(State,`Group Population`,topic_type, estimate) %>%
    spread(topic_type,estimate) %>% 
    select(State, `Group Population`,`Less than HS`,`HS or GED`,`Some College or AA`, `BA or higher`) %>% 
    write_csv(paste("old_quickstats/state_edu_",filename,"_prop.csv",sep=""),na="")
  
  
}

ins_state %>% count(group)
ins_state %>% 
  # rename(`Group Population`=summary_est) %>% 
  filter(group == "Asian Alone") %>% 
  filter(estimate_type=="count") %>%
  select(State,`Group Population`,topic_type, estimate) %>%
  spread(topic_type,estimate) 
