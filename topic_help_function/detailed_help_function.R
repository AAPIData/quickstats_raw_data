detailed_total_updatter <- function(){
  
  #first of set up the grabber function
  detailed_grabber <- function(geo, year){
    
    #load data
    table1 <- get_acs(table = "B02015", geography = "state", year = 2016, summary_var = "B02015_001")
    table2 <- get_acs(table = "B02016", geography = "state", year = 2016, summary_var = "B02016_001")
    table3 <- get_acs(table = "B02018", geography = "state", year = 2016, summary_var = "B02018_001")
    table4 <- get_acs(table = "B02019", geography = "state", year = 2016, summary_var = "B02019_001")
    
    table1 <- table1 %>% mutate(group ="Asian Alone")
    table2 <- table2 %>% mutate(group="NHPI Alone")
    table3 <- table3 %>% mutate(group="Asian Alone or in Combo")
    table4 <- table4 %>% mutate(group="NHPI Alone or in Combo")
    
    table <- rbind(table1, table2, table3, table4)
    
    table <- table %>% 
      select(NAME, variable, estimate, group, moe, summary_est, summary_moe) %>% 
      left_join(label) %>% select(label, everything()) # join table
    
    tbl <- table %>% 
      separate(label, into=c("label", "topic_type"))
  }
  
}