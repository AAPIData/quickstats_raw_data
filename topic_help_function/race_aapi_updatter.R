race_aapi_updatter <- function(){
  
  #first of set up the grabber function
  aapi_grabber <- function(geo, year){
    
    #load data
    table <- get_acs(variables =c('B02001_005',"B02001_006","B02011_001","B02012_001"),
                     geography =geo, year = year, summary_var = "B02001_001") %>% 
      select(NAME, variable, estimate, moe, summary_est)
  
    tbl <- table %>% 
      mutate(topic_type = case_when(
        variable == 'B02001_005' ~ "Asian Alone",
        variable == 'B02001_006' ~ "NHPI Alone",
        variable == 'B02011_001' ~ "Asian Combo",
        variable == 'B02012_001' ~ "NHPI Combo")) %>% 
      mutate(estimate = case_when(
        moe <= .25*estimate ~estimate,
        TRUE ~NA_real_)) %>% 
      select(NAME,topic_type, estimate, summary_est) %>% 
      mutate(prop = round(estimate/summary_est*100,2),
             topic = "AAPI Population",
             geo = geo) %>% 
      select(geo, NAME, topic,topic_type, estimate, prop,summary_est)
    
    tbl_count <- tbl %>% select(-prop) %>% 
      mutate(estimate_type = "count")
    
    tbl_prop <- tbl %>% select(-estimate) %>% 
      mutate(estimate_type = "prop") %>% 
      rename(estimate = prop)
    
    tbl_final <- rbind(tbl_count, tbl_prop)

    return(tbl_final)
  }
  geo <- c("state", "county", "congressional district")
  year <- c(2017, 2017, 2017)
  argList <- list(geo, year)
  
  final <- pmap_dfr(argList,aapi_grabber) # Running pov_grabber
  return(final)
}